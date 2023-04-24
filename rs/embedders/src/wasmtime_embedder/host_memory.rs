use anyhow::bail;
use ic_types::MAX_STABLE_MEMORY_IN_BYTES;
use wasmtime::MemoryType;
use wasmtime_environ::{WASM32_MAX_PAGES, WASM_PAGE_SIZE};

use crate::LinearMemory;

use ic_sys::PAGE_SIZE;

use libc::c_void;
use libc::MAP_FAILED;
use libc::{mmap, munmap};
use libc::{MAP_ANON, MAP_PRIVATE, PROT_NONE};

use std::collections::HashMap;
use std::io::Error;
use std::ops::Deref;
use std::ptr;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc, Mutex,
};

pub fn round_up_to_page_size(size: usize, page_size: usize) -> usize {
    (size + (page_size - 1)) & !(page_size - 1)
}

fn round_up_to_os_page_size(size: usize) -> usize {
    round_up_to_page_size(size, PAGE_SIZE)
}

#[derive(Hash, PartialEq, Eq)]
pub(crate) struct MemoryStart(pub(crate) usize);

pub(crate) struct MemoryPageSize(Arc<AtomicUsize>);

impl Deref for MemoryPageSize {
    type Target = Arc<AtomicUsize>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct WasmtimeMemoryCreator {
    created_memories: Arc<Mutex<HashMap<MemoryStart, MemoryPageSize>>>,
}

impl WasmtimeMemoryCreator {
    pub(crate) fn new(created_memories: Arc<Mutex<HashMap<MemoryStart, MemoryPageSize>>>) -> Self {
        Self { created_memories }
    }
}

unsafe impl wasmtime::MemoryCreator for WasmtimeMemoryCreator {
    fn new_memory(
        &self,
        ty: MemoryType,
        _minimum: usize,
        _maximum: Option<usize>,
        reserved_size_in_bytes: Option<usize>,
        guard_size: usize,
    ) -> Result<Box<dyn wasmtime::LinearMemory>, String> {
        // We don't use the `reserved_size_in_bytes` because the size of the
        // memory allocation is determined based on the memory type: 64-bit
        // memories have size at most the maximum stable memory size and 32-bit
        // memories have size at most 4GiB. So we always allocate that amount
        // (unless the module explicitly lists a smaller maximum).
        //
        // If we get a `reserved_size_in_bytes` that exceeds the max stable
        // memory size then there has been a change in our setting of the
        // `static_memory_maximum_size` for the `wasmtime::Config` which isn't
        // compatible with the memory sizes we expect to see here.
        if let Some(reserved_size) = reserved_size_in_bytes {
            assert!(
                reserved_size <= MAX_STABLE_MEMORY_IN_BYTES as usize,
                "Reserved bytes for wasm memory {} exceeds the maximum expected {}",
                reserved_size,
                MAX_STABLE_MEMORY_IN_BYTES
            )
        }
        let max_pages = if ty.is_64() {
            MAX_STABLE_MEMORY_IN_BYTES / (WASM_PAGE_SIZE as u64)
        } else {
            WASM32_MAX_PAGES
        };
        let min = std::cmp::min(ty.minimum(), max_pages) as usize;
        let max = std::cmp::min(ty.maximum().unwrap_or(max_pages), max_pages) as usize;

        let mem = MmapMemory::new(max * WASM_PAGE_SIZE as usize, guard_size);

        match self.created_memories.lock() {
            Err(err) => Err(format!("Error locking map of created memories: {:?}", err)),
            Ok(mut created_memories) => {
                let new_memory = WasmtimeMemory::new(mem, min, max);
                created_memories.insert(
                    MemoryStart(wasmtime::LinearMemory::as_ptr(&new_memory) as usize),
                    MemoryPageSize(Arc::clone(&new_memory.used)),
                );
                Ok(Box::new(new_memory))
            }
        }
    }
}

/// Represents Wasm memory together with its prologue and epilogue guard regions:
/// `[prologue guard region][Wasm memory][epilogue guard region]`.
/// The guard regions are unmapped and catch out-of-bounds accesses. Note that
/// the prologue guard region is not necessary for correctness, we use as a
/// safety measure to improve security.
pub struct MmapMemory {
    // The address of the prologue guard region.
    start: *mut c_void,
    // The size of the entire region including all guard regions.
    size_in_bytes: usize,
    // The start of the actual memory exposed to Wasm.
    wasm_memory: *mut c_void,
}

/// SAFETY: This type is not actually Send/Sync but this it is only used
/// internally by `wasmtime` where they should be synchronizing access to the
/// pointers themselves.
unsafe impl Send for MmapMemory {}
unsafe impl Sync for MmapMemory {}

/// The minimal required guard region for correctness is 2GiB. We use 4GiB as a
/// safety measure since the allocation happens in the virtual memory and its
/// overhead is negligible.
const MIN_GUARD_REGION_SIZE: usize = 4 * 1024 * 1024 * 1024;

impl MmapMemory {
    pub fn new(mem_size_in_bytes: usize, guard_size_in_bytes: usize) -> Self {
        let guard_size_in_bytes =
            round_up_to_os_page_size(guard_size_in_bytes.max(MIN_GUARD_REGION_SIZE));
        let prologue_guard_size_in_bytes = guard_size_in_bytes;
        let epilogue_guard_size_in_bytes = guard_size_in_bytes;
        let size_in_bytes = prologue_guard_size_in_bytes
            + round_up_to_os_page_size(mem_size_in_bytes)
            + epilogue_guard_size_in_bytes;

        // SAFETY: These are valid arguments to `mmap`. Only `mem_size` is non-constant,
        // but any `usize` will result in a valid call.
        //
        // It is important to reserve the memory with PROT_NONE. Otherwise,
        // depending on the overcommit strategy configured in the kernel, the
        // call to mmap may fail. See:
        // https://www.kernel.org/doc/Documentation/vm/overcommit-accounting
        let start = unsafe {
            mmap(
                ptr::null_mut(),
                size_in_bytes,
                PROT_NONE,
                MAP_PRIVATE | MAP_ANON,
                -1,
                0,
            )
        };
        assert_ne!(
            start,
            MAP_FAILED,
            "mmap failed: size={} {}",
            size_in_bytes,
            Error::last_os_error()
        );

        // SAFETY: The allocated region includes the prologue guard region.
        let wasm_memory =
            unsafe { (start as *mut u8).add(prologue_guard_size_in_bytes) as *mut c_void };

        Self {
            start,
            size_in_bytes,
            wasm_memory,
        }
    }
}

impl LinearMemory for MmapMemory {
    fn as_ptr(&self) -> *mut c_void {
        self.wasm_memory
    }
}

impl Drop for MmapMemory {
    fn drop(&mut self) {
        let result = unsafe { munmap(self.start, self.size_in_bytes) };
        assert_eq!(result, 0, "munmap failed: {}", Error::last_os_error());
    }
}

pub struct WasmtimeMemory<M: LinearMemory> {
    mem: M,
    maximum: usize,
    used: MemoryPageSize,
}

impl<M: LinearMemory + Send> WasmtimeMemory<M> {
    fn new(mem: M, min: usize, maximum: usize) -> Self {
        Self {
            mem,
            maximum,
            used: MemoryPageSize(Arc::new(AtomicUsize::new(min))),
        }
    }
}

fn convert_pages_to_bytes(pages: usize) -> usize {
    let (result, overflow) = pages.overflowing_mul(WASM_PAGE_SIZE as usize);
    if overflow {
        panic!("Unable to convert memory page size {} to bytes", pages)
    }
    result
}

unsafe impl<M: LinearMemory + Send + Sync + 'static> wasmtime::LinearMemory for WasmtimeMemory<M> {
    /// Returns the number of allocated wasm pages.
    fn byte_size(&self) -> usize {
        convert_pages_to_bytes(self.used.load(Ordering::SeqCst))
    }

    fn maximum_byte_size(&self) -> Option<usize> {
        Some(convert_pages_to_bytes(self.maximum))
    }

    fn grow_to(&mut self, new_size: usize) -> anyhow::Result<()> {
        if new_size % WASM_PAGE_SIZE as usize != 0 {
            bail!(
                "Requested wasm page size increase wasn't a multiple of the wasm page size: {}",
                new_size
            )
        }
        let new_pages = new_size / WASM_PAGE_SIZE as usize;
        match self
            .used
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |prev_pages| {
                if new_pages <= prev_pages || new_pages > self.maximum {
                    None
                } else {
                    Some(new_pages)
                }
            }) {
            Ok(_) => Ok(()),
            Err(prev_pages) => bail!(
                "Unable to grow wasm memory from {} pages to {} pages",
                prev_pages,
                new_pages
            ),
        }
    }

    fn as_ptr(&self) -> *mut u8 {
        self.mem.as_ptr() as *mut _
    }
}
