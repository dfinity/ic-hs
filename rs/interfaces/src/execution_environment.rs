//! The execution environment public interface.
mod errors;

pub use errors::{CanisterOutOfCyclesError, HypervisorError, TrapCode};
use ic_base_types::NumBytes;
use ic_error_types::UserError;
use ic_ic00_types::EcdsaKeyId;
use ic_registry_provisional_whitelist::ProvisionalWhitelist;
use ic_registry_subnet_type::SubnetType;
use ic_sys::{PageBytes, PageIndex};
use ic_types::{
    crypto::canister_threshold_sig::MasterEcdsaPublicKey,
    ingress::{IngressStatus, WasmResult},
    messages::{
        AnonymousQuery, AnonymousQueryResponse, CertificateDelegation, HttpQueryResponse,
        MessageId, SignedIngressContent, UserQuery,
    },
    CpuComplexity, Cycles, ExecutionRound, Height, NumInstructions, NumPages, Randomness, Time,
};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::sync::Arc;
use std::{collections::BTreeMap, ops};
use std::{convert::Infallible, fmt};
use tower::{limit::ConcurrencyLimit, util::BoxCloneService};

/// Instance execution statistics. The stats are cumulative and
/// contain measurements from the point in time when the instance was
/// created up until the moment they are requested.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct InstanceStats {
    /// Total number of (host) pages accessed (read or written) by the instance
    /// and loaded into the linear memory.
    pub accessed_pages: usize,

    /// Total number of (host) pages modified by the instance.
    /// By definition a page that has been dirtied has also been accessed,
    /// hence this dirtied_pages <= accessed_pages
    pub dirty_pages: usize,

    /// Number of times a write access is handled when the page has already been
    /// read.
    pub read_before_write_count: usize,

    /// Number of times a write access is handled when the page has not yet been
    /// read.
    pub direct_write_count: usize,
}

/// Errors that can be returned when fetching the available memory on a subnet.
#[derive(Debug)]
pub enum SubnetAvailableMemoryError {
    InsufficientMemory {
        requested_total: NumBytes,
        message_requested: NumBytes,
        wasm_custom_sections_requested: NumBytes,
        available_total: i64,
        available_messages: i64,
        available_wasm_custom_sections: i64,
    },
}

/// Performance counter type.
#[derive(Debug)]
pub enum PerformanceCounterType {
    // The number of WebAssembly instructions the canister has executed based on
    // the given `i64` instruction counter.
    Instructions(i64),
}

/// Tracks the execution complexity.
///
/// Each execution has an associated complexity, i.e. how much CPU, memory,
/// disk or network bandwidth it takes.
///
/// For now, the complexity counters do not translate into Cycles, but they are rather
/// used to prevent too complex messages to slow down the whole subnet.
///
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct ExecutionComplexity {
    /// Accumulated CPU complexity, in instructions.
    pub cpu: CpuComplexity,
    /// The number of dirty pages in stable memory.
    pub stable_dirty_pages: NumPages,
}

impl ExecutionComplexity {
    /// Execution complexity with maximum values.
    pub const MAX: Self = Self {
        cpu: CpuComplexity::new(i64::MAX),
        stable_dirty_pages: NumPages::new(u64::MAX),
    };

    /// Creates execution complexity with a specified CPU complexity.
    pub fn with_cpu(cpu: NumInstructions) -> Self {
        Self {
            cpu: (cpu.get() as i64).into(),
            ..Default::default()
        }
    }

    /// Returns true if the CPU complexity reached the specified
    /// instructions limit.
    pub fn cpu_reached(&self, limit: NumInstructions) -> bool {
        self.cpu.get() >= limit.get() as i64
    }

    /// Returns the maximum of each complexity.
    pub fn max(&self, rhs: Self) -> Self {
        Self {
            cpu: self.cpu.max(rhs.cpu),
            stable_dirty_pages: self.stable_dirty_pages.max(rhs.stable_dirty_pages),
        }
    }
}

impl ops::Add for &ExecutionComplexity {
    type Output = ExecutionComplexity;

    fn add(self, rhs: &ExecutionComplexity) -> ExecutionComplexity {
        ExecutionComplexity {
            cpu: self.cpu + rhs.cpu,
            stable_dirty_pages: self
                .stable_dirty_pages
                .get()
                .saturating_add(rhs.stable_dirty_pages.get())
                .into(),
        }
    }
}

impl ops::Sub for &ExecutionComplexity {
    type Output = ExecutionComplexity;

    fn sub(self, rhs: &ExecutionComplexity) -> ExecutionComplexity {
        ExecutionComplexity {
            cpu: self.cpu - rhs.cpu,
            stable_dirty_pages: self
                .stable_dirty_pages
                .get()
                .saturating_sub(rhs.stable_dirty_pages.get())
                .into(),
        }
    }
}

impl fmt::Display for ExecutionComplexity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ cpu = {} stable_dirty_pages = {} }}",
            self.cpu, self.stable_dirty_pages,
        )
    }
}

/// Tracks the available memory on a subnet. The main idea is to separately track
/// the total available memory, the message available memory and the wasm custom
/// sections available memory. When trying to allocate message memory or wasm
/// custom sections memory one can do this as long as there is sufficient total
/// memory as well as message or wasm custom sections memory available
/// respectively. When trying to allocate non-message or wasm custom sections
/// memory only the total memory needs to suffice.
///
/// Note that there are situations where total available memory is smaller than
/// the available message or wasm custom sections memory, i.e., when the memory
/// is consumed by something other than messages or wasm custom sections.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default)]
pub struct SubnetAvailableMemory {
    /// The total memory available on the subnet
    total_memory: i64,
    /// The memory available for messages
    message_memory: i64,
    /// The memory available for Wasm custom sections
    wasm_custom_sections_memory: i64,
}

impl SubnetAvailableMemory {
    pub fn new(total_memory: i64, message_memory: i64, wasm_custom_sections_memory: i64) -> Self {
        SubnetAvailableMemory {
            total_memory,
            message_memory,
            wasm_custom_sections_memory,
        }
    }

    /// Returns the total available memory.
    pub fn get_total_memory(&self) -> i64 {
        self.total_memory
    }

    /// Returns the memory available for messages, ignoring the totally available memory.
    pub fn get_message_memory(&self) -> i64 {
        self.message_memory
    }

    /// Returns the memory available for Wasm custom sections, ignoring the
    /// totally available memory.
    pub fn get_wasm_custom_sections_memory(&self) -> i64 {
        self.wasm_custom_sections_memory
    }

    /// Returns the maximal amount of memory that is available for messages.
    ///
    /// This amount is computed as the minimum of total available memory and available
    /// message memory. This is useful to decide whether it is still possible to allocate
    /// memory for messages.
    pub fn max_available_message_memory(&self) -> i64 {
        self.total_memory.min(self.message_memory)
    }

    /// Try to use some memory capacity and fail if not enough is available
    pub fn try_decrement(
        &mut self,
        requested: NumBytes,
        message_requested: NumBytes,
        wasm_custom_sections_requested: NumBytes,
    ) -> Result<(), SubnetAvailableMemoryError> {
        debug_assert!(requested >= message_requested);

        let is_available =
            |requested: NumBytes, available: i64| match i64::try_from(requested.get()) {
                Ok(x) => x <= available || x == 0,
                Err(_) => false,
            };

        if is_available(requested, self.total_memory)
            && is_available(message_requested, self.message_memory)
            && is_available(
                wasm_custom_sections_requested,
                self.wasm_custom_sections_memory,
            )
        {
            self.total_memory -= requested.get() as i64;
            self.message_memory -= message_requested.get() as i64;
            self.wasm_custom_sections_memory -= wasm_custom_sections_requested.get() as i64;
            Ok(())
        } else {
            Err(SubnetAvailableMemoryError::InsufficientMemory {
                requested_total: requested,
                message_requested,
                wasm_custom_sections_requested,
                available_total: self.total_memory,
                available_messages: self.message_memory,
                available_wasm_custom_sections: self.wasm_custom_sections_memory,
            })
        }
    }

    pub fn increment(
        &mut self,
        total_amount: NumBytes,
        message_amount: NumBytes,
        wasm_custom_sections_amount: NumBytes,
    ) {
        debug_assert!(total_amount >= message_amount);

        self.total_memory += total_amount.get() as i64;
        self.message_memory += message_amount.get() as i64;
        self.wasm_custom_sections_memory += wasm_custom_sections_amount.get() as i64;
    }
}

impl ops::Div<i64> for SubnetAvailableMemory {
    type Output = Self;

    fn div(self, rhs: i64) -> Self::Output {
        Self {
            total_memory: self.total_memory / rhs,
            message_memory: self.message_memory / rhs,
            wasm_custom_sections_memory: self.wasm_custom_sections_memory / rhs,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ExecutionMode {
    Replicated,
    NonReplicated,
}

pub type HypervisorResult<T> = Result<T, HypervisorError>;

/// Interface for the component to execute internal queries triggered by IC.
// Since this service will be shared across many connections we must
// make it cloneable by introducing a bounded buffer infront of it.
// https://docs.rs/tower/0.4.10/tower/buffer/index.html
// The buffer also dampens usage by reducing the risk of
// spiky traffic when users retry in case failed requests.
pub type AnonymousQueryService =
    ConcurrencyLimit<BoxCloneService<AnonymousQuery, AnonymousQueryResponse, Infallible>>;

/// Interface for the component to filter out ingress messages that
/// the canister is not willing to accept.
// Since this service will be shared across many connections we must
// make it cloneable by introducing a bounded buffer infront of it.
// https://docs.rs/tower/0.4.10/tower/buffer/index.html
// The buffer also dampens usage by reducing the risk of
// spiky traffic when users retry in case failed requests.
pub type IngressFilterService = ConcurrencyLimit<
    BoxCloneService<
        (ProvisionalWhitelist, SignedIngressContent),
        Result<(), UserError>,
        Infallible,
    >,
>;

// Since this service will be shared across many connections we must
// make it cloneable by introducing a bounded buffer infront of it.
// https://docs.rs/tower/0.4.10/tower/buffer/index.html
// The buffer also dampens usage by reducing the risk of
// spiky traffic when users retry in case failed requests.
pub type QueryExecutionService = ConcurrencyLimit<
    BoxCloneService<(UserQuery, Option<CertificateDelegation>), HttpQueryResponse, Infallible>,
>;

/// Interface for the component to execute queries on canisters.  It can be used
/// by the HttpHandler and other system components to execute queries.
pub trait QueryHandler: Send + Sync {
    /// Type of state managed by StateReader.
    ///
    /// Should typically be `ic_replicated_state::ReplicatedState`.
    // Note [Associated Types in Interfaces]
    type State;

    /// Handle a query of type `UserQuery` which was sent by an end user.
    fn query(
        &self,
        query: UserQuery,
        state: Arc<Self::State>,
        data_certificate: Vec<u8>,
    ) -> Result<WasmResult, UserError>;
}

/// Errors that can be returned when reading/writing from/to ingress history.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IngressHistoryError {
    StateRemoved(Height),
    StateNotAvailableYet(Height),
}

/// Interface for reading the history of ingress messages.
#[allow(clippy::type_complexity)]
pub trait IngressHistoryReader: Send + Sync {
    /// Returns a function that can be used to query the status for a given
    /// `message_id` using the latest execution state.
    fn get_latest_status(&self) -> Box<dyn Fn(&MessageId) -> IngressStatus>;

    /// Return a function that can be used to query the status for a given
    /// `message_id` using the state at given `height`.
    ///
    /// Return an error if the the state is not available.
    fn get_status_at_height(
        &self,
        height: Height,
    ) -> Result<Box<dyn Fn(&MessageId) -> IngressStatus>, IngressHistoryError>;
}

/// Interface for updating the history of ingress messages.
pub trait IngressHistoryWriter: Send + Sync {
    /// Type of state this Writer can update.
    ///
    /// Should typically be `ic_replicated_state::ReplicatedState`.
    // Note [Associated Types in Interfaces]
    type State;

    /// Allows to set status on a message.
    ///
    /// The allowed status transitions are:
    /// * "None" -> {"Received", "Processing", "Completed", "Failed"}
    /// * "Received" -> {"Processing", "Completed", "Failed"}
    /// * "Processing" -> {"Processing", "Completed", "Failed"}
    fn set_status(&self, state: &mut Self::State, message_id: MessageId, status: IngressStatus);
}

/// A trait for handling `out_of_instructions()` calls from the Wasm module.
pub trait OutOfInstructionsHandler {
    // This function is invoked if the Wasm instruction counter is negative.
    //
    // If it is impossible to recover from the out-of-instructions error then
    // the function returns `Err(HypervisorError::InstructionLimitExceeded)`.
    // Otherwise, the function returns a new positive instruction counter.
    fn out_of_instructions(
        &self,
        instruction_counter: i64,
        execution_complexity: ExecutionComplexity,
    ) -> HypervisorResult<i64>;
}

/// Indicates the type of stable memory API being used.
pub enum StableMemoryApi {
    Stable64 = 0,
    Stable32 = 1,
}

impl TryFrom<i32> for StableMemoryApi {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Stable64),
            1 => Ok(Self::Stable32),
            _ => Err(()),
        }
    }
}

#[test]
fn stable_memory_api_round_trip() {
    for i in 0..10 {
        if let Ok(api) = StableMemoryApi::try_from(i) {
            assert_eq!(i, api as i32)
        }
    }
}

/// Indicates whether an attempt to grow stable memory succeeded or failed.
pub enum StableGrowOutcome {
    Success,
    Failure,
}

/// A trait for providing all necessary imports to a Wasm module.
pub trait SystemApi {
    /// Stores the complexity accumulated during the message execution.
    fn set_execution_complexity(&mut self, complexity: ExecutionComplexity);

    /// Returns the accumulated execution complexity.
    fn execution_complexity(&self) -> &ExecutionComplexity;

    /// Stores the execution error, so that the user can evaluate it later.
    fn set_execution_error(&mut self, error: HypervisorError);

    /// Returns the reference to the execution error.
    fn get_execution_error(&self) -> Option<&HypervisorError>;

    /// Returns the amount of instructions needed to copy `num_bytes`.
    fn get_num_instructions_from_bytes(&self, num_bytes: NumBytes) -> NumInstructions;

    /// Returns the indexes of all dirty pages in stable memory.
    fn stable_memory_dirty_pages(&self) -> Vec<(PageIndex, &PageBytes)>;

    /// Returns the current size of the stable memory in wasm pages.
    fn stable_memory_size(&self) -> usize;

    /// Returns the subnet type the replica runs on.
    fn subnet_type(&self) -> SubnetType;

    /// Returns the message instruction limit, which is the total instruction
    /// limit for all slices combined.
    fn message_instruction_limit(&self) -> NumInstructions;

    /// Returns the number of instructions executed in the current message,
    /// which is the sum of instructions executed in all slices including the
    /// current one.
    fn message_instructions_executed(&self, instruction_counter: i64) -> NumInstructions;

    /// Returns the instruction limit for the current execution slice.
    fn slice_instruction_limit(&self) -> NumInstructions;

    /// Returns the number of instructions executed in the current slice.
    fn slice_instructions_executed(&self, instruction_counter: i64) -> NumInstructions;

    /// Copies `size` bytes starting from `offset` inside the opaque caller blob
    /// and copies them to heap[dst..dst+size]. The caller is the canister
    /// id in case of requests or the user id in case of an ingress message.
    fn ic0_msg_caller_copy(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Returns the size of the opaque caller blob.
    fn ic0_msg_caller_size(&self) -> HypervisorResult<u32>;

    /// Returns the size of msg.payload.
    fn ic0_msg_arg_data_size(&self) -> HypervisorResult<u32>;

    /// Copies `length` bytes from msg.payload[offset..offset+size] to
    /// memory[dst..dst+size].
    fn ic0_msg_arg_data_copy(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Used to look up the size of the method_name that the message wants to
    /// call. Can only be called in the context of inspecting messages.
    fn ic0_msg_method_name_size(&self) -> HypervisorResult<u32>;

    /// Used to copy the method_name that the message wants to call to heap. Can
    /// only be called in the context of inspecting messages.
    fn ic0_msg_method_name_copy(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    // If the canister calls this method, then the message will be accepted
    // otherwise rejected. Can only be called in the context of accepting
    // messages.
    fn ic0_accept_message(&mut self) -> HypervisorResult<()>;

    /// Copies the data referred to by src/size out of the canister and appends
    /// it to the (initially empty) data reply.
    fn ic0_msg_reply_data_append(
        &mut self,
        src: u32,
        size: u32,
        heap: &[u8],
    ) -> HypervisorResult<()>;

    /// Replies to the sender with the data assembled using
    /// `msg_reply_data_append`.
    fn ic0_msg_reply(&mut self) -> HypervisorResult<()>;

    /// Returns the reject code, if the current function is invoked as a
    /// reject callback.
    ///
    /// It returns the special “no error” code 0 if the callback is not invoked
    /// as a reject callback
    fn ic0_msg_reject_code(&self) -> HypervisorResult<i32>;

    /// Replies to sender with an error message
    fn ic0_msg_reject(&mut self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<()>;

    /// Returns the length of the reject message in bytes.
    ///
    /// # Panics
    ///
    /// This traps if not invoked from a reject callback.
    fn ic0_msg_reject_msg_size(&self) -> HypervisorResult<u32>;

    /// Copies length bytes from self.reject_msg[offset..offset+size] to
    /// memory[dst..dst+size]
    ///
    /// # Panics
    ///
    /// This traps if offset+size is greater than the size of the reject
    /// message, or if dst+size exceeds the size of the Wasm memory, or if not
    /// called from inside a reject callback.
    fn ic0_msg_reject_msg_copy(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Returns the size of the blob corresponding to the id of the canister.
    fn ic0_canister_self_size(&self) -> HypervisorResult<usize>;

    /// Copies `size` bytes starting from `offset` in the id blob of the
    /// canister to heap[dst..dst+size].
    fn ic0_canister_self_copy(
        &mut self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Outputs the specified bytes on the heap as a string on STDOUT.
    fn ic0_debug_print(&self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<()>;

    /// Traps, with a possibly helpful message
    fn ic0_trap(&self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<()>;

    /// Begins assembling a call to the canister specified by
    /// callee_src/callee_size at method name_src/name_size. Two mandatory
    /// callbacks are recorded which will be invoked on success and error
    /// respectively.
    ///
    /// Subsequent calls to other `call_*` apis set further attributes of this
    /// call until the call is concluded (with `ic0.call_perform) or discarded
    /// (by returning without calling `ic0.call_perform` or by starting a new
    /// call with `ic0.call_new`).
    #[allow(clippy::too_many_arguments)]
    fn ic0_call_new(
        &mut self,
        callee_src: u32,
        callee_size: u32,
        name_src: u32,
        name_len: u32,
        reply_fun: u32,
        reply_env: u32,
        reject_fun: u32,
        reject_env: u32,
        heap: &[u8],
    ) -> HypervisorResult<()>;

    /// Appends the specified bytes to the argument of the call. Initially, the
    /// argument is empty. This can be called multiple times between
    /// `ic0.call_new` and `ic0.call_perform`.
    fn ic0_call_data_append(&mut self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<()>;

    /// Specifies the closure to be called if the reply/reject closures trap.
    /// Can be called at most once between `ic0.call_new` and
    /// `ic0.call_perform`.
    ///
    /// See https://sdk.dfinity.org/docs/interface-spec/index.html#system-api-call
    fn ic0_call_on_cleanup(&mut self, fun: u32, env: u32) -> HypervisorResult<()>;

    /// (deprecated) Please use `ic0_call_cycles_add128` instead, as this API
    /// can only add a 64-bit value.
    ///
    /// Adds cycles to a call by moving them from the
    /// canister's balance onto the call under construction.
    /// The cycles are deducted immediately from the canister's
    /// balance and moved back if the call cannot be performed (e.g. if
    /// `ic0.call_perform` signals an error or if the canister invokes
    /// `ic0.call_new` or returns without invoking `ic0.call_perform`).
    ///
    /// This traps if trying to transfer more cycles than are in the current
    /// balance of the canister.
    fn ic0_call_cycles_add(&mut self, amount: u64) -> HypervisorResult<()>;

    /// Adds cycles to a call by moving them from the canister's balance onto
    /// the call under construction. The cycles are deducted immediately
    /// from the canister's balance and moved back if the call cannot be
    /// performed (e.g. if `ic0.call_perform` signals an error or if the
    /// canister invokes `ic0.call_new` or returns without invoking
    /// `ic0.call_perform`).
    ///
    /// This traps if trying to transfer more cycles than are in the current
    /// balance of the canister.
    fn ic0_call_cycles_add128(&mut self, amount: Cycles) -> HypervisorResult<()>;

    /// This call concludes assembling the call. It queues the call message to
    /// the given destination, but does not actually act on it until the current
    /// WebAssembly function returns without trapping.
    ///
    /// If the system returns 0, then the system was able to enqueue the call,
    /// if a non-zero value is returned then the call could not be enqueued.
    ///
    /// After `ic0.call_perform` and before the next `ic0.call_new`, all other
    /// `ic0.call_*` calls trap.
    fn ic0_call_perform(&mut self) -> HypervisorResult<i32>;

    /// Returns the current size of the stable memory in WebAssembly pages.
    fn ic0_stable_size(&self) -> HypervisorResult<u32>;

    /// Tries to grow the stable memory by additional_pages many pages
    /// containing zeros.
    /// If successful, returns the previous size of the memory (in pages).
    /// Otherwise, returns -1
    fn ic0_stable_grow(&mut self, additional_pages: u32) -> HypervisorResult<i32>;

    /// Same implementation as `ic0_stable_read`, but doesn't do any bounds
    /// checks on the stable memory size. This is part of the hidden API and
    /// should only be called from instrumented code that has already done the
    /// bounds checks within Wasm code. Calls that access pages outsize of
    /// the current stable memory size will get zeros for those pages.
    fn stable_read_without_bounds_checks(
        &self,
        dst: u64,
        offset: u64,
        size: u64,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Copies the data referred to by offset/size out of the stable memory and
    /// replaces the corresponding bytes starting at dst in the canister memory.
    ///
    /// This system call traps if dst+size exceeds the size of the WebAssembly
    /// memory or offset+size exceeds the size of the stable memory.
    fn ic0_stable_read(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Copies the data referred to by src/size out of the canister and replaces
    /// the corresponding segment starting at offset in the stable memory.
    ///
    /// This system call traps if src+size exceeds the size of the WebAssembly
    /// memory or offset+size exceeds the size of the stable memory.
    /// Returns the number of **new** dirty pages created by the write.
    fn ic0_stable_write(
        &mut self,
        offset: u32,
        src: u32,
        size: u32,
        heap: &[u8],
    ) -> HypervisorResult<()>;

    /// Returns the current size of the stable memory in WebAssembly pages.
    ///
    /// It supports bigger stable memory sizes indexed by 64 bit pointers.
    fn ic0_stable64_size(&self) -> HypervisorResult<u64>;

    /// Tries to grow the stable memory by additional_pages many pages
    /// containing zeros.
    /// If successful, returns the previous size of the memory (in pages).
    /// Otherwise, returns -1
    ///
    /// It supports bigger stable memory sizes indexed by 64 bit pointers.
    fn ic0_stable64_grow(&mut self, additional_pages: u64) -> HypervisorResult<i64>;

    /// Copies the data from location [offset, offset+size) of the stable memory
    /// to the location [dst, dst+size) in the canister memory.
    ///
    /// This system call traps if dst+size exceeds the size of the WebAssembly
    /// memory or offset+size exceeds the size of the stable memory.
    ///
    /// It supports bigger stable memory sizes indexed by 64 bit pointers.
    fn ic0_stable64_read(
        &self,
        dst: u64,
        offset: u64,
        size: u64,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Copies the data from location [src, src+size) of the canister memory to
    /// location [offset, offset+size) in the stable memory.
    ///
    /// This system call traps if src+size exceeds the size of the WebAssembly
    /// memory or offset+size exceeds the size of the stable memory.
    ///
    /// It supports bigger stable memory sizes indexed by 64 bit pointers.
    /// Returns the number of **new** dirty pages created by the write.
    fn ic0_stable64_write(
        &mut self,
        offset: u64,
        src: u64,
        size: u64,
        heap: &[u8],
    ) -> HypervisorResult<()>;

    /// Determines the number of dirty pages that a stable write would create
    /// and the cost for those dirty pages (without actually doing the write).
    fn dirty_pages_from_stable_write(
        &self,
        offset: u64,
        size: u64,
    ) -> HypervisorResult<(NumPages, NumInstructions)>;

    /// The canister can query the IC for the current time.
    fn ic0_time(&self) -> HypervisorResult<Time>;

    /// The canister can set a global one-off timer at the specific time.
    fn ic0_global_timer_set(&mut self, time: Time) -> HypervisorResult<Time>;

    /// The canister can query the IC for its version.
    fn ic0_canister_version(&self) -> HypervisorResult<u64>;

    /// The canister can query the "performance counter", which is
    /// a deterministic monotonically increasing integer approximating
    /// the amount of work the canister has done since the beginning of
    /// the current execution.
    ///
    /// The argument type decides which performance counter to return:
    ///     0 : instruction counter. The number of WebAssembly
    ///         instructions the system has determined that the canister
    ///         has executed.
    ///
    /// Note: as the instruction counters are not available on the SystemApi level,
    /// the `ic0_performance_counter_helper()` in `wasmtime_embedder` module does
    /// most of the work. Yet the function is still implemented here for the consistency.
    fn ic0_performance_counter(
        &self,
        performance_counter_type: PerformanceCounterType,
    ) -> HypervisorResult<u64>;

    /// This system call is not part of the public spec and it is invoked when
    /// Wasm execution has run out of instructions.
    ///
    /// If it is impossible to recover from the out-of-instructions error then
    /// the functions return `Err(HypervisorError::InstructionLimitExceeded)`.
    /// Otherwise, the function return a new non-negative instruction counter.
    fn out_of_instructions(&mut self, instruction_counter: i64) -> HypervisorResult<i64>;

    /// This system call is not part of the public spec. It's called after a
    /// native `memory.grow` has been called to check whether there's enough
    /// available memory left.
    fn update_available_memory(
        &mut self,
        native_memory_grow_res: i64,
        additional_pages: u64,
    ) -> HypervisorResult<()>;

    /// Attempts to allocate memory before calling stable grow. Will also check
    /// that the current size if valid for the stable memory API being used and
    /// the resulting size doesn't exceed the maximum stable memory limit.
    ///
    /// This is enough to guarantee that growing the stable memory from within
    /// wasm will succeed.
    fn try_grow_stable_memory(
        &mut self,
        current_size: u64,
        additional_pages: u64,
        stable_memory_api: StableMemoryApi,
    ) -> HypervisorResult<StableGrowOutcome>;

    /// (deprecated) Please use `ic0_canister_cycle_balance128` instead.
    /// This API supports only 64-bit values.
    ///
    /// Returns the current balance in cycles.
    ///
    /// Traps if current canister balance cannot fit in a 64-bit value.
    fn ic0_canister_cycle_balance(&self) -> HypervisorResult<u64>;

    /// This system call indicates the current cycle balance
    /// of the canister.
    ///
    /// The amount of cycles is represented by a 128-bit value
    /// and is copied in the canister memory starting
    /// starting at the location `dst`.
    fn ic0_canister_cycle_balance128(&self, dst: u32, heap: &mut [u8]) -> HypervisorResult<()>;

    /// (deprecated) Please use `ic0_msg_cycles_available128` instead.
    /// This API supports only 64-bit values.
    ///
    /// Cycles sent in the current call and still available.
    ///
    /// Traps if the amount of cycles available cannot fit in a 64-bit value.
    fn ic0_msg_cycles_available(&self) -> HypervisorResult<u64>;

    /// This system call indicates the amount of cycles sent
    /// in the current call and still available.
    ///
    /// The amount of cycles is represented by a 128-bit value
    /// and is copied in the canister memory starting
    /// starting at the location `dst`.
    fn ic0_msg_cycles_available128(&self, dst: u32, heap: &mut [u8]) -> HypervisorResult<()>;

    /// (deprecated) Please use `ic0_msg_cycles_refunded128` instead.
    /// This API supports only 64-bit values.
    ///
    /// Cycles that came back with the response, as a refund.
    ///
    /// Traps if the amount of refunded cycles cannot fit in a 64-bit value.
    fn ic0_msg_cycles_refunded(&self) -> HypervisorResult<u64>;

    /// This system call indicates the amount of cycles sent
    /// that came back with the response as a refund.
    ///
    /// The amount of cycles is represented by a 128-bit value
    /// and is copied in the canister memory starting
    /// starting at the location `dst`.
    fn ic0_msg_cycles_refunded128(&self, dst: u32, heap: &mut [u8]) -> HypervisorResult<()>;

    /// (deprecated) Please use `ic0_msg_cycles_accept128` instead.
    /// This API supports only 64-bit values.
    ///
    /// This moves cycles from the
    /// call to the canister balance. It can be called multiple times, each
    /// time adding more cycles to the balance.
    ///
    /// It moves no more cycles than `max_amount`.
    ///
    /// It moves no more cycles than available according to
    /// `ic0.msg_cycles_available`, and
    ///
    /// The canister balance afterwards does not exceed
    /// maximum amount of cycles it can hold (public spec refers to this
    /// constant as MAX_CANISTER_BALANCE) minus any possible outstanding
    /// balances. However, canisters on system subnets have no balance
    /// limit.
    ///
    /// EXE-117: the last point is not properly handled yet.  In particular, a
    /// refund can come back to the canister after this call finishes which
    /// causes the canister's balance to overflow.
    fn ic0_msg_cycles_accept(&mut self, max_amount: u64) -> HypervisorResult<u64>;

    /// This moves cycles from the call to the canister balance.
    /// It can be called multiple times, each time adding more cycles to the
    /// balance.
    ///
    /// It moves no more cycles than `max_amount`.
    ///
    /// It moves no more cycles than available according to
    /// `ic0.msg_cycles_available128`, and
    ///
    /// The canister balance afterwards does not exceed
    /// maximum amount of cycles it can hold (public spec refers to this
    /// constant as MAX_CANISTER_BALANCE) minus any possible outstanding
    /// balances. However, canisters on system subnets have no balance
    /// limit.
    ///
    /// EXE-117: the last point is not properly handled yet.  In particular, a
    /// refund can come back to the canister after this call finishes which
    /// causes the canister's balance to overflow.
    fn ic0_msg_cycles_accept128(
        &mut self,
        max_amount: Cycles,
        dst: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Sets the certified data for the canister.
    /// See: https://sdk.dfinity.org/docs/interface-spec/index.html#system-api-certified-data
    fn ic0_certified_data_set(&mut self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<()>;

    /// If run in non-replicated execution (i.e. query),
    /// returns 1 if the data certificate is present, 0 otherwise.
    /// If run in replicated execution (i.e. an update call or a certified
    /// query), returns 0.
    fn ic0_data_certificate_present(&self) -> HypervisorResult<i32>;

    /// Returns the size of the data certificate if it is present
    /// (i.e. data_certificate_present returns 1).
    /// Traps if data_certificate_present returns 0.
    fn ic0_data_certificate_size(&self) -> HypervisorResult<i32>;

    /// Copies the data certificate into the heap if it is present
    /// (i.e. data_certificate_present returns 1).
    /// Traps if data_certificate_present returns 0.
    fn ic0_data_certificate_copy(
        &self,
        dst: u32,
        offset: u32,
        size: u32,
        heap: &mut [u8],
    ) -> HypervisorResult<()>;

    /// Returns the current status of the canister.  `1` indicates
    /// running, `2` indicates stopping, and `3` indicates stopped.
    fn ic0_canister_status(&self) -> HypervisorResult<u32>;

    /// Mints the `amount` cycles
    /// Adds cycles to the canister's balance.
    ///
    /// Adds no more cycles than `amount`.
    ///
    /// The canister balance afterwards does not exceed
    /// maximum amount of cycles it can hold.
    /// However, canisters on system subnets have no balance limit.
    ///
    /// Returns the amount of cycles added to the canister's balance.
    fn ic0_mint_cycles(&mut self, amount: u64) -> HypervisorResult<u64>;

    /// Checks whether the principal identified by src/size is one of the
    /// controllers of the canister. If yes, then a value of 1 is returned,
    /// otherwise a 0 is returned. It can be called multiple times.
    ///
    /// This system call traps if src+size exceeds the size of the WebAssembly memory.
    fn ic0_is_controller(&self, src: u32, size: u32, heap: &[u8]) -> HypervisorResult<u32>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]

/// Indicate whether a checkpoint will be taken after the current round or not.
pub enum ExecutionRoundType {
    CheckpointRound,
    OrdinaryRound,
}

/// Configuration of execution that comes from the registry.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegistryExecutionSettings {
    pub max_number_of_canisters: u64,
    pub provisional_whitelist: ProvisionalWhitelist,
    pub max_ecdsa_queue_size: u32,
    pub subnet_size: usize,
}

pub trait Scheduler: Send {
    /// Type modelling the replicated state.
    ///
    /// Should typically be
    /// `ic_replicated_state::ReplicatedState`.
    // Note [Associated Types in Interfaces]
    type State;

    /// Executes a list of messages. Triggered by the Coordinator as part of
    /// processing a batch.
    ///
    /// # Configuration parameters that might affect a round's execution
    ///
    /// * `scheduler_cores`: number of concurrent threads that the scheduler can
    ///   use during an execution round.
    /// * `max_instructions_per_round`: max number of instructions a single
    ///   round on a single thread can
    /// consume.
    /// * `max_instructions_per_message`: max number of instructions a single
    ///   message execution can consume.
    ///
    /// # Walkthrough of a round
    ///
    /// The scheduler decides on a deterministic and fair order of canisters to
    /// execute on each thread (not fully implemented yet).
    /// For each thread we want to schedule **at least** a `pulse` for the first
    /// canister. The canister's `pulse` can consume the entire round of the
    /// thread if it has enough messages or, if not, we can give a `pulse` to
    /// the next canister. Similarly, the second canister can use the rest
    /// of the round of the thread if it has enough messages or we can give
    /// a `pulse` to the next canister and so on.
    ///
    /// # Constraints
    ///
    /// * To be able to start a pulse for a canister we need to have at least
    ///   `max_instructions_per_message` left in the current round (basically we
    ///   need a guarantee that we are able to execute successfully at least one
    ///   message).
    /// * The round (and thus the first `pulse`) starts with a limit of
    ///   `max_instructions_per_round`. When the `pulse` ends it returns how
    ///   many instructions is left which is used to update the limit for the
    ///   next `pulse` and if the above constraint is satisfied, we can start
    ///   the `pulse`. And so on.
    /// * Deterministic time slicing puts additional constraints on the states.
    ///   Normally states form a chain, where the result of one execution
    ///   becomes the input of the subsequent execution. The chain may break due
    ///   to state sync, which starts a new execution chain. The function
    ///   assumes that the old chain will be completely abandoned and the
    ///   function will never be called on the old chain.
    #[allow(clippy::too_many_arguments)]
    fn execute_round(
        &self,
        state: Self::State,
        randomness: Randomness,
        ecdsa_subnet_public_keys: BTreeMap<EcdsaKeyId, MasterEcdsaPublicKey>,
        current_round: ExecutionRound,
        current_round_type: ExecutionRoundType,
        registry_settings: &RegistryExecutionSettings,
    ) -> Self::State;
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct WasmExecutionOutput {
    pub wasm_result: Result<Option<WasmResult>, HypervisorError>,
    pub num_instructions_left: NumInstructions,
    pub allocated_bytes: NumBytes,
    pub allocated_message_bytes: NumBytes,
    pub instance_stats: InstanceStats,
}

impl fmt::Display for WasmExecutionOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let wasm_result_str = match &self.wasm_result {
            Ok(result) => match result {
                None => "None".to_string(),
                Some(wasm_result) => format!("{}", wasm_result),
            },
            Err(err) => format!("{}", err),
        };
        write!(f, "wasm_result => [{}], instructions left => {}, instance_stats => [ accessed pages => {}, dirty pages => {}]",
               wasm_result_str,
               self.num_instructions_left,
               self.instance_stats.accessed_pages,
               self.instance_stats.dirty_pages,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_available_memory() {
        let available = SubnetAvailableMemory::new(20, 10, 4);
        assert_eq!(available.get_total_memory(), 20);
        assert_eq!(available.get_message_memory(), 10);
        assert_eq!(available.get_wasm_custom_sections_memory(), 4);
        assert_eq!(available.max_available_message_memory(), 10);

        let available = available / 2;
        assert_eq!(available.get_total_memory(), 10);
        assert_eq!(available.get_message_memory(), 5);
        assert_eq!(available.get_wasm_custom_sections_memory(), 2);
        assert_eq!(available.max_available_message_memory(), 5);

        let available = SubnetAvailableMemory::new(3, 2, 2);
        assert_eq!(available.get_total_memory(), 3);
        assert_eq!(available.get_message_memory(), 2);
        assert_eq!(available.get_wasm_custom_sections_memory(), 2);
        assert_eq!(available.max_available_message_memory(), 2);
    }

    #[test]
    fn test_subnet_available_memory() {
        let mut available: SubnetAvailableMemory =
            SubnetAvailableMemory::new(1 << 30, (1 << 30) - 5, 1 << 20);
        assert!(available
            .try_decrement(NumBytes::from(10), NumBytes::from(5), NumBytes::from(5))
            .is_ok());
        assert!(available
            .try_decrement(
                NumBytes::from((1 << 30) - 10),
                NumBytes::from((1 << 30) - 10),
                NumBytes::from(0),
            )
            .is_ok());
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(1), NumBytes::from(1))
            .is_err());
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(0), NumBytes::from(0))
            .is_err());

        let mut available: SubnetAvailableMemory =
            SubnetAvailableMemory::new(1 << 30, (1 << 30) - 5, 1 << 20);
        assert!(available
            .try_decrement(NumBytes::from(10), NumBytes::from(5), NumBytes::from(5))
            .is_ok());
        assert!(available
            .try_decrement(
                NumBytes::from((1 << 20) - 5),
                NumBytes::from(0),
                NumBytes::from((1 << 20) - 5),
            )
            .is_ok());
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(1), NumBytes::from(1))
            .is_err());
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(0), NumBytes::from(0))
            .is_ok());

        let mut available: SubnetAvailableMemory = SubnetAvailableMemory::new(1 << 30, -1, -1);
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(1), NumBytes::from(1))
            .is_err());
        assert!(available
            .try_decrement(NumBytes::from(10), NumBytes::from(0), NumBytes::from(0))
            .is_ok());
        assert!(available
            .try_decrement(
                NumBytes::from((1 << 30) - 10),
                NumBytes::from(0),
                NumBytes::from(0)
            )
            .is_ok());
        assert!(available
            .try_decrement(NumBytes::from(1), NumBytes::from(0), NumBytes::from(0))
            .is_err());

        assert!(available
            .try_decrement(
                NumBytes::from(u64::MAX),
                NumBytes::from(0),
                NumBytes::from(0)
            )
            .is_err());
        assert!(available
            .try_decrement(
                NumBytes::from(u64::MAX),
                NumBytes::from(u64::MAX),
                NumBytes::from(u64::MAX)
            )
            .is_err());
        assert!(available
            .try_decrement(
                NumBytes::from(i64::MAX as u64 + 1),
                NumBytes::from(0),
                NumBytes::from(0)
            )
            .is_err());
        assert!(available
            .try_decrement(
                NumBytes::from(i64::MAX as u64 + 1),
                NumBytes::from(i64::MAX as u64 + 1),
                NumBytes::from(i64::MAX as u64 + 1),
            )
            .is_err());

        let mut available: SubnetAvailableMemory = SubnetAvailableMemory::new(42, 43, 10);
        assert_eq!(available.get_total_memory(), 42);
        assert_eq!(available.get_message_memory(), 43);
        assert_eq!(available.get_wasm_custom_sections_memory(), 10);
        available = SubnetAvailableMemory::new(44, 45, 30);
        assert_eq!(available.get_total_memory(), 44);
        assert_eq!(available.get_message_memory(), 45);
        assert_eq!(available.get_wasm_custom_sections_memory(), 30);
        available.increment(NumBytes::from(1), NumBytes::from(0), NumBytes::from(0));
        assert_eq!(available.get_total_memory(), 45);
        assert_eq!(available.get_message_memory(), 45);
        assert_eq!(available.get_wasm_custom_sections_memory(), 30);
        available.increment(NumBytes::from(1), NumBytes::from(1), NumBytes::from(1));
        assert_eq!(available.get_total_memory(), 46);
        assert_eq!(available.get_message_memory(), 46);
        assert_eq!(available.get_wasm_custom_sections_memory(), 31);
    }
}
