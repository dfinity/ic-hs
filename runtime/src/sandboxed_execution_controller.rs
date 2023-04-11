use ic_canister_sandbox_common::controller_launcher_service::ControllerLauncherService;
use ic_canister_sandbox_common::launcher_service::LauncherService;
use ic_canister_sandbox_common::protocol::id::{MemoryId, WasmId};
use ic_canister_sandbox_common::protocol::sbxsvc::MemorySerialization;
use ic_canister_sandbox_common::protocol::structs::{SandboxExecInput, SandboxExecOutput};
use ic_canister_sandbox_common::sandbox_service::SandboxService;
use ic_canister_sandbox_common::{protocol, rpc};
use ic_config::embedders::Config as EmbeddersConfig;
use ic_embedders::wasm_executor::{
    wasm_execution_error, CanisterStateChanges, WasmExecutionResult,
};
use ic_embedders::{CompilationCache, CompilationResult, WasmExecutionInput};
use ic_interfaces::execution_environment::{HypervisorError, HypervisorResult};

use ic_replicated_state::canister_state::execution_state::{
    SandboxMemory, SandboxMemoryHandle, SandboxMemoryOwner, WasmBinary,
};
use ic_replicated_state::{
    EmbedderCache, ExportedFunctions, Global, Memory, NumWasmPages, PageMap,
};
use ic_types::ingress::WasmResult;
use ic_types::{CanisterId, NumInstructions};
use ic_wasm_types::CanisterModule;

use std::collections::{HashMap, VecDeque};

use std::process::ExitStatus;
use std::sync::Weak;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use crate::active_execution_state_registry::{ActiveExecutionStateRegistry, CompletionResult};
use crate::controller_service_impl::ControllerServiceImpl;
use crate::launch_as_process::{create_sandbox_process, spawn_launcher_process};
use crate::process_exe_and_args::{create_launcher_argv, create_sandbox_argv};
#[cfg(target_os = "linux")]
use crate::sandbox_process_eviction::{self, EvictionCandidate};
use ic_replicated_state::page_map::PageAllocatorFileDescriptor;

const SANDBOX_PROCESS_UPDATE_INTERVAL: Duration = Duration::from_secs(10);

// The percentage of sandbox processes to evict in one go in order to amortize
// for the eviction cost.
const SANDBOX_PROCESS_EVICTION_PERCENT: usize = 20;

/// Keeps history of the N most recent calls made to the sandbox backend
/// process. It will normally not be logged, but in case of an
/// unexpected sandbox process crash we can replay and log the history
/// to get a better idea of what led to this situation.
/// This is purely a debugging aid. Nothing functionally depends on it.
struct SandboxProcessRequestHistory {
    entries: Mutex<VecDeque<String>>,
    limit: usize,
}

impl SandboxProcessRequestHistory {
    fn new() -> Self {
        Self {
            entries: Default::default(),
            limit: 20,
        }
    }

    /// Records an entry of an action performed on a sandbox process.
    fn record(&self, msg: String) {
        let mut guard = self.entries.lock().unwrap();
        guard.push_back(msg);
        if guard.len() > self.limit {
            guard.pop_front();
        }
    }
}

pub struct SandboxProcess {
    /// Registry for all executions that are currently running on
    /// this backend process.
    execution_states: Arc<ActiveExecutionStateRegistry>,

    /// Handle for IPC down to sandbox.
    sandbox_service: Arc<dyn SandboxService>,

    /// Process id of the backend process.
    pub pid: u32,

    /// History of operations sent to sandbox process (for crash
    /// diagnostics).
    history: SandboxProcessRequestHistory,
}

impl Drop for SandboxProcess {
    fn drop(&mut self) {
        self.history.record("Terminate()".to_string());
        self.sandbox_service
            .terminate(protocol::sbxsvc::TerminateRequest {})
            .on_completion(|_| {});
    }
}

/// Manages the lifetime of a remote compiled Wasm and provides its id.
///
/// It keeps a weak reference to the sandbox service to allow early
/// termination of the sandbox process when it becomes inactive.
pub struct OpenedWasm {
    sandbox_process: Weak<SandboxProcess>,
    wasm_id: WasmId,
}

impl OpenedWasm {
    fn new(sandbox_process: Weak<SandboxProcess>, wasm_id: WasmId) -> Self {
        Self {
            sandbox_process,
            wasm_id,
        }
    }
}

impl Drop for OpenedWasm {
    fn drop(&mut self) {
        if let Some(sandbox_process) = self.sandbox_process.upgrade() {
            sandbox_process
                .history
                .record(format!("CloseWasm(wasm_id={})", self.wasm_id));
            sandbox_process
                .sandbox_service
                .close_wasm(protocol::sbxsvc::CloseWasmRequest {
                    wasm_id: self.wasm_id,
                })
                .on_completion(|_| {});
        }
    }
}

impl std::fmt::Debug for OpenedWasm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenedWasm")
            .field("wasm_id", &self.wasm_id)
            .finish()
    }
}

/// Manages the lifetime of a remote sandbox memory and provides its id.
pub struct OpenedMemory {
    sandbox_process: Weak<SandboxProcess>,
    memory_id: MemoryId,
}

impl OpenedMemory {
    fn new(sandbox_process: Weak<SandboxProcess>, memory_id: MemoryId) -> Self {
        Self {
            sandbox_process,
            memory_id,
        }
    }
}

impl SandboxMemoryOwner for OpenedMemory {
    fn get_sandbox_memory_id(&self) -> usize {
        self.memory_id.as_usize()
    }

    fn get_sandbox_process_id(&self) -> Option<usize> {
        self.sandbox_process.upgrade().map(|sp| sp.pid as usize)
    }
}

impl Drop for OpenedMemory {
    fn drop(&mut self) {
        if let Some(sandbox_process) = self.sandbox_process.upgrade() {
            sandbox_process
                .history
                .record(format!("CloseMemory(memory_id={})", self.memory_id));
            sandbox_process
                .sandbox_service
                .close_memory(protocol::sbxsvc::CloseMemoryRequest {
                    memory_id: self.memory_id,
                })
                .on_completion(|_| {});
        }
    }
}

impl std::fmt::Debug for OpenedMemory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenedMemory")
            .field("memory_id", &self.memory_id)
            .finish()
    }
}

pub enum Backend {
    Active {
        // The strong reference to `SandboxProcess` ensures that the sandbox
        // process will stay alive even if it is not used.
        sandbox_process: Arc<SandboxProcess>,
        stats: SandboxProcessStats,
    },
    Evicted {
        // The weak reference is used to promote the sandbox process back to
        // `active` if a new message execution starts.
        sandbox_process: Weak<SandboxProcess>,
        stats: SandboxProcessStats,
    },
    // A dummy, not observable state that is used as a placeholder in
    // `std::mem::replace()`.
    Empty,
}

#[derive(Clone)]
pub struct SandboxProcessStats {
    last_used: std::time::Instant,
}

pub enum SandboxProcessStatus {
    Active,
    Evicted,
}

/// Manages sandboxed processes, forwards requests to the appropriate
/// process.
pub struct SandboxedExecutionController {
    /// A registry of known sandbox processes. Each sandbox process can be in
    /// one of two states:
    ///
    /// - `active`: the entry in the registry keeps a strong reference to the
    /// sandbox process, so that it is guaranteed to stay alive.
    ///
    /// - `evicted`: the entry in the registry keeps a weak reference to the
    /// sandbox process, so that the sandbox process is terminated as soon as
    /// the last strong reference to it is dropped. In other words, the sandbox
    /// process is terminated as soon as all pending executions finish and no
    /// new execution starts.
    ///
    /// The sandbox process can move from `evicted` back to `active` if a new
    /// message execution starts.
    ///
    /// Invariants:
    ///
    /// - If a sandbox process has a strong reference from somewhere else in the
    /// replica process, then the registry has an entry for that sandbox process.
    /// The entry may be either the `active` or `evicted` state.
    ///
    /// - An entry is removed from the registry only if it is in the `evicted`
    /// state and the strong reference count reaches zero.
    pub backends: Arc<Mutex<HashMap<CanisterId, Backend>>>,
    min_sandbox_count: usize,
    max_sandbox_count: usize,
    max_sandbox_idle_time: Duration,
    /// Executable and arguments to be passed to `canister_sandbox` which are
    /// the same for all canisters.
    sandbox_exec_argv: Vec<String>,
    launcher_service: Box<dyn LauncherService>,
    fd_factory: Arc<dyn PageAllocatorFileDescriptor>,
}

impl SandboxedExecutionController {
    pub fn execute(
        self: &Arc<Self>,
        WasmExecutionInput {
            api_type,
            sandbox_safe_system_state,
            canister_current_memory_usage,
            execution_parameters,
            subnet_available_memory,
            func_ref,
            compilation_cache,
        }: WasmExecutionInput,
        wasm_binary: &WasmBinary,
        wasm_memory: &Memory,
        stable_memory: &Memory,
        exported_globals: Vec<Global>,
    ) -> (Option<CompilationResult>, WasmExecutionResult) {
        let message_instruction_limit = execution_parameters.instruction_limits.message();

        // Determine which process we want to run this on.
        let sandbox_process = self.get_sandbox_process(sandbox_safe_system_state.canister_id());

        // Ensure that Wasm is compiled.
        let (wasm_id, compilation_result) = match open_wasm(
            &sandbox_process,
            wasm_binary,
            compilation_cache,
            sandbox_safe_system_state.canister_id(),
        ) {
            Ok((wasm_id, compilation_result)) => (wasm_id, compilation_result),
            Err(err) => {
                return (None, wasm_execution_error(err, message_instruction_limit));
            }
        };

        // Create channel through which we will receive the execution
        // output from closure (running by IPC thread at end of
        // execution).
        let (tx, rx) = std::sync::mpsc::sync_channel(1);

        // Generate an ID for this execution, register it. We need to
        // pass the system state accessor as well as the completion
        // function that gets our result back in the end.
        let sandbox_process_weakref = Arc::downgrade(&sandbox_process);
        let exec_id =
            sandbox_process
                .execution_states
                .register_execution(move |exec_id, result| {
                    if let Some(sandbox_process) = sandbox_process_weakref.upgrade() {
                        sandbox_process
                            .history
                            .record(format!("Completion(exec_id={})", exec_id));
                    }
                    tx.send(result).unwrap();
                });

        // Now set up resources on the sandbox to drive the execution.
        let wasm_memory_handle = open_remote_memory(&sandbox_process, wasm_memory);
        let canister_id = sandbox_safe_system_state.canister_id();
        let wasm_memory_id = MemoryId::from(wasm_memory_handle.get_sandbox_memory_id());
        let next_wasm_memory_id = MemoryId::new();

        let stable_memory_handle = open_remote_memory(&sandbox_process, stable_memory);
        let stable_memory_id = MemoryId::from(stable_memory_handle.get_sandbox_memory_id());
        let next_stable_memory_id = MemoryId::new();

        sandbox_process.history.record(
            format!("StartExecution(exec_id={} wasm_id={} wasm_memory_id={} stable_member_id={} api_type={}, next_wasm_memory_id={} next_stable_memory_id={}",
                exec_id, wasm_id, wasm_memory_id, stable_memory_id, api_type.as_str(), next_wasm_memory_id, next_stable_memory_id));

        sandbox_process
            .sandbox_service
            .start_execution(protocol::sbxsvc::StartExecutionRequest {
                exec_id,
                wasm_id,
                wasm_memory_id,
                stable_memory_id,
                exec_input: SandboxExecInput {
                    func_ref,
                    api_type,
                    globals: exported_globals.clone(),
                    canister_current_memory_usage,
                    execution_parameters,
                    subnet_available_memory,
                    next_wasm_memory_id,
                    next_stable_memory_id,
                    sandbox_safe_system_state,
                    wasm_reserved_pages: NumWasmPages::from(0),
                },
            })
            .on_completion(|_| {});

        // Wait for completion.
        let result = rx.recv().unwrap();
        let execution_result = Self::process_completion(
            self,
            canister_id,
            wasm_memory,
            stable_memory,
            result,
            next_wasm_memory_id,
            next_stable_memory_id,
            message_instruction_limit,
            sandbox_process,
        );
        (compilation_result, execution_result)
    }

    pub fn create_execution_state(
        &self,
        canister_module: CanisterModule,
        canister_id: CanisterId,
        compilation_cache: Arc<CompilationCache>,
    ) -> HypervisorResult<(
        Arc<WasmBinary>,
        Memory,
        Memory,
        Vec<Global>,
        ExportedFunctions,
        NumInstructions,
        Option<CompilationResult>,
    )> {
        let sandbox_process = self.get_sandbox_process(canister_id);
        let wasm_binary = WasmBinary::new(canister_module);

        // The sandbox process prepares wasm memory, instantiates page maps
        // and compiles the wasm binary (or looks it up in the cache).
        // Then, through RPC, operations are sent to the sandbox, passing along
        // also serialized versions of the needed objects (e.g., the page allocator through the pagemap)
        let wasm_id = WasmId::new();
        let wasm_page_map = PageMap::new(Arc::clone(&self.fd_factory));
        let next_wasm_memory_id = MemoryId::new();

        let stable_memory_page_map = PageMap::new(Arc::clone(&self.fd_factory));

        let (memory_modifications, exported_globals, serialized_module, compilation_result) =
            match compilation_cache.get(&wasm_binary.binary) {
                None => {
                    sandbox_process.history.record(format!(
                        "CreateExecutionState(wasm_id={}, next_wasm_memory_id={})",
                        wasm_id, next_wasm_memory_id
                    ));
                    let reply = sandbox_process
                        .sandbox_service
                        .create_execution_state(protocol::sbxsvc::CreateExecutionStateRequest {
                            wasm_id,
                            wasm_binary: wasm_binary.binary.as_slice().to_vec(),
                            wasm_page_map: wasm_page_map.serialize(),
                            next_wasm_memory_id,
                            canister_id,
                            stable_memory_page_map: stable_memory_page_map.serialize(),
                        })
                        .sync()
                        .unwrap()
                        .0;
                    match reply {
                        Err(err) => {
                            compilation_cache.insert(&wasm_binary.binary, Err(err.clone()));
                            return Err(err);
                        }
                        Ok(reply) => {
                            let serialized_module = Arc::new(reply.serialized_module);
                            compilation_cache
                                .insert(&wasm_binary.binary, Ok(Arc::clone(&serialized_module)));
                            (
                                reply.wasm_memory_modifications,
                                reply.exported_globals,
                                serialized_module,
                                Some(reply.compilation_result),
                            )
                        }
                    }
                }
                Some(Err(err)) => {
                    return Err(err);
                }
                Some(Ok(serialized_module)) => {
                    sandbox_process.history.record(format!(
                        "CreateExecutionStateSerialized(wasm_id={}, next_wasm_memory_id={})",
                        wasm_id, next_wasm_memory_id
                    ));
                    let sandbox_result = sandbox_process
                        .sandbox_service
                        .create_execution_state_serialized(
                            protocol::sbxsvc::CreateExecutionStateSerializedRequest {
                                wasm_id,
                                serialized_module: Arc::clone(&serialized_module),
                                wasm_page_map: wasm_page_map.serialize(),
                                next_wasm_memory_id,
                                canister_id,
                                stable_memory_page_map: stable_memory_page_map.serialize(),
                            },
                        )
                        .sync()
                        .unwrap()
                        .0?;
                    (
                        sandbox_result.wasm_memory_modifications,
                        sandbox_result.exported_globals,
                        serialized_module,
                        None,
                    )
                }
            };

        cache_opened_wasm(
            &mut wasm_binary.embedder_cache.lock().unwrap(),
            &sandbox_process,
            wasm_id,
        );

        // Step 5. Create the execution state.
        let mut wasm_memory = Memory::new(wasm_page_map, memory_modifications.size);
        wasm_memory
            .page_map
            .deserialize_delta(memory_modifications.page_delta);
        wasm_memory.sandbox_memory =
            SandboxMemory::synced(wrap_remote_memory(&sandbox_process, next_wasm_memory_id));

        let stable_memory = Memory::new(
            stable_memory_page_map,
            ic_replicated_state::NumWasmPages::from(0),
        );
        Ok((
            wasm_binary,
            wasm_memory,
            stable_memory,
            exported_globals,
            ExportedFunctions::new(serialized_module.exported_functions.clone()),
            serialized_module.compilation_cost,
            compilation_result,
        ))
    }
}

impl SandboxedExecutionController {
    /// Create a new sandboxed execution controller. It provides the
    /// same interface as the `WasmExecutor`.
    pub fn new(
        embedder_config: &EmbeddersConfig,
        fd_factory: Arc<dyn PageAllocatorFileDescriptor>,
    ) -> std::io::Result<Self> {
        let launcher_exec_argv = create_launcher_argv().expect("No sandbox_launcher binary found");
        let min_sandbox_count = embedder_config.min_sandbox_count;
        let max_sandbox_count = embedder_config.max_sandbox_count;
        let max_sandbox_idle_time = embedder_config.max_sandbox_idle_time;
        let sandbox_exec_argv =
            create_sandbox_argv(embedder_config).expect("No canister_sandbox binary found");
        let backends = Arc::new(Mutex::new(HashMap::new()));

        let backends_copy = Arc::clone(&backends);

        std::thread::spawn(move || {
            SandboxedExecutionController::monitor_and_evict_sandbox_processes(
                backends_copy,
                min_sandbox_count,
                max_sandbox_count,
                max_sandbox_idle_time,
            );
        });

        let exit_watcher = Arc::new(ExitWatcher {
            backends: Arc::clone(&backends),
        });

        let (launcher_service, mut child) = spawn_launcher_process(
            &launcher_exec_argv[0],
            &launcher_exec_argv[1..],
            exit_watcher,
        )?;

        // We spawn a thread to wait for the exit notification of the launcher
        // process.
        thread::spawn(move || {
            let pid = child.id();
            let output = child.wait().unwrap();

            panic_due_to_exit(output, pid);
        });

        Ok(Self {
            backends,
            min_sandbox_count,
            max_sandbox_count,
            max_sandbox_idle_time,
            sandbox_exec_argv,
            launcher_service,
            fd_factory: Arc::clone(&fd_factory),
        })
    }

    // Periodically walk through all the backend processes and:
    // - evict inactive processes,
    // - update memory usage metrics.
    fn monitor_and_evict_sandbox_processes(
        backends: Arc<Mutex<HashMap<CanisterId, Backend>>>,
        min_sandbox_count: usize,
        max_sandbox_count: usize,
        max_sandbox_idle_time: Duration,
    ) {
        loop {
            {
                let mut guard = backends.lock().unwrap();
                evict_sandbox_processes(
                    &mut guard,
                    min_sandbox_count,
                    max_sandbox_count,
                    max_sandbox_idle_time,
                );
            }

            // Collect metrics sufficiently infrequently that it does not use
            // excessive compute resources. It might be sensible to scale this
            // based on the time measured to perform the collection and e.g.
            // ensure that we are 99% idle instead of using a static duration
            // here.
            std::thread::sleep(SANDBOX_PROCESS_UPDATE_INTERVAL);
        }
    }

    fn get_sandbox_process(&self, canister_id: CanisterId) -> Arc<SandboxProcess> {
        let mut guard = self.backends.lock().unwrap();

        if let Some(backend) = (*guard).get_mut(&canister_id) {
            let old = std::mem::replace(backend, Backend::Empty);
            let sandbox_process_and_stats = match old {
                Backend::Active {
                    sandbox_process,
                    stats,
                } => Some((sandbox_process, stats)),
                Backend::Evicted {
                    sandbox_process,
                    stats,
                } => sandbox_process.upgrade().map(|p| (p, stats)),
                Backend::Empty => None,
            };
            if let Some((sandbox_process, _stats)) = sandbox_process_and_stats {
                let now = std::time::Instant::now();
                if self.max_sandbox_count > 0 {
                    *backend = Backend::Active {
                        sandbox_process: Arc::clone(&sandbox_process),
                        stats: SandboxProcessStats { last_used: now },
                    };
                } else {
                    *backend = Backend::Evicted {
                        sandbox_process: Arc::downgrade(&sandbox_process),
                        stats: SandboxProcessStats { last_used: now },
                    };
                }
                return sandbox_process;
            }
        }

        if guard.len() > self.max_sandbox_count {
            let to_evict = self.max_sandbox_count * SANDBOX_PROCESS_EVICTION_PERCENT / 100;
            let max_active_sandboxes = self.max_sandbox_count.saturating_sub(to_evict);
            evict_sandbox_processes(
                &mut guard,
                self.min_sandbox_count,
                max_active_sandboxes,
                self.max_sandbox_idle_time,
            );
        }

        // No sandbox process found for this canister. Start a new one and register it.
        let reg = Arc::new(ActiveExecutionStateRegistry::new());
        let controller_service = ControllerServiceImpl::new(Arc::clone(&reg));

        let (sandbox_service, pid) = create_sandbox_process(
            controller_service,
            &*self.launcher_service,
            canister_id,
            self.sandbox_exec_argv.clone(),
        )
        .unwrap();

        let sandbox_process = Arc::new(SandboxProcess {
            execution_states: reg,
            sandbox_service,
            pid,
            history: SandboxProcessRequestHistory::new(),
        });

        let now = std::time::Instant::now();
        let backend = Backend::Active {
            sandbox_process: Arc::clone(&sandbox_process),
            stats: SandboxProcessStats { last_used: now },
        };
        (*guard).insert(canister_id, backend);

        sandbox_process
    }

    #[allow(clippy::too_many_arguments)]
    fn process_completion(
        self: &Arc<Self>,
        canister_id: CanisterId,
        wasm_memory: &Memory,
        stable_memory: &Memory,
        result: CompletionResult,
        next_wasm_memory_id: MemoryId,
        next_stable_memory_id: MemoryId,
        message_instruction_limit: NumInstructions,
        sandbox_process: Arc<SandboxProcess>,
    ) -> WasmExecutionResult {
        let mut exec_output = match result {
            CompletionResult::Finished(exec_output) => {
                let _execution_status = match exec_output.wasm.wasm_result.clone() {
                    Ok(Some(WasmResult::Reply(_))) => "Success",
                    Ok(Some(WasmResult::Reject(_))) => "Reject",
                    Ok(None) => "NoResponse",
                    Err(e) => e.as_str(),
                };
                exec_output
            }
            _ => todo!(),
        };

        // If sandbox is compromised this value could be larger than the initial limit.
        if exec_output.wasm.num_instructions_left > message_instruction_limit {
            exec_output.wasm.num_instructions_left = message_instruction_limit;
        }

        let canister_state_changes = self.update_execution_state(
            &mut exec_output,
            wasm_memory,
            stable_memory,
            next_wasm_memory_id,
            next_stable_memory_id,
            canister_id,
            sandbox_process,
        );

        WasmExecutionResult::Finished(exec_output.slice, exec_output.wasm, canister_state_changes)
    }

    // Unless execution trapped, commit state (applying execution state
    // changes, returning system state changes to caller).
    #[allow(clippy::too_many_arguments)]
    fn update_execution_state(
        &self,
        exec_output: &mut SandboxExecOutput,
        wasm_memory: &Memory,
        stable_memory: &Memory,
        next_wasm_memory_id: MemoryId,
        next_stable_memory_id: MemoryId,
        _canister_id: CanisterId,
        sandbox_process: Arc<SandboxProcess>,
    ) -> Option<CanisterStateChanges> {
        // If the execution has failed, then we don't apply any changes.
        if exec_output.wasm.wasm_result.is_err() {
            return None;
        }
        match exec_output.state.take() {
            None => None,
            Some(state_modifications) => {
                // TODO: If a canister has broken out of wasm then it might have allocated more
                // wasm or stable memory then allowed. We should add an additional check here
                // that thet canister is still within it's allowed memory usage.
                let mut wasm_memory = wasm_memory.clone();
                wasm_memory
                    .page_map
                    .deserialize_delta(state_modifications.wasm_memory.page_delta);
                wasm_memory.size = state_modifications.wasm_memory.size;
                wasm_memory.sandbox_memory = SandboxMemory::synced(wrap_remote_memory(
                    &sandbox_process,
                    next_wasm_memory_id,
                ));
                let mut stable_memory = stable_memory.clone();
                stable_memory
                    .page_map
                    .deserialize_delta(state_modifications.stable_memory.page_delta);
                stable_memory.size = state_modifications.stable_memory.size;
                stable_memory.sandbox_memory = SandboxMemory::synced(wrap_remote_memory(
                    &sandbox_process,
                    next_stable_memory_id,
                ));
                Some(CanisterStateChanges {
                    globals: state_modifications.globals,
                    wasm_memory,
                    stable_memory,
                    system_state_changes: state_modifications.system_state_changes,
                })
            }
        }
    }
}

/// Cache the sandbox process and wasm id of the opened wasm in the embedder
/// cache.
fn cache_opened_wasm(
    embedder_cache: &mut Option<EmbedderCache>,
    sandbox_process: &Arc<SandboxProcess>,
    wasm_id: WasmId,
) {
    let opened_wasm: HypervisorResult<OpenedWasm> =
        Ok(OpenedWasm::new(Arc::downgrade(sandbox_process), wasm_id));
    *embedder_cache = Some(EmbedderCache::new(opened_wasm));
}

/// Cache an error from compilation so that we don't try to recompile just to
/// get the same error.
fn cache_errored_wasm(embedder_cache: &mut Option<EmbedderCache>, err: HypervisorError) {
    let cache: HypervisorResult<OpenedWasm> = Err(err);
    *embedder_cache = Some(EmbedderCache::new(cache));
}

// Get compiled wasm object in sandbox. Ask cache first, upload + compile if
// needed.
fn open_wasm(
    sandbox_process: &Arc<SandboxProcess>,
    wasm_binary: &WasmBinary,
    compilation_cache: Arc<CompilationCache>,
    _canister_id: CanisterId,
) -> HypervisorResult<(WasmId, Option<CompilationResult>)> {
    let mut embedder_cache = wasm_binary.embedder_cache.lock().unwrap();
    if let Some(cache) = embedder_cache.as_ref() {
        if let Some(opened_wasm) = cache.downcast::<HypervisorResult<OpenedWasm>>() {
            match opened_wasm {
                Ok(opened_wasm) => {
                    if let Some(cached_sandbox_process) = opened_wasm.sandbox_process.upgrade() {
                        assert!(Arc::ptr_eq(&cached_sandbox_process, sandbox_process));
                        return Ok((opened_wasm.wasm_id, None));
                    }
                }
                Err(err) => {
                    return Err(err.clone());
                }
            }
        }
    }

    let wasm_id = WasmId::new();
    match compilation_cache.get(&wasm_binary.binary) {
        None => {
            sandbox_process
                .history
                .record(format!("OpenWasm(wasm_id={})", wasm_id));
            match sandbox_process
                .sandbox_service
                .open_wasm(protocol::sbxsvc::OpenWasmRequest {
                    wasm_id,
                    wasm_src: wasm_binary.binary.as_slice().to_vec(),
                })
                .sync()
                .unwrap()
                .0
            {
                Ok((compilation_result, serialized_module)) => {
                    cache_opened_wasm(&mut embedder_cache, sandbox_process, wasm_id);
                    compilation_cache.insert(&wasm_binary.binary, Ok(Arc::new(serialized_module)));
                    Ok((wasm_id, Some(compilation_result)))
                }
                Err(err) => {
                    compilation_cache.insert(&wasm_binary.binary, Err(err.clone()));
                    cache_errored_wasm(&mut embedder_cache, err.clone());
                    Err(err)
                }
            }
        }
        Some(Err(err)) => {
            cache_errored_wasm(&mut embedder_cache, err.clone());
            Err(err)
        }
        Some(Ok(serialized_module)) => {
            sandbox_process
                .history
                .record(format!("OpenWasmSerialized(wasm_id={})", wasm_id));
            sandbox_process
                .sandbox_service
                .open_wasm_serialized(protocol::sbxsvc::OpenWasmSerializedRequest {
                    wasm_id,
                    serialized_module: Arc::clone(&serialized_module.bytes),
                })
                .on_completion(|_| ());
            cache_opened_wasm(&mut embedder_cache, sandbox_process, wasm_id);
            Ok((wasm_id, None))
        }
    }
}

// Returns the id of the remote memory after making sure that the remote memory
// is in sync with the local memory.
fn open_remote_memory(
    sandbox_process: &Arc<SandboxProcess>,
    memory: &Memory,
) -> SandboxMemoryHandle {
    let mut guard = memory.sandbox_memory.lock().unwrap();
    if let SandboxMemory::Synced(id) = &*guard {
        if let Some(pid) = id.get_sandbox_process_id() {
            // There is a at most one sandbox process per canister at any time.
            assert_eq!(pid, sandbox_process.pid as usize);
            return id.clone();
        }
    }

    // Here we have two cases:
    // 1) either the memory was never synchronized with any sandbox process,
    // 2) or the memory was synchronized was some sandbox process that got evicted
    //    and terminated in the meantime.
    // In both cases, we need to synchronize the memory with the given sandbox
    // process.

    let serialized_page_map = memory.page_map.serialize();
    let serialized_memory = MemorySerialization {
        page_map: serialized_page_map,
        num_wasm_pages: memory.size,
    };
    let memory_id = MemoryId::new();
    sandbox_process
        .history
        .record(format!("OpenMemory(memory_id={})", memory_id));
    sandbox_process
        .sandbox_service
        .open_memory(protocol::sbxsvc::OpenMemoryRequest {
            memory_id,
            memory: serialized_memory,
        })
        .on_completion(|_| {});
    let handle = wrap_remote_memory(sandbox_process, memory_id);
    *guard = SandboxMemory::Synced(handle.clone());
    handle
}

fn wrap_remote_memory(
    sandbox_process: &Arc<SandboxProcess>,
    memory_id: MemoryId,
) -> SandboxMemoryHandle {
    let opened_memory = OpenedMemory::new(Arc::downgrade(sandbox_process), memory_id);
    SandboxMemoryHandle::new(Arc::new(opened_memory))
}

// Evicts some sandbox process backends according to the heuristics of the
// `sandbox_process_eviction::evict()` function. See the comments of that
// function for the explanation of the threshold parameters.
fn evict_sandbox_processes(
    backends: &mut HashMap<CanisterId, Backend>,
    min_active_sandboxes: usize,
    max_active_sandboxes: usize,
    max_sandbox_idle_time: Duration,
) {
    // Remove the already terminated processes.
    backends.retain(|_id, backend| match backend {
        Backend::Active { .. } => true,
        Backend::Evicted {
            sandbox_process, ..
        } => {
            // Once `strong_count` reaches zero, then `upgrade()` will always
            // return `None`. This means that such entries never be used again,
            // so it is safe to remove them from the hash map.
            sandbox_process.strong_count() > 0
        }
        Backend::Empty => false,
    });

    let candidates: Vec<_> = backends
        .iter()
        .filter_map(|(id, backend)| match backend {
            Backend::Active { stats, .. } => Some(EvictionCandidate {
                id: *id,
                last_used: stats.last_used,
            }),
            Backend::Evicted { .. } | Backend::Empty => None,
        })
        .collect();

    let last_used_threshold = match Instant::now().checked_sub(max_sandbox_idle_time) {
        Some(threshold) => threshold,
        None => {
            // This case may happen on MacOS where `Instant::now()` returns the time after the reboot.
            // Since `Instant` doesn't have a default/zero value, we return the oldest `last_used`.
            candidates
                .iter()
                .map(|x| x.last_used)
                .min()
                .unwrap_or_else(Instant::now)
        }
    };

    let evicted = sandbox_process_eviction::evict(
        candidates,
        min_active_sandboxes,
        max_active_sandboxes,
        last_used_threshold,
    );

    // Actually evict all the selected eviction candidates.
    for EvictionCandidate { id, .. } in evicted.iter() {
        if let Some(backend) = backends.get_mut(id) {
            let old = std::mem::replace(backend, Backend::Empty);
            let new = match old {
                Backend::Active {
                    sandbox_process,
                    stats,
                } => Backend::Evicted {
                    sandbox_process: Arc::downgrade(&sandbox_process),
                    stats,
                },
                Backend::Evicted { .. } | Backend::Empty => old,
            };
            *backend = new;
        }
    }
}

pub fn panic_due_to_exit(output: ExitStatus, pid: u32) {
    match output.code() {
        Some(code) => panic!(
            "Error from launcher process, pid {} exited with status code: {}",
            pid, code
        ),
        None => panic!(
            "Error from launcher process, pid {} exited due to signal!",
            pid
        ),
    }
}

/// Service responsible for printing the history of a canister's activity when
/// it unexpectedly exits.
struct ExitWatcher {
    backends: Arc<Mutex<HashMap<CanisterId, Backend>>>,
}

impl ControllerLauncherService for ExitWatcher {
    fn sandbox_exited(
        &self,
        req: protocol::ctllaunchersvc::SandboxExitedRequest,
    ) -> ic_canister_sandbox_common::rpc::Call<protocol::ctllaunchersvc::SandboxExitedReply> {
        let guard = self.backends.lock().unwrap();
        let _sandbox_process = match guard.get(&req.canister_id).unwrap_or_else(|| {
            panic!(
                "Sandbox exited for unrecognized canister id {}",
                req.canister_id,
            )
        }) {
            Backend::Active {
                sandbox_process, ..
            } => sandbox_process,
            Backend::Evicted { .. } | Backend::Empty => {
                return rpc::Call::new_resolved(Ok(protocol::ctllaunchersvc::SandboxExitedReply));
            }
        };
        rpc::Call::new_resolved(Ok(protocol::ctllaunchersvc::SandboxExitedReply))
    }
}
