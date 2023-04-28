mod active_execution_state_registry;
mod controller_service_impl;
mod launch_as_process;
mod process_exe_and_args;
mod sandbox_process_eviction;
mod sandboxed_execution_controller;

use lazy_static::lazy_static;
use std::sync::Mutex;

use crate::sandboxed_execution_controller::SandboxedExecutionController;
use hs_bindgen::*;
use ic_base_types::NumSeconds;
use ic_config::embedders::Config as EmbeddersConfig;
use ic_config::execution_environment::Config as ExecutionConfig;
use ic_config::flag_status::FlagStatus;
use ic_config::subnet_config::SubnetConfigs;
use ic_cycles_account_manager::CyclesAccountManager;
use ic_embedders::wasm_executor::WasmExecutionResult::Finished;
use ic_embedders::CompilationCache;
use ic_embedders::WasmExecutionInput;
use ic_interfaces::execution_environment::{ExecutionMode, SubnetAvailableMemory};
use ic_registry_subnet_type::SubnetType;
use ic_replicated_state::canister_state::execution_state::WasmBinary;
use ic_replicated_state::canister_state::DEFAULT_QUEUE_CAPACITY;
use ic_replicated_state::page_map::TestPageAllocatorFileDescriptorImpl;
use ic_replicated_state::{ExportedFunctions, Global, Memory};
use ic_system_api::sandbox_safe_system_state::CanisterStatusView;
use ic_system_api::sandbox_safe_system_state::SandboxSafeSystemState;
use ic_system_api::ApiType;
use ic_system_api::ResponseStatus::NotRepliedYet;
use ic_system_api::{ExecutionParameters, InstructionLimits};
use ic_types::ingress::WasmResult;
use ic_types::messages::CallContextId;
use ic_types::methods::{FuncRef, SystemMethod, WasmMethod};
use ic_types::MemoryAllocation;
use ic_types::{CanisterId, PrincipalId, SubnetId};
use ic_types::{CanisterTimer, ComputeAllocation, Cycles, Time};
use ic_wasm_types::CanisterModule;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use base64::{engine::general_purpose, Engine as _};

use serde::{Deserialize, Serialize};
use serde_cbor::{from_slice, to_vec};
use serde_with::{serde_as, Bytes};

struct RuntimeState {
    pub controller: Arc<SandboxedExecutionController>,
    pub cycles_account_manager: CyclesAccountManager,
    pub wasm_binary: Arc<WasmBinary>,
    pub wasm_memory: Memory,
    pub stable_memory: Memory,
    pub exported_functions: ExportedFunctions,
    pub exported_globals: Vec<Global>,
}

lazy_static! {
    static ref STATE: Mutex<BTreeMap<CanisterId, RuntimeState>> = Mutex::new(BTreeMap::new());
}

#[derive(Serialize)]
enum CanisterResponse {
    NoResponse(()),
    CanisterReply(serde_bytes::ByteBuf),
    CanisterReject(String),
    CanisterTrap(String),
}

type RuntimeCycles = (u64, u64);

#[derive(Serialize)]
struct RuntimeResponse {
    pub response: CanisterResponse,
    pub cycles_accept: RuntimeCycles,
    pub cycles_mint: RuntimeCycles,
    pub new_certified_data: Option<serde_bytes::ByteBuf>,
    pub new_global_timer: Option<u64>,
}

impl RuntimeResponse {
    pub fn trap(m: String) -> Self {
        RuntimeResponse {
            response: CanisterResponse::CanisterTrap(m),
            cycles_accept: (0, 0),
            cycles_mint: (0, 0),
            new_certified_data: None,
            new_global_timer: None,
        }
    }

    pub fn noop() -> Self {
        RuntimeResponse {
            response: CanisterResponse::NoResponse(()),
            cycles_accept: (0, 0),
            cycles_mint: (0, 0),
            new_certified_data: None,
            new_global_timer: None,
        }
    }
}

impl RuntimeState {
    fn execute(&mut self, input: WasmExecutionInput) -> Result<RuntimeResponse, String> {
        let res = self
            .controller
            .execute(
                input,
                &self.wasm_binary,
                &self.wasm_memory,
                &self.stable_memory,
                self.exported_globals.clone(),
            )
            .1;
        let (exec_output_wasm, state_changes) = match res {
            Finished(_, exec_output_wasm, state_changes) => (exec_output_wasm, state_changes),
            _ => panic!("DTS should be disabled"),
        };
        if let Err(e) = exec_output_wasm.wasm_result {
            return Err(e.to_string());
        }
        let mut new_certified_data = None;
        let mut new_global_timer = None;
        if let Some(state_changes) = state_changes {
            self.wasm_memory = state_changes.wasm_memory;
            self.stable_memory = state_changes.stable_memory;
            self.exported_globals = state_changes.globals;
            new_certified_data = state_changes
                .system_state_changes
                .new_certified_data
                .map(serde_bytes::ByteBuf::from);
            new_global_timer =
                state_changes
                    .system_state_changes
                    .new_global_timer
                    .map(|t| match t {
                        CanisterTimer::Inactive => 0,
                        CanisterTimer::Active(t) => t.as_nanos_since_unix_epoch(),
                    });
        }
        let output = match exec_output_wasm.wasm_result {
            Ok(Some(WasmResult::Reply(bytes))) => {
                CanisterResponse::CanisterReply(serde_bytes::ByteBuf::from(bytes))
            }
            Ok(Some(WasmResult::Reject(msg))) => CanisterResponse::CanisterReject(msg),
            Ok(None) => CanisterResponse::NoResponse(()),
            Err(_) => panic!("unreachable"),
        };
        Ok(RuntimeResponse {
            response: output,
            // new_calls:          // TODO(0): new calls
            cycles_accept: (0, 0), // TODO(1): cycles accepted
            cycles_mint: (0, 0),   // TODO(2): cycles minted
            new_certified_data,
            new_global_timer,
        })
    }
}

#[serde_as]
#[derive(Debug, Clone, Deserialize, Serialize)]
struct Certificate {
    #[serde_as(deserialize_as = "Bytes")]
    bytes: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Clone, Deserialize, Serialize)]
struct Env {
    #[serde_as(deserialize_as = "Bytes")]
    canister_id: Vec<u8>,
    time: u64,
    balance: RuntimeCycles,
    status: CanisterStatusView,
    certificate: Option<Certificate>,
    canister_version: u64,
    global_timer: u64,
    controllers: Vec<PrincipalId>, 
    memory_allocation: u64,
    freeze_threshold: u64,
    subnet_id: SubnetId, 
    subnet_type: SubnetType,
    subnet_size: u64,
    all_subnets: Vec<CanisterId>,
}

impl Env {
    fn new(can_id: Vec<u8>) -> Env {
        Env {
            canister_id: can_id,
            time: 0u64,
            balance: (0, (1 << 40)),
            status: CanisterStatusView::Running,
            certificate: None,
            canister_version: 0u64,
            global_timer: 0u64, 
            controllers: vec![], 
            memory_allocation: 0, 
            freeze_threshold: 1 << 5, 
            subnet_id: SubnetId::from(PrincipalId::default()),
            subnet_type: SubnetType::Application,
            subnet_size: 2,
            all_subnets: vec![],
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct WasmClosure {
    closure_idx: i32,
    closure_env: i32,
}

#[derive(Debug, Deserialize, Serialize)]
struct Callback {
    reply_closure: WasmClosure,
    reject_closure: WasmClosure,
    cleanup_closure: Option<WasmClosure>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct ResponseReply {
    #[serde_as(deserialize_as = "Bytes")]
    reply_payload: Vec<u8>,
}

#[derive(Debug, Deserialize, Serialize)]
struct ResponseReject {
    reject_code: u8,
    reject_msg: String,
}

#[derive(Debug, Deserialize, Serialize)]
enum Response {
    Reply(ResponseReply),
    Reject(ResponseReject),
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeInstantiate {
    #[serde_as(deserialize_as = "Bytes")]
    module: Vec<u8>,
    prefix: String,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeInitialize {
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeUpdate {
    method: String,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    needs_to_respond: bool,
    cycles: RuntimeCycles,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeQuery {
    method: String,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeCallback {
    callback: Callback,
    env: Env,
    needs_to_respond: bool,
    cycles_available: RuntimeCycles,
    response: Response,
    refunded_cycles: RuntimeCycles,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeCleanup {
    wasm_closure: WasmClosure,
    env: Env,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimePreUpgrade {
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimePostUpgrade {
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeInspectMessage {
    method: String,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[derive(Debug, Deserialize, Serialize)]
struct RuntimeHeartbeat {
    env: Env,
}

#[derive(Debug, Deserialize, Serialize)]
struct RuntimeGlobalTimer {
    env: Env,
}

#[derive(Debug, Deserialize, Serialize)]
enum RuntimeInvokeEnum {
    RuntimeInstantiate(RuntimeInstantiate),
    RuntimeInitialize(RuntimeInitialize),
    RuntimeUpdate(RuntimeUpdate),
    RuntimeQuery(RuntimeQuery),
    RuntimeCallback(RuntimeCallback),
    RuntimeCleanup(RuntimeCleanup),
    RuntimePreUpgrade(RuntimePreUpgrade),
    RuntimePostUpgrade(RuntimePostUpgrade),
    RuntimeInspectMessage(RuntimeInspectMessage),
    RuntimeHeartbeat(RuntimeHeartbeat),
    RuntimeGlobalTimer(RuntimeGlobalTimer),
}

fn get_env(rte: &RuntimeInvokeEnum) -> Option<Env> {
    match rte {
        RuntimeInvokeEnum::RuntimeInstantiate(_) => {None},
        RuntimeInvokeEnum::RuntimeInitialize(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeUpdate(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeQuery(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeCallback(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeCleanup(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimePreUpgrade(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimePostUpgrade(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeInspectMessage(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeHeartbeat(x) => {Some(x.env.clone())},
        RuntimeInvokeEnum::RuntimeGlobalTimer(x) => {Some(x.env.clone())},
    }
}


#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct RuntimeInvoke {
    #[serde_as(deserialize_as = "Bytes")]
    canister_id: Vec<u8>,
    entry_point: RuntimeInvokeEnum,
}

#[hs_bindgen(invoke :: CString -> IO CString)]
pub fn invoke(arg: &str) -> String {
    let r: RuntimeInvoke =
        from_slice(general_purpose::STANDARD.decode(arg).unwrap().as_slice()).unwrap();
    let canister_id: CanisterId = r.canister_id.clone().try_into().unwrap();
    let env: Env = get_env(&r.entry_point).unwrap_or(Env::new(r.canister_id.clone()));

    let mut state_map = STATE.lock().unwrap();

    let subnet_type = env.subnet_type;
    let subnet_id = env.subnet_id;

    let subnet_config = SubnetConfigs::default().own_subnet_config(subnet_type);
    let dirty_page_overhead = subnet_config.scheduler_config.dirty_page_overhead;

    match r.entry_point {
        RuntimeInvokeEnum::RuntimeInstantiate(ref x) => {
            let mut embedder_config = EmbeddersConfig::new();
            embedder_config.subnet_type = subnet_type;
            embedder_config.dirty_page_overhead = dirty_page_overhead;
            let controller = Arc::new(
                SandboxedExecutionController::new(
                    &embedder_config,
                    Arc::new(TestPageAllocatorFileDescriptorImpl::new()),
                    &x.prefix,
                )
                .unwrap(),
            );
            let cycles_account_manager = CyclesAccountManager::new(
                subnet_config.scheduler_config.max_instructions_per_message,
                subnet_type,
                subnet_id,
                subnet_config.cycles_account_manager_config,
            );
            let canister_module = CanisterModule::new(x.module.clone());
            let (
                wasm_binary,
                wasm_memory,
                stable_memory,
                exported_globals,
                exported_functions,
                _,
                _,
            ) = controller
                .create_execution_state(
                    canister_module,
                    canister_id,
                    Arc::new(CompilationCache::default()),
                )
                .unwrap();
            let current_state: RuntimeState = RuntimeState {
                controller,
                cycles_account_manager,
                wasm_binary,
                wasm_memory,
                stable_memory,
                exported_functions,
                exported_globals,
            };
            state_map.insert(canister_id, current_state);
        }
        _ => {}
    };

    let current_state = state_map.get_mut(&canister_id).unwrap();

    let call_ctx_id = dummy_below; // put it everywhere and leave dummy as TODO

    let sandbox_safe_system_state = SandboxSafeSystemState::new_internal(
        canister_id,
        env.status, 
        NumSeconds::new(env.freeze_threshold),
        if env.memory_allocation == 0 {MemoryAllocation::BestEffort} else {MemoryAllocation::Reserved((env.memory_allocation).into())}, 
        Cycles::new(((env.balance.0 as u128) << 64) + env.balance.1 as u128),
        BTreeMap::new(),                    // TODO(9): call_context_balances (only the current call context suffices). map cycles attached to call (call context (which the caller supplies)) 
        current_state.cycles_account_manager,
        Some(666),                          // TODO(10): next_callback_id
        BTreeMap::new(),
        DEFAULT_QUEUE_CAPACITY,
        BTreeSet::from_iter(env.all_subnets.into_iter()), 
        env.subnet_size.try_into().unwrap(),
        dirty_page_overhead,
        CanisterTimer::from_time(Time::from_nanos_since_unix_epoch(env.global_timer)),
        env.canister_version, 
        BTreeSet::from_iter(env.controllers.into_iter()),
    );

    let exec_config = ExecutionConfig::default();
    let subnet_available_memory: SubnetAvailableMemory = SubnetAvailableMemory::new(
        exec_config.subnet_memory_capacity.get() as i64,
        exec_config.subnet_message_memory_capacity.get() as i64,
        exec_config
            .subnet_wasm_custom_sections_memory_capacity
            .get() as i64,
    );

    let execution_parameters = ExecutionParameters {
        instruction_limits: InstructionLimits::new(
            FlagStatus::Disabled,
            subnet_config.scheduler_config.max_instructions_per_message,
            subnet_config.scheduler_config.max_instructions_per_slice,
        ),
        canister_memory_limit: exec_config.max_canister_memory_size,
        compute_allocation: ComputeAllocation::default(), // TODO(16): compute allocation
        subnet_type,
        execution_mode: ExecutionMode::Replicated, // TODO(17): replicated vs non-replicated
    };

    let (method, api_type) = match r.entry_point {
        RuntimeInvokeEnum::RuntimeInstantiate(_) => {
            if !current_state
                .exported_functions
                .has_method(&WasmMethod::System(SystemMethod::CanisterStart))
            {
                return general_purpose::STANDARD.encode(to_vec(&RuntimeResponse::noop()).unwrap());
            }
            (
                WasmMethod::System(SystemMethod::CanisterStart),
                ApiType::Start {
                    time: env.time, // using the default env's default time
                },
            )
        }
        RuntimeInvokeEnum::RuntimeInitialize(x) => (
            WasmMethod::System(SystemMethod::CanisterInit),
            ApiType::Init {
                time: x.env.time,  
                incoming_payload: x.arg,
                caller: x.caller.try_into().unwrap(),
            },
        ),
        RuntimeInvokeEnum::RuntimeUpdate(x) => (
            WasmMethod::Update(x.method),
            ApiType::Update {
                time: x.env.time,                           // TODO? 
                incoming_payload: vec![],                   // TODO
                incoming_cycles: Cycles::new(0),            // TODO
                caller: PrincipalId::default(),             // EntityId from EntryPoint data
                call_context_id: CallContextId::from(42),   // TODO
                response_data: vec![],                      // final 
                response_status: NotRepliedYet,             // final 
                outgoing_request: None,                     // final
                max_reply_size: 2000000.into(),             // TODO 
            },
        ),
        RuntimeInvokeEnum::RuntimeQuery(x) => {(
            WasmMethod::Query(x.method),
            if execution_parameters.execution_mode  == ExecutionMode::Replicated {
                ApiType::ReplicatedQuery {
                    time: Time,
                    incoming_payload: Vec<u8>,
                    caller: PrincipalId,
                    response_data: Vec<u8>,
                    response_status: ResponseStatus,
                    data_certificate: Option<Vec<u8>>,
                    max_reply_size: NumBytes,
                }
            } else {
                ApiType::NonReplicatedQuery {
                    time: Time,
                    caller: PrincipalId,
                    own_subnet_id: SubnetId,
                    incoming_payload: Vec<u8>,
                    data_certificate: Option<Vec<u8>>,
                    // Begins as empty and used to accumulate data for sending replies.
                    response_data: Vec<u8>,
                    response_status: ResponseStatus,
                    max_reply_size: NumBytes,
                    query_kind: NonReplicatedQueryKind,
                }
            }
        )},
        RuntimeInvokeEnum::RuntimeCallback(x) => {
            match x.response {
                Response::Reply(response_reply) => {
                    (
                        WasmMethod::System(SystemMethod::Empty),  // TODO: no idea what WasmMethod to use here
                        ApiType::ReplyCallback { 
                            time: (), 
                            incoming_payload: (), 
                            incoming_cycles: (), 
                            call_context_id: (), 
                            response_data: (), 
                            response_status: (), 
                            outgoing_request: (), 
                            max_reply_size: (), 
                            execution_mode: () 
                        }
                    )
                }
                Response::Reject(response_reject) => {
                    (
                        WasmMethod::System(SystemMethod::Empty),  // TODO: no idea what WasmMethod to use here
                        ApiType::RejectCallback { 
                            time: Time,
                            reject_context: RejectContext,
                            incoming_cycles: Cycles,
                            call_context_id: CallContextId,
                            // Begins as empty and used to accumulate data for sending replies.
                            #[serde(with = "serde_bytes")]
                            response_data: Vec<u8>,
                            response_status: ResponseStatus,
                            /// Optional outgoing request under construction. If `None` no outgoing
                            /// request is currently under construction.
                            outgoing_request: Option<RequestInPrep>,
                            max_reply_size: NumBytes,
                            execution_mode: ExecutionMode,
                        }
                    )
                }
            }

        },
        RuntimeInvokeEnum::RuntimeCleanup(x) => {
            (
                WasmMethod::System(SystemMethod::Empty),
                ApiType::Cleanup { time: x.env.time }
            )
        },
        RuntimeInvokeEnum::RuntimePreUpgrade(x) => {
            (
                WasmMethod::System(SystemMethod::CanisterPreUpgrade),
                ApiType::PreUpgrade { 
                    caller: (), 
                    time: x.env.time,
                }
            )
        },
        RuntimeInvokeEnum::RuntimePostUpgrade(x) => {

        },
        RuntimeInvokeEnum::RuntimeInspectMessage(x) => {
            if !current_state
                .exported_functions
                .has_method(&WasmMethod::System(SystemMethod::CanisterInspectMessage))
            {
                return general_purpose::STANDARD.encode(to_vec(&RuntimeResponse::noop()).unwrap());
            }
            (
                WasmMethod::System(SystemMethod::CanisterInspectMessage),
                InspectMessage {
                    caller: PrincipalId::default(),
                    method_name: x.method,
                    incoming_payload: vec![],
                    time: Time::from_nanos_since_unix_epoch(0),
                    message_accepted: false,
                },
            )
        }
        RuntimeInvokeEnum::RuntimeHeartbeat(_) => {
            return general_purpose::STANDARD.encode(to_vec(&RuntimeResponse::noop()).unwrap());
        }
        RuntimeInvokeEnum::RuntimeGlobalTimer(x) => {

        },
        _ => {
            return general_purpose::STANDARD.encode(to_vec(&RuntimeResponse::noop()).unwrap());
        }
    };
    

    let input = WasmExecutionInput {
        api_type,
        sandbox_safe_system_state: sandbox_safe_system_state.clone(),
        canister_current_memory_usage: 0.into(),
        execution_parameters: execution_parameters.clone(),
        subnet_available_memory,
        func_ref: FuncRef::Method(method),
        compilation_cache: Arc::new(CompilationCache::default()),
    };
    let res = current_state.execute(input);
    if let Err(e) = res {
        return general_purpose::STANDARD.encode(to_vec(&RuntimeResponse::trap(e)).unwrap());
    }
    general_purpose::STANDARD.encode(to_vec(&res.unwrap()).unwrap())
}
