mod active_execution_state_registry;
mod controller_service_impl;
mod launch_as_process;
mod process_exe_and_args;
mod sandbox_process_eviction;
mod sandboxed_execution_controller;

use crate::sandboxed_execution_controller::SandboxedExecutionController;
use hs_bindgen::*;
use ic_base_types::NumBytes;
use ic_base_types::NumSeconds;
use ic_config::embedders::Config as EmbeddersConfig;
use ic_config::execution_environment::Config as ExecutionConfig;
use ic_config::flag_status::FlagStatus;
use ic_config::subnet_config::SubnetConfigs;
use ic_cycles_account_manager::CyclesAccountManager;
use ic_embedders::wasm_executor::CanisterStateChanges;
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
use ic_system_api::sandbox_safe_system_state::SystemStateChanges;
use ic_system_api::ApiType;
use ic_system_api::NonReplicatedQueryKind;
use ic_system_api::ResponseStatus::{AlreadyReplied, NotRepliedYet};
use ic_system_api::{ExecutionParameters, InstructionLimits};
use ic_types::ingress::WasmResult;
use ic_types::messages::inter_canister::{Callback, WasmClosure};
use ic_types::messages::CallContextId;
use ic_types::methods::{FuncRef, SystemMethod, WasmMethod};
use ic_types::MemoryAllocation;
use ic_types::NumInstructions;
use ic_types::{CanisterId, PrincipalId, SubnetId};
use ic_types::{CanisterTimer, Cycles, Time};
use ic_wasm_types::CanisterModule;
use lazy_static::lazy_static;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::sync::Mutex;

use base64::{engine::general_purpose, Engine as _};

use serde::{Deserialize, Serialize};
use serde_cbor::{from_slice, to_vec};
use serde_with::{serde_as, Bytes};

struct RuntimeState {
    // the following field never changes
    pub cycles_account_manager: CyclesAccountManager,

    // the following two fields are only committed after successful ApiType::Init
    pub wasm_binary: Arc<WasmBinary>,
    pub exported_functions: ExportedFunctions,

    // during ApiType::Start and ApiType::Init, we use the following two fields instead
    pub new_wasm_binary: Option<Arc<WasmBinary>>,
    pub new_exported_functions: Option<ExportedFunctions>,

    // the following three fields
    //   are input to every execution except for ApiType::Start and ApiType::Init
    //   are committed after every successful execution except for ApiType::PreUpgrade and ApiType::Start
    pub wasm_memory: Memory,
    pub exported_globals: Vec<Global>,
    pub stable_memory: Memory,

    // the following two fields
    //   are input to ApiType::Start and ApiType::Init
    //   are committed after successful execution of ApiType::Start
    pub persisted_wasm_memory: Option<Memory>,
    pub persisted_exported_globals: Option<Vec<Global>>,
    // the following field
    //   is input to ApiType::Start and ApiType::Init
    //   is committed after successful execution of ApiType::PreUpgrade and ApiType::Start
    pub persisted_stable_memory: Option<Memory>,
}

lazy_static! {
    static ref STATE: Mutex<BTreeMap<CanisterId, RuntimeState>> = Mutex::new(BTreeMap::new());
    static ref CTRL_MAP: Mutex<BTreeMap<SubnetId, Arc<SandboxedExecutionController>>> =
        Mutex::new(BTreeMap::new());
    static ref PREFIX: Mutex<Option<String>> = Mutex::new(None);
}

#[derive(Serialize)]
enum CanisterResponse {
    NoResponse(()),
    CanisterReply(serde_bytes::ByteBuf),
    CanisterReject(String),
    CanisterTrap(String),
}

type RuntimeCycles = (u64, u64);

pub fn to_cycles(rtc: RuntimeCycles) -> Cycles {
    Cycles::new(((rtc.0 as u128) << 64) + rtc.1 as u128)
}

#[derive(Serialize)]
struct RuntimeMethodCall {
    call_callee: CanisterId,
    call_method_name: String,
    #[serde(with = "serde_bytes")]
    call_arg: Vec<u8>,
    call_callback: Callback,
    call_transferred_cycles: RuntimeCycles,
}

#[derive(Serialize)]
struct RuntimeResponse {
    pub response: CanisterResponse,
    pub cycles_accept: RuntimeCycles,
    pub cycles_mint: RuntimeCycles,
    pub new_certified_data: Option<serde_bytes::ByteBuf>,
    pub new_global_timer: Option<u64>,
    pub new_calls: Vec<RuntimeMethodCall>,
}

impl RuntimeResponse {
    pub fn trap(m: String) -> Self {
        RuntimeResponse {
            response: CanisterResponse::CanisterTrap(m),
            cycles_accept: (0, 0),
            cycles_mint: (0, 0),
            new_certified_data: None,
            new_global_timer: None,
            new_calls: vec![],
        }
    }
}

impl RuntimeState {
    fn execute(
        &mut self,
        input: WasmExecutionInput,
        controller: Arc<SandboxedExecutionController>,
    ) -> Result<RuntimeResponse, String> {
        let (wasm_binary, exported_functions, wasm_memory, stable_memory, exported_globals) =
            match &input.api_type {
                ApiType::Start { .. } | ApiType::Init { .. } => (
                    self.new_wasm_binary.as_ref().unwrap(),
                    self.new_exported_functions.as_ref().unwrap(),
                    self.persisted_wasm_memory.as_ref().unwrap(),
                    self.persisted_stable_memory.as_ref().unwrap(),
                    self.persisted_exported_globals.as_ref().unwrap(),
                ),
                _ => (
                    &self.wasm_binary,
                    &self.exported_functions,
                    &self.wasm_memory,
                    &self.stable_memory,
                    &self.exported_globals,
                ),
            };
        let should_commit_wasm_binary = match &input.api_type {
            ApiType::Init { .. } => true,
            _ => false,
        };
        let should_commit_canister_memory = match &input.api_type {
            ApiType::PreUpgrade { .. } | ApiType::Start { .. } => false,
            _ => true,
        };
        let should_commit_persisted_wasm_memory = match &input.api_type {
            ApiType::Start { .. } => true,
            _ => false,
        };
        let should_commit_persisted_stable_memory = match &input.api_type {
            ApiType::PreUpgrade { .. } | ApiType::Start { .. } => true,
            _ => false,
        };
        let has_method = match &input.func_ref {
            FuncRef::Method(m) => exported_functions.has_method(m),
            _ => true,
        };
        let res = if has_method {
            Some(controller.execute(
                input,
                wasm_binary,
                wasm_memory,
                stable_memory,
                exported_globals.to_vec(),
            ))
        } else {
            None
        };
        let (exec_output_wasm_result, state_changes) = match res {
            Some(Finished(_, exec_output_wasm, state_changes)) => {
                (exec_output_wasm.wasm_result, state_changes)
            }
            None => (Ok(None), None),
            _ => panic!("DTS should be disabled"),
        };
        // trap
        if let Err(e) = exec_output_wasm_result {
            return Err(e.to_string());
        }
        // we are successful!
        let state_changes = state_changes.unwrap_or(CanisterStateChanges {
            globals: exported_globals.to_vec(),
            wasm_memory: wasm_memory.clone(),
            stable_memory: stable_memory.clone(),
            system_state_changes: SystemStateChanges::default(),
        });
        if should_commit_wasm_binary {
            self.wasm_binary = self.new_wasm_binary.as_ref().unwrap().clone();
            self.exported_functions = self.new_exported_functions.as_ref().unwrap().clone();
        }
        if should_commit_canister_memory {
            self.wasm_memory = state_changes.wasm_memory.clone();
            self.exported_globals = state_changes.globals.clone();
            self.stable_memory = state_changes.stable_memory.clone();
        }
        if should_commit_persisted_wasm_memory {
            self.persisted_wasm_memory = Some(state_changes.wasm_memory);
            self.persisted_exported_globals = Some(state_changes.globals);
        }
        if should_commit_persisted_stable_memory {
            self.persisted_stable_memory = Some(state_changes.stable_memory);
        }
        let output = match exec_output_wasm_result {
            Ok(Some(WasmResult::Reply(bytes))) => {
                CanisterResponse::CanisterReply(serde_bytes::ByteBuf::from(bytes))
            }
            Ok(Some(WasmResult::Reject(msg))) => CanisterResponse::CanisterReject(msg),
            Ok(None) => CanisterResponse::NoResponse(()),
            Err(_) => panic!("unreachable"),
        };
        let cycles_accepted = state_changes.system_state_changes.cycles_accepted;
        let cycles_minted = state_changes.system_state_changes.cycles_minted;
        let new_certified_data = state_changes
            .system_state_changes
            .new_certified_data
            .map(serde_bytes::ByteBuf::from);
        let new_global_timer =
            state_changes
                .system_state_changes
                .new_global_timer
                .map(|t| match t {
                    CanisterTimer::Inactive => 0,
                    CanisterTimer::Active(t) => t.as_nanos_since_unix_epoch(),
                });
        let new_calls = state_changes
            .system_state_changes
            .requests
            .into_iter()
            .map(|r| RuntimeMethodCall {
                call_callee: r.receiver,
                call_method_name: r.method_name,
                call_arg: r.method_payload,
                call_callback: r.callback.unwrap(),
                call_transferred_cycles: r.payment.into_parts(),
            })
            .collect();
        Ok(RuntimeResponse {
            response: output,
            new_calls,
            cycles_accept: cycles_accepted.into_parts(),
            cycles_mint: cycles_minted.into_parts(),
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
    compute_allocation: u64,
    freeze_threshold: u64,
    subnet_id: SubnetId,
    subnet_type: SubnetType,
    subnet_size: u64,
    all_subnets: Vec<CanisterId>,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct ResponseReply {
    #[serde_as(deserialize_as = "Bytes")]
    reply_payload: Vec<u8>,
}

#[derive(Debug, Deserialize, Serialize)]
struct ResponseReject {
    reject_code: u64,
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
    persist_stable_memory: bool,
    env: Env,
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

fn get_env(rte: &RuntimeInvokeEnum) -> Env {
    match rte {
        RuntimeInvokeEnum::RuntimeInstantiate(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeInitialize(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeUpdate(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeQuery(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeCallback(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeCleanup(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimePreUpgrade(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimePostUpgrade(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeInspectMessage(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeHeartbeat(x) => x.env.clone(),
        RuntimeInvokeEnum::RuntimeGlobalTimer(x) => x.env.clone(),
    }
}

fn get_or_create_controller(
    subnet_id: &SubnetId,
    subnet_type: &SubnetType,
    dirty_page_overhead: &NumInstructions,
) -> Arc<SandboxedExecutionController> {
    let mut ctrl_map = CTRL_MAP.lock().unwrap();
    let controller: Arc<SandboxedExecutionController> = if ctrl_map.contains_key(subnet_id) {
        ctrl_map.get(subnet_id).unwrap().clone()
    } else {
        let guard = PREFIX.lock().unwrap();
        let prefix = guard.as_ref().unwrap().clone();
        drop(guard);
        let mut embedder_config = EmbeddersConfig::new();
        embedder_config.subnet_type = subnet_type.clone();
        embedder_config.dirty_page_overhead = dirty_page_overhead.clone();
        embedder_config.min_sandbox_count = 1;
        embedder_config.max_sandbox_count = 10;
        let ctrl = SandboxedExecutionController::new(
            &embedder_config,
            Arc::new(TestPageAllocatorFileDescriptorImpl::new()),
            &prefix,
        )
        .unwrap();
        ctrl_map.insert(subnet_id.clone(), Arc::new(ctrl));
        ctrl_map.get(subnet_id).unwrap().clone()
    };
    controller
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
    let env: Env = get_env(&r.entry_point);

    let mut state_map = STATE.lock().unwrap();

    let subnet_type = env.subnet_type;
    let subnet_id = env.subnet_id;

    let subnet_config = SubnetConfigs::default().own_subnet_config(subnet_type);
    let dirty_page_overhead = subnet_config.scheduler_config.dirty_page_overhead;
    let call_ctx_id: CallContextId = CallContextId::from(0);

    match r.entry_point {
        RuntimeInvokeEnum::RuntimeInstantiate(ref x) => {
            // we only run this, if we do not have any state for canister
            if state_map.get(&canister_id).is_none() {
                // set prefix if it does not exist yet. we need this "lazy initialization"
                // because we only get the prefix in the first invoke call, with RuntimeInstantiate
                let mut prefix = PREFIX.lock().unwrap();
                if prefix.is_none() {
                    *prefix = Some(x.prefix.clone());
                }
                drop(prefix);

                // get controller by subnet, not by canister id
                let controller =
                    get_or_create_controller(&subnet_id, &subnet_type, &dirty_page_overhead);

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
                    cycles_account_manager,
                    wasm_binary: wasm_binary.clone(),
                    exported_functions: exported_functions.clone(),
                    new_wasm_binary: Some(wasm_binary),
                    new_exported_functions: Some(exported_functions),
                    wasm_memory: wasm_memory.clone(),
                    exported_globals: exported_globals.clone(),
                    stable_memory: stable_memory.clone(),
                    persisted_wasm_memory: Some(wasm_memory),
                    persisted_exported_globals: Some(exported_globals),
                    persisted_stable_memory: Some(stable_memory),
                };
                state_map.insert(canister_id, current_state);
            } else {
                // get controller by subnet, not by canister id
                let controller =
                    get_or_create_controller(&subnet_id, &subnet_type, &dirty_page_overhead);

                let mut current_state = state_map.get_mut(&canister_id).unwrap();
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
                current_state.new_wasm_binary = Some(wasm_binary);
                current_state.new_exported_functions = Some(exported_functions);
                current_state.persisted_wasm_memory = Some(wasm_memory);
                current_state.persisted_exported_globals = Some(exported_globals);
                if !x.persist_stable_memory {
                    current_state.persisted_stable_memory = Some(stable_memory);
                }
            }
        }
        _ => {}
    };
    // get controller by subnet, not by canister id
    let controller = get_or_create_controller(&subnet_id, &subnet_type, &dirty_page_overhead);
    let current_state = state_map.get_mut(&canister_id).unwrap();

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
        compute_allocation: env.compute_allocation.try_into().unwrap(),
        subnet_type,
        execution_mode: match r.entry_point {
            RuntimeInvokeEnum::RuntimeQuery(_) => ExecutionMode::NonReplicated,
            _ => ExecutionMode::Replicated,
        },
    };

    let (method, api_type, call_ctx_available_cycles) = match r.entry_point {
        RuntimeInvokeEnum::RuntimeInstantiate(_) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterStart)),
            ApiType::Start {
                time: Time::from_nanos_since_unix_epoch(env.time),
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimeInitialize(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterInit)),
            ApiType::Init {
                time: Time::from_nanos_since_unix_epoch(x.env.time),
                incoming_payload: x.arg,
                caller: x.caller.try_into().unwrap(),
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimeUpdate(x) => {
            (
                FuncRef::Method(WasmMethod::Update(x.method)),
                ApiType::Update {
                    time: Time::from_nanos_since_unix_epoch(x.env.time),
                    incoming_payload: x.arg,
                    incoming_cycles: to_cycles(x.cycles),
                    caller: x.caller.try_into().unwrap(),
                    call_context_id: call_ctx_id,
                    response_data: vec![],
                    response_status: NotRepliedYet,
                    outgoing_request: None,
                    max_reply_size: 2097152.into(), // TODO
                },
                to_cycles(x.cycles),
            )
        }
        RuntimeInvokeEnum::RuntimeQuery(x) => {
            let cert_vec = match x.env.certificate {
                Some(Certificate { bytes }) => Some(bytes),
                _ => None,
            };
            (
                FuncRef::Method(WasmMethod::Query(x.method)),
                if execution_parameters.execution_mode == ExecutionMode::Replicated {
                    ApiType::ReplicatedQuery {
                        time: Time::from_nanos_since_unix_epoch(x.env.time),
                        incoming_payload: x.arg,
                        caller: x.caller.try_into().unwrap(),
                        response_data: vec![],
                        response_status: NotRepliedYet,
                        data_certificate: cert_vec,
                        max_reply_size: 2097152.into(), // TODO
                    }
                } else {
                    ApiType::NonReplicatedQuery {
                        time: Time::from_nanos_since_unix_epoch(x.env.time),
                        caller: x.caller.try_into().unwrap(),
                        own_subnet_id: subnet_id,
                        incoming_payload: x.arg,
                        data_certificate: cert_vec,
                        response_data: vec![],
                        response_status: NotRepliedYet,
                        max_reply_size: 2097152.into(), // TODO
                        query_kind: NonReplicatedQueryKind::Pure,
                    }
                },
                Cycles::new(0),
            )
        }
        RuntimeInvokeEnum::RuntimeCallback(x) => {
            let response_status = if x.needs_to_respond {
                NotRepliedYet
            } else {
                AlreadyReplied
            };
            match x.response {
                Response::Reply(response_reply) => {
                    let reply_closure = ic_types::methods::WasmClosure {
                        func_idx: x.callback.reply_closure.closure_idx as u32,
                        env: x.callback.reply_closure.closure_env as u32,
                    };
                    (
                        FuncRef::UpdateClosure(reply_closure),
                        ApiType::ReplyCallback {
                            time: Time::from_nanos_since_unix_epoch(x.env.time),
                            incoming_payload: response_reply.reply_payload,
                            incoming_cycles: to_cycles(x.refunded_cycles),
                            call_context_id: call_ctx_id,
                            response_data: vec![],
                            response_status,
                            outgoing_request: None,
                            max_reply_size: 2097152.into(), // TODO
                            execution_mode: execution_parameters.execution_mode.clone(),
                        },
                        to_cycles(x.cycles_available),
                    )
                }
                Response::Reject(response_reject) => {
                    let reject_closure = ic_types::methods::WasmClosure {
                        func_idx: x.callback.reject_closure.closure_idx as u32,
                        env: x.callback.reject_closure.closure_env as u32,
                    };
                    let reject_ctx = ic_types::messages::RejectContext {
                        code: response_reject.reject_code.try_into().unwrap(),
                        message: response_reject.reject_msg,
                    };
                    (
                        FuncRef::UpdateClosure(reject_closure),
                        ApiType::RejectCallback {
                            time: Time::from_nanos_since_unix_epoch(x.env.time),
                            reject_context: reject_ctx,
                            incoming_cycles: to_cycles(x.refunded_cycles),
                            call_context_id: call_ctx_id,
                            response_data: vec![],
                            response_status,
                            outgoing_request: None,
                            max_reply_size: 2097152.into(), // TODO
                            execution_mode: execution_parameters.execution_mode.clone(),
                        },
                        to_cycles(x.cycles_available),
                    )
                }
            }
        }
        RuntimeInvokeEnum::RuntimeCleanup(x) => {
            let closure = ic_types::methods::WasmClosure {
                func_idx: x.wasm_closure.closure_idx as u32,
                env: x.wasm_closure.closure_env as u32,
            };
            (
                FuncRef::UpdateClosure(closure),
                ApiType::Cleanup {
                    time: Time::from_nanos_since_unix_epoch(x.env.time),
                },
                Cycles::new(0),
            )
        }
        RuntimeInvokeEnum::RuntimePreUpgrade(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterPreUpgrade)),
            ApiType::PreUpgrade {
                caller: x.caller.try_into().unwrap(),
                time: Time::from_nanos_since_unix_epoch(x.env.time),
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimePostUpgrade(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterPostUpgrade)),
            ApiType::Init {
                time: Time::from_nanos_since_unix_epoch(x.env.time),
                incoming_payload: x.arg,
                caller: x.caller.try_into().unwrap(),
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimeInspectMessage(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterInspectMessage)),
            ApiType::InspectMessage {
                caller: x.caller.try_into().unwrap(),
                method_name: x.method,
                incoming_payload: x.arg,
                time: Time::from_nanos_since_unix_epoch(x.env.time),
                message_accepted: false,
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimeHeartbeat(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterHeartbeat)),
            ApiType::SystemTask {
                system_task: SystemMethod::CanisterHeartbeat,
                time: Time::from_nanos_since_unix_epoch(x.env.time),
                call_context_id: call_ctx_id,
                outgoing_request: None,
            },
            Cycles::new(0),
        ),
        RuntimeInvokeEnum::RuntimeGlobalTimer(x) => (
            FuncRef::Method(WasmMethod::System(SystemMethod::CanisterGlobalTimer)),
            ApiType::SystemTask {
                system_task: SystemMethod::CanisterGlobalTimer,
                time: Time::from_nanos_since_unix_epoch(x.env.time),
                call_context_id: call_ctx_id,
                outgoing_request: None,
            },
            Cycles::new(0),
        ),
    };

    let sandbox_safe_system_state = SandboxSafeSystemState::new_internal(
        canister_id,
        env.status,
        NumSeconds::new(env.freeze_threshold),
        if env.memory_allocation == 0 {
            MemoryAllocation::BestEffort
        } else {
            MemoryAllocation::Reserved((env.memory_allocation).into())
        },
        to_cycles(env.balance),
        BTreeMap::from_iter(
            vec![(call_ctx_id.clone(), call_ctx_available_cycles.clone())].into_iter(),
        ),
        current_state.cycles_account_manager,
        Some(666),
        BTreeMap::new(),
        DEFAULT_QUEUE_CAPACITY,
        BTreeSet::from_iter(env.all_subnets.into_iter()),
        env.subnet_size.try_into().unwrap(),
        dirty_page_overhead,
        CanisterTimer::from_time(Time::from_nanos_since_unix_epoch(env.global_timer)),
        env.canister_version,
        BTreeSet::from_iter(env.controllers.into_iter()),
    );

    let input = WasmExecutionInput {
        api_type,
        sandbox_safe_system_state: sandbox_safe_system_state,
        canister_current_memory_usage: NumBytes::new(0),
        execution_parameters: execution_parameters,
        subnet_available_memory,
        func_ref: method,
        compilation_cache: Arc::new(CompilationCache::default()),
    };
    let res = current_state
        .execute(input, controller)
        .unwrap_or_else(|e| RuntimeResponse::trap(e));
    general_purpose::STANDARD.encode(to_vec(&res).unwrap())
}
