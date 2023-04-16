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
use ic_config::flag_status::FlagStatus;
use ic_config::subnet_config::CyclesAccountManagerConfig;
use ic_config::subnet_config::SchedulerConfig;
use ic_cycles_account_manager::CyclesAccountManager;
use ic_embedders::wasm_executor::WasmExecutionResult::Finished;
use ic_embedders::CompilationCache;
use ic_embedders::WasmExecutionInput;
use ic_interfaces::execution_environment::{ExecutionMode, SubnetAvailableMemory};
use ic_registry_subnet_type::SubnetType;
use ic_replicated_state::canister_state::execution_state::WasmBinary;
use ic_replicated_state::page_map::TestPageAllocatorFileDescriptorImpl;
use ic_replicated_state::{ExportedFunctions, Global, Memory};
use ic_system_api::sandbox_safe_system_state::CanisterStatusView::Running;
use ic_system_api::sandbox_safe_system_state::SandboxSafeSystemState;
use ic_system_api::ApiType::Update as ApiUpdate;
use ic_system_api::ApiType::{Init, InspectMessage, Start};
use ic_system_api::ResponseStatus::NotRepliedYet;
use ic_system_api::{ExecutionParameters, InstructionLimits};
use ic_types::ingress::WasmResult;
use ic_types::messages::CallContextId;
use ic_types::methods::{FuncRef, SystemMethod, WasmMethod};
use ic_types::MemoryAllocation::BestEffort;
use ic_types::{CanisterId, PrincipalId};
use ic_types::{CanisterTimer::*, ComputeAllocation, Cycles, NumBytes, NumInstructions, Time};
use ic_wasm_types::CanisterModule;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use base64::{engine::general_purpose, Engine as _};

use serde::{Deserialize, Serialize};
use serde_cbor::{from_slice, to_vec};
use serde_with::{serde_as, Bytes};

const DEFAULT_NUM_INSTRUCTIONS: NumInstructions = NumInstructions::new(5_000_000_000);

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

#[derive(Serialize)]
struct RuntimeResponse {
    pub response: CanisterResponse,
    pub cycles_accept: u64,
    pub cycles_mint: u64,
    pub new_certified_data: Option<serde_bytes::ByteBuf>,
    pub new_global_timer: Option<u64>,
}

impl RuntimeResponse {
    pub fn trap(m: String) -> Self {
        RuntimeResponse {
            response: CanisterResponse::CanisterTrap(m),
            cycles_accept: 0,
            cycles_mint: 0,
            new_certified_data: None,
            new_global_timer: None,
        }
    }

    pub fn noop() -> Self {
        RuntimeResponse {
            response: CanisterResponse::NoResponse(()),
            cycles_accept: 0,
            cycles_mint: 0,
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
                        Inactive => 0,
                        Active(t) => t.as_nanos_since_unix_epoch(),
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
            cycles_accept: 0,
            cycles_mint: 0,
            new_certified_data,
            new_global_timer,
        })
    }
}

#[derive(Debug, Deserialize, Serialize)]
enum CanisterStatus {
    Running,
    Stopping,
    Stopped,
}

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
struct Env {
    #[serde_as(deserialize_as = "Bytes")]
    canister_id: Vec<u8>,
    time: u64,
    balance: u64,
    status: CanisterStatus,
    certificate: Option<Vec<u8>>,
    canister_version: u64,
    global_timer: u64,
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
    cycles: u64,
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
enum RuntimeInvokeEnum {
    RuntimeInstantiate(RuntimeInstantiate),
    RuntimeInitialize(RuntimeInitialize),
    RuntimeUpdate(RuntimeUpdate),
    RuntimeInspectMessage(RuntimeInspectMessage),
    RuntimeHeartbeat(RuntimeHeartbeat),
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
    let canister_id: CanisterId = r.canister_id.try_into().unwrap();

    let mut state_map = STATE.lock().unwrap();

    match r.entry_point {
        RuntimeInvokeEnum::RuntimeInstantiate(ref x) => {
            let controller = Arc::new(
                SandboxedExecutionController::new(
                    &EmbeddersConfig::default(),
                    Arc::new(TestPageAllocatorFileDescriptorImpl::new()),
                    &x.prefix,
                )
                .unwrap(),
            );
            let cycles_account_manager = CyclesAccountManager::new(
                NumInstructions::new(1_000_000_000),
                SubnetType::Application,
                PrincipalId::default().into(),
                CyclesAccountManagerConfig::application_subnet(),
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

    let execution_parameters = ExecutionParameters {
        instruction_limits: InstructionLimits::new(
            FlagStatus::Disabled,
            DEFAULT_NUM_INSTRUCTIONS,
            DEFAULT_NUM_INSTRUCTIONS,
        ),
        canister_memory_limit: NumBytes::from(4 << 30),
        compute_allocation: ComputeAllocation::default(),
        subnet_type: SubnetType::Application,
        execution_mode: ExecutionMode::Replicated,
    };

    const DEFAULT_FREEZE_THRESHOLD: NumSeconds = NumSeconds::new(1 << 5);
    let dirty_page_overhead = SchedulerConfig::application_subnet().dirty_page_overhead;
    let sandbox_safe_system_state = SandboxSafeSystemState::new_internal(
        canister_id,
        PrincipalId::default(),
        Running,
        DEFAULT_FREEZE_THRESHOLD,
        BestEffort,
        Cycles::new(1 << 40),
        BTreeMap::new(),
        current_state.cycles_account_manager,
        Some(666),
        BTreeMap::new(),
        1000,
        BTreeSet::new(),
        2,
        dirty_page_overhead,
        Inactive,
        0,
        BTreeSet::new(),
    );

    let subnet_available_memory: SubnetAvailableMemory =
        SubnetAvailableMemory::new(100000000, 100000000, 100000000);

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
                Start {
                    time: Time::from_nanos_since_unix_epoch(0),
                },
            )
        }
        RuntimeInvokeEnum::RuntimeInitialize(x) => (
            WasmMethod::System(SystemMethod::CanisterInit),
            Init {
                time: Time::from_nanos_since_unix_epoch(0),
                incoming_payload: x.arg,
                caller: x.caller.try_into().unwrap(),
            },
        ),
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
        RuntimeInvokeEnum::RuntimeUpdate(x) => (
            WasmMethod::Update(x.method),
            ApiUpdate {
                time: Time::from_nanos_since_unix_epoch(0),
                incoming_payload: vec![],
                incoming_cycles: Cycles::new(0),
                caller: PrincipalId::default(),
                call_context_id: CallContextId::from(42),
                response_data: vec![],
                response_status: NotRepliedYet,
                outgoing_request: None,
                max_reply_size: 2000000.into(),
            },
        ),
        RuntimeInvokeEnum::RuntimeHeartbeat(_) => {
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
