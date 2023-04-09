pub mod active_execution_state_registry;
pub mod controller_service_impl;
pub mod launch_as_process;
mod process_exe_and_args;
pub mod process_os_metrics;
mod sandbox_process_eviction;
pub mod sandboxed_execution_controller;

use lazy_static::lazy_static; // 1.4.0
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
use ic_system_api::ApiType::ReplicatedQuery;
use ic_system_api::ApiType::Update as ApiUpdate;
use ic_system_api::ApiType::{Init, InspectMessage, Start};
use ic_system_api::ResponseStatus::NotRepliedYet;
use ic_system_api::{ExecutionParameters, InstructionLimits};
use ic_types::ingress::WasmResult;
use ic_types::messages::CallContextId;
use ic_types::methods::{FuncRef, SystemMethod, WasmMethod};
use ic_types::{CanisterId, PrincipalId};
use ic_types::{CanisterTimer::Inactive, MemoryAllocation::BestEffort};
use ic_types::{ComputeAllocation, Cycles, NumBytes, NumInstructions, Time};
use ic_wasm_types::CanisterModule;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use serde::Deserialize;
use serde_cbor::{from_slice, Value};
use serde_with::{serde_as, Bytes};

const DEFAULT_NUM_INSTRUCTIONS: NumInstructions = NumInstructions::new(5_000_000_000);

const COUNTER_WAT: &str = r#"
    (module
        (import "ic0" "call_new"
            (func $ic0_call_new
                (param i32 i32)
                (param $method_name_src i32)    (param $method_name_len i32)
                (param $reply_fun i32)          (param $reply_env i32)
                (param $reject_fun i32)         (param $reject_env i32)
            ))
        (import "ic0" "call_data_append" (func $ic0_call_data_append (param $src i32) (param $size i32)))
        (import "ic0" "call_cycles_add" (func $ic0_call_cycles_add (param $amount i64)))
        (import "ic0" "call_perform" (func $ic0_call_perform (result i32)))
        (import "ic0" "msg_reply" (func $msg_reply))
        (import "ic0" "msg_reply_data_append"
            (func $msg_reply_data_append (param i32 i32)))
        (func $inc
            (call $ic0_call_new
                (i32.const 100) (i32.const 10)  ;; callee canister id = 777
                (i32.const 0) (i32.const 18)    ;; refers to "some_remote_method" on the heap
                (i32.const 11) (i32.const 22)   ;; fictive on_reply closure
                (i32.const 33) (i32.const 44)   ;; fictive on_reject closure
            )
            (call $ic0_call_data_append
                (i32.const 19) (i32.const 3)    ;; refers to "XYZ" on the heap
            )
            (call $ic0_call_cycles_add
                (i64.const 100)
            )
            (drop (call $ic0_call_perform))
            ;; Increment a counter.
            (i32.store
                (i32.const 0)
                (i32.add (i32.load (i32.const 0)) (i32.const 4))))
        (func $read
            (call $msg_reply_data_append
                (i32.const 0) ;; the counter from heap[0]
                (i32.const 4)) ;; length
            (call $msg_reply))
        (func $canister_init
            ;; Increment the counter by 41 in canister_init.
            (i32.store
                (i32.const 0)
                (i32.add (i32.load (i32.const 0)) (i32.const 41))))
        (start $inc)    ;; Increments counter by 1 in canister_start
        (memory $memory 1)
        (export "canister_query read" (func $read))
        (export "canister_update inc" (func $inc))
        (export "canister_init" (func $canister_init))
    )"#;

#[serde_as]
#[derive(Debug, Deserialize)]
struct RuntimeInstantiate {
    #[serde_as(deserialize_as = "Bytes")]
    cid: Vec<u8>,
    #[serde_as(deserialize_as = "Bytes")]
    wasm: Vec<u8>,
}

struct RuntimeState {
    pub canister_id: CanisterId,
    pub controller: Arc<SandboxedExecutionController>,
    pub cycles_account_manager: CyclesAccountManager,
    //pub canister_module: CanisterModule,
    pub wasm_binary: Arc<WasmBinary>,
    pub wasm_memory: Memory,
    pub stable_memory: Memory,
    pub exported_functions: ExportedFunctions,
    pub exported_globals: Vec<Global>,
}

lazy_static! {
    static ref STATE: Mutex<BTreeMap<CanisterId, RuntimeState>> = Mutex::new(BTreeMap::new());
}

impl RuntimeState {
    fn execute(&mut self, input: WasmExecutionInput) -> Result<String, String> {
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
            Finished(_, exec_output_wasm, Some(state_changes)) => (exec_output_wasm, state_changes),
            Finished(_, exec_output_wasm, None) => {
                if let Err(e) = exec_output_wasm.wasm_result {
                    return Err(format!("trap: {}", e));
                }
                return Err("execution has failed".to_string());
            }
            _ => panic!("DTS should be disabled"),
        };
        self.wasm_memory = state_changes.wasm_memory;
        self.stable_memory = state_changes.stable_memory;
        self.exported_globals = state_changes.globals;
        let output = match exec_output_wasm.wasm_result.clone() {
            Ok(Some(WasmResult::Reply(bytes))) => format!("reply: {}", base64::encode(bytes)),
            Ok(Some(WasmResult::Reject(msg))) => format!("reject: {}", msg),
            Ok(None) => "NoResponse".to_string(),
            Err(e) => format!("trap: {}", e),
        };
        Ok(output)
    }
}

#[hs_bindgen(instantiate :: CString -> IO CString)]
pub fn instantiate(arg: &str) -> String {
    let a = base64::decode(arg).unwrap();
    let x: RuntimeInstantiate = from_slice(a.as_slice()).unwrap();
    let canister_id: CanisterId = x.cid.try_into().unwrap();
    let controller = Arc::new(
        SandboxedExecutionController::new(
            &EmbeddersConfig::default(),
            Arc::new(TestPageAllocatorFileDescriptorImpl::new()),
        )
        .unwrap(),
    );
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
    let cycles_account_manager = CyclesAccountManager::new(
        NumInstructions::new(1_000_000_000),
        SubnetType::Application,
        PrincipalId::default().into(),
        CyclesAccountManagerConfig::application_subnet(),
    );
    let canister_module = CanisterModule::new(x.wasm);
    let (wasm_binary, wasm_memory, stable_memory, exported_globals, exported_functions, _, _) =
        controller
            .create_execution_state(
                canister_module,
                canister_id,
                Arc::new(CompilationCache::default()),
            )
            .unwrap();
    let mut current_state: RuntimeState = RuntimeState {
        canister_id,
        controller,
        cycles_account_manager,
        //canister_module,
        wasm_binary,
        wasm_memory,
        stable_memory,
        exported_functions,
        exported_globals,
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
    let res = current_state.execute(WasmExecutionInput {
        api_type: Start {
            time: Time::from_nanos_since_unix_epoch(0),
        },
        sandbox_safe_system_state: sandbox_safe_system_state.clone(),
        canister_current_memory_usage: 0.into(),
        execution_parameters: execution_parameters.clone(),
        subnet_available_memory: subnet_available_memory,
        func_ref: FuncRef::Method(WasmMethod::System(SystemMethod::CanisterStart)),
        compilation_cache: Arc::new(CompilationCache::default()),
    });
    if let Err(e) = res {
        return format!("trap: {}", e);
    }

    STATE.lock().unwrap().insert(canister_id, current_state);

    format!("ok")
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct Env {
    tag: u64,
    #[serde_as(deserialize_as = "Bytes")]
    self_id: Vec<u8>,
    time: (u64, u64),
    balance: u64,
    status: Vec<u8>,
    certificate: Vec<u8>,
    canister_version: u64,
    global_timer: u64,
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct RuntimeInitialize {
    tag: u64,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct RuntimeUpdate {
    tag: u64,
    method: String,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    needs_to_respond: (u64, bool),
    cycles: u64,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct RuntimeInspect {
    tag: u64,
    method: String,
    #[serde_as(deserialize_as = "Bytes")]
    caller: Vec<u8>,
    env: Env,
    #[serde_as(deserialize_as = "Bytes")]
    arg: Vec<u8>,
}

#[derive(Debug, Deserialize)]
struct RuntimeHeartbeat {
    tag: u64,
    env: Env,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RuntimeInvokeEnum {
    RuntimeInitialize(RuntimeInitialize),
    RuntimeInspect(RuntimeInspect),
    RuntimeUpdate(RuntimeUpdate),
    RuntimeHeartbeat(RuntimeHeartbeat),
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct RuntimeInvoke {
    #[serde_as(deserialize_as = "Bytes")]
    cid: Vec<u8>,
    payload: RuntimeInvokeEnum,
}

#[hs_bindgen(invoke :: CString -> IO CString)]
pub fn invoke(arg: &str) -> String {
    let a = base64::decode(arg).unwrap();
    let v: Value = from_slice(a.as_slice()).unwrap();
    //return format!("{:?}", v);
    let xx: RuntimeInvoke = from_slice(a.as_slice()).unwrap();
    let canister_id: CanisterId = xx.cid.try_into().unwrap();

    let mut state_map = STATE.lock().unwrap();
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

    let (method, api_type) = match xx.payload {
        RuntimeInvokeEnum::RuntimeInitialize(x) => (
            WasmMethod::System(SystemMethod::CanisterInit),
            Init {
                time: Time::from_nanos_since_unix_epoch(0),
                incoming_payload: x.arg,
                caller: x.caller.try_into().unwrap(),
            },
        ),
        RuntimeInvokeEnum::RuntimeInspect(x) => {
            if !current_state
                .exported_functions
                .has_method(&WasmMethod::System(SystemMethod::CanisterInspectMessage))
            {
                return format!("inspect: ok");
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
            return format!("heartbeat");
        }
        _ => panic!("unsupported"),
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
        return format!("trap: {}", e);
    }

    format!("ok: {}", res.unwrap())
}
