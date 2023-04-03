
pub mod active_execution_state_registry;
pub mod controller_service_impl;
pub mod launch_as_process;
mod process_exe_and_args;
pub mod process_os_metrics;
mod sandbox_process_eviction;
pub mod sandboxed_execution_controller;

use ic_base_types::NumSeconds;
use crate::sandboxed_execution_controller::SandboxedExecutionController;
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
use ic_replicated_state::page_map::TestPageAllocatorFileDescriptorImpl;
use ic_system_api::sandbox_safe_system_state::CanisterStatusView::Running;
use ic_system_api::sandbox_safe_system_state::SandboxSafeSystemState;
use ic_system_api::ApiType::Init;
use ic_system_api::ApiType::ReplicatedQuery;
use ic_system_api::ApiType::Update as ApiUpdate;
use ic_system_api::ResponseStatus::NotRepliedYet;
use ic_system_api::{ExecutionParameters, InstructionLimits};
use ic_types::messages::CallContextId;
use ic_types::methods::{FuncRef, SystemMethod, WasmMethod};
use ic_types::{CanisterId, PrincipalId};
use ic_types::{CanisterTimer::Inactive, MemoryAllocation::BestEffort};
use ic_types::{ComputeAllocation, Cycles, NumBytes, NumInstructions, Time};
use ic_wasm_types::CanisterModule;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use hs_bindgen::*;

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

#[hs_bindgen(instantiate :: CString -> IO CString)]
pub fn instantiate(arg: &str) -> String {
    format!("hoi: {}", arg)
}

#[hs_bindgen(invoke :: CString -> IO CString)]
pub fn invoke(arg: &str) -> String {
    format!("hello: {}", arg)
}

pub fn main() {
    let controller = Arc::new(
        SandboxedExecutionController::new(
            &EmbeddersConfig::default(),
            Arc::new(TestPageAllocatorFileDescriptorImpl::new()),
        )
        .unwrap(),
    );

    let wat = COUNTER_WAT;
    let canister_module = CanisterModule::new(wat::parse_str(wat).unwrap());
    let canister_id: CanisterId = PrincipalId::default().try_into().unwrap();
    let (wasm_binary, wasm_memory, stable_memory, exported_globals, _, _) = controller
        .create_execution_state(
            canister_module,
            canister_id,
            Arc::new(CompilationCache::default()),
        )
        .unwrap();

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
    let cycles_account_manager = CyclesAccountManager::new(
        NumInstructions::new(1_000_000_000),
        SubnetType::Application,
        PrincipalId::default().into(),
        CyclesAccountManagerConfig::application_subnet(),
    );
    let dirty_page_overhead = SchedulerConfig::application_subnet().dirty_page_overhead;

    let sandbox_safe_system_state = SandboxSafeSystemState::new_internal(
        canister_id,
        PrincipalId::default(),
        Running,
        DEFAULT_FREEZE_THRESHOLD,
        BestEffort,
        Cycles::new(1<<40),
        BTreeMap::new(),
        cycles_account_manager,
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

    let res = controller
        .clone()
        .execute(
            WasmExecutionInput {
                api_type: Init {
                    time: Time::from_nanos_since_unix_epoch(0),
                    incoming_payload: vec![],
                    caller: canister_id.into(),
                },
                sandbox_safe_system_state: sandbox_safe_system_state.clone(),
                canister_current_memory_usage: 100.into(),
                execution_parameters: execution_parameters.clone(),
                subnet_available_memory: SubnetAvailableMemory::new(100000000, 100000000, 100000000),
                func_ref: FuncRef::Method(WasmMethod::System(SystemMethod::CanisterInit)),
                compilation_cache: Arc::new(CompilationCache::default()),
            },
            &wasm_binary,
            &wasm_memory,
            &stable_memory,
            exported_globals.clone(),
        )
        .1;
    //println!("{:?}", res);
    let state_changes = match res {
        Finished(_, _, Some(state_changes)) => state_changes,
        _ => panic!(""),
    };

    let res = controller
        .clone()
        .execute(
            WasmExecutionInput {
                api_type: ApiUpdate {
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
                sandbox_safe_system_state: sandbox_safe_system_state.clone(),
                canister_current_memory_usage: 100.into(),
                execution_parameters: execution_parameters.clone(),
                subnet_available_memory: SubnetAvailableMemory::new(100000000, 100000000, 100000000),
                func_ref: FuncRef::Method(WasmMethod::Update("inc".to_string())),
                compilation_cache: Arc::new(CompilationCache::default()),
            },
            &wasm_binary,
            &state_changes.wasm_memory,
            &state_changes.stable_memory,
            state_changes.globals,
        )
        .1;
    //println!("{:?}", res);
    let state_changes = match res {
        Finished(_, _, Some(state_changes)) => state_changes,
        _ => panic!(""),
    };

    let res = controller
        .execute(
            WasmExecutionInput {
                api_type: ReplicatedQuery {
                    time: Time::from_nanos_since_unix_epoch(0),
                    incoming_payload: vec![],
                    caller: PrincipalId::default(),
                    response_data: vec![],
                    response_status: NotRepliedYet,
                    data_certificate: None,
                    max_reply_size: 2000000.into(),
                },
                sandbox_safe_system_state,
                canister_current_memory_usage: 100.into(),
                execution_parameters,
                subnet_available_memory: SubnetAvailableMemory::new(100000000, 100000000, 100000000),
                func_ref: FuncRef::Method(WasmMethod::Query("read".to_string())),
                compilation_cache: Arc::new(CompilationCache::default()),
            },
            &wasm_binary,
            &state_changes.wasm_memory,
            &state_changes.stable_memory,
            state_changes.globals,
        )
        .1;
    //println!("{:?}", res);
}
