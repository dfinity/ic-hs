with open("a.wat", "w") as f:
  print('''(module
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
    (func $inc
        ;; Increment a counter.
        (i32.store
            (i32.const 0)
            (i32.add (i32.load (i32.const 0)) (i32.const 4))))
    (memory $memory 1)
    (export "canister_update read" (func $inc))
    (export "canister_query get" (func $inc))
    (export "canister_update inc" (func $inc))
    (export "canister_init" (func $inc))
)''', file=f)

with open("b.wat", "w") as f:
  print('''(module
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
    (func $inc
        ;; Increment a counter.
        (i32.store
            (i32.const 0)
            (i32.add (i32.load (i32.const 0)) (i32.const 4))))
    (memory $memory 1)
    (export "canister_update read" (func $inc))
    (export "canister_query get" (func $inc))
    (export "canister_update inc" (func $inc))
    (export "canister_callback" (func $inc))
)''', file=f)
