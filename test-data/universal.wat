;; A universal test canister. Universal in the sense that it can be used to
;; test many different things, i.e. used in many scenarios.

;; A core idea is that get_state reply with a full
;; representation of the canister state, to assert state.

;; The canister doesn’t implement interesting logic, it is just state getting
;; and setting. The goal is not to test Wasm execution, but more the interplay
;; of the system api.

;; We use the first 4 bytes of the heap (or of stable memory) only. Makes it
;; easy to use i32.load/store operations.

(module
  (import "ic0" "msg_reply" (func $msg_reply))
  (import "ic0" "msg_reply_data_append" (func $msg_reply_data_append (param i32 i32)))
  (import "ic0" "msg_reject" (func $msg_reject (param i32 i32)))
  (import "ic0" "stable_size" (func $stable_size (result i32)))
  (import "ic0" "stable_grow" (func $stable_grow (param i32) (result i32)))
  (import "ic0" "stable_read" (func $stable_read (param $dst i32) (param $offset i32) (param $size i32)))
  (import "ic0" "stable_write" (func $stable_write (param $offset i32) (param $src i32) (param $size i32)))
  (import "ic0" "msg_arg_data_size" (func $msg_arg_data_size (result i32)))
  (import "ic0" "msg_arg_data_copy" (func $msg_arg_data_copy (param i32) (param i32) (param i32)))
  (import "ic0" "msg_reject_code" (func $msg_reject_code (result i32)))
  (import "ic0" "msg_reject_msg_size" (func $msg_reject_msg_size (result i32)))
  (import "ic0" "msg_reject_msg_copy" (func $msg_reject_msg_copy (param i32) (param i32) (param i32)))
  (import "ic0" "msg_caller_size" (func $msg_caller_size (result i32)))
  (import "ic0" "msg_caller_copy" (func $msg_caller_copy (param i32) (param i32) (param i32)))
  (import "ic0" "canister_self_size" (func $canister_self_size (result i32)))
  (import "ic0" "canister_self_copy" (func $canister_self_copy (param i32) (param i32) (param i32)))
  (import "ic0" "call_simple" (func $call_simple (param i32 i32 i32 i32 i32 i32 i32 i32 i32 i32) (result i32)))

  ;; get_state returns, via simple concatentation:
  ;; * the heap content (first four bytes)
  ;; * the content of the global
  ;; * the stable memory content (first four bytes)
  ;; * the argument
  ;; * the caller
  (func $get_state
    ;; heap
    (call $msg_reply_data_append (i32.const 0) (i32.const 4))
    ;; global
    (i32.store (global.get $scratch) (global.get 0))
    (call $msg_reply_data_append (global.get $scratch) (i32.const 4))
    ;; stable memory
    (if (i32.eqz (call $stable_size)) (then) (else
      (call $stable_read (global.get $scratch) (i32.const 0) (i32.const 4))
      (call $msg_reply_data_append (global.get $scratch) (i32.const 4))
    ))
    ;; argument
    (call $msg_arg_data_copy (global.get $scratch) (i32.const 0) (call $msg_arg_data_size))
    (call $msg_reply_data_append (global.get $scratch) (call $msg_arg_data_size))
    ;; caller
    (call $msg_caller_copy (global.get $scratch) (i32.const 0) (call $msg_caller_size))
    (call $msg_reply_data_append (global.get $scratch) (call $msg_caller_size))

    (call $msg_reply)
   )

  ;; just reject with the argument
  (func $reject
    (call $msg_arg_data_copy (global.get $scratch) (i32.const 0) (call $msg_arg_data_size))
    (call $msg_reject (global.get $scratch) (call $msg_arg_data_size))
   )

  (func $get_canister_self
    (call $canister_self_copy (global.get $scratch) (i32.const 0) (call $canister_self_size))
    (call $msg_reply_data_append (global.get $scratch) (call $canister_self_size))
    (call $msg_reply)
   )

  (func $set_mem
    (call $msg_reply_data_append (i32.const 0) (i32.const 4))
    (call $msg_reply)
    (call $msg_arg_data_copy (i32.const 0) (i32.const 0) (call $msg_arg_data_size))
  )

  (func $grow_stable_mem
    (i32.store (global.get $scratch) (call $stable_grow (i32.const 1)))
    (call $msg_reply_data_append (global.get $scratch) (i32.const 4))
    (call $msg_reply)
  )

  (func $set_stable_mem
    (if (i32.eqz (call $stable_size)) (then) (else
      (call $stable_read (global.get $scratch) (i32.const 0) (i32.const 4))
      (call $msg_reply_data_append (global.get $scratch) (i32.const 4))
    ))
    (call $msg_reply)

    (call $msg_arg_data_copy (global.get $scratch) (i32.const 0) (call $msg_arg_data_size))
    (call $stable_write (i32.const 0) (global.get $scratch) (i32.const 4))
  )

  (func $no_reply)

  ;; inter-canister call: calls get_state on the canister whose id is provided
  ;; as the argument, with an empty argument
  ;; Both callbacks simply reply the argument / reject code + message
  (func $forward_call
    (call $msg_arg_data_copy (global.get $scratch) (i32.const 0) (call $msg_arg_data_size))
    (call $call_simple
      (global.get $scratch) (call $msg_arg_data_size)
      (global.get $get_state) (i32.const 9)
      (i32.const 0) (i32.const 42)
      (i32.const 1) (i32.const 42)
      (i32.const 0) (i32.const 0)
    )
    (if (i32.eqz) (then) (else unreachable))
  )

  (func $reply_callback
    (call $msg_arg_data_copy (global.get $scratch) (i32.const 0) (call $msg_arg_data_size))
    (call $msg_reply_data_append (global.get $scratch) (call $msg_arg_data_size))
    (call $msg_reply)
  )
  (func $reject_callback
    (i32.store (global.get $scratch) (call $msg_reject_code))
    (call $msg_reply_data_append (global.get $scratch) (i32.const 4))
    (call $msg_reject_msg_copy (global.get $scratch) (i32.const 0) (call $msg_reject_msg_size))
    (call $msg_reply_data_append (global.get $scratch) (call $msg_reject_msg_size))
    (call $msg_reply)
  )

  (memory $memory 1)
  (table funcref (elem $reply_callback $reject_callback))
  (global (mut i32) (i32.const 0))
  (global $scratch i32 (i32.const 14))
  (global $get_state i32 (i32.const 4))
  (data 0 (i32.const 0) "memo")
  (data 0 (i32.const 4) "get_state") ;; get_state
  (export "memory" (memory $memory))

  (export "canister_update get_state" (func $get_state))
  (export "canister_query get_state_query" (func $get_state))
  (export "canister_update set_mem" (func $set_mem))
  (export "canister_query set_mem_query" (func $set_mem))
  (export "canister_update grow_stable_mem" (func $grow_stable_mem))
  (export "canister_query grow_stable_mem_query" (func $grow_stable_mem))
  (export "canister_update set_stable_mem" (func $set_stable_mem))
  (export "canister_query set_stable_mem_query" (func $set_stable_mem))
  (export "canister_update no_reply" (func $no_reply))
  (export "canister_query no_reply_query" (func $no_reply))
  (export "canister_update forward_call" (func $forward_call))
  (export "canister_update_query forward_call_query" (func $forward_call))
  (export "canister_query get_canister_self" (func $get_canister_self))
  (export "canister_update reject" (func $reject))
  (export "canister_query reject_query" (func $reject))
)
