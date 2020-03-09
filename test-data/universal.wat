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
  (import "ic0" "stable_size" (func $stable_size (result i32)))
  (import "ic0" "stable_grow" (func $stable_grow (param i32) (result i32)))
  (import "ic0" "stable_read" (func $stable_read (param $dst i32) (param $offset i32) (param $size i32)))
  (import "ic0" "stable_write" (func $stable_write (param $offset i32) (param $src i32) (param $size i32)))
  (import "ic0" "msg_arg_data_size" (func $msg_arg_data_size (result i32)))
  (import "ic0" "msg_arg_data_copy" (func $msg_arg_data_copy (param i32) (param i32) (param i32)))
  (import "ic0" "msg_caller_size" (func $msg_caller_size (result i32)))
  (import "ic0" "msg_caller_copy" (func $msg_caller_copy (param i32) (param i32) (param i32)))

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
    (i32.store (i32.const 4) (global.get 0))
    (call $msg_reply_data_append (i32.const 4) (i32.const 4))
    ;; stable memory
    (if (i32.eqz (call $stable_size)) (then) (else
      (call $stable_read (i32.const 4) (i32.const 0) (i32.const 4))
      (call $msg_reply_data_append (i32.const 4) (i32.const 4))
    ))
    ;; argument
    (call $msg_arg_data_copy (i32.const 4) (i32.const 0) (call $msg_arg_data_size))
    (call $msg_reply_data_append (i32.const 4) (call $msg_arg_data_size))
    ;; caller
    (call $msg_caller_copy (i32.const 4) (i32.const 0) (call $msg_caller_size))
    (call $msg_reply_data_append (i32.const 4) (call $msg_caller_size))

    (call $msg_reply)
   )

  (func $set_mem
    (call $msg_reply_data_append (i32.const 0) (i32.const 4))
    (call $msg_reply)
    (call $msg_arg_data_copy (i32.const 0) (i32.const 0) (call $msg_arg_data_size))
  )

  (func $grow_stable_mem
    (i32.store (i32.const 4) (call $stable_grow (i32.const 1)))
    (call $msg_reply_data_append (i32.const 4) (i32.const 4))
    (call $msg_reply)
  )

  (func $set_stable_mem
    (if (i32.eqz (call $stable_size)) (then) (else
      (call $stable_read (i32.const 4) (i32.const 0) (i32.const 4))
      (call $msg_reply_data_append (i32.const 4) (i32.const 4))
    ))
    (call $msg_reply)

    (call $msg_arg_data_copy (i32.const 4) (i32.const 0) (call $msg_arg_data_size))
    (call $stable_write (i32.const 0) (i32.const 4) (i32.const 4))
  )

  (memory $memory 1)
  (global (mut i32) (i32.const 0))
  (data 0 (i32.const 0) "memo")
  (export "memory" (memory $memory))

  (export "canister_update get_state" (func $get_state))
  (export "canister_query get_state_query" (func $get_state))
  (export "canister_update set_mem" (func $set_mem))
  (export "canister_query set_mem_query" (func $set_mem))
  (export "canister_update grow_stable_mem" (func $grow_stable_mem))
  (export "canister_query grow_stable_mem_query" (func $grow_stable_mem))
  (export "canister_update set_stable_mem" (func $set_stable_mem))
  (export "canister_query set_stable_mem_query" (func $set_stable_mem))

)
