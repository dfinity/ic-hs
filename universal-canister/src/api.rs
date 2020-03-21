// use `wee_alloc` as the global allocator.
extern crate wee_alloc;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc<'_> = wee_alloc::WeeAlloc::INIT;

mod ic0 {
    #[link(wasm_import_module = "ic0")]
    extern "C" {
        pub fn canister_self_copy(dst: u32, offset: u32, size: u32) -> ();
        pub fn canister_self_size() -> u32;
        pub fn debug_print(offset: u32, size: u32) -> ();
        pub fn msg_arg_data_copy(dst: u32, offset: u32, size: u32) -> ();
        pub fn msg_arg_data_size() -> u32;
        pub fn msg_caller_copy(dst: u32, offset: u32, size: u32) -> ();
        pub fn msg_caller_size() -> u32;
        pub fn msg_reject(src: u32, size: u32) -> ();
        pub fn msg_reject_code() -> u32;
        pub fn msg_reject_msg_copy(dst: u32, offset: u32, size: u32) -> ();
        pub fn msg_reject_msg_size() -> u32;
        pub fn msg_reply() -> ();
        pub fn msg_reply_data_append(offset: u32, size: u32) -> ();
        pub fn trap(offset: u32, size: u32) -> !;
        pub fn call_simple(
            callee_src: u32,
            callee_size: u32,
            name_src: u32,
            name_size: u32,
            reply_fun: usize,
            reply_env: u32,
            reject_fun: usize,
            reject_env: u32,
            data_src: u32,
            data_size: u32,
        ) -> i32;
        pub fn stable_size() -> u32;
        pub fn stable_grow(additional_pages: u32) -> u32;
        pub fn stable_read(dst: u32, offset: u32, size: u32) -> ();
        pub fn stable_write(offset: u32, src: u32, size: u32) -> ();
    }
}

// Convenience wrappers around the DFINTY System API

// call static
pub fn call_static(
    callee: &[u8],
    method: &[u8],
    on_reply: fn(u32) -> (),
    reply_env: u32,
    on_reject: fn(u32) -> (),
    reject_env: u32,
    data: &[u8],
) -> i32 {
    unsafe {
      ic0::call_simple(
          callee.as_ptr() as u32,
          callee.len() as u32,
          method.as_ptr() as u32,
          method.len() as u32,
          on_reply as usize,
          reply_env as u32,
          on_reject as usize,
          reject_env as u32,
          data.as_ptr() as u32,
          data.len() as u32,
      )
    }
}

/// Returns the argument extracted from the message payload.
pub fn arg_data() -> Vec<u8> {
    let len: u32 = unsafe { ic0::msg_arg_data_size() };
    let mut bytes = vec![0; len as usize];
    unsafe {
        ic0::msg_arg_data_copy(bytes.as_mut_ptr() as u32, 0, len);
    }
    bytes
}

/// Returns the caller of the current call.
pub fn caller() -> Vec<u8> {
    let len: u32 = unsafe { ic0::msg_caller_size() };
    let mut bytes = vec![0; len as usize];
    unsafe {
        ic0::msg_caller_copy(bytes.as_mut_ptr() as u32, 0, len);
    }
    bytes
}

/// Returns the canister id as a blob.
pub fn id() -> Vec<u8> {
    let len: u32 = unsafe { ic0::canister_self_size() };
    let mut bytes = vec![0; len as usize];
    unsafe {
        ic0::canister_self_copy(bytes.as_mut_ptr() as u32, 0, len);
    }
    bytes
}

/// Returns the rejection message.
pub fn reject_message() -> Vec<u8> {
    let len: u32 = unsafe { ic0::msg_reject_msg_size() };
    let mut bytes = vec![0; len as usize];
    unsafe {
        ic0::msg_reject_msg_copy(bytes.as_mut_ptr() as u32, 0, len);
    }
    bytes
}

pub fn reply_data_append(payload: &[u8]) {
    unsafe {
        ic0::msg_reply_data_append(payload.as_ptr() as u32, payload.len() as u32);
    }
}

pub fn reply() {
    unsafe {
        ic0::msg_reply();
    }
}

/// Rejects the current call with the given message.
pub fn reject(err_message: &[u8]) {
    unsafe {
        ic0::msg_reject(err_message.as_ptr() as u32, err_message.len() as u32);
    }
}

/// Returns the rejection code.
pub fn reject_code() -> u32 {
    unsafe { ic0::msg_reject_code() }
}

pub fn stable_size() -> u32 {
    unsafe { ic0::stable_size() }
}

pub fn stable_grow(additional_pages: u32) -> u32 {
    unsafe { ic0::stable_grow(additional_pages) }
}

pub fn stable_read(offset: u32, size: u32) -> Vec<u8> {
    let mut bytes = vec![0; size as usize];
    unsafe {
        ic0::stable_read(bytes.as_mut_ptr() as u32, offset, size);
    }
    bytes
}

pub fn stable_write(offset: u32, data : &[u8]) {
    unsafe {
        ic0::stable_write(offset, data.as_ptr() as u32, data.len() as u32);
    }
}

/// Prints the given message.
pub fn print(data : &[u8]) {
    unsafe {
        ic0::debug_print(data.as_ptr() as u32, data.len() as u32);
    }
}

pub fn bad_print() {
    unsafe {
        ic0::debug_print(u32::max_value()-2, 1);
        ic0::debug_print(u32::max_value()-2, 3);
    }
}

/// Traps with the given message.
pub fn trap_with_blob(data: &[u8]) -> ! {
    unsafe {
        ic0::trap(data.as_ptr() as u32, data.len() as u32);
    }
}
pub fn trap_with(message: &str) -> ! {
    unsafe {
        ic0::trap(message.as_ptr() as u32, message.len() as u32);
    }
}

use std::panic;
pub fn set_panic_hook() {
    panic::set_hook(Box::new(|i| {
        let s = i.to_string();
        trap_with(&s);
    }));
}

