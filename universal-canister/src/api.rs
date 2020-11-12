// use `wee_alloc` as the global allocator.
extern crate wee_alloc;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc<'_> = wee_alloc::WeeAlloc::INIT;

mod ic0 {
    #[link(wasm_import_module = "ic0")]
    extern "C" {
        pub fn canister_self_copy(dst: u32, offset: u32, size: u32) -> ();
        pub fn canister_self_size() -> u32;
        pub fn canister_status() -> u32;
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
        pub fn msg_funds_available( unit_src: u32, unit_size: u32 ) -> u64;
        pub fn msg_funds_refunded( unit_src: u32, unit_size: u32 ) -> u64;
        pub fn msg_funds_accept( unit_src: u32, unit_size: u32, amount : u64 ) -> ();
        pub fn canister_balance( unit_src: u32, unit_size: u32 ) -> u64;
        pub fn trap(offset: u32, size: u32) -> !;
        pub fn call_new(
            callee_src: u32,
            callee_size: u32,
            name_src: u32,
            name_size: u32,
            reply_fun : u32,
            reply_env : u32,
            reject_fun : u32,
            reject_env : u32,
            ) -> ();
        pub fn call_data_append( src: u32, size: u32 ) -> ();
        pub fn call_funds_add( unit_src: u32, unit_size: u32, amount : u64 ) -> ();
        pub fn call_perform() -> u32;
        pub fn stable_size() -> u32;
        pub fn stable_grow(additional_pages: u32) -> u32;
        pub fn stable_read(dst: u32, offset: u32, size: u32) -> ();
        pub fn stable_write(offset: u32, src: u32, size: u32) -> ();
        pub fn certified_data_set(src: u32, size: u32) -> ();
        pub fn data_certificate_present() -> u32;
        pub fn data_certificate_size() -> u32;
        pub fn data_certificate_copy(dst: u32, offset: u32, size: u32) -> ();

        pub fn time() -> u64;
    }
}

// Convenience wrappers around the DFINTY System API

pub fn call_new (
    callee: &[u8],
    method: &[u8],
    reply_fun: fn(u32) -> (), reply_env: u32,
    reject_fun: fn(u32) -> (), reject_env: u32,
) {
    unsafe {
      ic0::call_new(
          callee.as_ptr() as u32,
          callee.len() as u32,
          method.as_ptr() as u32,
          method.len() as u32,
          reply_fun as u32,
          reply_env as u32,
          reject_fun as u32,
          reject_env as u32,
      )
    }
}

pub fn call_data_append(payload: &[u8]) {
    unsafe {
        ic0::call_data_append(payload.as_ptr() as u32, payload.len() as u32);
    }
}

pub fn call_funds_add(unit: &[u8], amount : u64) {
    unsafe {
        ic0::call_funds_add(unit.as_ptr() as u32, unit.len() as u32, amount);
    }
}

pub fn call_perform() -> u32 {
    unsafe {
        ic0::call_perform()
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

pub fn status() -> u32 {
    unsafe { ic0::canister_status() }
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

pub fn reject_code() -> u32 {
    unsafe { ic0::msg_reject_code() }
}

pub fn funds_available( unit: &[u8] ) -> u64 {
    unsafe { ic0::msg_funds_available(unit.as_ptr() as u32, unit.len() as u32) }
}

pub fn funds_refunded( unit: &[u8] ) -> u64 {
    unsafe { ic0::msg_funds_refunded(unit.as_ptr() as u32, unit.len() as u32) }
}

pub fn accept( unit: &[u8], amount : u64) {
    unsafe { ic0::msg_funds_accept(unit.as_ptr() as u32, unit.len() as u32, amount) }
}


pub fn balance( unit: &[u8] ) -> u64 {
    unsafe { ic0::canister_balance(unit.as_ptr() as u32, unit.len() as u32) }
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

pub fn certified_data_set(data : &[u8]) {
    unsafe {
        ic0::certified_data_set(data.as_ptr() as u32, data.len() as u32);
    }
}

pub fn data_certificate_present() -> u32 {
    unsafe { ic0::data_certificate_present() }
}

pub fn data_certificate() -> Vec<u8> {
    let len: u32 = unsafe { ic0::data_certificate_size() };
    let mut bytes = vec![0; len as usize];
    unsafe {
        ic0::data_certificate_copy(bytes.as_mut_ptr() as u32, 0, len);
    }
    bytes
}

pub fn time() -> u64 {
    unsafe {
        ic0::time()
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

