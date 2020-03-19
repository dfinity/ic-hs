use std::convert::TryInto;

mod api;

// A simple dynamically typed stack

enum Val { I32(u32), Blob(Vec<u8>) }

struct Stack(Vec<Val>);

impl Stack {
    fn new() -> Self {
      Stack(Vec::new())
    }

    fn drop(self : &mut Self) {
        self.0.pop();
    }

    fn push_int(self : &mut Self, x : u32) {
        self.0.push(Val::I32(x));
    }

    fn push_blob(self : &mut Self, x : Vec<u8>) {
        self.0.push(Val::Blob(x));
    }

    fn pop_int(self : &mut Self) -> u32 {
        if let Some(Val::I32(i)) = self.0.pop() {
          i
        } else {
          api::trap_with("did not find I32 on stack")
        }
    }

    fn pop_blob(self : &mut Self) -> Vec<u8> {
        if let Some(Val::Blob(blob)) = self.0.pop() {
          blob
        } else {
          api::trap_with("did not find blob on stack")
        }
    }
}

// Reading data from the operations stream

type Ops<'a> = &'a[u8];

fn read_bytes<'a>(ops : &mut Ops<'a>, len : usize) -> &'a[u8] {
  if len < ops.len() {
    let (bytes, rest) = ops.split_at(len as usize);
    *ops = rest;
    bytes
  } else {
    panic!("cannot read {} bytes of a {} byte string", len, ops.len());
  }
}

fn read_int(ops : &mut Ops) -> u32 {
  let bytes = read_bytes(ops, std::mem::size_of::<u32>());
  u32::from_le_bytes(bytes.try_into().unwrap())
}

fn eval(ops : Ops) {
  let mut ops : Ops = ops;
  let mut stack : Stack = Stack::new();

  while let Some((op,rest)) = ops.split_first() {
      ops = rest;
      match op {
        // noop
        0 => (),
        // drop
        1 => stack.drop(),
        // push int
        2 => {
          let a = read_int(&mut ops);
          stack.push_int(a);
        }
        // push bytes
        3 => {
          let len = read_int(&mut ops);
          let blob = read_bytes(&mut ops, len as usize).to_vec();
          stack.push_blob(blob);
        }
        // reply_data_append
        4 => api::reply_data_append(&stack.pop_blob()),
        // reply
        5 => api::reply(),

        // self
        6 => stack.push_blob(api::id()),

        // reject
        7 => api::reject(&stack.pop_blob()),

        // caller
        8 => stack.push_blob(api::caller()),

        // call
        9 => {
          // pop in reverse order!
          let data = stack.pop_blob();
          let on_reject = stack.pop_blob();
          let on_reply = stack.pop_blob();
          let method = stack.pop_blob();
          let callee = stack.pop_blob();
          let reply_env = add_callback(on_reply);
          let reject_env = add_callback(on_reject);
          let err_code = api::call_static(
                  &callee,
                  &method,
                  callback,
                  reply_env,
                  callback,
                  reject_env,
                  &data,
              );
          if err_code != 0 {
              api::trap_with("call_simple failed")
          }
        }

        // reject_msg
        10 => stack.push_blob(api::reject_message()),

        // reject_code
        11 => stack.push_int(api::reject_code()),

        // int to blob
        12 => {
            let i = stack.pop_int();
            stack.push_blob(i.to_le_bytes().to_vec())
        }

        // msg_data
        13 => stack.push_blob(api::arg_data()),

        // concat
        14 => {
          let mut b = stack.pop_blob();
          let mut a = stack.pop_blob();
          a.append(&mut b);
          stack.push_blob(a);
        }

        // stable memory
        15 => stack.push_int(api::stable_size()),
        16 => {
            let i = stack.pop_int();
            stack.push_int(api::stable_grow(i))
        }
        17 => {
            let size = stack.pop_int();
            let offset = stack.pop_int();
            stack.push_blob(api::stable_read(offset, size))
        }
        18 => {
            let data = stack.pop_blob();
            let offset = stack.pop_int();
            api::stable_write(offset, &data)
        }

        // debugging
        19 => api::print(&stack.pop_blob()),
        20 => api::trap_with_blob(&stack.pop_blob()),

        // some simple state
        21 => set_global(stack.pop_blob()),
        22 => stack.push_blob(get_global()),

        _ => api::trap_with(&format!("unknown op {}", op)),
      }
  }
}
#[export_name = "canister_update update"]
fn update() {
  setup();
  eval(&api::arg_data());
}

#[export_name = "canister_query query"]
fn query() {
  setup();
  eval(&api::arg_data());
}

#[export_name = "canister_init"]
fn init() {
  setup();
  eval(&api::arg_data());
}

/* A global variable */
lazy_static! {
  static ref GLOBAL : Mutex<Vec<u8>> = Mutex::new(Vec::new());
}
fn set_global(data : Vec<u8>) {
  *GLOBAL.lock().unwrap() = data;
}
fn get_global() -> Vec<u8> {
  GLOBAL.lock().unwrap().clone()
}

/* Callback handling */

#[macro_use]
extern crate lazy_static;
use std::sync::Mutex;
lazy_static! {
  static ref CALLBACKS : Mutex<Vec<Option<Vec<u8>>>> = Mutex::new(Vec::new());
}

fn add_callback(code : Vec<u8>) -> u32 {
  let mut vec = CALLBACKS.lock().unwrap();
  vec.push(Some(code));
  return (vec.len() as u32) - 1;
}

fn get_callback(idx : u32) -> Vec<u8> {
  let mut vec = CALLBACKS.lock().unwrap();
  if let Some(entry) = vec.get_mut(idx as usize) {
    if let Some(code) = entry.take() {
        return code
    } else {
      panic!("get_callback: {} already taken", idx)
    }
  } else {
    panic!("get_callback: {} out of bounds", idx)
  }
}

fn callback(env: u32) {
    eval(&get_callback(env));
}


/* Panic setup */

use std::sync::Once;

static START: Once = Once::new();

fn setup() {
  START.call_once(|| {
      api::set_panic_hook();
  });
}

fn main() {}
