def add_exports(n):
    ret = ""
    for i in range(n):
      if i % 3 == 0:
        t = "update"
      elif i % 3 == 1:
        t = "query"
      else:
        t = "composite_query"
      ret += export("canister_{} m{}".format(t, i), "read")
    return ret

def add_long_exported_function_names(n):
    ret = ""
    a = n // 3
    n -= a
    b = n // 2
    n -= b
    c = n
    ret += export("canister_update {}".format("a" * a), "read")
    ret += export("canister_update {}".format("b" * b), "read")
    ret += export("canister_update {}".format("c" * c), "read")
    return ret

def add_functions(n):
    ret = ""
    for i in range(n):
        ret += '    (func $m{})\n'.format(i)
    return ret

def add_globals(n):
    ret = ""
    for i in range(n):
        ret += '    (global i32 (i32.const {}))\n'.format(i)
    return ret

def enc32(x):
  assert x != 0
  r = b''
  while x != 0:
    b = x % 128
    x //= 128
    if x != 0:
      b += 128
    r += b.to_bytes(1, 'little')
  return r

def add_custom_section(n, c):
    ret = b'\x00'
    ret += enc32(len(enc32(len(n))) + len(n) + len(c))
    ret += enc32(len(n))
    ret += n
    ret += c
    return ret

def with_custom_sections(cs):
    ret = bytes([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00])
    for n, c in cs:
      ret += add_custom_section(n, c)
    return ret

def export(name, what):
    return '    (export "{}" (func ${}))\n'.format(name, what)

base = '''(module
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
    (import "ic0" "msg_reply_data_append"
        (func $msg_reply_data_append (param i32 i32)))
    (import "ic0" "msg_reply" (func $msg_reply))
    (func $reta (result i32)
        (return (i32.const 42)))
    (func $retb (param $src i32))
    (func $inc
        (i32.store
            (i32.const 0)
            (i32.add (i32.load (i32.const 0)) (i32.const 4))))
    (func $read
        (call $msg_reply_data_append
            (i32.const 0)
            (i32.const 4))
        (call $msg_reply))
'''

end = '''(memory $memory 1)
)'''

with open("valid_import.wat", "w") as f:
    wat = '''(module
    (import "ic0" "call_perform" (func $ic0_call_perform (result i32)))
)'''
    print(wat, file=f)

with open("invalid_import.wat", "w") as f:
    wat = '''(module
    (import "ic0" "my_call_perform" (func $ic0_call_perform (result i32)))
)'''
    print(wat, file=f)

with open("start.wat", "w") as f:
    wat = base + '    (start $inc)\n' + export("canister_query read", "read") + end
    print(wat, file=f)

with open("no_start.wat", "w") as f:
    wat = base + export("canister_query read", "read") + end
    print(wat, file=f)

for fun in ["reta", "retb"]:
    for method in ["canister_init", "canister_inspect_message", "canister_heartbeat", "canister_global_timer", "canister_update upd", "canister_query que", "canister_composite_query cq", "canister_pre_upgrade", "canister_post_upgrade"]:
        with open("invalid_{}_{}.wat".format(method.replace(' ', '_'), fun), "w") as f:
            wat = base + export(method, fun) + end
            print(wat, file=f)

with open("name_clash_update_query.wat", "w") as f:
    wat = base + export("canister_update read", "read") + export("canister_query read", "read") + end
    print(wat, file=f)

with open("name_clash_update_composite_query.wat", "w") as f:
    wat = base + export("canister_update read", "read") + export("canister_composite_query read", "read") + end
    print(wat, file=f)

with open("name_clash_query_composite_query.wat", "w") as f:
    wat = base + export("canister_query read", "read") + export("canister_composite_query read", "read") + end
    print(wat, file=f)

with open("invalid_canister_export.wat", "w") as f:
    wat = base + export("canister_callback read", "read") + end
    print(wat, file=f)

with open("duplicate_custom_section.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:public x", 'utf-8'), bytes("a", 'utf-8')), (bytes("icp:private x", 'utf-8'), bytes("b", 'utf-8'))])
    f.write(wasm)

with open("invalid_custom_section.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:mine x", 'utf-8'), bytes("y", 'utf-8'))])
    f.write(wasm)

with open("many_functions.wat", "w") as f:
    wat = base + add_functions(50000 - 4) + end # there are already 4 functions declared in base (excl. imported)
    print(wat, file=f)

with open("too_many_functions.wat", "w") as f:
    wat = base + add_functions((50000 - 4) + 1) + end # there are already 4 functions declared in base (excl. imported)
    print(wat, file=f)

with open("many_globals.wat", "w") as f:
    wat = '(module\n' + add_globals(300) + end
    print(wat, file=f)

with open("too_many_globals.wat", "w") as f:
    wat = '(module\n' + add_globals(301) + end
    print(wat, file=f)

with open("many_custom_sections.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:{} c{}".format("public" if i % 2 == 0 else "private", i), 'utf-8'), bytes("x{}".format(i), 'utf-8')) for i in range(16)] + [(bytes("ic:{} c{}".format("public" if i % 2 == 0 else "private", i), 'utf-8'), bytes("x{}".format(i), 'utf-8')) for i in range(16)])
    f.write(wasm)

with open("too_many_custom_sections.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:{} c{}".format("public" if i % 2 == 0 else "private", i), 'utf-8'), bytes("x{}".format(i), 'utf-8')) for i in range(17)] + [(bytes("ic:{} c{}".format("public" if i % 2 == 0 else "private", i), 'utf-8'), bytes("x{}".format(i), 'utf-8')) for i in range(16)])
    f.write(wasm)

with open("many_exports.wat", "w") as f:
    wat = base + add_exports(1000) + end
    print(wat, file=f)

with open("too_many_exports.wat", "w") as f:
    wat = base + add_exports(1001) + end
    print(wat, file=f)

with open("long_exported_function_names.wat", "w") as f:
    wat = base + add_long_exported_function_names(20000) + end
    print(wat, file=f)

with open("too_long_exported_function_names.wat", "w") as f:
    wat = base + add_long_exported_function_names(20001) + end
    print(wat, file=f)

with open("large_custom_sections.wasm", "wb") as f:
    n = 2**20 - 2 # subtract the name lengths
    a = n // 2
    n -= a
    b = n
    wasm = with_custom_sections([(bytes("icp:private x", 'utf-8'), b"a" * a), (bytes("icp:public y", 'utf-8'), b"b" * b)])
    f.write(wasm)

with open("too_large_custom_sections.wasm", "wb") as f:
    n = ((2**20) - 2) + 1 # subtract the name lengths
    a = n // 2
    n -= a
    b = n
    wasm = with_custom_sections([(bytes("icp:private x", 'utf-8'), b"a" * a), (bytes("icp:public y", 'utf-8'), b"b" * b)])
    f.write(wasm)

# Corner cases

with open("invalid_empty_query_name.wat", "w") as f:
    wat = base + export("canister_query", "read") + end
    print(wat, file=f)

with open("empty_query_name.wat", "w") as f:
    wat = base + export("canister_query ", "read") + end
    print(wat, file=f)

with open("query_name_with_spaces.wat", "w") as f:
    wat = base + export("canister_query name with spaces", "read") + end
    print(wat, file=f)

with open("invalid_empty_custom_section_name.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:public", 'utf-8'), b"a")])
    f.write(wasm)

with open("empty_custom_section_name.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:public ", 'utf-8'), b"a")])
    f.write(wasm)

with open("custom_section_name_with_spaces.wasm", "wb") as f:
    wasm = with_custom_sections([(bytes("icp:public name with spaces", 'utf-8'), b"a")])
    f.write(wasm)
