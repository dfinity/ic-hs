"""
This module defines utilities for building Rust canisters.
"""

load("@rules_rust//rust:defs.bzl", "rust_binary")

def _wasm_rust_transition_impl(_settings, _attr):
    return {
        "//command_line_option:platforms": "@rules_rust//rust/platform:wasm",
        "@rules_rust//:extra_rustc_flags": [
            "-C",
            "linker-plugin-lto",
            "-C",
            "opt-level=z",
            "-C",
            "debug-assertions=no",
            "-C",
            "debuginfo=0",
            "-C",
            "lto",
        ],
    }

wasm_rust_transition = transition(
    implementation = _wasm_rust_transition_impl,
    inputs = [],
    outputs = [
        "//command_line_option:platforms",
        "@rules_rust//:extra_rustc_flags",
    ],
)

def _wasm_binary_impl(ctx):
    out = ctx.actions.declare_file(ctx.label.name + ".wasm")
    ctx.actions.run(
        executable = "cp",
        arguments = [ctx.files.binary[0].path, out.path],
        outputs = [out],
        inputs = ctx.files.binary,
    )

    return [DefaultInfo(files = depset([out]), runfiles = ctx.runfiles([out]))]

wasm_rust_binary_rule = rule(
    implementation = _wasm_binary_impl,
    attrs = {
        "binary": attr.label(mandatory = True, cfg = wasm_rust_transition),
        "_whitelist_function_transition": attr.label(default = "@bazel_tools//tools/whitelists/function_transition_whitelist"),
    },
)

def rust_canister(name, **kwargs):
    """Defines a rust program that builds into a WebAssembly module.

    Args:
      name: the name of the target that produces a Wasm module.
      **kwargs: additional arguments to pass a rust_binary rule.
    """
    wasm_name = "_wasm_" + name.replace(".", "_")
    kwargs.setdefault("visibility", ["//visibility:public"])

    rust_binary(
        name = wasm_name,
        crate_type = "bin",
        **kwargs
    )

    wasm_rust_binary_rule(
        name = name + ".raw",
        binary = ":" + wasm_name,
    )

    # Invokes canister WebAssembly module optimizer.
    native.genrule(
        name = name + ".opt",
        srcs = [name + ".raw"],
        outs = [name + ".opt.wasm"],
        message = "Shrinking canister " + name,
        exec_tools = ["@crate_index//:ic-wasm__ic-wasm"],
        cmd_bash = "$(location @crate_index//:ic-wasm__ic-wasm) $< -o $@ shrink",
    )
