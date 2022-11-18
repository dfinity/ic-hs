# Give your project a name. :)
workspace(name = "IC_HS")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.15",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.15.tar.gz"],
    sha256 = "aba3c16015a2363b16e2f867bdc5c792fa71c68cb97d8fe95fddc41e409d6ba8",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

# Load nixpkgs_git_repository from rules_nixpkgs,
# which was already initialized by rules_haskell_dependencies above.
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

# Fetch a version of nixpkgs from GitHub.
# For more information see the documentation of rules_nixpkgs at
# https://github.com/tweag/rules_nixpkgs/blob/master/README.md
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "21.11",
    sha256 = "c77bb41cf5dd82f4718fa789d49363f512bb6fa6bc25f8d60902fe2d698ed7cc",
)

nixpkgs_cc_configure(
    repository = "@nixpkgs",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

nixpkgs_package(
    repository = "@nixpkgs",
    name = "ic-hs",
    attribute_path = "ic-hs",
    nix_file = "//:default.nix",
    nix_file_deps = ["//:cbits/aes.c", "//:cbits/arch.h", "//:cbits/big_384_58.c", "//:cbits/big_384_58.h", "//:cbits/bls_BLS12381.c", "//:cbits/bls_BLS12381.h", "//:cbits/config_big_384_58.h", "//:cbits/config_curve_BLS12381.h", "//:cbits/config_field_BLS12381.h", "//:cbits/core.h", "//:cbits/ecdh_BLS12381.h", "//:cbits/ecp2_BLS12381.c", "//:cbits/ecp2_BLS12381.h", "//:cbits/ecp_BLS12381.c", "//:cbits/ecp_BLS12381.h", "//:cbits/fp12_BLS12381.c", "//:cbits/fp12_BLS12381.h", "//:cbits/fp2_BLS12381.c", "//:cbits/fp2_BLS12381.h", "//:cbits/fp4_BLS12381.c", "//:cbits/fp4_BLS12381.h", "//:cbits/fp_BLS12381.c", "//:cbits/fp_BLS12381.h", "//:cbits/gcm.c", "//:cbits/hash.c", "//:cbits/hmac.c", "//:cbits/hpke_BLS12381.h", "//:cbits/mpin_BLS12381.h", "//:cbits/newhope.c", "//:cbits/newhope.h", "//:cbits/oct.c", "//:cbits/pair_BLS12381.c", "//:cbits/pair_BLS12381.h", "//:cbits/randapi.c", "//:cbits/randapi.h", "//:cbits/rand.c", "//:cbits/README.md", "//:cbits/rom_curve_BLS12381.c", "//:cbits/rom_field_BLS12381.c", "//:cbits/share.c", "//:cbits/x509.h", "//:ic.did", "//:ic-hs.cabal", "//:LICENSE", "//:nix/default.nix", "//:nix/generated/all.nix", "//:nix/generated/candid.nix", "//:nix/generated/ic-hs.nix", "//:nix/generated/leb128-cereal.nix", "//:nix/generated/README.md", "//:nix/generated/winter.nix", "//:nix/generate.nix", "//:nix/gitSource.nix", "//:nix/python-cbor2.nix", "//:nix/secp256k1/default.nix", "//:nix/sources.json", "//:src/IC/Canister.hs", "//:src/IC/Canister/Imp.hs", "//:src/IC/Canister/Snapshot.hs", "//:src/IC/Canister/StableMemory.hs", "//:src/IC/CBOR/Parser.hs", "//:src/IC/CBOR/Patterns.hs", "//:src/IC/CBOR/Utils.hs", "//:src/IC/Certificate/CBOR.hs", "//:src/IC/Certificate.hs", "//:src/IC/Certificate/Validate.hs", "//:src/IC/Certificate/Value.hs", "//:src/IC/Constants.hs", "//:src/IC/Crypto/Bitcoin.hs", "//:src/IC/Crypto/BLS.hsc", "//:src/IC/Crypto/CanisterSig.hs", "//:src/IC/Crypto/DER_BLS.hs", "//:src/IC/Crypto/DER/Decode.hs", "//:src/IC/Crypto/DER.hs", "//:src/IC/Crypto/ECDSA.hs", "//:src/IC/Crypto/Ed25519.hs", "//:src/IC/Crypto.hs", "//:src/IC/Crypto/Secp256k1.hs", "//:src/IC/Crypto/WebAuthn.hs", "//:src/IC/Debug/JSON.hs", "//:src/IC/DRun/Parse.hs", "//:src/IC/Hash.hs", "//:src/IC/HashTree/CBOR.hs", "//:src/IC/HashTree.hs", "//:src/IC/HTTP/CBOR.hs", "//:src/IC/HTTP/GenR.hs", "//:src/IC/HTTP/GenR/Parse.hs", "//:src/IC/HTTP.hs", "//:src/IC/HTTP/Request.hs", "//:src/IC/HTTP/RequestId.hs", "//:src/IC/HTTP/Status.hs", "//:src/IC/Id/Forms.hs", "//:src/IC/Id/Fresh.hs", "//:src/IC/Management.hs", "//:src/IC/Purify.hs", "//:src/ic-ref.hs", "//:src/IC/Ref.hs", "//:src/IC/Ref/IO.hs", "//:src/ic-ref-run.hs", "//:src/ic-ref-test.hs", "//:src/ic-request-id.hs", "//:src/IC/Serialise.hs", "//:src/IC/StateFile.hs", "//:src/IC/Test/Agent/Calls.hs", "//:src/IC/Test/Agent.hs", "//:src/IC/Test/BLS.hs", "//:src/IC/Test/ECDSA.hs", "//:src/IC/Test/HashTree.hs", "//:src/IC/Test/Options.hs", "//:src/IC/Test/Secp256k1.hs", "//:src/IC/Test/Spec.hs", "//:src/IC/Test/Spec/TECDSA.hs", "//:src/IC/Test/Spec/Utils.hs", "//:src/IC/Test/StableMemory.hs", "//:src/IC/Test/Universal.hs", "//:src/IC/Test/WebAuthn.hs", "//:src/IC/Types.hs", "//:src/IC/Utils.hs", "//:src/IC/Version.hs", "//:src/IC/Wasm/Imports.hs", "//:src/IC/Wasm/Winter.hs", "//:src/IC/Wasm/Winter/Persist.hs", "//:src/SourceId.hs", "//:src/unit-tests.hs", "//:universal-canister/Cargo.lock", "//:universal-canister/Cargo.toml", "//:universal-canister/default.nix", "//:universal-canister/.envrc", "//:universal-canister/.gitignore", "//:universal-canister/shell.nix", "//:universal-canister/src/api.rs", "//:universal-canister/src/lib.rs", "//:universal-canister/src/main.rs"],
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# To find additional information on this release or newer ones visit:
# https://github.com/bazelbuild/rules_rust/releases
http_archive(
    name = "rules_rust",
    sha256 = "696b01deea96a5e549f1b5ae18589e1bbd5a1d71a36a243b5cf76a9433487cf2",
    urls = ["https://github.com/bazelbuild/rules_rust/releases/download/0.11.0/rules_rust-v0.11.0.tar.gz"],
)

load("@rules_rust//rust:repositories.bzl", "rules_rust_dependencies", "rust_register_toolchains")

rules_rust_dependencies()

rust_register_toolchains()

load("@rules_rust//crate_universe:repositories.bzl", "crate_universe_dependencies")

crate_universe_dependencies()

load("@rules_rust//crate_universe:defs.bzl", "crates_repository")

crates_repository(
    name = "crate_index",
    cargo_lockfile = "//:Cargo.Bazel.lock",
    manifests = ["//universal-canister:Cargo.toml"],
)

load("@crate_index//:defs.bzl", "crate_repositories")

crate_repositories()
