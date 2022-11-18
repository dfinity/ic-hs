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
    nix_file_deps = ["//:LICENSE", "//:ic-hs.cabal", "//:nix/generate.nix", "//:nix/sources.json", "//:nix/python-cbor2.nix", "//:nix/secp256k1/default.nix", "//:nix/generated/winter.nix", "//:nix/generated/README.md", "//:nix/generated/all.nix", "//:nix/generated/leb128-cereal.nix", "//:nix/generated/candid.nix", "//:nix/generated/ic-hs.nix", "//:nix/default.nix", "//:nix/gitSource.nix", "//:universal-canister/.envrc", "//:universal-canister/.gitignore", "//:universal-canister/shell.nix", "//:universal-canister/src/lib.rs", "//:universal-canister/src/api.rs", "//:universal-canister/src/main.rs", "//:universal-canister/Cargo.toml", "//:universal-canister/Cargo.lock", "//:universal-canister/default.nix", "//:src/ic-ref-test.hs", "//:src/ic-ref.hs", "//:src/IC/HTTP/Request.hs", "//:src/IC/HTTP/GenR.hs", "//:src/IC/HTTP/Status.hs", "//:src/IC/HTTP/GenR/Parse.hs", "//:src/IC/HTTP/CBOR.hs", "//:src/IC/HTTP/RequestId.hs", "//:src/IC/DRun/Parse.hs", "//:src/IC/StateFile.hs", "//:src/IC/Canister.hs", "//:src/IC/Utils.hs", "//:src/IC/HashTree.hs", "//:src/IC/Management.hs", "//:src/IC/Version.hs", "//:src/IC/Ref.hs", "//:src/IC/Id/Forms.hs", "//:src/IC/Id/Fresh.hs", "//:src/IC/HashTree/CBOR.hs", "//:src/IC/HTTP.hs", "//:src/IC/Constants.hs", "//:src/IC/Purify.hs", "//:src/IC/Certificate/Value.hs", "//:src/IC/Certificate/Validate.hs", "//:src/IC/Certificate/CBOR.hs", "//:src/IC/Types.hs", "//:src/IC/Crypto/DER/Decode.hs", "//:src/IC/Crypto/ECDSA.hs", "//:src/IC/Crypto/Bitcoin.hs", "//:src/IC/Crypto/Ed25519.hs", "//:src/IC/Crypto/DER_BLS.hs", "//:src/IC/Crypto/Secp256k1.hs", "//:src/IC/Crypto/DER.hs", "//:src/IC/Crypto/BLS.hsc", "//:src/IC/Crypto/CanisterSig.hs", "//:src/IC/Crypto/WebAuthn.hs", "//:src/IC/Wasm/Winter.hs", "//:src/IC/Wasm/Winter/Persist.hs", "//:src/IC/Wasm/Imports.hs", "//:src/IC/Hash.hs", "//:src/IC/CBOR/Utils.hs", "//:src/IC/CBOR/Patterns.hs", "//:src/IC/CBOR/Parser.hs", "//:src/IC/Test/Agent.hs", "//:src/IC/Test/Spec/Utils.hs", "//:src/IC/Test/Spec/TECDSA.hs", "//:src/IC/Test/ECDSA.hs", "//:src/IC/Test/Spec.hs", "//:src/IC/Test/Options.hs", "//:src/IC/Test/HashTree.hs", "//:src/IC/Test/BLS.hs", "//:src/IC/Test/Secp256k1.hs", "//:src/IC/Test/StableMemory.hs", "//:src/IC/Test/Universal.hs", "//:src/IC/Test/Agent/Calls.hs", "//:src/IC/Test/WebAuthn.hs", "//:src/IC/Debug/JSON.hs", "//:src/IC/Ref/IO.hs", "//:src/IC/Canister/StableMemory.hs", "//:src/IC/Canister/Snapshot.hs", "//:src/IC/Canister/Imp.hs", "//:src/IC/Serialise.hs", "//:src/IC/Certificate.hs", "//:src/IC/Crypto.hs", "//:src/ic-request-id.hs", "//:src/ic-ref-run.hs", "//:src/unit-tests.hs", "//:src/SourceId.hs", "//:cbits/big_384_58.h", "//:cbits/ecp_BLS12381.c", "//:cbits/newhope.h", "//:cbits/arch.h", "//:cbits/bls_BLS12381.c", "//:cbits/ecdh_BLS12381.h", "//:cbits/fp2_BLS12381.c", "//:cbits/mpin_BLS12381.h", "//:cbits/hmac.c", "//:cbits/gcm.c", "//:cbits/rand.c", "//:cbits/randapi.c", "//:cbits/share.c", "//:cbits/newhope.c", "//:cbits/README.md", "//:cbits/fp2_BLS12381.h", "//:cbits/fp4_BLS12381.h", "//:cbits/fp4_BLS12381.c", "//:cbits/config_curve_BLS12381.h", "//:cbits/ecp2_BLS12381.c", "//:cbits/fp12_BLS12381.c", "//:cbits/hpke_BLS12381.h", "//:cbits/pair_BLS12381.h", "//:cbits/fp12_BLS12381.h", "//:cbits/pair_BLS12381.c", "//:cbits/oct.c", "//:cbits/fp_BLS12381.c", "//:cbits/bls_BLS12381.h", "//:cbits/aes.c", "//:cbits/fp_BLS12381.h", "//:cbits/big_384_58.c", "//:cbits/ecp2_BLS12381.h", "//:cbits/randapi.h", "//:cbits/hash.c", "//:cbits/rom_field_BLS12381.c", "//:cbits/x509.h", "//:cbits/rom_curve_BLS12381.c", "//:cbits/core.h", "//:cbits/config_field_BLS12381.h", "//:cbits/config_big_384_58.h", "//:cbits/ecp_BLS12381.h", "//:ic.did"],
)
