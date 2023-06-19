# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, bindings-DSL
, bytestring
, lib
, primitive
, tasty
, tasty-hunit
, transformers
, vector
, wasmtime
, wide-word
}:
mkDerivation {
  pname = "wasmtime";
  version = "0.0.0.0";
  src = pkgs.sources.wasmtime-hs;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base
    bindings-DSL
    bytestring
    primitive
    tasty
    tasty-hunit
    transformers
    vector
    wide-word
  ];
  librarySystemDepends = [ wasmtime ];
  testHaskellDepends = [
    base
    bytestring
    primitive
    tasty
    tasty-hunit
  ];
  doCheck = false;
  homepage = "https://github.com/dfinity/wasmtime-hs";
  description = "Haskell bindings to the wasmtime WASM engine";
  license = lib.licenses.bsd2;
}
