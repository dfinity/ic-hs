# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, aeson
, base
, base32
, binary
, bytestring
, candid
, cborg
, containers
, crc
, cryptonite
, data-default-class
, directory
, ed25519
, filepath
, hex-text
, http-client
, http-types
, leb128-cereal
, memory
, mtl
, optparse-applicative
, prettyprinter
, primitive
, process
, random
, row-types
, split
, stdenv
, tasty
, tasty-html
, tasty-hunit
, tasty-rerun
, template-haskell
, text
, time
, transformers
, unordered-containers
, utf8-string
, vector
, wai
, warp
, winter
}:
mkDerivation {
  pname = "ic-ref";
  version = "0.0.1";
  src = import ../gitSource.nix "impl";
  configureFlags = [ "-frelease" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    base32
    binary
    bytestring
    candid
    cborg
    containers
    crc
    cryptonite
    data-default-class
    directory
    ed25519
    filepath
    hex-text
    http-client
    http-types
    leb128-cereal
    memory
    mtl
    optparse-applicative
    prettyprinter
    primitive
    process
    random
    row-types
    split
    tasty
    tasty-html
    tasty-hunit
    tasty-rerun
    template-haskell
    text
    time
    transformers
    unordered-containers
    utf8-string
    vector
    wai
    warp
    winter
  ];
  testHaskellDepends = [
    base
    bytestring
    cborg
    containers
    cryptonite
    ed25519
    leb128-cereal
    memory
    mtl
    tasty
    tasty-hunit
    text
    unordered-containers
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
