# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, aeson
, asn1-encoding
, asn1-types
, base
, base32
, binary
, bindings-DSL
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
, hashable
, hex-text
, http-client
, http-types
, leb128-cereal
, memory
, MonadRandom
, mtl
, optparse-applicative
, prettyprinter
, primitive
, process
, process-extras
, QuickCheck
, quickcheck-io
, random
, row-types
, split
, stdenv
, tasty
, tasty-html
, tasty-hunit
, tasty-quickcheck
, tasty-rerun
, template-haskell
, temporary
, text
, time
, transformers
, unordered-containers
, utf8-string
, vector
, wai
, wai-extra
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
    asn1-encoding
    asn1-types
    base
    base32
    binary
    bindings-DSL
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
    hashable
    hex-text
    http-client
    http-types
    leb128-cereal
    memory
    MonadRandom
    mtl
    optparse-applicative
    prettyprinter
    primitive
    process
    process-extras
    random
    row-types
    split
    tasty
    tasty-html
    tasty-hunit
    tasty-rerun
    template-haskell
    temporary
    text
    time
    transformers
    unordered-containers
    utf8-string
    vector
    wai
    wai-extra
    warp
    winter
  ];
  testHaskellDepends = [
    asn1-encoding
    asn1-types
    base
    bindings-DSL
    bytestring
    cborg
    containers
    cryptonite
    ed25519
    hashable
    leb128-cereal
    memory
    mtl
    process-extras
    QuickCheck
    quickcheck-io
    tasty
    tasty-hunit
    tasty-quickcheck
    temporary
    text
    unordered-containers
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
