# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, aeson
, asn1-encoding
, asn1-types
, async
, atomic-write
, base
, base32
, base64-bytestring
, binary
, bytestring
, candid
, case-insensitive
, cborg
, cereal
, connection
, containers
, crc
, cryptonite
, data-default-class
, directory
, ed25519
, either
, filepath
, hashable
, haskoin-core
, hex-text
, http-client
, http-client-tls
, http-types
, leb128-cereal
, lib
, memory
, MonadRandom
, mtl
, network-uri
, optparse-applicative
, parallel
, prettyprinter
, primitive
, process
, quickcheck-io
, random
, row-types
, serialise
, split
, splitmix
, tasty
, tasty-ant-xml
, tasty-html
, tasty-hunit
, tasty-quickcheck
, tasty-rerun
, template-haskell
, temporary
, text
, time
, tls
, transformers
, uglymemo
, unordered-containers
, utf8-string
, vector
, wai
, wai-cors
, wai-extra
, warp
, wide-word
, winter
, word8
, x509
, x509-store
, x509-validation
, zlib
}:
mkDerivation {
  pname = "ic-hs";
  version = "0.0.1";
  src = pkgs.lib.sourceByRegex (pkgs.subpath "/") [ "^src.*" "^bin.*" "^tests.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
    async
    atomic-write
    base
    base32
    base64-bytestring
    binary
    bytestring
    candid
    case-insensitive
    cborg
    cereal
    connection
    containers
    crc
    cryptonite
    data-default-class
    directory
    ed25519
    either
    filepath
    hashable
    haskoin-core
    hex-text
    http-client
    http-client-tls
    http-types
    leb128-cereal
    memory
    MonadRandom
    mtl
    network-uri
    optparse-applicative
    parallel
    prettyprinter
    primitive
    process
    quickcheck-io
    random
    row-types
    serialise
    split
    splitmix
    tasty
    tasty-ant-xml
    tasty-html
    tasty-hunit
    tasty-quickcheck
    tasty-rerun
    template-haskell
    temporary
    text
    time
    tls
    transformers
    uglymemo
    unordered-containers
    utf8-string
    vector
    wai
    wai-cors
    wai-extra
    warp
    wide-word
    winter
    word8
    x509
    x509-store
    x509-validation
    zlib
  ];
  executableHaskellDepends = [
    async
    base
    bytestring
    candid
    containers
    hex-text
    MonadRandom
    mtl
    optparse-applicative
    prettyprinter
    row-types
    tasty
    tasty-ant-xml
    tasty-html
    tasty-rerun
    text
    time
    transformers
    unordered-containers
    wai-cors
    wai-extra
    warp
    x509-store
  ];
  testHaskellDepends = [
    base
    containers
    directory
    tasty
    tasty-hunit
    temporary
  ];
  doCheck = false;
  license = "LicenseRef-IC-1.0";
}
