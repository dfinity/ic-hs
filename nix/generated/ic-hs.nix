# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, aeson
, asn1-encoding
, asn1-types
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
, transformers
, uglymemo
, unordered-containers
, utf8-string
, vector
, wai
, wai-cors
, wai-extra
, warp
, winter
, zlib
}:
mkDerivation {
  pname = "ic-hs";
  version = "0.0.1";
  src = pkgs.lib.sourceByRegex (pkgs.subpath "/") [ "^src.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did" ];
  configureFlags = [ "-frelease" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
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
    transformers
    uglymemo
    unordered-containers
    utf8-string
    vector
    wai
    wai-cors
    wai-extra
    warp
    winter
    zlib
  ];
  executableHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
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
    optparse-applicative
    parallel
    prettyprinter
    primitive
    process
    random
    row-types
    serialise
    split
    splitmix
    tasty
    tasty-ant-xml
    tasty-html
    tasty-hunit
    tasty-rerun
    template-haskell
    text
    time
    transformers
    uglymemo
    unordered-containers
    utf8-string
    vector
    wai
    wai-cors
    wai-extra
    warp
    winter
    zlib
  ];
  testHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
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
    http-types
    leb128-cereal
    memory
    MonadRandom
    mtl
    parallel
    primitive
    quickcheck-io
    random
    row-types
    serialise
    split
    splitmix
    tasty
    tasty-hunit
    tasty-quickcheck
    temporary
    text
    time
    transformers
    uglymemo
    unordered-containers
    utf8-string
    vector
    winter
    zlib
  ];
  doCheck = false;
  license = "LicenseRef-IC-1.0";
}
