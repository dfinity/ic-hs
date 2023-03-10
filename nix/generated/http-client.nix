# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, array
, async
, base
, base64-bytestring
, blaze-builder
, bytestring
, case-insensitive
, containers
, cookie
, deepseq
, directory
, exceptions
, filepath
, ghc-prim
, hspec
, hspec-discover
, http-types
, iproute
, lib
, mime-types
, monad-control
, network
, network-uri
, random
, stm
, streaming-commons
, text
, time
, transformers
, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.7.13.1";
  src = pkgs.sources.http-client;
  libraryHaskellDepends = [
    array
    async
    base
    base64-bytestring
    blaze-builder
    bytestring
    case-insensitive
    containers
    cookie
    deepseq
    exceptions
    filepath
    ghc-prim
    http-types
    iproute
    mime-types
    network
    network-uri
    random
    stm
    streaming-commons
    text
    time
    transformers
  ];
  testHaskellDepends = [
    async
    base
    blaze-builder
    bytestring
    case-insensitive
    containers
    cookie
    deepseq
    directory
    hspec
    http-types
    monad-control
    network
    network-uri
    streaming-commons
    text
    time
    transformers
    zlib
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = lib.licenses.mit;
}
