{ mkDerivation
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
  version = "0.7.11";
  sha256 = "bbc6c385ff5dba4adc5e3538154133211d46c410751f6f8de6b2893aefdc478a";
  revision = "1";
  editedCabalFile = "0rqrhx4cjqcqhcfad6sjc0wl2anzs6h4186xdj26hdrazs7sqn9z";
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
