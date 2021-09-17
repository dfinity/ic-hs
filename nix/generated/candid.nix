# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, base32
, bytestring
, cereal
, constraints
, containers
, crc
, directory
, dlist
, doctest
, file-embed
, filepath
, hex-text
, leb128-cereal
, lib
, megaparsec
, mtl
, optparse-applicative
, parser-combinators
, prettyprinter
, row-types
, scientific
, smallcheck
, split
, tasty
, tasty-hunit
, tasty-quickcheck
, tasty-rerun
, tasty-smallcheck
, template-haskell
, text
, transformers
, unordered-containers
, vector
}:
mkDerivation {
  pname = "candid";
  version = "0.2";
  src = pkgs.sources.haskell-candid;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    base32
    bytestring
    cereal
    constraints
    containers
    crc
    dlist
    file-embed
    hex-text
    leb128-cereal
    megaparsec
    mtl
    parser-combinators
    prettyprinter
    row-types
    scientific
    split
    template-haskell
    text
    transformers
    unordered-containers
    vector
  ];
  executableHaskellDepends = [
    base
    bytestring
    hex-text
    optparse-applicative
    prettyprinter
    text
  ];
  testHaskellDepends = [
    base
    bytestring
    directory
    doctest
    filepath
    leb128-cereal
    prettyprinter
    row-types
    smallcheck
    tasty
    tasty-hunit
    tasty-quickcheck
    tasty-rerun
    tasty-smallcheck
    template-haskell
    text
    unordered-containers
    vector
  ];
  homepage = "https://github.com/dfinity/candid";
  description = "Candid integration";
  license = lib.licenses.asl20;
}
