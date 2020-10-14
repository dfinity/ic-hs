# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, base16-bytestring
, base32
, bytestring
, cereal
, constraints
, containers
, crc
, dlist
, doctest
, hex-text
, leb128-cereal
, megaparsec
, mtl
, optparse-applicative
, prettyprinter
, row-types
, scientific
, smallcheck
, split
, stdenv
, tasty
, tasty-hunit
, tasty-rerun
, tasty-smallcheck
, template-haskell
, text
, unordered-containers
, vector
}:
mkDerivation {
  pname = "candid";
  version = "0.1";
  src = pkgs.sources.haskell-candid;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    base16-bytestring
    base32
    bytestring
    cereal
    constraints
    containers
    crc
    dlist
    hex-text
    leb128-cereal
    megaparsec
    mtl
    prettyprinter
    row-types
    scientific
    split
    template-haskell
    text
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
    doctest
    leb128-cereal
    prettyprinter
    row-types
    smallcheck
    tasty
    tasty-hunit
    tasty-rerun
    tasty-smallcheck
    text
    unordered-containers
    vector
  ];
  license = stdenv.lib.licenses.asl20;
}
