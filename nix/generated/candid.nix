# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, base, bytestring, cereal, containers, dlist
, doctest, hex-text, leb128-cereal, mtl, parsec, prettyprinter
, row-types, smallcheck, stdenv, tasty, tasty-hunit
, tasty-smallcheck, template-haskell, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "candid";
  version = "0.1";
  src = pkgs.sources.haskell-candid;
  libraryHaskellDepends = [
    base bytestring cereal containers dlist hex-text leb128-cereal mtl
    parsec prettyprinter row-types template-haskell text
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring doctest leb128-cereal prettyprinter row-types
    smallcheck tasty tasty-hunit tasty-smallcheck text
    unordered-containers vector
  ];
  license = stdenv.lib.licenses.asl20;
}
