{ mkDerivation
, base
, constraints
, criterion
, deepseq
, generic-lens
, hashable
, lib
, profunctors
, text
, unordered-containers
}:
mkDerivation {
  pname = "row-types";
  version = "0.4.0.0";
  sha256 = "67b2f5667aa1cd46a1dea9b925794ba882d5704f7f8cf191369af2627c65dc90";
  libraryHaskellDepends = [
    base
    constraints
    deepseq
    generic-lens
    hashable
    profunctors
    text
    unordered-containers
  ];
  testHaskellDepends = [ base generic-lens ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "https://github.com/target/row-types";
  description = "Open Records and Variants";
  license = lib.licenses.mit;
}
