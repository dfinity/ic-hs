{ mkDerivation
, base
, constraints
, deepseq
, gauge
, generic-lens
, hashable
, lib
, profunctors
, text
, unordered-containers
}:
mkDerivation {
  pname = "row-types";
  version = "1.0.1.0";
  sha256 = "42e83595be831a7194ea50e8293aec8b9236f8dad68a2e769294415b8d0e5357";
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
  benchmarkHaskellDepends = [ base deepseq gauge ];
  homepage = "https://github.com/target/row-types";
  description = "Open Records and Variants";
  license = lib.licenses.mit;
}
