# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, array, base, bifunctors, containers, contravariant
, directory, filepath, ghc, ghc-boot, ghc-paths, happy, indents
, lens, mtl, optparse-applicative, parsec, pipes, semigroups
, stdenv, syb, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, transformers, validation, wl-pprint-text, yaml
}:
mkDerivation {
  pname = "hs-to-coq";
  version = "0.0.0.0";
  src = pkgs.sources.hs-to-coq;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors containers contravariant directory filepath
    ghc ghc-boot ghc-paths indents lens mtl optparse-applicative parsec
    pipes semigroups syb template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 text transformers
    validation wl-pprint-text yaml
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [ base ];
  homepage = "http://www.deepspec.org/research/Haskell/";
  description = "Convert Haskell datatypes/functions to Coq";
  license = stdenv.lib.licenses.mit;
}
