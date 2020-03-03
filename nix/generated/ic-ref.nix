# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, aeson, base, binary, bytestring, cborg, containers
, cryptonite, data-default-class, filepath, hex-text, http-types
, memory, mtl, optparse-applicative, primitive, stdenv, text
, transformers, unordered-containers, utf8-string, vector, wai
, warp, winter
}:
mkDerivation {
  pname = "ic-ref";
  version = "0.1.0.0";
  src = import ../gitSource.nix "impl";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring cborg containers cryptonite
    data-default-class filepath hex-text http-types memory mtl
    optparse-applicative primitive text transformers
    unordered-containers utf8-string vector wai warp winter
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
