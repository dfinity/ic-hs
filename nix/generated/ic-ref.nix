# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, aeson, base, binary, bytestring, cborg, containers
, crc, cryptonite, data-default-class, directory, ed25519, filepath
, hex-text, http-client, http-types, memory, mtl
, optparse-applicative, primitive, process-extras, random, stdenv
, tasty, tasty-html, tasty-hunit, text, transformers
, unordered-containers, utf8-string, vector, wai, warp, winter
}:
mkDerivation {
  pname = "ic-ref";
  version = "0.3.1";
  src = import ../gitSource.nix "impl";
  configureFlags = [ "-frelease" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring cborg containers crc cryptonite
    data-default-class directory ed25519 filepath hex-text http-client
    http-types memory mtl optparse-applicative primitive process-extras
    random tasty tasty-html tasty-hunit text transformers
    unordered-containers utf8-string vector wai warp winter
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
