# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, aeson, base, binary, bytestring, cborg, containers
, crc, cryptonite, data-default-class, directory, ed25519, filepath
, hex-text, http-client, http-types, leb128-cereal, memory, mtl
, optparse-applicative, primitive, random, stdenv, tasty
, tasty-html, tasty-hunit, text, transformers, unordered-containers
, utf8-string, vector, wai, warp, winter
}:
mkDerivation {
  pname = "ic-ref";
  version = "0.2.13";
  src = import ../gitSource.nix "impl";
  configureFlags = [ "-frelease" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring cborg containers crc cryptonite
    data-default-class directory ed25519 filepath hex-text http-client
    http-types leb128-cereal memory mtl optparse-applicative primitive
    random tasty tasty-html tasty-hunit text transformers
    unordered-containers utf8-string vector wai warp winter
  ];
  testHaskellDepends = [
    base bytestring cborg containers cryptonite ed25519 leb128-cereal
    memory mtl tasty tasty-hunit text unordered-containers
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
