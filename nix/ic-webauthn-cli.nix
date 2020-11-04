{ sources, lib, buildPythonPackage, fido2, cbor2, pynacl }:

buildPythonPackage rec {
  name = "ic-webauthn-cli";
  src = sources.ic-webauthn-cli;
  propagatedBuildInputs = [ fido2 cbor2 pynacl ];
  doCheck = false;
}
