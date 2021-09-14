{ mkDerivation, base, logict, mtl, pretty, stdenv }:
mkDerivation {
  pname = "smallcheck";
  version = "1.2.1";
  sha256 = "e41f9d11b50e0526dd28c9bc6cf6dddf98cebd782911a00c3e5cbe4ce53fc869";
  libraryHaskellDepends = [ base logict mtl pretty ];
  homepage = "https://github.com/Bodigrim/smallcheck";
  description = "A property-based testing library";
  license = stdenv.lib.licenses.bsd3;
}
