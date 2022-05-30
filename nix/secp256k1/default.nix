{ lib
, stdenv
, fetchFromGitHub
, autoreconfHook
}:

stdenv.mkDerivation {
  pname = "secp256k1";

  version = "unstable-2022-05-19";

  src = fetchFromGitHub {
    owner = "bitcoin-core";
    repo = "secp256k1";
    rev = "44c2452fd387f7ca604ab42d73746e7d3a44d8a2";
    sha256 = "sha256-VXs4hwErka+E29r2d4DwJ4Fdtmrpy0vM3mShfNxxgEM";
  };

  nativeBuildInputs = [ autoreconfHook ];

  configureFlags = [
    "--enable-benchmark=no"
    "--enable-exhaustive-tests=no"
    "--enable-experimental"
    "--enable-module-ecdh"
    "--enable-module-recovery"
    "--enable-module-schnorrsig"
    "--enable-tests=yes"
  ];

  doCheck = true;

  checkPhase = "./tests";

  meta = with lib; {
    description = "Optimized C library for EC operations on curve secp256k1";
    longDescription = ''
      Optimized C library for EC operations on curve secp256k1. Part of
      Bitcoin Core. This library is a work in progress and is being used
      to research best practices. Use at your own risk.
    '';
    homepage = "https://github.com/bitcoin-core/secp256k1";
    license = with licenses; [ mit ];
    maintainers = with maintainers; [ ];
    platforms = with platforms; all;
  };
}
