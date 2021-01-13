nix: subpath:
  self: super: {
  winter = super.callPackage generated/winter.nix {};
  ic-ref = super.callPackage generated/ic-ref.nix {};
  leb128-cereal = super.callPackage generated/leb128-cereal.nix {};
  candid = super.callPackage generated/candid.nix {};
  # no base32 in nixos-20.03
  base32 = super.callPackage generated/base32.nix {};
  megaparsec = super.callPackage generated/megaparsec.nix {};
  # need newer version
  base64-bytestring = super.callPackage generated/base64-bytestring.nix {};

  # Only the test suite of crc is broken
  # https://github.com/MichaelXavier/crc/issues/2
  crc = nix.haskell.lib.markUnbroken (nix.haskell.lib.dontCheck super.crc);
}
