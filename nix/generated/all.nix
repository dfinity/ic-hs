self: super: {
  candid = super.callPackage ./candid.nix { };
  cborg = super.callPackage ./cborg.nix { };
  http-client = super.callPackage ./http-client.nix { };
  ic-hs = super.callPackage ./ic-hs.nix { };
  leb128-cereal = super.callPackage ./leb128-cereal.nix { };
  winter = super.callPackage ./winter.nix { };
}
