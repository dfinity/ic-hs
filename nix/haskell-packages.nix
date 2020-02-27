nix: subpath:
  self: super: {
  winter = super.callPackage generated/winter.nix {};
  ic-ref = super.callPackage generated/ic-ref.nix {};
}
