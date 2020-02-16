nix: subpath:
  self: super: {
  winter = self.callCabal2nixWithOptions "winter" nix.sources.winter "--no-check" {};
  ic-ref = self.callCabal2nixWithOptions "ic-ref" (subpath "impl") "-frelease" { };
}
