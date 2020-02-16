nix: subpath:
  self: super: {
  winter = self.callCabal2nixWithOptions "winter" nix.sources.winter "--no-check" {};
  ic-stub = self.callCabal2nixWithOptions "ic-stub" (subpath "impl") "-frelease" { };
}
