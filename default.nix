{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in

let stdenv = nixpkgs.stdenv; in

let subpath = p: import ./nix/gitSource.nix p; in

let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = import nix/haskell-packages.nix nixpkgs subpath;
    }; in

rec {
  inherit (haskellPackages) ic-stub;

  # populate our nix cache with the right version of cabal2nix that
  # is used by self.callCabal2nix at evalution time
  inherit (nixpkgs) cabal2nix;

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [ ic-stub ];
  };

  # include shell in default so that the cache has the extra shell packages
  shell =
    let extra-pkgs = [
      nixpkgs.cabal-install
      nixpkgs.ghcid
    ]; in
    haskellPackages.ic-stub.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
    });
}
