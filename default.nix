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
  inherit (haskellPackages) ic-ref;

  check-generated = nixpkgs.runCommandNoCC "check-generated" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = import ./nix/generate.nix { pkgs = nixpkgs; };
      dir = ./nix/generated;
    } ''
      diff -r -U 3 $expected $dir
      touch $out
    '';

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      ic-ref
      check-generated
    ];
  };

  # include shell in default so that the cache has the extra shell packages
  shell =
    let extra-pkgs = [
      nixpkgs.cabal-install
      nixpkgs.ghcid
    ]; in
    haskellPackages.ic-ref.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
    });
}
