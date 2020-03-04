{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };

          # old version of nixpkgs without this problem:
          # https://github.com/NixOS/nixpkgs/pull/77149#issuecomment-594576339
          pkgs-old = import pkgs.sources.nixpkgs-old { inherit system; };
        })
      ];
    };
in
pkgs
