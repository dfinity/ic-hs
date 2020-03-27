{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  snapshot = "lts-12.26";
  stackage = import (fetchTarball {
    url = "https://stackage.serokell.io/ad0kwmbwynr9hk0g2xl9jc0cxnhjvl2f-stackage/default.nix.tar.gz";
    sha256 = "1imf2h1brpgpl5rfbyr7iqh3xpqflcgdi7p6g0nzx022yyrg0m91";
  });

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };
        })
	stackage."${snapshot}"
      ];
    };
in
pkgs
