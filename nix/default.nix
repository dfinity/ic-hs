{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/506b896788d9705899592a303de95d8819504c55/nix/sources.nix;
    sha256 = "007bgq4zy1mjnnkbmaaxvvn4kgpla9wkm0d3lfrz3y1pa3wp9ha1";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };

          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/426bea8219a2e65fb386e14a89ec91bf1a2b42d4.tar.gz";
            sha256 = "1njycwg10abk5142sjynppwwrplgnwqnzlmcza2pfh9rbmgfp4vl";
          };

          # nixpkgs's rustc does not inclue the wasm32-unknown-unknown target, so
          # lets add it here. With this we can build the universal canister with stock
          # nixpkgs + naersk, in particular no dependency on internal repositories.
          rustc = super.rustc.overrideAttrs (old: {
            configureFlags = self.lib.lists.forEach old.configureFlags (flag:
              if self.lib.strings.hasPrefix "--target=" flag
              then flag + ",wasm32-unknown-unknown"
              else flag
            );
          });
        })
      ];
    };
in
pkgs
