{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.21/nix/sources.nix;
    sha256 = "129xhkih5sjdifcdfgfy36vj0a9qlli3cgxlrpqq8qfz42avn93v";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  # dump nixpkgs patches here
  nixpkgs-patches = [];

  nixpkgs-patched = if nixpkgs-patches == [] then nixpkgs_src else
    let
      bootstrap-pkgs = import nixpkgs_src {
        system = builtins.currentSystem;
      };
    in bootstrap-pkgs.applyPatches {
      name = "nixpkgs-patched";
      src = nixpkgs_src;
      patches = nixpkgs-patches;
    };

  pkgs =
    import nixpkgs-patched {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };

          subpath = import ./gitSource.nix;

          # nixpkgs's rustc does not inclue the wasm32-unknown-unknown target, so
          # lets add it here. With this we can build the universal canister with stock
          # nixpkgs + naersk, in particular no dependency on internal repositories.
          # But rename this so that we do not rebuilt unrelated tools written in rust.
          rustc-wasm = super.rustc.overrideAttrs (old: {
            src = builtins.fetchurl {
                url = "https://static.rust-lang.org/dist/rustc-1.66.0-src.tar.gz";
                sha256 = "sha256:0j2sjdrafx910n2d6y7bv5v1a0hn9jsz0d8bbpknd8l2bbmd6g1v";
            };
            version = "1.66.0";
            configureFlags = self.lib.lists.forEach old.configureFlags (flag:
              if self.lib.strings.hasPrefix "--target=" flag
              then flag + ",wasm32-unknown-unknown"
              else flag) ++ [
                # https://github.com/rust-lang/rust/issues/76526
                "--set=build.docs=false"
              ];
          });

          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/35f4996e28c5ba20a3a633346f21abe2072afeb6.tar.gz";
            sha256 = "sha256-L/PmFUGlBOOd5rAx4NFxv+s2USI9q0YgOsfpdeRDyds=";
          };

          # We override secp256k1 since the version in nixpkgs doesn't provide a
          # .a library needed for a static build of ic-hs.
          secp256k1 = super.callPackage ./secp256k1 {};
        })
      ];
    };
in
pkgs
