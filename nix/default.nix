{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
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
            configureFlags = self.lib.lists.forEach old.configureFlags (flag:
              if self.lib.strings.hasPrefix "--target=" flag
              then flag + ",wasm32-unknown-unknown"
              else flag) ++ [
                # https://github.com/rust-lang/rust/issues/76526
                "--set=build.docs=false"
              ];
          });

          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d4d63b04cd9f6ed263db8df4cdd6dcc667f96ccd.tar.gz";
            sha256 = "0pd9zfy8wwbwqg0qkgi28kfi2q3kwl1lwkq2k7b50wm5pb3nngrm";
          };

          # We override secp256k1 since the version in nixpkgs doesn't provide a
          # .a library needed for a static build of ic-hs.
          #
          # TODO: We can probably remove this override once we upgrade nixpkgs
          # to release-22.05.
          secp256k1 = super.callPackage ./secp256k1 {};
        })
      ];
    };
in
pkgs
