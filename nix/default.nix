{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  bootstrap-pkgs = import nixpkgs_src {
    system = builtins.currentSystem;
  };

  nixpkgs-patched = bootstrap-pkgs.applyPatches {
    name = "nixpkgs-patched";
    src = nixpkgs_src;
    patches = [
    ];
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
          rustc = super.rustc.overrideAttrs (old: {
            configureFlags = self.lib.lists.forEach old.configureFlags (flag:
              if self.lib.strings.hasPrefix "--target=" flag
              then flag + ",wasm32-unknown-unknown"
              else flag) ++ [
                # https://github.com/rust-lang/rust/issues/76526
		"--set=build.docs=false"
	      ];
          });

          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/174d622dcd2324afe16a2f191ce1d319028d3935.tar.gz";
            sha256 = "0arh58mszf3b11d9yxldd63l1f4hr6fz03vjcv5yfwnkwy5yyyda";
          };
        })
      ];
    };
in
pkgs
