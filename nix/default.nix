{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.16/nix/sources.nix;
    sha256 = "03fl8wfm2nhdiws7pmfz2kcbf47mv2f8gk30fzg4m07gb5zdv6gv";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  pkgs =
    import nixpkgs_src {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };


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

	  all-cabal-hashes = self.fetchurl {
	    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c0d6a08431fe1b73c0731a3a0480850a06b7e0a.tar.gz";
	    sha256 = "1v1hiaihgj976383xnjymp21gz52b0j4d9cmms7g3vnbi5mzw269";
	  };
        })
      ];
    };
in
pkgs
