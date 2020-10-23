{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.18/nix/sources.nix;
    sha256 = "0vsjk1dj88kb40inlhb9xgfhm5dfhb6g3vyca62glk056sn4504l";
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

          cbor2 = self.python3.pkgs.callPackage ./python-cbor2.nix {};

          ic-webauthn-cli = self.callPackage ./ic-webauthn-cli.nix {
            inherit (self.python3.pkgs) buildPythonPackage fido2 pynacl;
            inherit (self) cbor2;
          };
        })
      ];
    };
in
pkgs
