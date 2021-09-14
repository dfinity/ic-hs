{ system ? builtins.currentSystem }:
(import ./default.nix {inherit system;}).ic-hs-shell
