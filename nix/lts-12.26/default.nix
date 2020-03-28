let name = builtins.readFile ./name; in

self: super:

{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${name}" = self.callPackage ./set.nix {};
    };
  };
}
