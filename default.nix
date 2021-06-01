{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in
let stdenv = nixpkgs.stdenv; in
let subpath = nixpkgs.subpath; in

let naersk = nixpkgs.callPackage nixpkgs.sources.naersk {}; in
let universal-canister = (naersk.buildPackage rec {
    name = "universal-canister";
    src = subpath ./universal-canister;
    root = ./universal-canister;
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${nixpkgs.llvmPackages_9.lld}/bin/lld";
    RUSTFLAGS = "-C link-arg=-s"; # much smaller wasm
    cargoBuildOptions = x : x ++ [ "--target wasm32-unknown-unknown" ];
    doCheck = false;
    release = true;
}).overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      mv $out/bin/universal_canister $out/universal_canister.wasm
      rmdir $out/bin
    '';
}); in


let haskellPackages = nixpkgs.haskellPackages.override {
  overrides = import nix/haskell-packages.nix nixpkgs subpath;
}; in

let
  ic-hs = nixpkgs.haskell.lib.dontCheck (
    haskellPackages.ic-hs.overrideAttrs (old: {
      installPhase = (old.installPhase or "") + ''
        mkdir $out/test-data
        cp ${universal-canister}/universal_canister.wasm $out/test-data
      '';
      # variant of justStaticExecutables that retains propagatedBuildInputs
      postFixup = "rm -rf $out/lib $out/share/doc";
    })
  );

  # Alias, to be replaced with a derivation that just copies bin/ic-ref
  ic-ref = ic-hs;

  # This is a static build of the ic-ref tool only,
  # for distribution independent of nix
  ic-ref-dist =
    if nixpkgs.stdenv.isDarwin
    # on Darwin, use dylibbundler to include non-system libraries
    then nixpkgs.runCommandNoCC "ic-ref-dist" {
        buildInputs = [ nixpkgs.macdylibbundler nixpkgs.removeReferencesTo ];
        allowedRequisites = [];
      } ''
        mkdir -p $out/bin
        cp ${ic-ref}/bin/ic-ref $out/bin
        chmod u+w $out/bin/ic-ref
        dylibbundler \
          -b \
          -x $out/bin/ic-ref \
          -d $out/bin \
          -p '@executable_path' \
          -i /usr/lib/system \
          -i ${nixpkgs.darwin.Libsystem}/lib

        # there are still plenty of nix store references
        # but they should not matter
        remove-references-to \
          -t ${nixpkgs.darwin.Libsystem} \
          -t ${nixpkgs.darwin.CF} \
          -t ${nixpkgs.libiconv} \
          $out/bin/*

        # sanity check
        $out/bin/ic-ref --version
      ''

    # on Linux, build statically using musl
    # and until we are open source, also using integer-simple
    # (once we can use ghc-9.0 we can maybe use ghc-bignum native, which should be faster)
    else
      let
        muslHaskellPackages = nixpkgs.pkgsMusl.haskell.packages.integer-simple.ghc884.override {
          overrides = self: super:
            import nix/haskell-packages.nix nixpkgs subpath self super
            // {
              cryptonite = super.cryptonite.overrideAttrs(old: {
                configureFlags = "-f-integer-gmp";
                doCheck = false; # test suite too slow without integer-gmp
              });
              # more test suites too slow withour integer-gmp
              scientific = nixpkgs.haskell.lib.dontCheck super.scientific;
              math-functions = nixpkgs.haskell.lib.dontCheck super.math-functions;
            };
        };
        ic-hs-musl =
          muslHaskellPackages.ic-hs.overrideAttrs (
            old: {
              configureFlags = [
                "-frelease"
                "-f-library"
                "--ghc-option=-optl=-static"
                "--extra-lib-dirs=${nixpkgs.pkgsMusl.zlib.static}/lib"
                "--extra-lib-dirs=${nixpkgs.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              ];
            }
          );
        in nixpkgs.runCommandNoCC "ic-ref-dist" {
          allowedRequisites = [];
        } ''
          mkdir -p $out/bin
          cp ${ic-hs-musl}/bin/ic-ref $out/bin
        '';


  # We run the unit test suite only as part of coverage checking.
  # It is enough to run it once, and we definitely want it as part of
  # of coverage checking.
  ic-hs-coverage = nixpkgs.haskell.lib.doCheck (nixpkgs.haskell.lib.doCoverage ic-hs);
in



rec {
  inherit ic-hs;
  inherit ic-ref;
  inherit ic-ref-dist;
  inherit ic-hs-coverage;
  inherit universal-canister;

  ic-ref-test = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ ic-hs ];
    } ''
      function kill_ic_ref () { kill %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      mkdir -p $out
      LANG=C.UTF8 ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/" --html $out/report.html

      mkdir -p $out/nix-support
      echo "report test-results $out report.html" >> $out/nix-support/hydra-build-products
    '';

  coverage = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ haskellPackages.ghc ic-hs-coverage ];
    } ''
      function kill_ic_ref () { kill  %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      LANG=C.UTF8 ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/"
      kill -INT %1
      trap - EXIT PIPE
      sleep 5 # wait for ic-ref.tix to be written

      find
      LANG=C.UTF8 hpc markup ic-ref.tix --hpcdir=${ic-hs-coverage}/share/hpc/vanilla/mix/ic-ref --srcdir=${subpath ./.}  --destdir $out

      mkdir -p $out/nix-support
      echo "report coverage $out hpc_index.html" >> $out/nix-support/hydra-build-products
    '';

  # The following two derivations keep the cabal.products.freeze files
  # up to date. It is quite hacky to get the package data base for the ic-hs
  # derivation, and then convince Cabal to use that...
  cabal-freeze = (nixpkgs.haskell.lib.doCheck haskellPackages.ic-hs).overrideAttrs(old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ nixpkgs.cabal-install ];
      phases = [ "unpackPhase" "setupCompilerEnvironmentPhase" "buildPhase" "installPhase" ];
      buildPhase = ''
        rm -f cabal.project.freeze cabal.project
        unset GHC_PACKAGE_PATH
        mkdir .cabal
        touch .cabal/config # empty, no repository
        HOME=$PWD cabal v2-freeze --ghc-pkg-options="-f $packageConfDir" --offline --enable-tests || true
      '';
      outputs = ["out"]; # no docs
      installPhase = ''
        mkdir -p $out
        echo "-- Run nix-shell .. -A check-cabal-freeze to update this file" > $out/cabal.project.freeze
        cat cabal.project.freeze >> $out/cabal.project.freeze
      '';
    });

  check-cabal-freeze = nixpkgs.runCommandNoCC "check-cabal-freeze" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = cabal-freeze + /cabal.project.freeze;
      actual = ./cabal.project.freeze;
      cmd = "nix-shell . -A check-cabal-freeze";
      shellHook = ''
        dest=${toString ./cabal.project.freeze}
        rm -f $dest
        cp -v $expected $dest
        chmod u-w $dest
        exit 0
      '';
    } ''
      diff -r -U 3 $actual $expected ||
        { echo "To update, please run"; echo "nix-shell . -A check-cabal-freeze"; exit 1; }
      touch $out
    '';

  check-generated = nixpkgs.runCommandNoCC "check-generated" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = import ./nix/generate.nix { pkgs = nixpkgs; };
      dir = ./nix/generated;
    } ''
      diff -r -U 3 $expected $dir
      touch $out
    '';

  # A simple license check: Check that all used Haskell packages
  # declare a liberal (non-GPL) license.
  # This does not necessarily cover imported C libraries!
  license-check = haskellPackages.ic-hs.overrideAttrs(old: {
      name = "ic-hs-license-check";
      phases = [ "unpackPhase" "setupCompilerEnvironmentPhase" "buildPhase" "installPhase" ];
      buildPhase = ''
        cd $packageConfDir
        ! grep -i '^license:' *.conf | grep -v 'BSD\|Apache\|MIT\|ISC\|PublicDomain'
      '';
      outputs = ["out"]; # no docs
      installPhase = ''
        touch $out
      '';
    });

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      ic-hs
      ic-ref
      ic-ref-dist
      ic-ref-test
      ic-hs-coverage
      universal-canister
      check-generated
    ];
  };

  # include shell in default.nix so that the nix cache will have pre-built versions
  # of all the dependencies that are only depended on by nix-shell.
  ic-hs-shell =
    let extra-pkgs = [
      nixpkgs.cabal-install
      nixpkgs.ghcid
    ]; in

    haskellPackages.ic-hs.env.overrideAttrs (old: {
      propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ extra-pkgs ;
    });
}
