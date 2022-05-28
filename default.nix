{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in
let stdenv = nixpkgs.stdenv; in
let subpath = nixpkgs.subpath; in

let naersk = nixpkgs.callPackage nixpkgs.sources.naersk { rustc = nixpkgs.rustc-wasm; }; in
let universal-canister = (naersk.buildPackage rec {
    name = "universal-canister";
    src = subpath ./universal-canister;
    root = ./universal-canister;
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${nixpkgs.llvmPackages_12.lld}/bin/lld";
    RUSTFLAGS = "-C link-arg=-s"; # much smaller wasm
    cargoBuildOptions = x : x ++ [ "--target wasm32-unknown-unknown" ];
    doCheck = false;
    release = true;
}).overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      mv $out/bin/universal_canister.wasm $out/universal_canister.wasm
      rmdir $out/bin
    '';
}); in


let haskellPackages = nixpkgs.haskellPackages.override {
  overrides = self: super:
    let generated = import nix/generated/all.nix self super; in
    generated //
    {
      # the downgrade of cborg in nix/generated.nix makes cborgs test suite depend on
      # older versions of stuff, so let’s ignore the test suite.
      cborg = nixpkgs.haskell.lib.dontCheck generated.cborg;
      # here more adjustments can be made if needed, e.g.
      # crc = nixpkgs.haskell.lib.markUnbroken (nixpkgs.haskell.lib.dontCheck super.crc);
      murmur3 = nixpkgs.haskell.lib.markUnbroken super.murmur3;
      secp256k1-haskell = nixpkgs.haskell.lib.markUnbroken super.secp256k1-haskell_0_6_0;
      haskoin-core = nixpkgs.haskell.lib.dontCheck super.haskoin-core;
    };
}; in

let staticHaskellPackages = nixpkgs.pkgsStatic.haskell.packages.integer-simple.ghc8107.override {
  # We override GHC such that TemplateHaskell doesn't require shared libraries
  # which are not available in pkgsStatic.
  # See: https://github.com/NixOS/nixpkgs/issues/61575#issuecomment-879403341
  ghc = (nixpkgs.pkgsStatic.buildPackages.haskell.compiler.integer-simple.ghc8107.override {
    enableRelocatedStaticLibs = true;
    enableShared = false;
  }).overrideAttrs (oldAttr: { preConfigure = ''
      ${oldAttr.preConfigure or ""}
      echo "GhcLibHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
      echo "GhcRtsHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
    '';
  });
  overrides = self: super:
    let generated = import nix/generated/all.nix self super; in
    generated //
    {
      # the downgrade of cborg in nix/generated.nix makes cborgs test suite depend on
      # older versions of stuff, so let’s ignore the test suite.
      cborg = nixpkgs.haskell.lib.dontCheck (
        nixpkgs.haskell.lib.appendConfigureFlag generated.cborg "-f-optimize-gmp"
      );

      murmur3 = nixpkgs.haskell.lib.markUnbroken super.murmur3;

      secp256k1-haskell =
        nixpkgs.haskell.lib.addBuildTool
          (nixpkgs.haskell.lib.markUnbroken super.secp256k1-haskell_0_6_0)
          nixpkgs.pkg-config;

      haskoin-core = nixpkgs.haskell.lib.dontCheck super.haskoin-core;

      cryptonite = nixpkgs.haskell.lib.dontCheck (
        nixpkgs.haskell.lib.appendConfigureFlag super.cryptonite "-f-integer-gmp"
      );

      # more test suites too slow withour integer-gmp
      scientific = nixpkgs.haskell.lib.dontCheck super.scientific;
      math-functions = nixpkgs.haskell.lib.dontCheck super.math-functions;

      # We disable haddock to prevent the error:
      #
      #   Haddock coverage:
      #   haddock: panic! (the 'impossible' happened)
      #     (GHC version 8.10.7:
      #           lookupGlobal
      #
      #   Failed to load interface for ‘GHC.Integer.Type’
      #   Perhaps you haven't installed the "dyn" libraries for package ‘integer-simple-0.1.2.0’?
      cmdargs = nixpkgs.haskell.lib.dontHaddock super.cmdargs;
      file-embed = nixpkgs.haskell.lib.dontHaddock super.file-embed;
      QuickCheck = nixpkgs.haskell.lib.dontHaddock super.QuickCheck;
      candid = nixpkgs.haskell.lib.dontHaddock super.candid;
      winter = nixpkgs.haskell.lib.dontHaddock generated.winter;
    };
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
          -i ${nixpkgs.libiconv}/lib \
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
        ic-hs-static =
          nixpkgs.haskell.lib.justStaticExecutables
            (nixpkgs.haskell.lib.failOnAllWarnings
              staticHaskellPackages.ic-hs);
      in nixpkgs.runCommandNoCC "ic-ref-dist" {
        allowedReferences = [];
      } ''
        mkdir -p $out/bin
        cp ${ic-hs-static}/bin/ic-ref $out/bin
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

  haskoin-core = haskellPackages.haskoin-core;

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
      # Prevent rebuilds whenever non-Haskell related files (like .nix) change.
      srcdir = nixpkgs.lib.sourceByRegex (nixpkgs.subpath ./.)
        [ "^src.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did" ];
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
      LANG=C.UTF8 hpc markup ic-ref.tix --hpcdir=${ic-hs-coverage}/share/hpc/vanilla/mix/ic-ref --srcdir=$srcdir  --destdir $out

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
        echo "-- Run nix-shell . -A check-cabal-freeze to update this file" > $out/cabal.project.freeze
        cat cabal.project.freeze |grep -v active-repositories >> $out/cabal.project.freeze
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
