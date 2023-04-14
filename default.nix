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
      mv $out/bin/universal-canister.wasm $out/universal-canister.wasm
      rmdir $out/bin
    '';
}); in

let naersk_1_66 = nixpkgs.pkgsMusl.callPackage nixpkgs.sources.naersk {
    inherit (nixpkgs.pkgsMusl.rustPackages_1_66) cargo rustc;
}; in

let runtime = (naersk_1_66.buildPackage rec {
    name = "runtime";
    root = nixpkgs.subpath ./.;
    copyLibs = true;
    copyBins = false;
    doCheck = false;
    release = true;
    nativeBuildInputs = with nixpkgs.pkgsMusl; [ pkg-config protobuf ];
    buildInputs = with nixpkgs.pkgsMusl; [ openssl ];
}); in

let haskellOverrides = self: super:
    let generated = import nix/generated/all.nix self super; in
    generated //
    {
      haskoin-core = nixpkgs.haskell.lib.dontCheck (nixpkgs.haskell.lib.markUnbroken super.haskoin-core);
      ic-hs = nixpkgs.haskell.lib.addExtraLibrary generated.ic-hs runtime;

      #ic-hs = nixpkgs.haskell.lib.addExtraLibrary generated.ic-hs runtime;
    }; in

let haskellPackages = nixpkgs.haskellPackages.override {
  overrides = haskellOverrides;
}; in

let staticHaskellPackages = nixpkgs.pkgsStatic.haskellPackages.override {
  overrides = haskellOverrides;
}; in

let
  ic-hs = nixpkgs.haskell.lib.dontCheck (
    haskellPackages.ic-hs.overrideAttrs (old: {
      installPhase = (old.installPhase or "") + ''
        mkdir $out/test-data
        cp ${universal-canister}/universal-canister.wasm $out/test-data
      '';
      # variant of justStaticExecutables that retains propagatedBuildInputs
      preInstall = ''
        cp ${runtime}/lib/libruntime.a dist/build/libruntime.a;
        cp ${runtime}/lib/libruntime.a dist/build/libruntime-ghc9.0.2.so;
      '';
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
        mkdir -p $out/build/libs
        cp ${ic-ref}/bin/ic-ref $out/build
        cp ${ic-ref}/bin/ic-ref-test $out/build
        mkdir -p $out/test-data
        cp ${ic-ref}/test-data/universal-canister.wasm $out/test-data/universal-canister.wasm
        chmod u+w $out/build/ic-ref
        chmod u+w $out/build/ic-ref-test
        dylibbundler \
          -b \
          -x $out/build/ic-ref \
          -x $out/build/ic-ref-test \
          -d $out/build/libs \
          -p '@executable_path/libs' \
          -i /usr/lib/system \
          -i ${nixpkgs.libiconv}/lib \
          -i ${nixpkgs.darwin.Libsystem}/lib

        # there are still plenty of nix store references
        # but they should not matter
        remove-references-to \
          -t ${nixpkgs.darwin.Libsystem} \
          -t ${nixpkgs.darwin.CF} \
          -t ${nixpkgs.libiconv} \
          -t ${staticHaskellPackages.tasty-html.data} \
          $out/build/*

        # sanity check
        $out/build/ic-ref --version
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
        nativeBuildInputs = [ nixpkgs.removeReferencesTo ];
      } ''
        mkdir -p $out/build
        cp ${ic-hs-static}/bin/ic-ref $out/build
        cp ${ic-hs-static}/bin/ic-ref-test $out/build
        mkdir -p $out/test-data
        cp ${ic-hs}/test-data/universal-canister.wasm $out/test-data/universal-canister.wasm

        # The Paths_warp module in warp contains references to warp's /nix/store path like:
        #
        #   warp_bindir="/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/bin"
        #   warp_libdir="/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/lib/ghc-8.10.7/x86_64-linux-ghc-8.10.7/warp-3.3.17-LFuiV3JNZfpKQMWWUSmbjd"
        #   warp_dynlibdir="/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/lib/ghc-8.10.7/x86_64-linux-ghc-8.10.7"
        #   warp_datadir"/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/share/x86_64-linux-ghc-8.10.7/warp-3.3.17"
        #   warp_libexecdir"/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/libexec/x86_64-linux-ghc-8.10.7/warp-3.3.17"
        #   warp_sysconfdir"/nix/store/...-warp-static-x86_64-unknown-linux-musl-3.3.17/etc"
        #
        # These paths end up in the statically compiled $out/build/ic-ref which
        # will fail the `allowedReferences = []` check.
        #
        # Fortunatley warp doesn't use these `warp_*` paths:
        #
        #   /tmp/warp-3.3.19 $ grep -r -w Paths_warp
        #   warp.cabal:                     Paths_warp
        #   warp.cabal:                     Paths_warp
        #   Network/Wai/Handler/Warp/Response.hs:import qualified Paths_warp
        #   Network/Wai/Handler/Warp/Response.hs:warpVersion = showVersion Paths_warp.version
        #   Network/Wai/Handler/Warp/Settings.hs:import qualified Paths_warp
        #   Network/Wai/Handler/Warp/Settings.hs:    , settingsServerName = C8.pack $ "Warp/" ++ showVersion Paths_warp.version
        #
        # So we can safely remove the references to warp:
        remove-references-to -t ${staticHaskellPackages.warp} $out/build/ic-ref
        remove-references-to \
          -t ${staticHaskellPackages.tasty-html} \
          -t ${staticHaskellPackages.tasty-html.data} \
          $out/build/ic-ref-test
      '';


  # We run the unit test suite only as part of coverage checking.
  # It is enough to run it once, and we definitely want it as part of
  # of coverage checking.
  ic-hs-coverage = nixpkgs.haskell.lib.doCheck (nixpkgs.haskell.lib.doCoverage ic-hs);
in

  let httpbin = (naersk.buildPackage rec {
    name = "httpbin-rs";
    root = subpath ./httpbin-rs;
    doCheck = false;
    release = true;
    nativeBuildInputs = with nixpkgs; [ pkg-config ];
    buildInputs = with nixpkgs; [ openssl ];
}).overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      mv $out/bin/httpbin-rs $out/httpbin-rs
      rmdir $out/bin
    '';
}); in

rec {
  inherit runtime;

  inherit ic-hs;
  inherit ic-ref;
  inherit ic-ref-dist;
  inherit ic-hs-coverage;
  inherit universal-canister;

  openssl = nixpkgs.openssl;

  ic-ref-test = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ ic-hs ];
    } ''
      function kill_jobs () {
        pids="$(jobs -p)"
        kill $pids
      }
      ${openssl}/bin/openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -nodes -subj '/C=CH/ST=Zurich/L=Zurich/O=DFINITY/CN=127.0.0.1'
      ${httpbin}/httpbin-rs --port 8003 --cert-file cert.pem --key-file key.pem &
      sleep 1
      ic-ref --pick-port --write-port-to port --cert-path "cert.pem" &
      trap kill_jobs EXIT PIPE
      sleep 1
      test -e port
      mkdir -p $out
      sleep 1
      LANG=C.UTF8 ic-ref-test --test-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" --peer-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" --endpoint "http://127.0.0.1:$(cat port)/" --httpbin "127.0.0.1:8003" --html $out/report-1.html
      LANG=C.UTF8 ic-ref-test --test-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" --peer-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" --endpoint "http://127.0.0.1:$(cat port)/" --httpbin "127.0.0.1:8003" --html $out/report-2.html
      pids="$(jobs -p)"
      kill -INT $pids
      trap - EXIT PIPE
      mkdir -p $out/nix-support
      echo "report test-results $out report-1.html" >> $out/nix-support/hydra-build-products
      echo "report test-results $out report-2.html" >> $out/nix-support/hydra-build-products
    '';

  coverage = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ haskellPackages.ghc ic-hs-coverage ];
      # Prevent rebuilds whenever non-Haskell related files (like .nix) change.
      srcdir = nixpkgs.lib.sourceByRegex (nixpkgs.subpath ./.)
        [ "^src.*" "^bin.*" "^tests.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did" ];
    } ''
      function kill_jobs () {
        pids="$(jobs -p)"
        kill $pids
      }
      ${openssl}/bin/openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -nodes -subj '/C=CH/ST=Zurich/L=Zurich/O=DFINITY/CN=127.0.0.1'
      ${httpbin}/httpbin-rs --port 8003 --cert-file cert.pem --key-file key.pem &
      sleep 1
      ic-ref --pick-port --write-port-to port --cert-path "cert.pem" &
      trap kill_jobs EXIT PIPE
      sleep 1
      test -e port
      sleep 1
      LANG=C.UTF8 ic-ref-test \
        --test-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" \
        --peer-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" \
        --endpoint "http://127.0.0.1:$(cat port)/" \
        --httpbin "127.0.0.1:8003"
      LANG=C.UTF8 ic-ref-test \
        --test-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" \
        --peer-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" \
        --endpoint "http://127.0.0.1:$(cat port)/" \
        --httpbin "127.0.0.1:8003"
      pids="$(jobs -p)"
      kill -INT $pids
      trap - EXIT PIPE
      sleep 5 # wait for ic-ref.tix to be written

      find
      LANG=C.UTF8 hpc markup \
        --srcdir=$srcdir \
        --destdir $out \
        --hpcdir=${ic-hs-coverage}/share/hpc/vanilla/mix/ic-hs-0.0.1 \
        --hpcdir=${ic-hs-coverage}/share/hpc/vanilla/mix/ic-ref \
        ic-ref.tix

      mkdir -p $out/nix-support
      echo "report coverage $out hpc_index.html" >> $out/nix-support/hydra-build-products
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
    haskellPackages.shellFor {
      packages = p: [ p.ic-hs ];
      buildInputs = [
        nixpkgs.cabal-install
        nixpkgs.ghcid
        nixpkgs.cachix
        haskellPackages.haskell-language-server
      ];
    };
}
