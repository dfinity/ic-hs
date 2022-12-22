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

let haskellOverrides = self: super:
    let generated = import nix/generated/all.nix self super; in
    generated //
    {
      haskoin-core = nixpkgs.haskell.lib.dontCheck (nixpkgs.haskell.lib.markUnbroken super.haskoin-core);
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

let
  httpbin =
    let
      my-python-package = ps: ps.callPackage ./httpbin.nix {};
      python-with-my-packages = nixpkgs.python3.withPackages(ps: with ps; [
        (my-python-package ps) ps.gunicorn ps.gevent
      ]);
      openssl = nixpkgs.openssl;
    in "${openssl}/bin/openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -nodes -subj '/C=CH/ST=Zurich/L=Zurich/O=DFINITY/CN=127.0.0.1';
        echo \"import gunicorn.http.wsgi
from six import wraps

def wrap_default_headers(func):
    @wraps(func)
    def default_headers(*args, **kwargs):
        return [header for header in func(*args, **kwargs) if not header.startswith('Server: ') and not header.startswith('Date: ')]
    return default_headers

gunicorn.http.wsgi.Response.default_headers = wrap_default_headers(gunicorn.http.wsgi.Response.default_headers)\" > conf.py;
        ${python-with-my-packages}/bin/gunicorn -b 127.0.0.1:8003 --limit-request-line 0 --limit-request-field_size 0 --certfile cert.pem --keyfile key.pem httpbin:app -k gevent -c conf.py";
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
      function kill_jobs () {
        pids="$(jobs -p)"
        kill $pids
      }
      ${httpbin} &
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
        [ "^src.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did" ];
    } ''
      function kill_jobs () {
        pids="$(jobs -p)"
        kill $pids
      }
      ${httpbin} &
      sleep 1
      ic-ref --pick-port --write-port-to port --cert-path "cert.pem" &
      trap kill_jobs EXIT PIPE
      sleep 1
      test -e port
      sleep 1
      LANG=C.UTF8 ic-ref-test --test-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" --peer-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" --endpoint "http://127.0.0.1:$(cat port)/" --httpbin "127.0.0.1:8003"
      LANG=C.UTF8 ic-ref-test --test-subnet-config "(\"jdzfx-2szde-tnkmk-2m5zt-t6gga-pnl22-v36hx-hz5zg-r6mei-tw3q4-nae\",application,1,[(1048576,2097151)])" --peer-subnet-config "(\"bn26o-3iapb-njhsq-6mjum-ssjtx-lcwrs-id2x6-2z7ce-yaweh-xamz5-7qe\",system,1,[(0,1048575)])" --endpoint "http://127.0.0.1:$(cat port)/" --httpbin "127.0.0.1:8003"
      pids="$(jobs -p)"
      kill -INT $pids
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
    haskellPackages.shellFor {
      packages = p: [ p.ic-hs ];
      buildInputs = [
        nixpkgs.cabal-install
        nixpkgs.ghcid
        haskellPackages.haskell-language-server
      ];
    };
}
