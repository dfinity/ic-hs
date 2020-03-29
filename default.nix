{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in
let stdenv = nixpkgs.stdenv; in
let subpath = p: import ./nix/gitSource.nix p; in

# Building the universal_canister is relatively convluted:
#  * We need to use the rust patches from common, as they
#    include a rustc with the wasm32-unknown-unknown target
#  * Not sure if I am using naersk the right way here.
let rust_pkgs = import nixpkgs.sources.common { inherit system; }; in
let universal-canister = (rust_pkgs.naersk.buildPackage rec {
    name = "universal-canister";
    src = subpath ./universal-canister;
    root = ./universal-canister;
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${rust_pkgs.llvmPackages_9.lld}/bin/lld";
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


let hs-to-coq-pkgs = nixpkgs.haskell.packages."lts-12.26"; in
let hs-to-coq-unwrapped = hs-to-coq-pkgs.callPackage nix/generated/hs-to-coq.nix {}; in

# running hs-to-coq requires loading files into GHC-the-library
# so we need the dependencies available. This derivation
# builds the appropriate package data base
let
  ic-ref-ghc-env = nixpkgs.runCommandNoCC "ic-ref-ghc-env" {
    nativeBuildInputs = [ hs-to-coq-pkgs.ghc ];
  } ''
    packageConfDir="$out"
    mkdir -p $packageConfDir
    cp -f "${hs-to-coq-pkgs.memory}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${hs-to-coq-pkgs.basement}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${hs-to-coq-pkgs.cryptonite}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${nixpkgs.haskell.packages.ghc844.primitive}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${nixpkgs.haskell.packages.ghc844.vector}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${nixpkgs.haskell.packages.ghc844.base16-bytestring}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${nixpkgs.haskell.packages.ghc844.hex-text}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    cp -f "${nixpkgs.haskell.lib.markUnbroken (nixpkgs.haskell.lib.dontCheck nixpkgs.haskell.packages.ghc844.crc)}/lib/${hs-to-coq-pkgs.ghc.name}/package.conf.d/"*.conf $packageConfDir/
    ghc-pkg --package-conf="$packageConfDir" recache
    GHC_PACKAGE_PATH=$packageConfDir: ghc-pkg check
  '';

  hs-to-coq = stdenv.mkDerivation {
    name = "hs-to-coq";
    phases = [ "buildPhase" "fixupPhase" ];
    setupHook = nixpkgs.writeText "setupHook.sh" ''
      addHsToCoqPath () {
        if test -d "''$1/lib/hs-to-coq"; then
          export HS_TO_COQ_ARGS="''${HS_TO_COQ_ARGS-}''${HS_TO_COQ_ARGS:+ }--iface-dir=''$1/lib/hs-to-coq/"
        fi
        if test -e "''$1/lib/hs-to-coq/edits"; then
          export HS_TO_COQ_ARGS="''${HS_TO_COQ_ARGS-}''${HS_TO_COQ_ARGS:+ }-e ''$1/lib/hs-to-coq/edits"
        fi
      }
      addEnvHooks "$targetOffset" addHsToCoqPath
    '';
    buildPhase = ''
      mkdir -p $out/bin
      cat > $out/bin/hs-to-coq <<__END__
      #!/usr/bin/env bash
      unset NIX_GHCPKG NIX_GHC_LIBDIR NIX_GHC_DOCDIR NIX_GHC
      export GHC_PACKAGE_PATH=${ic-ref-ghc-env}:
      exec ${hs-to-coq-unwrapped}/bin/hs-to-coq \$HS_TO_COQ_ARGS "\$@"
      __END__
      chmod +x $out/bin/hs-to-coq
    '';
  };

  mkHsToCoqLib = name: src: subpath: stdenv.mkDerivation {
    name = name;
    src = src;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      cd ${subpath}
      mkdir -p $out/lib/hs-to-coq/
      shopt -s globstar
      cp --parents --dereference --target-directory=$out/lib/hs-to-coq **/*.h2ci
      if test -e edits; then cp --dereference edits $out/lib/hs-to-coq; fi
    '';
  };

  hs-to-coq-base = mkHsToCoqLib "base" nixpkgs.sources.hs-to-coq "base";
  hs-to-coq-transformers = mkHsToCoqLib "transformers" nixpkgs.sources.hs-to-coq "examples/transformers/lib";
  hs-to-coq-containers = mkHsToCoqLib "containers" nixpkgs.sources.hs-to-coq "examples/containers/lib";

  ic-ref-coq-files = stdenv.mkDerivation {
    name = "ic-ref-coq-files";
    nativeBuildInputs = [
      hs-to-coq-pkgs.ghc hs-to-coq
      hs-to-coq-base hs-to-coq-transformers hs-to-coq-containers
    ];
    src = subpath ./proofs;
    buildPhase = ''
      mkdir -p $out
      LANG=C.UTF8 SRC=${subpath impl/src} make vfiles
      test -e lib/IC/Ref.v # sanity check
    '';
    installPhase = ''
      cp -r lib $out
    '';
  };

  mkCoqLib = { name, src, subdir, delete ? [], deps ? [] }:
   stdenv.mkDerivation {
    name = "coq${nixpkgs.coq.coq-version}-${name}";
    inherit src;
    preBuild = ''
      cd ${subdir}
      echo '-R . ""' > _CoqProject
      rm -f ${nixpkgs.lib.concatStringsSep " " delete}
      find -name \*.v >> _CoqProject
      coq_makefile -f _CoqProject -o Makefile
    '';
    buildInputs = [ nixpkgs.coq ] ++ deps;
    installFlags = "COQLIB=$(out)/lib/coq/${nixpkgs.coq.coq-version}/";
    meta = { description = "hs-to-coq coq library ${name}"; };
  };

  coq-base = mkCoqLib {
    name = "base";
    src = nixpkgs.sources.hs-to-coq;
    subdir = "base";
    delete = ["Data/Char.v"];
  };
  coq-transformers = mkCoqLib {
    name = "transformers";
    src = nixpkgs.sources.hs-to-coq;
    subdir = "examples/transformers/lib";
    deps = [ coq-base ];
  };
  coq-containers = mkCoqLib {
    name = "containers";
    src = nixpkgs.sources.hs-to-coq;
    subdir = "examples/containers/lib";
    deps = [ coq-base ];
    delete = ["Data/SequenceManual.v"];
  };
  coq-ic-ref = mkCoqLib {
    name = "ic-ref";
    src = ic-ref-coq-files;
    subdir = ".";
    deps = [ coq-base coq-transformers coq-containers ];
  };
in


let ic-ref = haskellPackages.ic-ref.overrideAttrs (old: {
  installPhase = (old.installPhase or "") + ''
    cp -rv test-data $out/test-data
    # replace symlink with actually built
    rm -f $out/test-data/universal_canister.wasm
    cp ${universal-canister}/universal_canister.wasm $out/test-data
  '';
}); in

let ic-ref-coverage = nixpkgs.haskell.lib.doCoverage ic-ref; in


rec {
  inherit ic-ref;
  inherit ic-ref-coverage;
  inherit universal-canister;
  inherit hs-to-coq;
  inherit ic-ref-ghc-env;
  inherit ic-ref-coq-files;
  inherit coq-ic-ref;
  inherit hs-to-coq-base;

  ic-ref-test = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ ic-ref nixpkgs.wabt ];
    } ''
      function kill_ic_ref () { kill %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      mkdir -p $out
      ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/" --html $out/report.html

      mkdir -p $out/nix-support
      echo "report test-results $out report.html" >> $out/nix-support/hydra-build-products
    '';

  coverage = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ haskellPackages.ghc ic-ref-coverage nixpkgs.wabt ];
    } ''
      function kill_ic_ref () { kill  %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/"
      kill -INT %1
      trap - EXIT PIPE
      sleep 5 # wait for ic-ref.tix to be written

      find
      LANG=C.UTF8 hpc markup ic-ref.tix --hpcdir=${ic-ref-coverage}/share/hpc/vanilla/mix/ic-ref --srcdir=${subpath ./impl}  --destdir $out

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

  public-spec =
    nixpkgs.stdenv.mkDerivation {
    name = "public-spec";
    src = subpath ./spec;
    phases = [ "unpackPhase" "buildPhase" "checkPhase" ];
    buildInputs = with nixpkgs;
      [ asciidoctor plantuml jre graphviz python cpio html-proofer ];
    FONTCONFIG_FILE = nixpkgs.makeFontsConf { fontDirectories = []; };
    asciidoctor_args = [
      "-r asciidoctor-diagram"
      "-a plantuml-format=svg"
      "-a ditaa-format=svg"
      "-a graphviz-format=svg"
      "-a source-highlighter=rouge"
      "-a rouge-css=style"
    ];
    buildPhase = ''
      doc_path="spec"
      mkdir -p $out/$doc_path
      asciidoctor $asciidoctor_args --failure-level WARN -v \
        -R $PWD -D $out/$doc_path/ index.adoc
      find . -type f -name '*.png' | cpio -pdm $out/$doc_path/

      mkdir -p $out/nix-support
      echo "report spec $out/$doc_path index.html" >> $out/nix-support/hydra-build-products
    '';

    # These ones are needed for htmlproofer
    LOCALE_ARCHIVE = nixpkgs.lib.optionalString nixpkgs.stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_TYPE = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    doCheck = true;
    checkPhase = ''
      htmlproofer --disable-external $out/$doc_path
      if [[ ! -s $out/$doc_path/index.html ]]; then
        >&2 echo "There is no $out/$doc_path/index.html or it is empty, aborting."
        exit 1
      fi
    '';

  };

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      ic-ref
      ic-ref-test
      universal-canister
      public-spec
      check-generated
    ];
  };

  # include shell in default.nix so that the nix cache will have pre-built versions
  # of all the dependencies that are only dependent on by nix-shell.
  shell =
    let extra-pkgs = [
      nixpkgs.cabal-install
      nixpkgs.ghcid
    ] ++
    # and to build the rust stuff
    universal-canister.nativeBuildInputs ++
    # and a bunch of coq stuff
    [ nixpkgs.coq
      coq-base coq-containers coq-transformers
      hs-to-coq
      hs-to-coq-base hs-to-coq-transformers hs-to-coq-containers
    ]
    ; in

    haskellPackages.ic-ref.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
      CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${rust_pkgs.llvmPackages_9.lld}/bin/lld";
    });
}
