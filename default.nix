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
let hs-to-coq = hs-to-coq-pkgs.callPackage nix/generated/hs-to-coq.nix {}; in

let ic-ref-coq-files = nixpkgs.runCommandNoCC "ic-ref-coq-files" {
    nativeBuildInputs = [ hs-to-coq-pkgs.ghc hs-to-coq ];
    src = subpath impl/src;
  } ''
    # running hs-to-coq requires loading files into GHC-the-library
    # so we need the dependencies available
    packageConfDir="$TMPDIR/package.conf.d"
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

    mkdir -p $out
    GHC_PACKAGE_PATH=$packageConfDir: LANG=C.UTF8 hs-to-coq -i $src/ IC.Ref --iface-dir $out --iface-dir ${nixpkgs.sources.hs-to-coq}/base -e ${nixpkgs.sources.hs-to-coq}/base/edits -o $out/ --midamble=${./midamble} -e ${./edit} --ghc -DCANISTER_FAKE
    touch $out
  ''; in

let
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
  inherit coq-ic-ref;

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
    [ nixpkgs.coq coq-base coq-containers coq-transformers coq-ic-ref ]
    ; in

    haskellPackages.ic-ref.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
      CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${rust_pkgs.llvmPackages_9.lld}/bin/lld";
    });
}
