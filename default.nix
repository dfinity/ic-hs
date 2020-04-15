{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in
let stdenv = nixpkgs.stdenv; in
let subpath = p: import ./nix/gitSource.nix p; in

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

      comment_template=$out/nix-support/gh-comment
      cat <<EOF >$comment_template
        {{{! comment:edit-one }}}This PR changes the public spec, you can preview the result at:
        {{{ host }}}/build/{{{ build }}}/download/1/spec/index.html
      EOF
      echo "comment manifest $comment_template comment.mustache" >> $out/nix-support/hydra-build-products

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
    universal-canister.nativeBuildInputs; in

    haskellPackages.ic-ref.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ extra-pkgs ;
      CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${nixpkgs.llvmPackages_9.lld}/bin/lld";
    });
}
