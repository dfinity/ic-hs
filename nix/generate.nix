# This file generates the contents of nix/generated/. Use
#
#  nix-shell generate.nix
#
# to update

{ pkgs ? import ../nix {} }:

let

  # `haskellSrc2nixWithDoc` is used to generate `default.nix` files for
  # Haskell packages which are intended to be stored in the repository.
  #
  # The function generates a directory containing a `default.nix` which
  # is the result of running `cabal2nix` with the `extraCabal2nixOptions`
  # on the provided `src`.
  #
  # A header is added to `default.nix` which contains instructions on
  # how to regenerate that file.
  #
  # Finally the `src` attribute in the `default.nix` will be defined as
  # `src_subst` such that it can be pointed to local or niv-managed
  # sources.
  haskellSrc2nixWithDoc = {name, src, src_subst, extraCabal2nixOptions ? ""}:
    let
      drv = pkgs.haskellPackages.haskellSrc2nix {
        inherit name extraCabal2nixOptions src;
      };
    in drv.overrideAttrs (oldAttrs: {
      message = ''
        # THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!\
        # See ./nix/generate.nix for instructions.\

      '';
      src_subst = pkgs.lib.replaceStrings ["\n"] [" "] src_subst;
      installPhase = oldAttrs.installPhase + ''
        sed -i "1i$message;s|src = .*|src = $src_subst;|" $out/default.nix
        # Accept `pkgs` as an argument in case the `src_subst` depends on it.
        sed -i "s|{ mkDerivation|{ mkDerivation, pkgs|" $out/default.nix
      '';
    });

  packages = {
    ic-hs = haskellSrc2nixWithDoc {
      name = "ic-hs";
      src = pkgs.subpath "/";
      # since the haskell code now lives on the top-level,
      # exclude some more files to avoid rebuilds
      src_subst = ''
        pkgs.lib.sourceByRegex (pkgs.subpath "/")
          ["^src.*" "^bin.*" "^test.*" "^ic-hs.cabal" "^cbits.*" "^LICENSE" "^ic.did"]
      '';
      extraCabal2nixOptions =  "--no-check -frelease";
    };

    winter = haskellSrc2nixWithDoc {
      name = "winter";
      src = pkgs.sources.winter;
      src_subst = "pkgs.sources.winter";
      extraCabal2nixOptions = "--no-check";
    };
    leb128-cereal = haskellSrc2nixWithDoc {
      name = "leb128-cereal";
      src = pkgs.sources.leb128-cereal;
      src_subst = "pkgs.sources.leb128-cereal";
    };
    candid = haskellSrc2nixWithDoc {
      name = "candid";
      src = pkgs.sources.haskell-candid;
      src_subst = "pkgs.sources.haskell-candid";
    };

    # To pull other versions from hackage:

    # 0.2.5.0 broke with ghc-8.10 and integer-simple,
    # see https://github.com/well-typed/cborg/issues/267
    cborg = pkgs.haskellPackages.hackage2nix "cborg" "0.2.4.0";
    http-client = pkgs.haskellPackages.hackage2nix "http-client" "0.7.11";
  };

  allGenerated = pkgs.runCommandNoCC "generated" {
    buildInputs = [ pkgs.nixpkgs-fmt ];
  } (
    ''
    mkdir -p $out
    echo 'self: super: {' >> $out/all.nix
    '' + builtins.concatStringsSep "" (
      pkgs.lib.flip pkgs.lib.mapAttrsToList packages (
        n: pkg: ''
          cp ${pkg}/default.nix $out/${n}.nix
          echo '  ${n} = super.callPackage ./${n}.nix { };' >> $out/all.nix
        ''
      )
    ) + ''
      echo '}' >> $out/all.nix
      chmod u+w $out/*.nix
      nixpkgs-fmt $out/*.nix
      cat <<__END__ > $out/README.md
      The contents of this directory are automatically generated.
      To update, please run nix-shell generate.nix
      __END__
    ''
  );
in
allGenerated.overrideAttrs (
  old: {
    shellHook = if pkgs.lib.inNixShell then
      ''
        dest=${toString ./generated}

        rm -f $dest/*.nix $dest/README.md
        cp -v -t $dest/ ${allGenerated}/*
        chmod u-w -R $dest/*

        exit 0
      '' else null;
  }
)
