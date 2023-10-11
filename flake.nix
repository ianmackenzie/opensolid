{
  inputs = {
    # pinned revision from nixpkgs-unstable branch (2023-09-15)
    nixpkgs.url = "github:NixOS/nixpkgs?rev=46688f8eb5cd6f1298d873d4d2b9cf245e09e88e";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVer = "9.4.6";
        pkgs = import nixpkgs { inherit system; };
        overlay = final: prev: {
          opensolid = final.callCabal2nix "opensolid" ./opensolid { };
          opensolid-sandbox = final.callCabal2nix "sandbox" ./sandbox { };
          opensolid-ffi = pkgs.haskell.lib.overrideCabal
            (final.callCabal2nix "opensolid-ffi" ./opensolid-ffi { })
            (drv: {
              # unfortunatelly callCabal2nix does not extract `build-depends`
              # from `foreign-library`, so they have to be specified here:
              libraryHaskellDepends = [
                final.base
                final.opensolid
                final.template-haskell
                final.language-python
              ];
              # cabal puts the `foreign-library` in `/lib/ghc-9.4.6`,
              # this script makes the library available in `/lib`
              # and OpenSolidFFI_stub.h in `/include`
              postInstall = (drv.postInstall or "") + ''
                ln -s $out/lib/ghc-*/libopensolid-ffi.* $out/lib
                ln -s $out/lib/ghc-*/*/*/include $out/include
              '';
            });
        };

        ghcVerShort = builtins.replaceStrings [ "." ] [ "" ] ghcVer; # "9.4.6" -> "946"
        myHaskellPackages = pkgs.haskell.packages."ghc${ghcVerShort}".extend overlay;

        # When developing locally, `cabal build opensolid-ffi:flib:opensolid-ffi` puts the
        # foreign library in a deeply nested path. We need this path for
        # `cdll.LoadLibrary` in python bindings.
        ffiVer = myHaskellPackages.opensolid-ffi.version;
        ghcSystem = builtins.replaceStrings [ "darwin" ] [ "osx" ] system; # fix for osx, x86_64-linux should be ok
        localLibraryPath = "dist-newstyle/build/${ghcSystem}/ghc-${ghcVer}/opensolid-ffi-${ffiVer}/f/opensolid-ffi/build/opensolid-ffi";
      in
      {
        apps = rec {
          sandbox = {
            type = "app";
            program = "${myHaskellPackages.opensolid-sandbox}/bin/sandbox";
          };
          default = sandbox;
        };

        packages = rec {
          opensolid = myHaskellPackages.opensolid;
          opensolid-ffi = myHaskellPackages.opensolid-ffi;
          sandbox = myHaskellPackages.opensolid-sandbox;
          default = opensolid;
        };

        devShells = {
          default = myHaskellPackages.shellFor {
            packages = p: [
              p.opensolid
              p.opensolid-sandbox
              p.opensolid-ffi
            ];
            buildInputs = with myHaskellPackages; [
              haskell-language-server
              cabal-install
              pkgs.nil
              pkgs.nixpkgs-fmt
            ];
            nativeBuildInputs = with pkgs; [ python3 ];

            # Help python find the opensolid-ffi library
            LD_LIBRARY_PATH = "${localLibraryPath}";
            shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
              export DYLD_LIBRARY_PATH="${localLibraryPath}";
            '';
          };
        };
      }
    );
}
