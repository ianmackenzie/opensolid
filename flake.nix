{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
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
          opensolid-test-server = final.callCabal2nix "test-server" ./test-server { };
          opensolid-api = final.callCabal2nix "opensolid-api" ./opensolid-api { };
          opensolid-python = final.callCabal2nix "opensolid-python" ./opensolid-python { };
          opensolid-ffi = pkgs.haskell.lib.overrideCabal
            (final.callCabal2nix "opensolid-ffi" ./opensolid-ffi { })
            (drv: {
              # unfortunatelly callCabal2nix does not extract `build-depends`
              # from `foreign-library`, so they have to be specified here:
              libraryHaskellDepends = [
                final.base
                final.opensolid
                final.opensolid-api
                final.template-haskell
              ];
              # cabal puts the `foreign-library` in `/lib/ghc-9.4.6`,
              # this script makes the library available in `/lib`
              postInstall = (drv.postInstall or "") + ''
                ln -s $out/lib/ghc-*/libopensolid-ffi.* $out/lib
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
          test-server = {
            type = "app";
            program = "${myHaskellPackages.opensolid-test-server}/bin/test-server";
          };
          default = sandbox;
        };

        packages = rec {
          opensolid = myHaskellPackages.opensolid;
          opensolid-ffi = myHaskellPackages.opensolid-ffi;
          sandbox = myHaskellPackages.opensolid-sandbox;
          test-server = myHaskellPackages.opensolid-test-server;
          default = opensolid;
        };

        devShells = {
          default = myHaskellPackages.shellFor {
            packages = p: [
              p.opensolid
              p.opensolid-sandbox
              p.opensolid-test-server
              p.opensolid-api
              p.opensolid-ffi
              p.opensolid-python
            ];
            buildInputs = with myHaskellPackages; [
              haskell-language-server
              cabal-install
              pkgs.nil
              pkgs.nixpkgs-fmt
            ];
            nativeBuildInputs = with pkgs; [
              ruff
              nodePackages.pyright
              python312
            ];
            # Help python find the opensolid-ffi library
            LD_LIBRARY_PATH = "${localLibraryPath}";
            shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
              export DYLD_LIBRARY_PATH="${localLibraryPath}";
              alias run="echo -e :main | cabal repl --verbose=0 $1"
            '';
          };
        };
      }
    );
}
