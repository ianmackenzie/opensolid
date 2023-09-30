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
        ghcVer = "ghc946";
        pkgs = import nixpkgs { inherit system; };
        overlay = final: prev: {
          opensolid = final.callCabal2nix "opensolid" ./opensolid { };
          opensolid-sandbox = final.callCabal2nix "sandbox" ./sandbox { };
          opensolid-ffi = pkgs.haskell.lib.overrideCabal
            (final.callCabal2nix "opensolid-ffi" ./opensolid-ffi { })
            (drv: {
              # unfortunatelly callCabal2nix does not extract `build-depends`
              # from `foreign-library`, so they have to be specified here:
              libraryHaskellDepends = [ final.base final.opensolid ];
              # cabal puts the `foreign-library` in `/lib/ghc-9.4.6`,
              # this script makes the library available in `/lib`
              postInstall = (drv.postInstall or "") + ''
                ln -s $out/lib/ghc-*/* $out/lib
              '';
            });
        };
        myHaskellPackages = pkgs.haskell.packages.${ghcVer}.extend overlay;
        libraryPath = pkgs.lib.makeLibraryPath [ myHaskellPackages.opensolid-ffi ];
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

        devShells = rec {
          haskell = myHaskellPackages.shellFor {
            packages = p: [
              p.opensolid
              p.opensolid-sandbox
              p.opensolid-ffi
            ];
            buildInputs = with myHaskellPackages; [
              haskell-language-server
              cabal-install
            ];
          };
          python = pkgs.mkShellNoCC {
            nativeBuildInputs = with pkgs; [ python3 ];
            LD_LIBRARY_PATH = "${libraryPath}";
            shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
              export DYLD_LIBRARY_PATH="${libraryPath}";
            '';
          };
          default = haskell;
        };
      }
    );
}
