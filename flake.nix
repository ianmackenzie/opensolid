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
          opensolid = final.callCabal2nix "opensolid" ./. { };
        };
        myHaskellPackages = pkgs.haskell.packages.${ghcVer}.extend overlay;
      in
      {
        apps = rec {
          sandbox = {
            type = "app";
            program = "${myHaskellPackages.opensolid}/bin/sandbox";
          };
          default = sandbox;
        };

        packages = rec {
          opensolid = myHaskellPackages.opensolid;
          default = opensolid;
        };

        devShells.default = myHaskellPackages.shellFor {
          packages = p: [
            p.opensolid
          ];
          buildInputs = with myHaskellPackages; [
            haskell-language-server
            cabal-install
          ];
        };
      }
    );
}
