{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        overlay = final: prev: {
          opensolid = final.callCabal2nix "opensolid" ./. { };
        };
        myHaskellPackages = pkgs.haskellPackages.extend overlay;
      in
      rec
      {
        apps = rec {
          sandbox = {
            type = "app";
            program = "${myHaskellPackages.opensolid}/sandbox";
          };
        };

        packages = rec {
          default = myHaskellPackages.opensolid;
        };

        devShells = rec {
          default = myHaskellPackages.shellFor {
            packages = p: [
              p.opensolid
            ];
            buildInputs = with myHaskellPackages; [
              haskell-language-server
              cabal-install
            ];
          };
        };
      }
    );
}
