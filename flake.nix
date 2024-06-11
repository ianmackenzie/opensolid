{
  inputs = {
    # Note that this should be consistent with the Stack resolver defined in stack.yaml
    # ('nixpkgs.url' here and 'resolver' there should use the same GHC version)
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  # The standard flake-utils boilerplate
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Actually set up a development shell
        # with all necessary packages installed
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.ghc # The Haskell compiler itself
            pkgs.stack # The Stack build tool
            pkgs.haskell-language-server # Haskell IDE support
            pkgs.zlib # Needed so that GHC can link against it
            pkgs.haskellPackages.fourmolu # For formatting Haskell files
            pkgs.haskellPackages.cabal-gild # For formatting Cabal files
            pkgs.haskellPackages.implicit-hie # For generating hie.yaml
            pkgs.nixpkgs-fmt # For formatting Nix files
            pkgs.python312 # For testing the Python extension
            pkgs.ruff # For formatting/linting Python files
          ];
          # Simple script to format all .cabal files with cabal-gild
          shellHook = ''
            alias format-cabal-files="for path in \$(find -name *.cabal); do cabal-gild --io=\$path; done"
          '';
        };
      });
}
