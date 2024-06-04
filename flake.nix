{
  inputs = {
    # Note that this should be consistent with the Stack resolver defined in stack.yaml
    # ('nixpkgs.url' here and 'resolver' there should use the same GHC version)
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            zlib
            ghc
            cabal-install
            stack
            haskell-language-server
            haskellPackages.fourmolu # For formatting Haskell files
            haskellPackages.cabal-gild # For formatting Cabal files
            haskellPackages.implicit-hie # For generating hie.yaml
            nixpkgs-fmt # For formatting Nix files
            python312
            ruff # For formatting/linting Python files
          ];
        };
      });
}
