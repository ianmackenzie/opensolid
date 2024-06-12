{
  # Note that this should be consistent with the Stack resolver defined in stack.yaml
  # (the NixOS version here and the Stack resolver there should use the same GHC version)
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  outputs = { self, nixpkgs }:
    # All supported platforms/architectures
    let supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      # Define a dev shell for each supported platform/architecture
      devShells = nixpkgs.lib.genAttrs supportedSystems (system:
        # Get the Nix packages for the current platform/architecture
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          # Define the configuration for an OpenSolid development shell
          default = pkgs.mkShell {
            # Development tools
            packages = [
              # The Haskell compiler itself
              pkgs.ghc
              # The Stack build tool,
              # configured in stack.yaml to use whatever GHC is on the PATH
              # so that we use the GHC we just installed above
              # instead of having Stack install it
              pkgs.stack
              # Haskell editor/IDE support
              pkgs.haskell-language-server
              # Needed so that GHC can link against it
              pkgs.zlib
              # For formatting Haskell files
              pkgs.haskellPackages.fourmolu
              # For formatting Cabal files
              pkgs.haskellPackages.cabal-gild
              # For (re)generating hie.yaml;
              # only needed if you add/remove/rename .cabal files
              pkgs.haskellPackages.implicit-hie
              # For formatting this file =)
              pkgs.nixpkgs-fmt
              # For testing the Python extension
              pkgs.python312
              # For formatting/linting Python files
              # (used in the opensolid-python executable
              # for formatting the generated code)
              pkgs.ruff
            ];
            # Executed when entering the development shell
            shellHook = builtins.concatStringsSep "\n" [
              # Add the 'scripts' directory to PATH for convenience,
              # to allow e.g. format-cabal-files to be run directly
              "export PATH=$PATH:./scripts"
              # Set LD_LIBRARY_PATH to the build directory containing libopensolid-ffi.so,
              # so that it can be found by Python when loading the 'opensolid' module
              "export LD_LIBRARY_PATH=opensolid-ffi/$(stack path --dist-dir)/build/opensolid-ffi"
            ];
          };
        });
    };
}
