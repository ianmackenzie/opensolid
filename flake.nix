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
        # Set up a development shell with all necessary tools installed
        devShell = pkgs.mkShell {
          # Development tools
          buildInputs = [
            # The Haskell compiler itself
            pkgs.ghc 
            # The Stack build tool
            # (configured in stack.yaml to use whatever GHC is on the PATH,
            # so we use the GHC we just installed above
            # instead of having Stack install it)
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
          # Convenient tweaks to the development shell
          shellHook = builtins.concatStringsSep "\n" [
            # Add the 'scripts' directory to PATH for convenience,
            # to allow e.g. format-cabal-files to be run directly
            "export PATH=$PATH:./scripts"

            # Set LD_LIBRARY_PATH to the build directory containing libopensolid-ffi.so,
            # so that it can be found by Python when loading the 'opensolid' module
            "export LD_LIBRARY_PATH=opensolid-ffi/$(stack path --dist-dir)/build/opensolid-ffi"
          ];
        };
      }
    );
}
