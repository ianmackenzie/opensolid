{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  outputs =
    { nixpkgs, ... }:
    # All supported platforms/architectures
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    {
      # Define a dev shell for each supported platform/architecture
      devShells = nixpkgs.lib.genAttrs supportedSystems (
        system:
        # Get the Nix packages for the current platform/architecture
        let
          pkgs = nixpkgs.legacyPackages.${system};
          # Allow Python packages to be built against libstdc++.so
          ld_library_path = pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ];
        in
        {
          # Define the configuration for an OpenSolid development shell
          default = pkgs.mkShellNoCC {
            # Development tools
            packages = [
              # Get Nix to provide a proper Bash shell with builtins like 'complete', see e.g.
              # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
              pkgs.bashInteractive
              # For formatting Haskell files
              pkgs.haskellPackages.fourmolu
              # For formatting .cabal files;
              # only needed if you're editing .cabal files,
              # e.g. when adding/removing/renaming Haskell source files
              pkgs.haskellPackages.cabal-gild
              # For visualizing Haskell profiling (.prof) files
              pkgs.haskellPackages.profiteur
              # For formatting this file =)
              pkgs.nixfmt-classic
              # Language servers for Nix files
              # (somehow Zed wants *both* of these)
              pkgs.nixd
              pkgs.nil
              # For formatting/linting Python files
              # (used in the opensolid-python executable
              # for formatting the generated code)
              pkgs.ruff
              # For type-checking Python files
              # (used in the opensolid-python executable
              # for type-checking the generated code)
              pkgs.pyright
            ];
            # Executed when entering the development shell
            shellHook = builtins.concatStringsSep "\n" [
              # Add the 'scripts' directory to PATH for convenience,
              # to allow e.g. format-cabal-files to be run directly
              "export PATH=$PATH:./scripts"
              # Set LD_LIBRARY_PATH to the build directory containing libopensolid-ffi.so,
              # so that it can be found by Python when loading the 'opensolid' module
              "export LD_LIBRARY_PATH=${ld_library_path}"
              # Set PYTHONPATH so 'import opensolid' works
              # when running Python interactively from the repository root
              "export PYTHONPATH=$PWD/opensolid-python"
              # Allow clangd to find GHC's Rts.h
              "export CPATH=$(find $(ghc --print-libdir) -name Rts.h | sed -r 's|Rts.h||')"
            ];
          };
        }
      );
    };
}
