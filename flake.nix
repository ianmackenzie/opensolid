{
  # Note that this should be consistent with the Stack resolver defined in stack.yaml
  # (the NixOS version here and the Stack resolver there should use the same GHC version)
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { nixpkgs, ... }:
    # All supported platforms/architectures
    let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      # Define a dev shell for each supported platform/architecture
      devShells = nixpkgs.lib.genAttrs supportedSystems (system:
        # Get the Nix packages for the current platform/architecture
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ld_library_path = builtins.concatStringsSep ":" [
            # Allow Haskell to find libopensolid_jit.a
            "$PWD/opensolid-jit/target/release"
            # Allow Python to find libopensolid-ffi.so
            "$PWD/opensolid-ffi/$(stack path --dist-dir)/build/opensolid-ffi"
          ];
        in {
          # Define the configuration for an OpenSolid development shell
          default = pkgs.mkShell {
            # Development tools
            packages = [
              # Get Nix to provide a proper Bash shell with builtins like 'complete', see e.g.
              # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
              pkgs.bashInteractive
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
              # Haskell static analysis tool,
              # provides various warnings/suggestions
              pkgs.haskellPackages.stan
              # For formatting Haskell files
              pkgs.haskellPackages.fourmolu
              # For formatting .cabal files;
              # only needed if you're editing .cabal files,
              # e.g. when adding/removing/renaming Haskell source files
              pkgs.haskellPackages.cabal-gild
              # For (re)generating hie.yaml;
              # only needed if you add/remove/rename .cabal files,
              # e.g. when adding a whole new sub-project
              pkgs.haskellPackages.implicit-hie
              # For formatting this file =)
              pkgs.nixfmt
              # Language server for Nix files
              pkgs.nixd
              # The Rust compiler, for libopensolidjit
              pkgs.rustc
              # The Cargo build tool for Rust
              pkgs.cargo
              # For formatting Rust files
              pkgs.rustfmt
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
              "export LD_LIBRARY_PATH=${ld_library_path}"
              # Set PYTHONPATH so 'import opensolid' works
              # when running Python interactively from the repository root
              "export PYTHONPATH=$PWD/opensolid-python"
              # Ensure rust-analyzer can find Rust core library source code
              "export RUST_SRC_PATH=${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}"
            ];
          };
        });
    };
}
