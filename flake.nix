{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
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
          overlay = final: prev: {
            haskell-language-server = prev.haskell-language-server.override {
              supportedGhcVersions = [ "910" ];
            };
          };
          pkgs = nixpkgs.legacyPackages.${system}.extend overlay;
          ld_library_path = builtins.concatStringsSep ":" [
            # Allow Haskell to find libopensolid_rs
            "$PWD/opensolid-rs/target/release"
            # Allow Python packages to be built against libstdc++.so
            "${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ]}"
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
              pkgs.haskell.compiler.ghc910
              # The Cabal build tool for Haskell
              pkgs.cabal-install
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
              pkgs.nixfmt-classic
              # Language servers for Nix files
              # (somehow Zed wants *both* of these)
              pkgs.nixd
              pkgs.nil
              # The Rust compiler, for libopensolidjit
              pkgs.rustc
              # The Cargo build tool for Rust
              pkgs.cargo
              # For formatting Rust files
              pkgs.rustfmt
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
