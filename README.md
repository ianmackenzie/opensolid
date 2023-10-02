# opensolid

## Developing with nix flakes

To activate flakes, include `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf`.

Then, run `nix develop` or `nix-shell`. This will give you development environment with `ghc`, `cabal-install` and `haskell-language-server`. If you use vscode, you should be able to run `code .` from this shell.

## Developing python bindings

1. run `cabal build opensolid-ffi:flib:opensolid-ffi` to compile the foreign library
2. then you can run `python opensolid-ffi/test.py` to test the bindings

`nix-shell` automatically sets `LD_LIBRARY_PATH`, so that python knows where to find the 
library during the local development.
