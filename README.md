# opensolid

## Developing with nix flakes

To activate flakes, include `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf`.

Then, run `nix develop` or `nix-shell`. This will give you development environment with `ghc`, `cabal-install` and `haskell-language-server`. If you use vscode, you should be able to run `code .` from this shell.

## Developing python bindings

1. run `cabal build opensolid-ffi` to compile the foreign library. If you need to print the generated code, call with `--ghc-options="-ddump-splices"` (you may need to delete `dist-newstyle`).
2. run `cabal run opensolid-python -v0 > opensolid-python/opensolid.py` to generate the python bindings
3. then you can run `python opensolid-python/test.py` to test the bindings
4. run `mypy opensolid-python` to validate the typings

`nix-shell` automatically sets `LD_LIBRARY_PATH`, so that python knows where to find the
library during the local development.
