# opensolid

## Developing with nix flakes

To activate flakes, include `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf`.

Then, run `nix develop` or `nix-shell`. This will give you development environment with `ghc`, `cabal-install` and `haskell-language-server`. If you use vscode, you should be able to run `code .` from this shell.

## Developing python bindings

`nix develop .#python` will compile the `opensolid-ffi` module and start a nix shell with python installed.

From there you can run `python opensolid-ffi/test.py`.
