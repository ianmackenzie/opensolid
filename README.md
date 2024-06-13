# opensolid

## Setup

This project uses a Nix flake as the primary way to set up a build environment.
(If you'd prefer to use something else like plain GHCup, that should also work -
but consult `flake.nix` as a reference, it's thoroughly commented!)

To activate flakes, include `experimental-features = nix-command flakes` in `~/.config/nix/nix.conf`.
Then, run `nix develop` within a Git checkout of this repository.
This will give you a development environment with `ghc`, `stack` and `haskell-language-server` installed
(plus a variety of other useful tools such as `fourmolu` for source code formatting).

## Development

Run `stack build` to build everything, and `stack test` to run the tests.
There's a random collection of little mini-scripts in `opensolid-sandbox/src/Main.hs`
which you can run with `stack run sandbox` (try adding your own!).

### Python bindings

1. Run `stack build` to build the core library plus Python extension (native library).
2. Run `generate-python-bindings` to generate the Python bindings (Python code).
3. Run `python opensolid-python/test.py` to test the bindings.

`nix develop` automatically sets `LD_LIBRARY_PATH`,
so that Python knows where to find the extension library.
