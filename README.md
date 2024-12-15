# OpenSolid

OpenSolid is an under-development 2D/3D CAD library aiming to be:

  * **Powerful**: The goal is to eventually support complex 3D operations like booleans,
    rounds/fillets, shells etc. that are common in existing CAD kernels, but also difficult
    operations such as finding the [medial axis][medial-axis] of a 2D shape or, being able to set up
    and solve solve problems like "find all segments of a curve where its [curvature][curvature]
    is greater than a given value."
  * **Multi-language**: OpenSolid is implemented primarily in Haskell (with some bits of Rust and
    [Cranelift][cranelift] for performance), but is designed to have first-class support in a
    variety of other languages via language bindings. Python bindings are
    [currently implemented][python-bindings] (Python seemed like the obvious first choice), but the
    infrastructure is in place to support a variety of other languages as well. C# and perhaps C++
    seem like likely candidates since both are popular in engineering applications, but other
    languages like Java or TypeScript or Ruby should also be doable; please reach out if you're
    interested in seeing support added for a particular language!
  * **Safe**: OpenSolid uses the type-checking functionality in each target language to try to
    prevent common errors like mixing up units; units conversions are handled automatically and
    you'll get a type error if you e.g. attempt to add a `Length` to an `Area`. In addition, where
    many geometry libraries use a single `Vector3D` or similar type for everything, OpenSolid has
    separate types for e.g. `Point3d`, `Direction3d` and `Displacement3d` that have their own rules
    (e.g. you can add a `Displacement3d` to a `Point3d` to get a new `Point3d`, or you can subtract
    two `Point3d` values from each other to get a `Displacement3d`, but you can't add two `Point3d`
    values since that doesn't make sense).
  * **Enjoyable to use**: This is of course _very_ subjective, but a lot of effort has gone into
    making OpenSolid as pleasant to use as possible, via careful API design and (reasonably)
    comprehensive documentation.

It's not really ready to be used yet, but the current rough roadmap is:

  - [ ] Set up automated publishing of Python bindings pre-built for Linux, macOS and Windows
  - [ ] Fill out Python bindings with existing 2D geometry functionality already implemented in
        Haskell, so it's possible to at least make interesting 2D drawings
  - [ ] Add some additional 2D geometry functionality related to 2D CNC toolpath planning, so that
        it's possible to use OpenSolid for cutting out simple objects on a CNC mill/router
  - [ ] Add enough initial 3D modelling functionality to be able to generate simple models for 3D
        printing or visualization
  - [ ] Add support for more complex 3D operations like tricky Boolean operations, rounds/fillets,
        shelling etc.
  - [ ] Add support exporting to (and hopefully importing from) STEP files
  - [ ] Add ability to generate input files for finite element analysis or ray-traced rendering
        software

The first 3-4 items above should be pretty doable in the short term (next few months); the others
will take some more thought and design work so may not be ready for a while longer.

[medial-axis]: https://www.sciencedirect.com/topics/mathematics/medial-axis
[curvature]: https://math.libretexts.org/Bookshelves/Calculus/CLP-4_Vector_Calculus_(Feldman_Rechnitzer_and_Yeager)/01%3A_Curves/1.03%3A_Curvature
[cranelift]: https://cranelift.dev/
[python-bindings]: https://pypi.org/project/opensolid/

## Development

This project uses a [Nix flake][nix-flake] as the primary way to set up a build environment. (If
you'd prefer to use something else like plain [GHCup][ghcup], that should also work - but consult
`flake.nix` as a reference, it's thoroughly commented!)

For working on the Python bindings, you'll also want to separately install [`uv`][uv] (for a variety
of reasons I chose _not_ to install `uv` with Nix, but it's a very good general tool to have on your
system for working with anything Python-related!).

Running `nix develop` in a checkout of this repository will give you a development environment with
`ghc`, `cabal` and `haskell-language-server` installed (plus a variety of other useful tools such as
`fourmolu` for source code formatting). You can then run

  * `cabal build opensolid` to build the main Haskell library
  * `cabal test opensolid` to run the main library tests
  * `cabal run sandbox` to run the 'sandbox' executable at `opensolid-sandbox/src/Main.hs`; this can
    be a good place to play around with writing Haskell code that uses OpenSolid
  * `generate-python-bindings` to generate the necessary files into `opensolid-python/lib/src` for
    you to experiment with the Python bindings. For example, once you've run
    `generate-python-bindings`, you can go into the `opensolid-python/lib` directory and run
    `uv run python` to drop you into a Python REPL where you can `import opensolid` and play around
    with the bindings.

[nix-flake]: https://nixos.wiki/wiki/Flakes
[ghcup]: https://www.haskell.org/ghcup/
[uv]: https://docs.astral.sh/uv/

## Questions? Comments? Want to get involved?

Please [reach out](https://github.com/ianmackenzie/), I'd love to hear from you!
