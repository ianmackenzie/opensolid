# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OpenSolid is a 2D/3D CAD geometry library implemented in Haskell (GHC 9.14.1) with Python bindings via FFI. Licensed under MPL-2.0.

## Build Commands

```bash
# Build
cabal build all

# Run tests
cabal test all

# Run tests filtered by name substring (also prints timing for matched tests)
cabal test all --test-options Curve2D

# Run sandbox executable (executables/sandbox/Main.hs)
cabal run sandbox

# Generate Python bindings (generates, formats, lints, type-checks, builds FFI)
generate-python-bindings

# Sanity-check generated Python bindings
cd opensolid-python/lib && uv run check.py

# Format Haskell
fourmolu --mode inplace <file.hs>

# Format cabal files
format-cabal-files
```

Note: `generate-python-bindings` and `format-cabal-files` are scripts in `scripts/` (added to PATH by the Nix dev shell).

## Package Structure

- **opensolid-core** — Main Haskell library (~170+ modules). All core geometry types and algorithms.
- **opensolid-api** — API abstraction layer describing the public API for FFI (Class, Function, Constructor, etc.)
- **opensolid-components** — Higher-level components built on core (e.g. SpurGear)
- **opensolid-ffi** — Native shared library (`foreign-library`) for FFI
- **opensolid-python** — Code generator that reads opensolid-api definitions and emits `__init__.py`
- **opensolid-http** — Simple HTTP server (wai/warp)
- **executables** — Sandbox and demo executables

Dependency flow: `opensolid-core` → `opensolid-components` → `opensolid-api` → `opensolid-ffi` / `opensolid-python`

## Key Architectural Patterns

### Custom Prelude
`NoImplicitPrelude` is enabled everywhere. `OpenSolid.Prelude` replaces the standard Prelude with custom arithmetic type classes (`Addition`, `Subtraction`, `Multiplication`, `Division`, `Negation`, `DotMultiplication`, `CrossMultiplication`, etc.) using functional dependencies for type-safe mixed-type arithmetic (e.g. `Length * Number = Length`).

### Units as Phantom Types
`Quantity units` wraps `Double`. Unit types (`Meters`, `Radians`, `SquareMeters`, etc.) are phantom type tags. `Units.Product` and `Units.Quotient` type families enforce dimensional analysis at compile time. The `?*?` / `?/?` operators are "abstract" variants for intermediate computations.

### Space Phantom Types
Geometric types carry a `space` phantom type parameter (e.g. `Vector2D units space`, `Point2D units space`) to prevent mixing vectors from different coordinate frames.

### Tolerance via Implicit Parameters
`type Tolerance units = ?tolerance :: Quantity units`. Approximate equality `(~=)` requires a `?tolerance` in scope. Set with `Tolerance.using tolerance expression`.

### Named Arguments
`name ::: value` (or `#name value` via `OverloadedLabels`) provides labeled arguments. Example: `new t1 t2 ("degenerate" ::: d)`.

### Performance
Core geometric types use GHC unboxed `Double#` fields. `StrictData` is enabled project-wide. Pattern synonyms provide a boxed view. C++ is used for bytecode evaluation. CMM (`primops.cmm`) implements custom GHC primitive operations.

### Result Monad
`Result x a` is a custom `Either`-like type used throughout for fallible computations, with `Functor`/`Applicative`/`Monad` instances.

### Module-Level API Style
Functions are accessed via qualified imports: `Point2D.origin`, `Curve2D.arcFrom`, `Length.meters 1.5`. Types are exported separately from their operations.

## Testing

Uses a custom test framework (not HSpec/Tasty), defined in `opensolid-core/test/Test.hs`:
- `verify` — runs once; `check n` — runs n times (fuzz/property testing with random seeds)
- `group` — groups tests; `expect`, `pass`, `fail`, `all` — assertion combinators
- `output` — adds labeled debug values to failure messages
- Tests generally run within `Tolerance.using Length.nanometer`

## Compiler Settings

`-Wall -Werror` is enabled for all packages. All GHC2021 defaults plus many extensions enabled project-wide (see cabal files). Key extensions: `BlockArguments`, `DataKinds`, `GADTs`, `ImplicitParams`, `NoImplicitPrelude`, `OverloadedLabels`, `OverloadedStrings`, `PatternSynonyms`, `QualifiedDo`, `RebindableSyntax`, `StrictData`, `TypeFamilies`.

## Formatting

Haskell: `fourmolu` — 2-space indent, trailing function arrows, leading commas, leading import/export style. Config in `fourmolu.yaml`.

Cabal files: `cabal-gild` via `scripts/format-cabal-files`.

Python: `ruff` for formatting and linting, `pyright` for type checking.
