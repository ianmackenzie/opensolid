# OpenSolid documentation generation

For now, until properly automated in CI,
make sure the OpenSolid version in `pyproject.toml` is up to date and then run e.g.

```bash
uv sync
LC_ALL="C.UTF-8" uv run sphinx-build -M html src/ build/
cp -r build/html ~/github/opensolid/opensolid.github.io/docs/0.7.0
```

(with the correct version number in the path) and then push the newly created subdir to GitHub.
