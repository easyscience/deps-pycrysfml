# CrysFML for Python

`crysfml` provides Python access to the CrysFML2008 crystallographic Fortran
library.

CrysFML2008 is a crystallographic Fortran 2008 library. This package makes
selected CrysFML functionality available from Python and provides pre-built
binary wheels for common platforms.

Upstream CrysFML2008: [code.ill.fr/scientific-software/CrysFML2008](https://code.ill.fr/scientific-software/CrysFML2008)

These first public beta releases are mainly intended to establish packaging,
binary wheel distribution, and early use from Python. Expect API and packaging
details to continue changing between beta releases.

## Install

```bash
pip install crysfml
```

Supported targets for this beta release:

- Python 3.11 to 3.14
- macOS, Ubuntu, and Windows
- Binary wheels built from the bundled CrysFML2008 source in this repository

## Build From Source

Source builds are supported, but they require a working GNU Fortran
(`gfortran`) toolchain on the target machine.

If a compatible wheel is not available, `pip` can fall back to the published
source distribution:

```bash
pip install --no-binary crysfml crysfml
```

For a local checkout, the repository-root build is the supported path:

```bash
python -m build
```

The project validates the `sdist -> wheel -> installed-wheel tests` path in CI.
For the full local maintainer workflow and validation commands, see
[docs/maintainer-workflows.md](docs/maintainer-workflows.md).

## Maintainer Workflow

The supported local maintainer path is the repo-owned root CMake build,
`scikit-build-core`, and the `pixi` tasks defined in
`pixi.toml`. Maintainer build and test dependencies live in `pixi`, while
`pyproject.toml` stays focused on package metadata and build requirements. The
current maintainer scope is GNU Fortran (`gfortran`) only.

For the supported local build, test, sdist, release, and vendored-CFML
maintenance commands, see [docs/maintainer-workflows.md](docs/maintainer-workflows.md).

## License

See the [LICENSE](https://github.com/easyscience/deps-pycrysfml/blob/master/LICENSE) for the
terms that apply to this distribution.
