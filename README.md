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
- macOS, Linux, and Windows
- Binary wheels built from the bundled CrysFML2008 source in this repository
- Source builds from an sdist or local checkout are supported with GNU Fortran (`gfortran`)

## License

See the [LICENSE](https://github.com/easyscience/deps-pycrysfml/blob/master/LICENSE) for the
terms that apply to this distribution.
