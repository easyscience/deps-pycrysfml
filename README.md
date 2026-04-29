# CrysFML for Python

`crysfml` provides Python access to the CrysFML2008 crystallographic Fortran 
library.

This first public beta release is mainly intended to establish packaging,
binary wheel distribution, and early use from Python.  Expect API and packaging 
details to continue changing between beta releases.

## Install

```bash
pip install crysfml
```

Supported targets for this beta release:

- Python 3.11 to 3.14
- macOS, Ubuntu, and Windows
- Binary wheels built from the bundled CrysFML2008 source in this repository

## Quick Start

```python
import crysfml

print(crysfml.__version__)
```

## About CrysFML

CrysFML2008 is a crystallographic Fortran 2008 library. This package makes
selected CrysFML functionality available from Python and provides pre-built
binary wheels for common platforms.

## Links

- Upstream CrysFML2008:
  [code.ill.fr/scientific-software/CrysFML2008](https://code.ill.fr/scientific-software/CrysFML2008)

## License

See the [LICENSE](https://github.com/easyscience/deps-pycrysfml/blob/master/LICENSE) for the
terms that apply to this distribution.
