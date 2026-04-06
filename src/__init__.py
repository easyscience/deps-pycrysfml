import importlib.metadata
import os
import sys


# Set package version
__version__ = importlib.metadata.version("pycrysfml")

# Add current path to system path
_pkg_dir = os.path.dirname(__file__)
sys.path.append(_pkg_dir)

# On Windows, register the package directory so that bundled DLLs
# (e.g. libgfortran-5.dll, libgcc_s_seh-1.dll, …) can be found when
# loading the compiled Fortran extension (crysfml08lib.pyd).
# os.add_dll_directory() is available since Python 3.8.
if sys.platform == 'win32' and hasattr(os, 'add_dll_directory'):
    os.add_dll_directory(_pkg_dir)

# Set environment variable CRYSFML_DB to be the path to the Databases directory
os.environ['CRYSFML_DB'] = os.path.join(_pkg_dir, 'Databases')
