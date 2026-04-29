import importlib.metadata
import os
import sys


# Set package version
__version__ = importlib.metadata.version("crysfml")

_pkg_dir = os.path.dirname(__file__)
_db_dir = os.path.join(_pkg_dir, 'Databases')
_dll_directory_handle = None


def _prepend_package_dir_to_path():
    path_value = os.environ.get('PATH', '')
    normalized_pkg_dir = os.path.normcase(os.path.normpath(_pkg_dir))
    path_entries = [entry for entry in path_value.split(os.pathsep) if entry]
    normalized_entries = {
        os.path.normcase(os.path.normpath(entry))
        for entry in path_entries
    }
    if normalized_pkg_dir not in normalized_entries:
        os.environ['PATH'] = os.pathsep.join([_pkg_dir, *path_entries])

# On Windows, register the package directory so that bundled DLLs
# (e.g. libgfortran-5.dll, libgcc_s_seh-1.dll, …) can be found when
# loading the compiled Fortran extension (crysfml08lib.pyd).
# os.add_dll_directory() is available since Python 3.8.
if sys.platform == 'win32' and hasattr(os, 'add_dll_directory'):
    _dll_directory_handle = os.add_dll_directory(_pkg_dir)
    _prepend_package_dir_to_path()

# Default CRYSFML_DB to the bundled databases without overriding caller configuration.
os.environ.setdefault('CRYSFML_DB', _db_dir)
