import os
import sys

# Tests

def test__import_pycrysfml():
    msg = 'import pycrysfml'
    try:
        import pycrysfml
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

def _test__from_pycrysfml_import_crysfml08lib():
    msg = 'from pycrysfml import crysfml08lib'
    try:
        from pycrysfml import crysfml08lib
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

def test__from_pycrysfml_import_cfml_py_utilities():
    msg = 'from pycrysfml import cfml_py_utilities'
    try:
        from pycrysfml import cfml_py_utilities
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

# Debug

if __name__ == '__main__':
    test__import_pycrysfml()
    test__from_pycrysfml_import_cfml_py_utilities()
