import os
import sys

# Tests

def test__import_crysfml():
    msg = 'import crysfml'
    try:
        import crysfml
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

def _test__from_crysfml_import_crysfml08lib():
    msg = 'from crysfml import crysfml08lib'
    try:
        from crysfml import crysfml08lib
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

def test__from_crysfml_import_cfml_py_utilities():
    msg = 'from crysfml import cfml_py_utilities'
    try:
        from crysfml import cfml_py_utilities
        print(f"::::: Succeeded to '{msg}'")
        assert True
    except Exception as e:
        print(f"::::: Failed to '{msg}': {e}")
        assert False

# Debug

if __name__ == '__main__':
    test__import_crysfml()
    test__from_crysfml_import_cfml_py_utilities()
