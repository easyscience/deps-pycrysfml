import os

#import cfml_py_utilities
#import crysfml
#from crysfml import crysfml08lib
from crysfml import cfml_py_utilities

# Tests

# Workaround to set env variable CRYSFML_DB for all the tests below
# If running this with python instead of pytest, CRYSFML_DB is set automatically
# from the crysfml __init__.py, when importing crysfml
def test__crysfml_db_path():
    #os.environ['CRYSFML_DB'] = os.path.join(os.path.dirname(crysfml08lib.__file__), 'Databases')
    os.environ['CRYSFML_DB'] = os.path.join(os.path.dirname(cfml_py_utilities.__file__), 'Databases')
    actual = os.getenv('CRYSFML_DB', default='')
    desired = os.environ['CRYSFML_DB']
    assert desired == actual

# Debug

if __name__ == '__main__':
    test__crysfml_db_path()
