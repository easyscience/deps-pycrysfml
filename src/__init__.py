import importlib.metadata
import os
import sys


# Set package version
__version__ = importlib.metadata.version("pycrysfml")

# Add current path to system path
sys.path.append(os.path.dirname(__file__))

# Set environment variable CRYSFML_DB to be the path to the Databases directory
os.environ['CRYSFML_DB'] = os.path.join(os.path.dirname(__file__), 'Databases')
