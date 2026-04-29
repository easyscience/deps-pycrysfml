# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html
import os
import sys
sys.path.append(os.path.abspath("./_ext"))

from bash_extension import ExtendedBashLexer
from cfl_lexer import CFLLexer
from sphinx.highlighting import lexers

lexers['cfl'] = CFLLexer()
lexers['bash_extension'] = ExtendedBashLexer()

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'fpsuite'
copyright = '2025, The FullProf team'
author = 'Juan Rodriguez-Carvajal, Nebil A. Katcho, Javier Gonzalez-Platas'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = []

templates_path = ['_templates']
exclude_patterns = []

# Custom roles
rst_prolog = """
.. role:: varenv
   :class: varenv-role

.. role:: filename
   :class: filename-role

.. role:: filext
   :class: filext-role

.. role:: filepath
   :class: filepath-role

.. role:: guielem
   :class: guielem-role

.. role:: menu
   :class: menu-role

.. role:: program
   :class: program-role
"""

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_static_path = ['_static']
html_theme = 'sphinx13'
html_theme_path = ['_themes']
html_css_files = [
    'sphinx13.css',
]
