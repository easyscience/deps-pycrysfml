The CFL Format
==============

A :filext:`.cfl` file is a format recognized by all programs based in the `CrysFML <https://code.ill.fr/scientific-software/CrysFML2008>`_ library. The new FullProf Suite we are developing is entirely built on this library, so the :filext:`.cfl` format will be the main way to provide input to all programs in the suite.

A :filext:`.cfl` file is a collection of keyword-value pairs. Keywords are always character strings, while values can be either character strings or numbers. Character strings can be uppercase or lowercase. As an example, the panel below shows the description of the :math:`\mathrm{LiFePO_4}` crystal structure in :filext:`CFL` format:

.. code-block:: cfl

    TITLE LiFePO4
    ! Unit cell parameters:  a, b, c, alpha, beta, gamma
    CELL  10.3006  5.9901  4.6782  90.0000  90.0000  90.0000
    ! Space Group
    SPGR  P n m a
    ! Atom-strings in the order: Label, Species, x, y, z, Biso, Occ [,2*Spin, charge]
    ATOM Fe     Fe  0.28250  0.25000  0.97390  0.00000  0.50000     0.00     2.00
    ATOM Li     Li  0.00000  0.00000  0.00000  0.00000  0.50000     0.00     1.00
    ATOM O      O   0.09300  0.25000  0.74100  0.00000  0.50000     0.00    -2.00
    ATOM O      O   0.46000  0.25000  0.20200  0.00000  0.50000     0.00    -2.00
    ATOM O      O   0.16700  0.04600  0.28200  0.00000  1.00000     0.00    -2.00
    ATOM P      P   0.09810  0.25000  0.41800  0.00000  0.50000     0.00     5.00

Throughout 2026, we will progressively add documentation for all keywords and their corresponding values recognized by the CFL format.

Keywords
--------

.. toctree::
   :maxdepth: 1

   keywords/atom
   keywords/cell
   keywords/gen
   keywords/hall
   keywords/kvec
   keywords/kvect
   keywords/qvec
   keywords/qvect
   keywords/moment
   keywords/nkvec
   keywords/nkvect
   keywords/nqvec
   keywords/nqvect
   keywords/qcoeff
   keywords/spaceg
   keywords/spgr
   keywords/title
