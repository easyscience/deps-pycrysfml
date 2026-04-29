About PyCrysFML
===============

The Crystallographic Fortran Modules Library, CrysFML, was originally a set of Fortran 90/95 modules containing procedures of interest in crystallographic applications. Later, the library was upgraded and rewritten in the Fortran 2008 standard, which is why the new library is called CrysFML08 to differentiate it from the original. This set of modules has been, and is still being, developed by us in order to facilitate the design and the development of crystallographic computing programs. Many of the algorithms and procedures of the library come from adaptations and modifications of existing codes of different sources.

The :code:`git` repositories of CrysFML, which is no longer developed, and CrysFML08 are, respectively, `<https://code.ill.fr/scientific-software/crysfml>`_ and `<https://code.ill.fr/scientific-software/CrysFML2008>`_.

CrysFML08 is an extension of CrysFML because it contains procedures that were not found in CrysFML but in :code:`FullProf`, one of the most widely used software in diffraction data analysis. Our idea is that CrysFML08 will become the core of a completely new :code:`FullProf` suite.

PyCrysFML is a Python wrapper of CrysFML08. It has two purposes:

1. To make high-level CrysFML08 procedures accessible from Python, facilitating the development of crystallographic programs in one of the current reference languages in scientific computing.
2. To be the basis of the graphical environment of the new FullProf suite.

PyCrysFML is based on `Forpy <https://github.com/ylikx/forpy>`_. :code:`Forpy` allows to use Python features in Fortran. In particular, it provides datastructures such as list, tuple, dict and interoperability of arrays using :code:`numpy`. In this way, the Fortran objects of CrysFML08, known as *derived types*, are returned in the Python side as dictionaries. Fortran numerical arrays are returned as numpy arrays, whereas non-numerical arrays are returned as lists. The complete correspondence between CrysFML08 and PyCrysFML is summarized in the following table:

.. list-table::
   :widths: 200 200
   :header-rows: 1

   * - CrysFML08
     - PyCrysFML
   * - Module
     - Module
   * - Subroutine
     - Function
   * - Function
     - Function
   * - Type
     - Dictionary
   * - Numerical array
     - Numpy array
   * - Non-numerical array
     - List

This user guide consists mainly of a description, for every module, of the available dictionaries and functions. Every function is accompanied by an example.

License
-------
The PyCrysFML project is distributed under LGPL. In agreement with the
Intergovernmental Convention of the ILL, this software cannot be used
in military applications.

Copyright (C) 2024  Institut Laue-Langevin (ILL), Grenoble, France.

Authors: Nebil Ayape Katcho (ILL), Juan Rodríguez-Carvajal (ILL) and Javier González-Platas (Universidad de la Laguna)

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, see `<http://www.gnu.org/licenses/>`_.