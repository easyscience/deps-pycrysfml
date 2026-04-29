CELL
====

Description
-----------
Defines the unit cell.

Syntax
------

.. code-block:: cfl

    ATOM a b c alpha beta gamma

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">a, b, c, alpha, beta, gamma</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Unit cell parameters.
     </div>
   </div>

Examples
--------

- Orthorhombic unit cell.

  .. code-block:: cfl

   CELL  8.47884  5.39672  6.95840  90.0000  90.0000  90.0000

- Orthorhombic unit cell with standard deviations.

  .. code-block:: cfl

   CELL  8.47884(15)  5.39672(10)  6.95840(13)  90.0000  90.0000  90.0000