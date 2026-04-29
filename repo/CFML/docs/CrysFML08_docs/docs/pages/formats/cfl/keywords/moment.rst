MOMENT
======

Description
-----------

Defines the magnetic moment of an atom.

Syntax
------

.. code-block:: cfl

    MOMENT mx my mz

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">mx,my,mz</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Components of the magnetic moment with respect to the crystal axes. This keyword must be after the keyword <span class="keyword">ATOM</span> but before any other keyword <span class="keyword">ATOM</span> defining a new atom.
     </div>
   </div>

Examples
--------

  .. code-block:: cfl

   MOMENT 0.23 0.00 0.00