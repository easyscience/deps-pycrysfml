GEN
===

Description
-----------
Defines a symmetry operator in Jones faithful notation. It is usually used as a generator of a space group.

Syntax
------

.. code-block:: cfl

    GEN generator

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">generator</div>
     <div class="var-type">Type: string</div>
     <div class="var-desc">
        Symmetry operator.
     </div>
   </div>

Examples
--------

- Symmetry operator of a crystallographic group.

  .. code-block:: cfl

   GEN -x,-y,z+1/2

- Non-primed operator of a Shubnikov group.

  .. code-block:: cfl

   GEN x,y,-z,1

- Primed operator of a Shubnikov group.

  .. code-block:: cfl

   GEN -x+1/2,y,-z,-1

- Operator of a superspace group of dimension 3+1

  .. code-block:: cfl

   GEN x1,x1-x2,-x3,-x4+1/2,-1