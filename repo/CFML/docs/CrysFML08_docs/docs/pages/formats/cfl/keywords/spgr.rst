SPGR
====

Description
-----------

Defines a crystallographic space group from its number, its standard symbol or its Hall symbol.

Syntax
------

.. code-block:: cfl

    SPGR symbol

.. raw:: html

   Equivalent keywords are <span class="keyword">SPACEG</span> and <span class="keyword">HALL.

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">symbol</div>
     <div class="var-type">Type: string | integer</div>
     <div class="var-desc">
        Number of symbol of the space group. The symbol can be a standard symbol or a Hall symbol.
     </div>
   </div>

Examples
--------

- Crystallographic group from its IT number.

  .. code-block:: cfl

     SPGR 17

- Space group from its standard symbol.

  .. code-block:: cfl

     SPGR P 2 2 21

- Space group from its Hall symbol

  .. code-block:: cfl

     SPGR P 2a 2a

