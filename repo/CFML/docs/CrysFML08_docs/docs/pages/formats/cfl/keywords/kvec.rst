KVEC
====

Description
-----------

Defines a propagation vector.

Syntax
------

.. code-block:: cfl

    KVEC kx ky kz

.. raw:: html

   Equivalent keywords are <span class="keyword">KVECT</span>, <span class="keyword">QVEC</span> and <span class="keyword">QVECT</span>. This keyword must always be given after <a href="nkvec.html"><span class="keyword">NKVEC</span></a>.

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">kx,ky,kz</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Components of the propagation vector with respect to the reciprocal unit cell.
     </div>
   </div>

Examples
--------

  .. code-block:: cfl

   KVEC 0.5 0.0 0.5