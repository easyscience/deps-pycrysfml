NKVEC
=====

Description
-----------

Defines the number of propagation vectors.

Syntax
------

.. code-block:: cfl

    NKVEC nk nq

.. raw:: html

   Equivalent keywords are <span class="keyword">NKVECT</span>, <span class="keyword">NQVEC</span> and <span class="keyword">NQVECT</span>. This keyword must always be given before <a href="kvec.html"><span class="keyword">KVEC</span></a>.

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">nk</div>
     <div class="var-type">Type: integer</div>
     <div class="var-desc">
        Number of basis vectors. The actual propagation vectors will be expressed as a linear combination of these vectors.
     </div>
     <div class="var-name">nq</div>
     <div class="var-type">Type: integer</div>
     <div class="var-desc">
        Number of sets of q-coefficients. Q-coefficients are the coefficients of the linear combinations. These coefficients are defined with the keyword <a href="qcoeff.html"><span class="keyword">Q_COEFF</span></a>.
     </div>
   </div>

Examples
--------

- Two basis vectors, three propagation vectors.

  .. code-block:: cfl

   NKVEC 2 3
   KVEC 0.000 0.123 0.000
   KVEC 0.000 0.000 0.321
   Q_COEFF 1 0
   Q_COEFF 0 1
   Q_COEFF 1 1