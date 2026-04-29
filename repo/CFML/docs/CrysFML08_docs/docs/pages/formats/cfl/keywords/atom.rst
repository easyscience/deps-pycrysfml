ATOM
====

Description
-----------
Defines an atom.

Syntax
------

.. code-block:: cfl

    ATOM label symbol x y z [B_iso] [occ] [mom] [charge]

.. raw:: html

    The <span class="keyword">ATOM</span> keyword requires five mandatory arguments: <span class="arg">label</span>, <span class="arg">symbol</span>, <span class="arg">x</span>, <span class="arg">y</span>, and <span class="arg">z</span>. The optional arguments, shown in brackets, are interpreted as follows. If six arguments are provided, the sixth is always interpreted as <span class="arg">B<sub>iso</sub></span>. The seventh is interpreted as <span class="arg">occ</span>, the eighth as <span class="arg">mom</span>, and the ninth as <span class="arg">charge</span>. <span class="arg">occ</span> cannot be specified without specifying <span class="arg">B<sub>iso</sub></span>, <span class="arg">mom</span> cannot be specified without specifying <span class="arg">occ</span>, and so on.

Arguments
---------

.. raw:: html

   <div class="variable-card">
     <div class="var-name">label</div>
     <div class="var-type">Type: string</div>
     <div class="var-desc">
        Atom label.
     </div>
     <div class="var-name">symbol</div>
     <div class="var-type">Type: string</div>
     <div class="var-desc">
        Chemical symbol or scattering factor symbol. If Symbol refers to a scattering factor, there are three possible cases:
        <ul>
            <li>Neutron scattering length: the scattering factor symbol corresponds to the chemical symbol.</li>
            <li>X-ray form factor: the scattering factor symbol is given by the chemical species (chemical symbol plus atomic charge), for instance AL, AL+3, F-1.</li>
            <li>Magnetic form factor: the scattering factor symbols is M followed by the chemical symbol and the formal charge, for instance MFE0, MFE2. In the case of rare earths, two choices are available:
                <ul>
                    <li>MHO3: magnetic form factor of Ho<sup>3+</sup> as &ltj<sub>0</sub>&gt.</li>
                    <li>JHO3: magnetic form factor of Ho<sup>3+</sup> as &ltj<sub>0</sub>&gt + c<sub>2</sub>&ltj<sub>2</sub>&gt.</li>
                </ul>
            </li>
        </ul>
     </div>
     <div class="var-name">x,y,z</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Atomic coordinates in reduced units.
     </div>
     <div class="var-name">B<sub>iso</sub></div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Isotropic atomic displacement parameter, B<sub>iso</sub> = 8&pi;<sup>2</sup>&langle;u<sup>2</sup>&rangle;, with &langle;u<sup>2</sup>&rangle; being the mean square atomic displacement parameter.
     </div>
     <div class="var-name">occ</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Occupancy factor. For a crystallographic site with ocuppancy occ and multiplicity m, and M the general position multiplicity, the occupancy factor is calculated as occ &middot m / M.
     </div>
     <div class="var-name">mom</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Magnitude of the atomic magnetic moment, in bohr magneton.
     </div>
     <div class="var-name">charge</div>
     <div class="var-type">Type: float</div>
     <div class="var-desc">
        Atomic charge.
     </div>
   </div>

Examples
--------

- Atom with chemical symbol and no optional arguments.

  .. code-block:: cfl

   ATOM   Fe  Fe  0.28220  0.25000  0.97410

- Atom with chemical species symbol, two optional arguments and standard deviations.

  .. code-block:: cfl

   ATOM   Pb  PB+2  0.18749(10)  0.25000  0.16719(16)  1.42(3)  0.50000

- Atom with magnetic form factor symbol and two optional arguments.

  .. code-block:: cfl

   ATOM   Ho_1  JHO3  0.28840  0.12500  0.11620  0.18451  0.50000