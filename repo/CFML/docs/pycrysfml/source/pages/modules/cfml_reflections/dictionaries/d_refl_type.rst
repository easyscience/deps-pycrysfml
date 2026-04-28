.. py:data:: refl_type

    Base dictionary for representing a Bragg reflection.

    Keys and types:

    - **h** (ndarray, dim=(:), int32): Miller index.
    - **imag** (integer): 0: nuclear, 1: magnetic, 2: nuclear + magnetic.
    - **mult** (integer): multiplicity.
    - **pcoeff** (integer): pointer to the harmonic :ref:`q_coeff <superspacegroup_type>`.
    - **s** (float): sin(theta) / lambda.

