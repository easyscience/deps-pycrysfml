.. py:data:: group_type

    Base dictionary for representing symmetry groups.

    Keys and types:

    - **d** (integer): dimension of operator matrices.
    - **inv** (ndarray, dim=(:), int32): pointer to the inverse of the symmetry operator.
    - **multip** (integer): time inversion.
    - **op** (list): symmetry operators. Every element of the list is a dictionary of type :ref:`symm_oper_type <symm_oper_type>`.
    - **symb_op** (list): symbols of the symmetry operators.

