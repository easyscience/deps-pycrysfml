.. py:data:: symm_oper_type

    A symmetry operator.

    Keys and types:

    - **dt** (integer): determinant of the submatrix [1:3,1:3], it must be 1 or -1.
    - **mat** (ndarray, dim=(:,:), float32): matrix operator.
    - **time_inv** (integer): time inversion.

