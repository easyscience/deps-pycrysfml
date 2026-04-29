.. py:data:: cell_g_type

    Extension of type :ref:`cell_type <cell_type>`.

    Keys and types:

    - **bl_m** (ndarray, dim=(3,3), float32): Busing-Levy B-matrix.
    - **carttype** (str): cartesian coordinate system, 'CA' (default) | 'BC' | 'BA' | 'CB'.

        'CA' : :math:`z` // :math:`c` and :math:`x` // :math:`a^*`.

        'AC' : :math:`x` // :math:`a` and :math:`z` // :math:`c^*`.

        'BC' : :math:`y` // :math:`b` and :math:`z` // :math:`c^*`.

        'BA' : :math:`y` // :math:`b` and :math:`x` // :math:`a^*`.
    - **cr_orth_cel** (ndarray, dim=(3,3), float32): matrix transformation from fractional to cartesian coordinates.
    - **gd** (ndarray, dim=(3,3), float32): direct matrix tensor.
    - **gr** (ndarray, dim=(3,3), float32): reciprocal matrix tensor.
    - **inv_bl_m** (ndarray, dim=(3,3), float32): inverse Busing-Levy B-matrix.
    - **orth_cr_cel** (ndarray, dim=(3,3), float32): matrix transformation from cartesian to fractional coordinates.
    - **rang** (ndarray, dim=(3), float32): reciprocal cell parameters :math:`\alpha^*`, :math:`\beta^*` and :math:`\gamma^*`.
    - **rcell** (ndarray, dim=(3), float32): reciprocal cell parameters :math:`a^*`, :math:`b^*` and :math:`c^*`.
    - **rvol** (float): reciprocal unit cell volume.

