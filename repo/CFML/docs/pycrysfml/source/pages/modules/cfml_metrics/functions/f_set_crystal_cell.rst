.. py:function:: set_cell(vcell,vang,**kwargs)

    Creates an object of type :ref:`cell_g_type <cell_g_type>`.

    :param vang: lattice parameters :math:`\alpha^*`, :math:`\beta^*` and :math:`\gamma^*`.
    :type vang: ndarray, dim=(3), type=float32
    :param vcell: lattice parameters :math:`a^*`, :math:`b^*` and :math:`c^*`.
    :type vcell: ndarray, dim=(3), type=float32
    :param kwargs: optional arguments.
    :param kwargs['carttype']: cartesian coordinate system.
    :type kwargs['carttype']: str
    :param kwargs['vsang']: vang standard deviations.
    :type kwargs['vsang']: ndarray, dim=(3), type=np.float32
    :param kwargs['vscell']: vcell standard deviations.
    :type kwargs['vscell']: ndarray, dim=(3), type=np.float32
    :returns: crystal cell
    :rtype: dict (:ref:`cell_g_type <cell_g_type>`)

    .. code-block:: python

        >>> from crysfml import cfml_metrics
        >>> import numpy as np
        >>> vcell = np.array([4.0,4.0,7.0],dtype=np.float32)
        >>> vang = np.array([90.0,90.0,120.0],dtype=np.float32)
        >>> cell = cfml_metrics.set_cell(vcell,vang,carttype='AC')
        >>> for k in cell:
        ...   print(f'{k :>10}: {cell[k]}')
        ...
               cell: [4. 4. 7.]
              scell: [0. 0. 0.]
                ang: [ 90.  90. 120.]
               sang: [0. 0. 0.]
                vol: 96.99484252929688
               svol: 0.0
              rcell: [0.28867513 0.28867513 0.14285715]
               rang: [90. 90. 60.]
               rvol: 0.010309826582670212
                 gd: [[16. -8.  0.]
                      [-8. 16.  0.]
                      [ 0.  0. 49.]]
                 gr: [[0.08333333 0.04166666 0.        ]
                      [0.04166666 0.08333333 0.        ]
                      [0.         0.         0.02040816]]
        cr_orth_cel: [[ 4.        -2.         0.       ]
                      [ 0.         3.4641016  0.       ]
                      [ 0.         0.         7.       ]]
        orth_cr_cel: [[ 0.25        0.14433756 -0.        ]
                      [-0.          0.28867513 -0.        ]
                      [ 0.         -0.          0.14285715]]
               bl_m: [[ 0.28867513  0.14433756  0.        ]
                      [ 0.          0.24999999 -0.        ]
                      [ 0.          0.          0.14285715]]
           inv_bl_m: [[ 3.4641016 -2.0000002 -0.       ]
                      [-0.         4.0000005  0.       ]
                      [ 0.        -0.         6.9999995]]
           carttype: AC