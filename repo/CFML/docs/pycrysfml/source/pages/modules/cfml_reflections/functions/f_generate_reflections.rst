.. py:function:: generate_reflections(cell,slmin,slmax,spg,**kwargs)

    Generates a list of reflections from a given cell and space group in the range defined by slmin and slmax. Object cell is created by calling (:ref:`set_crystal_cell <set_crystal_cell>`). Object spg is created by calling  (:ref:`set_spacegroup_from_dbase <superspacegroup_type>`).

    :param cell: the crystal cell.
    :type cell: dict (:ref:`cell_g_type <cell_g_type>`)
    :param slmin: Minimum value of sin(theta) / lambda.
    :type slmin: float.
    :param slmax: Maximum value of sin(theta) / lambda.
    :type slmax: float.
    :param spg: the space group.
    :type spg: dict (:ref:`spg_type <spg_type>` or :ref:`superspacegroup_type <superspacegroup_type>`)
    :param kwargs['friedel']: if True, apply Friedel's law. Defaul value: True.
    :type kwargs['friedel']: bool
    :param kwargs['hlim']: h,k,l limits.
    :type kwargs['hlim']: ndarray, dim=(3,2), type=np.int32
    :param kwargs['mag_only']: if True, only magnetic reflections are generated. Default value: False.
    :type kwargs['mag_only']: bool
    :param kwargs['order']: if True, reflections ordered by increasing sin(theta) / lambda. Default value: False.
    :type kwargs['order']: bool
    :param kwargs['seqindx']: sequence in which index changes in the reflection list. [3,2,1] indicates `l` changes first, then `k`, finally `h`.
    :type kwargs['seqindx']: ndarray, dim=(3), type=np.int32
    :param kwargs['unique']: if given, unique reflections are generated.
    :type kwargs['unique']: bool
    :param kwargs['ref_typ']: type of the returned dictionary. It can be :ref:`refl <refl_type>` or :ref:`srefl <srefl_type>` or :ref:`mrefl <mrefl_type>`. Default value: 'refl'.
    :type kwargs['ref_typ']: str.
    :returns: reflection list.
    :rtype: dict (:ref:`refl_type <refl_type>` or :ref:`srefl_type <srefl_type>` or :ref:`mrefl_type <mrefl_type>`)

    .. code-block:: python

        >>> from pycrysfml import cfml_metrics
        >>> from pycrysfml import cfml_gspacegroups
        >>> from pycrysfml import cfml_reflections
        >>> import numpy as np
        >>> vcell = np.array([4.0,5.0,6.0],dtype=np.float32)
        >>> vang = np.array([90.0,90.0,90.0],dtype=np.float32)
        >>> cell = cfml_metrics.set_cell(vcell,vang)
        >>> sg = cfml_gspacegroups.set_spacegroup_from_dbase("P222","shubn")
        >>> ref = cfml_reflections.generate_reflections(cell,0.0,0.6,sg,order=True)
        >>> ref['nref']
        868
        >>> ref['ref'][0]
        {'fortran_type': 'refl_type', 'h': array([ 0,  0, -1], dtype=int32), 'mult': 2, 's': 0.0833333358168602, 'imag': 0, 'pcoeff': 0}