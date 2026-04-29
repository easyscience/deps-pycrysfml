.. py:function:: get_chem_info(r)

    Returns the row r of the :ref:`table chem_info <chem_info_table>`.

    :param r: row
    :type r: int
    :returns: chemical information
    :rtype: dict. See :ref:`chem_info_type <chem_info_type>`.

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_chem_info(26)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: FE
          name: Iron
             z: 26
          atwe: 55.84700012207031
          rcov: 1.1699999570846558
        rwaals: 2.440000057220459
          vatm: 7.099999904632568
          oxid: [2 3 0 0 0]
          rion: [0.76 0.64 0.   0.   0.  ]
          sctf: 0.9449999928474426
        sedinc: 0.4000000059604645
           sea: 2.559999942779541