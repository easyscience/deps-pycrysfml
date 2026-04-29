.. py:function:: get_magnetic_j2(r)

    Returns the row r of the :ref:`table magnetic_j2 <magnetic_j2_table>`.

    :param r: row
    :type r: int
    :returns: magnetic form factor j2
    :rtype: dict. See :ref:`magnetic_form_type <magnetic_form_type>`.

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_magnetic_j2(1)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: SC0
          sctm: [1.08172e+01 5.43270e+01 4.73530e+00 1.48470e+01 6.07100e-01 4.21800e+00
         1.10000e-03]