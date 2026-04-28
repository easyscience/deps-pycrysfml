.. py:function:: get_magnetic_j6(r)

    Returns the row r of the :ref:`table magnetic_j6 <magnetic_j6_table>`.

    :param r: row
    :type r: int
    :returns: magnetic form factor j6
    :rtype: dict. See :ref:`magnetic_form_type <magnetic_form_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_magnetic_j6(1)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: CE2
          sctm: [-1.212e-01  7.994e+00 -6.390e-02  4.024e+00  1.519e-01  1.096e+00
          7.800e-03]