.. py:function:: get_magnetic_form(r)

    Returns the row r of the :ref:`table magnetic_form <magnetic_form_table>`.

    :param r: row
    :type r: int
    :returns: magnetic form factors
    :rtype: dict. See :ref:`magnetic_form_type <magnetic_form_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_magnetic_form(1)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: MSC0
          sctm: [ 2.51200e-01  9.00296e+01  3.29000e-01  3.94021e+01  4.23500e-01
          1.43222e+01 -4.30000e-03]