.. py:function:: get_magnetic_j4(r)

    Returns the row r of the :ref:`table magnetic_j4 <magnetic_j4_table>`.

    :param r: row
    :type r: int
    :returns: magnetic form factor j4
    :rtype: dict. See :ref:`magnetic_form_type <magnetic_form_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_magnetic_j4(1)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: SC0
          sctm: [ 1.342  10.2     0.3837  3.079   0.0468  0.118  -0.0328]