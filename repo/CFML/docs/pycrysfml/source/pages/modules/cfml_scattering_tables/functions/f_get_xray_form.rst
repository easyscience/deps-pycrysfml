.. py:function:: get_xray_form(r)

    Returns the row r of the :ref:`table xray_form <xray_form_table>`.

    :param r: row
    :type r: int
    :returns: x-ray scattering factor coefficients
    :rtype: dict. See :ref:`xray_form_type <xray_form_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_xray_form(54)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: fe+3
             z: 26
             a: [11.1764  7.3863  3.3948  0.0724]
             b: [ 4.6147  0.3005 11.6729 38.5566]
             c: 0.9707000255584717