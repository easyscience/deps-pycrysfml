.. py:function:: get_xray_wavelengths(r)

    Returns the row r of the :ref:`table xray_wavelengths <xray_wavelengths_table>`.

    :param r: row
    :type r: int
    :returns: k-series wavelengths for x-rays
    :rtype: dict. See :ref:`xray_wavelength_type <xray_wavelength_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_xray_wavelengths(1)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: CR
         kalfa: [2.28988 2.29428]
         kbeta: 2.0848000049591064