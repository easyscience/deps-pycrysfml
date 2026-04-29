.. py:function:: get_z_symb(symb)

    Returns the atomic number of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: atomic number
    :rtype: int

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_z_symb('Fe')
        26