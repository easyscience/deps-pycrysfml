.. py:function:: get_covalent_radius(symb)

    Returns the covalent radius (angstroms) of a chemical element. If an error ocurred, it returns 1.4.

    :param symb: atomic symbol
    :type symb: str
    :returns: covalent radius
    :rtype: float

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_covalent_radius('Fe')
        1.1699999570846558