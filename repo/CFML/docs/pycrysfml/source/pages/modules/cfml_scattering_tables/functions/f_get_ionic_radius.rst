.. py:function:: get_ionic_radius(symb,valence)

    Returns the ionic radius (angstroms) of an ion. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :param valence: ion valence
    :type valence: int
    :returns: ionic radius
    :rtype: float

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_covalent_radius('Fe',2)
        0.7599999904632568