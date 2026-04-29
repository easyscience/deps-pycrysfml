.. py:function:: get_atomic_mass(symb)

    Returns the atomic mass (amu) of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: atomic mass
    :rtype: float

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_atomic_mass('Fe')
        55.84700012207031