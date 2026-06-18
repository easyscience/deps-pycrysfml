.. py:function:: get_abs_xs(symb)

    Returns the absorption cross-section (barns, energy = 0.025 eV) of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: absorption cross-section
    :rtype: float

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_abs_xs('Fe')
        2.559999942779541