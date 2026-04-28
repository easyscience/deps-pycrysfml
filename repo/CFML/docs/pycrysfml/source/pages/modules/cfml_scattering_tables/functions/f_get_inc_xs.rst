.. py:function:: get_inc_xs(symb)

    Returns the incoherent scattering neutron cross section (barns) of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: incoherent scattering cross section
    :rtype: float

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_inc_xs('Fe')
        0.4000000059604645