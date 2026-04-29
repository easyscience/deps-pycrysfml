.. py:function:: get_chem_symb(label)

    Returns the chemical symbol of an element from a label.

    :param label: atom label
    :type label: str
    :returns: chemical symbol
    :rtype: str

    .. code-block:: python

        >>> from crysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_chem_symb('Fe+2')
        'Fe'