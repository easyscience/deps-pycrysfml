.. py:function:: get_atomic_vol(symb)

    Returns the atomic volume (cm\ :sup:`3`/mol) of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: atomic vol
    :rtype: float

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_atomic_vol('Fe')
        7.099999904632568