.. py:function:: get_fermi_length(symb)

    Returns the Fermi length (10\ :sup:`-12` cm)  of a chemical element. If an error ocurred, it returns 0.

    :param symb: atomic symbol
    :type symb: str
    :returns: Fermi length
    :rtype: float

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> cfml_scattering_tables.get_fermi_length('Fe')
        0.9449999928474426