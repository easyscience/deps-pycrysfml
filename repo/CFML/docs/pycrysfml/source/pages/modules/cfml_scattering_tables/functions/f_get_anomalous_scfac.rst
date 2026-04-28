.. py:function:: get_anomalous_scfac(r)

    Returns the row r of the :ref:`table anomalous_scfac <anomalous_scfac_table>`.

    :param r: row
    :type r: int
    :returns: anomalous scattering factors
    :rtype: dict. See :ref:`anomalous_sc_type <anomalous_sc_type>`.

    .. code-block:: python

        >>> from pycrysfml import cfml_scattering_tables
        >>> row = cfml_scattering_tables.get_anomalous_scfac(26)
        >>> for k in row:
        ...   print(f'{k :>6}: {row[k]}')
        ...
          symb: fe
            fp: [-1.339 -2.095 -1.179  0.301  0.244]
           fpp: [0.764 0.566 3.204 0.845 0.545]