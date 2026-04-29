.. py:function:: get_spgr_info(r)

    Returns the row r of the :ref:`table spgr_info <spgr_info_table>`.

    :param r: row
    :type r: int
    :returns: tabulated data of a crystallographic space group
    :rtype: dict. See :ref:`spgr_info_type <spgr_info_type>`.

    .. code-block:: python

        >>> from crysfml import cfml_symmetry_tables
        >>> row = cfml_symmetry_tables.get_spgr_info(100)
        >>> for k in row:
        ...   print(f'{k :>10}: {row[k]}')
        ...
                 n: 12
                hm: A 2/M
              hall: -A 2y
              laue: 2
                pg: 5
               asu: [ 0  0  0 12  6 24]
         inf_extra: b2