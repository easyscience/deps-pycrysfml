.. py:function:: get_shubnikov_info(r)

    Returns the row r of the :ref:`table shubnikov_info <shubnikov_info_table>`.

    :param r: row
    :type r: int
    :returns: tabulated data of a shubnikov group
    :rtype: dict. See :ref:`shub_spgr_info_type <shub_spgr_info_type>`.

    .. code-block:: python

        >>> from crysfml import cfml_symmetry_tables
        >>> row = cfml_symmetry_tables.get_shubnikov_info(1000)
        >>> for k in row:
        ...   print(f'{k :>10}: {row[k]}')
        ...
            id_bns: 123.340
               bns: P4/mmm1'
             id_og: 123.2.1000
                og: P4/mmm1'
               std: P4/mmm.1'
             mhall: -P 4 2 1'
        generators: -y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x,y,z,-1