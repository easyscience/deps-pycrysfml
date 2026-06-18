.. py:function:: set_spacegroup_from_dbase(mystr,mode,**kwargs)

    Creates a Shubnikov (:ref:`spg_type <spg_type>`) or superspace (:ref:`superspacegroup_type <superspacegroup_type>`) group from the database. Therefore, the database must be installed in the computer. If CrysFML08 is in your computer, define the environment variable :code:`CRYSFML_DB` and set it to the directory :code:`Src/Databases` in CrysFML08. If you have installed the FullProf suite, check that the environment variable :code:`FULLPROF` has been set. With either of the two environment variables, PyCrysFML will locate the database. Another option is to pass as an argument the path to the database (see below).

    :param mystr: string specifying the Shubnikov or superspace group.

          * From number.
               * Shubnikov group: :code:`mystr = "1236"`
               * Superspace group: :code:`mystr = "13232"`
          * From the standard symbol.
               * Shubnikov group: :code:`mystr = "Pn'ma"``
               * Superspace group: :code:`mystr = "Pnma(0,0,g)0s0"`
          * From the Hall symbol.
               * space #41 of standard symbol Aba2: :code:`mystr = "B 2 C B"`
          * From the unified symbol.
               * Shubnikov group: :code:`mystr = "UNI Pnna.1'_a[Pncm]"`
    :type mystr: str
    :param mode: type of group: 'shubn' | 'super'.
    :param kwargs: optional arguments.
    :param kwargs['xyztype']: coordinates notation: 'xyz' (default) | 'abc' | 'x1x2x3'.
    :type kwargs['xyztype']: str
    :param kwargs['setting']: setting transformation.
    :type kwargs['setting']: str
    :param kwargs['keepdb']: if not given, deallocate the database after the group generation.
    :type kwargs['keepdb']: bool
    :param kwargs['parent']: if given, set the parent group symbol.
    :type kwargs['parent']: str
    :param kwargs['database_path']: database path. Required if environment variables :code:`CRYSFML_DB` and :code:`FULLPROF` are not set.
    :type kwargs['database_path']: str
    :param kwargs['trn_to']: if given, transformation matrices between standard and new setting (if given) are computed.
    :type kwargs['trn_to']: bool
    :returns: space group
    :rtype: dict (:ref:`spg_type <spg_type>` or :ref:`superspacegroup_type <superspacegroup_type>`)

    .. code-block:: python

        >>> from pycrysfml import cfml_gspacegroups
        >>> sg = cfml_gspacegroups.set_spacegroup_from_dbase("Pn'ma","shubn",setting="-c,b,a;0,0,0")
        >>> for k in sg:
        ...   print(f'{k :>16}: {sg[k]}')
        ...
                  multip: 8
                       d: 4
                     inv: [0 0 0 0 0 0 0 0]
                      op: [{'time_inv': 1, 'dt': 1, 'mat': array([[1., 0., 0., 0.],
                          [0., 1., 0., 0.],
                          [0., 0., 1., 0.],
                          [0., 0., 0., 1.]], dtype=float32)}, {'time_inv': 1, 'dt': 1, 'mat': array([[-1. ,  0. ,  0. ,  0.5],
                          [ 0. , -1. ,  0. ,  0.5],
                          [ 0. ,  0. ,  1. ,  0.5],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}, {'time_inv': -1, 'dt': 1, 'mat': array([[-1. ,  0. ,  0. ,  0. ],
                          [ 0. ,  1. ,  0. ,  0.5],
                          [ 0. ,  0. , -1. ,  0. ],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}, {'time_inv': -1, 'dt': 1, 'mat': array([[ 1. ,  0. ,  0. ,  0.5],
                          [ 0. , -1. ,  0. ,  0. ],
                          [ 0. ,  0. , -1. ,  0.5],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}, {'time_inv': -1, 'dt': -1, 'mat': array([[-1.,  0.,  0.,  0.],
                          [ 0., -1.,  0.,  0.],
                          [ 0.,  0., -1.,  0.],
                          [ 0.,  0.,  0.,  1.]], dtype=float32)}, {'time_inv': -1, 'dt': -1, 'mat': array([[ 1. ,  0. ,  0. ,  0.5],
                          [ 0. ,  1. ,  0. ,  0.5],
                          [ 0. ,  0. , -1. ,  0.5],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}, {'time_inv': 1, 'dt': -1, 'mat': array([[ 1. ,  0. ,  0. ,  0. ],
                          [ 0. , -1. ,  0. ,  0.5],
                          [ 0. ,  0. ,  1. ,  0. ],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}, {'time_inv': 1, 'dt': -1, 'mat': array([[-1. ,  0. ,  0. ,  0.5],
                          [ 0. ,  1. ,  0. ,  0. ],
                          [ 0. ,  0. ,  1. ,  0.5],
                          [ 0. ,  0. ,  0. ,  1. ]], dtype=float32)}]
                 symb_op: ['x,y,z,+1','-x+1/2,-y+1/2,z+1/2,+1','-x,y+1/2,-z,-1', 'x+1/2,-y,-z+1/2,-1','-x,-y,-z,-1', 'x+1/2,y+1/2,-z+1/2,-1', 'x,-y+1/2,z,+1', '-x+1/2,y,z+1/2,+1']
                magnetic: True
        standard_setting: False
                  numspg: 0
                  numshu: 541
                  numops: 8
                 centred: 1
             anticentred: 2
                mag_type: 3
                 num_lat: 0
                num_alat: 0
              parent_num: 62
             bravais_num: 0
                 spg_lat: P
                 shu_lat: ['P', 'P']
              init_label: Pn'ma
              parent_spg: Pnma
            tfrom_parent:
                  centre: Non-centrosymmetric, Anti-centric with -1' @ origin
                spg_symb:
                 bns_num: 62.443
                  og_num: 62.3.504
                bns_symb: Pn'ma
                 og_symb: Pn'ma
                    hall: P 2ac' 2n -1'
                     uni: Pn'ma
                 uni_num:  504
              crystalsys: Orthorhombic
                      pg:
                  mag_pg: m'mm
                    laue:
                 setting: -c,b,a;0,0,0
                 mat2std: c,b,-a;0,0,0
             mat2std_shu:
                 matfrom: -c,b,a;0,0,0
         generators_list: -x+1/2,-y+1/2,z+1/2,+1;-x,y+1/2,-z,-1;x+1/2,-y,-z+1/2,-1;-x,-y,-z,-1;x+1/2,y+1/2,-z+1/2,-1;x,-y+1/2,z,+1;-x+1/2,y,z+1/2,+1
            centre_coord: [0. 0. 0.]
        anticentre_coord: [0. 0. 0.]
                  lat_tr: []
                 alat_tr: []