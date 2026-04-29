.. py:data:: spg_type

    A Shubnikov group. Extension of :ref:`group type <group_type>`.

    Keys and types:

    - **alat_tr** (ndarray, dim=(:,:), float32): anti-centring translations vectors, first dimension contains vector coordinates.
    - **anticentred** (integer): 0:anti-centric(-1' no at origin) | 1:anti-acentric | 2:anti-centric(-1' at origin).
    - **anticentre_coord** (ndarray, dim=(:), float32): coordinates of time inversion centres.
    - **bns_num** (str): numerical Label in the data base of Stokes-Campbell.
    - **bns_symb** (str): Belonov-Neronova-Smirnova Shubnikov group symbol.
    - **bravais_num** (integer): number of the Bravais class of the superspace (Stokes & Campbell database).
    - **centre** (str): alphanumeric information about the center of symmetry.
    - **centred** (integer): 0:centric(-1 no at origin) | 1:acentric, 2:centric(-1 at origin).
    - **centre_coord** (ndarray, dim=(:), float32): coordinates of inversion centres.
    - **crystalsys** (str): crystal system.
    - **generators_list** (str): list of generators.
    - **hall** (str): Hall symbol.
    - **init_label** (str): symbol of the space group provided for generating it.
    - **lat_tr** (ndarray, dim=(:,:), float32): centring translations vectors, first dimension contains vector coordinates.
    - **laue** (str): Laue group.
    - **magnetic** (bool): if True, magnetic group.
    - **matfrom** (str): transformation from standard space group.
    - **mat2std** (str): transformation to standard space group (parent).
    - **mat2std_shu** (str): transformation to standard Shubnikov group.
    - **mag_pg** (str): magnetic point group.
    - **mag_type** (integer): magnetic group type, 1:colorless | 2:paramagnetic | 3:black & white 1 | 4:black & white 2.
    - **numops** (integer): number of symmetry operators.
    - **numshu** (integer): Shubnikov group number.
    - **numspg** (integer): spacegroup number (IT if standard).
    - **num_alat** (integer): number of anti-centring translations.
    - **num_lat** (integer): number of centring translations.
    - **og_num** (str): numerical Label in the data base of D. Litvin.
    - **og_symb** (str): Opechowski-Guccione Shubnikov group symbol.
    - **parent_num** (integer): number of the parent group.
    - **parent_spg** (str): symbol of the parent group.
    - **pg** (str): point group.
    - **setting** (str): operators transformed by "setting" (e.g. -a+b,a+b,-c;1/2,0,0).
    - **shu_lat** (str): Shubnikov lattice type.
    - **spg_lat** (str): lattice type.
    - **spg_symb** (str): space group symbol.
    - **standard_setting** (bool): if True, group in the standard setting.
    - **tfrom_parent** (str): transformation of the parent basis to get the actual one.
    - **uni** (str): unified symbol.
    - **uni_num** (str): unified number.

