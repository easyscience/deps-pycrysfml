.. py:data:: chem_info_type

    Keys and types:

    - **atwe** (float): atomic weight (amu).
    - **name** (str): name of the element.
    - **oxid** (ndarray, dim=(5), type=int32): oxidation states.
    - **rcov** (float): covalent radius (angstroms).
    - **rion** (ndarray, dim=(5), type=float32): ionic radii (angstroms).
    - **rwaals** (float): van der Waals radius (angstroms).
    - **sctf** (str): Fermi length (10\ :sup:`-12` cm).
    - **sedinc** (str): incoherent neutron scattering cross section (barns (10\ :sup:`-24` cm\ :sup:`2`)).
    - **sea** (str): neutron absorption cross section (barns (10\ :sup:`-24` cm\ :sup:`2`)).
    - **symb** (str): chemical symbol.
    - **vatm** (str): atomic volume (cm\ :sup:`3`/mol).
    - **z** (int): atomic number.