from . import crysfml08lib
import numpy as np

def cw_powder_pattern_from_dict(json : dict):
    """
    Computes a powder pattern from information provided by a
    dictionary. This subroutine was introduced mainly for
    Python applications. Python can easily transform a json file
    into a dictionary.

    Parameters
    ----------
    json
        Python type    : dict
        Fortran type   : dict
        Fortran intent : inout
        Description    : json file content

    Returns
    -------
    xc
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : out
        Description    : two theta angle
    yc
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : out
        Description    : calculated intensity
    """

    xc,yc = crysfml08lib.f_cw_powder_pattern_from_dict(json)
    return xc,yc

def read_crystal_structure(filename : str,**kwargs):
    """
    Build the object crystal from data given in a cfl, cif or mcif file

    Parameters
    ----------
    filename
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : file containing the crystal data
    kwargs:
        database_path
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : full path of the database file

    Returns
    -------
    crystal
        Python type    : dict
        Fortran type   : crystal_type
        Fortran intent : inout
        Description    : crystal object
    """

    if kwargs:
        crystal = crysfml08lib.f_read_crystal_structure(filename,kwargs)[0]
    else:
        crystal = crysfml08lib.f_read_crystal_structure(filename)[0]
    return crystal

def magnetic_structure_factors_from_mcif(mcif_file : str,sfacsymb : list,**kwargs):
    """
    Computes nuclear and magnetic structure factors from a mcif file

    Parameters
    ----------
    mcif_file
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : mcif file
    sfacsymb
        Python type    : list
        Fortran type   : character
        Fortran intent : in
        Description    : scattering factor symbols for atoms in the mcif
    kwargs:
        experiment
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : powder: "p" | single-crystal: "s"
        sintlmin
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : minimum value of sin(theta) / lambda
        sintlmax
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : maximum value of sin(theta) / lambda
        unique
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : generate only unique reflections
        mag_only
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if true, generate only magnetic reflections
        mag_ext
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if true, consider magnetic extinctions
        friedel
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if true, apply friedel law

    Returns
    -------
    hkl
        Python type    : dict
        Fortran type   : reflist_type
        Fortran intent : inout
        Description    : reflection list
    """

    if kwargs:
        hkl = crysfml08lib.f_magnetic_structure_factors_from_mcif(mcif_file,sfacsymb,kwargs)[0]
    else:
        hkl = crysfml08lib.f_magnetic_structure_factors_from_mcif(mcif_file,sfacsymb)[0]
    return hkl

def set_boundary(crystal : dict,coor : dict,limits : np.ndarray,boundary_coordination : bool):
    """
    Returns atoms, magnetic moments and bonds inside limits

    Parameters
    ----------
    crystal
        Python type    : dict
        Fortran type   : crystal_type
        Fortran intent : inout
        Description    : crystal object
    coor
        Python type    : dict
        Fortran type   : coordination_crystal_type
        Fortran intent : in
        Description    : crystal coordination
    limits
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : in
        Description    : cell limits in crystal coordinates (2,3)
    boundary_coordination
        Python type    : bool
        Fortran type   : logical
        Fortran intent : in
        Description    : if true, includes atoms bonded to boundary atoms

    Returns
    -------
    c
        Python type    : dict
        Fortran type   : graphical_crystal_type
        Fortran intent : inout
        Description    : atoms, magnetic moments and bonds inside limits
    """

    c = crysfml08lib.f_set_boundary(crystal,coor,limits,boundary_coordination)[0]
    return c

def set_crystal_coordination(crystal : dict,**kwargs):
    """
    Computes the coordination of atoms in a crystal

    Parameters
    ----------
    crystal
        Python type    : dict
        Fortran type   : crystal_type
        Fortran intent : inout
        Description    : crystal object
    kwargs:
        dmin
            Python type    : np.ndarray
            Fortran type   : real
            Fortran intent : in
            Description    : minimum distance between species i,j of the asymmetric unit (nasu,nasu)
        dmax
            Python type    : np.ndarray
            Fortran type   : real
            Fortran intent : in
            Description    : maximum distance between species i,j of the asymmetric unit (nasu,nasu)

    Returns
    -------
    coor
        Python type    : dict
        Fortran type   : coordination_crystal_type
        Fortran intent : inout
        Description    : crystal coordination
    """

    if kwargs:
        coor = crysfml08lib.f_set_crystal_coordination(crystal,kwargs)[0]
    else:
        coor = crysfml08lib.f_set_crystal_coordination(crystal)[0]
    return coor

def set_mask_atoms(id_atom : np.ndarray,id_to_hide : np.ndarray):
    """
    Set the mask array for CrystalView

    Parameters
    ----------
    id_atom
        Python type    : np.ndarray
        Fortran type   : integer
        Fortran intent : in
        Description    : index referring to an atom in the asymmetric unit
    id_to_hide
        Python type    : np.ndarray
        Fortran type   : integer
        Fortran intent : in
        Description    : index referring to an atom in the asymmetric unit

    Returns
    -------
    mask_array
        Python type    : np.ndarray
        Fortran type   : integer
        Fortran intent : inout
        Description    : mask array: 0: invisible, 1: visible
    """

    mask_array = crysfml08lib.f_set_mask_atoms(id_atom,id_to_hide)[0]
    return mask_array

def structure_factors_from_cif(cif_file : str,**kwargs):
    """
    Computes structure factors from a cif file

    Parameters
    ----------
    cif_file
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : cif or mcif file
    kwargs:
        radiation
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : x-rays: "xra" | neutrons: "neu" | electrons: "ele"
        lambda
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : wavelength
        sintlmin
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : minimum value of sin(theta) / lambda
        sintlmax
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : maximum value of sin(theta) / lambda
        unique
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : generate only unique reflections
        friedel
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if true, apply friedel law

    Returns
    -------
    hkl
        Python type    : dict
        Fortran type   : reflist_type
        Fortran intent : inout
        Description    : reflection list
    """

    if kwargs:
        hkl = crysfml08lib.f_structure_factors_from_cif(cif_file,kwargs)[0]
    else:
        hkl = crysfml08lib.f_structure_factors_from_cif(cif_file)[0]
    return hkl

def tof_powder_pattern_from_dict(json : dict):
    """
    Computes a powder pattern from information provided by a
    dictionary. This subroutine was introduced mainly for
    Python applications. Python can easily transform a json file
    into a dictionary.

    Parameters
    ----------
    json
        Python type    : dict
        Fortran type   : dict
        Fortran intent : inout
        Description    : json file content

    Returns
    -------
    xc
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : out
        Description    : two theta angle
    yc
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : out
        Description    : calculated intensity
    """

    xc,yc = crysfml08lib.f_tof_powder_pattern_from_dict(json)
    return xc,yc

def update_global_phase(global_phase : float,crystal : dict,c : dict):
    """
    Update global phase of an incommensurate structure

    Parameters
    ----------
    global_phase
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : global_phase
    crystal
        Python type    : dict
        Fortran type   : crystal_type
        Fortran intent : in
        Description    : crystal object
    c
        Python type    : dict
        Fortran type   : graphical_crystal_type
        Fortran intent : inout
        Description    : atoms, magnetic moments and bonds inside limits
    """

    crysfml08lib.f_update_global_phase(global_phase,crystal,c)

def calculate_laue_image():
    """
    Calculate visible reflections

    Returns
    -------
    laue_img
        Python type    : dict
        Fortran type   : esmeralda_laue_image_type
        Fortran intent : inout
        Description    : laue img
    """

    laue_img = crysfml08lib.f_calculate_laue_image()[0]
    return laue_img

def set_instrument_esmeralda(name : str,dtype : str,d : float,h : float,v : float,np_h : int,np_v : int,xo : float,zo : float,ga_d : float,nu_d : float,l_min : float,l_max : float,x_min : float,x_max : float,z_min : float,z_max : float,gan_min : float,gan_max : float,gap_min : float,gap_max : float,nu_min : float,nu_max : float):
    """
    Set the Laue instrument for Esmeralda

    Parameters
    ----------
    name
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : instrument name
    dtype
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : detector type: 'rec' | 'cyl'
    d
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : sample-detector distance (mm)
    h
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : horizontal detector size (mm)
    v
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : vertical   detector size (mm)
    np_h
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : number of horizontal pixels
    np_v
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : number of vertical   pixels
    xo
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : coordinate x in pixels for detector origin
    zo
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : coordinate z in pixels for detector origin
    ga_d
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : angle gamma in degrees for detector origin
    nu_d
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : angle nu    in degrees for detector origin
    l_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : lambda min
    l_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : lambda max
    x_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : x min (mm)
    x_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : x max (mm)
    z_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : z min (mm)
    z_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : z max (mm)
    gan_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : negative gamma min (degrees)
    gan_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : negative gamma max (degrees)
    gap_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : positive gamma min (degrees)
    gap_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : positive gamma max (degrees)
    nu_min
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : nu min (degrees)
    nu_max
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : nu max (degrees)
    """

    crysfml08lib.f_set_instrument_esmeralda(name,dtype,d,h,v,np_h,np_v,xo,zo,ga_d,nu_d,l_min,l_max,x_min,x_max,z_min,z_max,gan_min,gan_max,gap_min,gap_max,nu_min,nu_max)

def set_spg_esmeralda(spg_id : str):
    """
    Set the space group for Esmeralda.

    Parameters
    ----------
    spg_id
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : string specifying the space group to be built
    """

    crysfml08lib.f_set_spg_esmeralda(spg_id)

def set_ub_esmeralda(ub : np.ndarray):
    """
    Set the UB matrix for Esmeralda

    Parameters
    ----------
    ub
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : in
        Description    : not documented yet
    """

    crysfml08lib.f_set_ub_esmeralda(ub)