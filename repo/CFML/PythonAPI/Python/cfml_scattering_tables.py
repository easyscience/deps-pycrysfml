from . import crysfml08lib
import numpy as np

def get_abs_xs(symb : str):
    """
    Returns the absorption cross-section (barns, energy = 0.025 eV)
    of a chemical element. If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    u
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : absorption cross-section
    """

    u = crysfml08lib.f_get_abs_xs(symb)[0]
    return u

def get_anomalous_scfac(r : int):
    """
    Returns the row r from the anomalous_scfac table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : anomalous_sc_type
        Fortran intent : inout
        Description    : anomalous scattering factors
    """

    info = crysfml08lib.f_get_anomalous_scfac(r)[0]
    return info

def get_atomic_mass(symb : str):
    """
    Returns the atomic mass (amu) of a chemical element.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    mass
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : atomic mass
    """

    mass = crysfml08lib.f_get_atomic_mass(symb)[0]
    return mass

def get_atomic_vol(symb : str):
    """
    Returns the atomic volume (cm3/mol) of a chemical element.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    vol
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : atomic volume
    """

    vol = crysfml08lib.f_get_atomic_vol(symb)[0]
    return vol

def get_chem_info(r : int):
    """
    Returns the row r of the chemical info table.

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : chem_info_type
        Fortran intent : inout
        Description    : chemical info
    """

    info = crysfml08lib.f_get_chem_info(r)[0]
    return info

def get_chem_symb(label : str):
    """
    Returns the chemical symbol of an element from a label.

    Parameters
    ----------
    label
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : label

    Returns
    -------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : inout
        Description    : atomic symbol
    """

    symb = crysfml08lib.f_get_chem_symb(label)[0]
    return symb

def get_covalent_radius(symb : str):
    """
    Returns the covalent radius (angstroms) of a chemical element.
    If an error ocurred, it returns 1.4.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    rad
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : atomic radius
    """

    rad = crysfml08lib.f_get_covalent_radius(symb)[0]
    return rad

def get_fermi_length(symb : str):
    """
    Returns the Fermi length (10^-12 cm)  of a chemical element.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    b
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : fermi length
    """

    b = crysfml08lib.f_get_fermi_length(symb)[0]
    return b

def get_inc_xs(symb : str):
    """
    Returns the incoherent scattering neutron cross section (barns) of a chemical element.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol

    Returns
    -------
    u
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : incoherent scattering neutron cross-section
    """

    u = crysfml08lib.f_get_inc_xs(symb)[0]
    return u

def get_ionic_radius(symb : str,valence : int):
    """
    Returns the ionic radius (angstroms) of an ion.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic symbol
    valence
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : valence

    Returns
    -------
    rad
        Python type    : float
        Fortran type   : real
        Fortran intent : inout
        Description    : ionic radius
    """

    rad = crysfml08lib.f_get_ionic_radius(symb,valence)[0]
    return rad

def get_magnetic_form(r : int):
    """
    Returns the row r from the magnetic_form table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : magnetic_form_type
        Fortran intent : inout
        Description    : magnetic form factors
    """

    info = crysfml08lib.f_get_magnetic_form(r)[0]
    return info

def get_magnetic_j2(r : int):
    """
    Returns the row r from the magnetic_j2 table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : magnetic_form_type
        Fortran intent : inout
        Description    : magnetic form factors j2
    """

    info = crysfml08lib.f_get_magnetic_j2(r)[0]
    return info

def get_magnetic_j4(r : int):
    """
    Returns the row r from the magnetic_j2 table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : magnetic_form_type
        Fortran intent : inout
        Description    : magnetic form factors j4
    """

    info = crysfml08lib.f_get_magnetic_j4(r)[0]
    return info

def get_magnetic_j6(r : int):
    """
    Returns the row r from the magnetic_j2 table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : magnetic_form_type
        Fortran intent : inout
        Description    : magnetic form factors j6
    """

    info = crysfml08lib.f_get_magnetic_j6(r)[0]
    return info

def get_z_symb(symb : str):
    """
    Returns the atomic number of a chemical element.
    If an error ocurred, it returns 0.

    Parameters
    ----------
    symb
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : atomic label

    Returns
    -------
    z
        Python type    : int
        Fortran type   : integer
        Fortran intent : inout
        Description    : atomic number
    """

    z = crysfml08lib.f_get_z_symb(symb)[0]
    return z

def get_xray_form(r : int):
    """
    Returns the row r from the xray_form table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : xray_form_type
        Fortran intent : inout
        Description    : xray form factors
    """

    info = crysfml08lib.f_get_xray_form(r)[0]
    return info

def get_xray_wavelengths(r : int):
    """
    Returns the row r from the xray_form table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : row

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : xray_wavelength_type
        Fortran intent : inout
        Description    : xray form factors
    """

    info = crysfml08lib.f_get_xray_wavelengths(r)[0]
    return info