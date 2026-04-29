from . import crysfml08lib
import numpy as np

def automatic_peak_background_search(pat : dict,x1 : float,x2 : float,mode : str):
    """
    Returns peak positions, intensities and background points in a region [x1,x2]
    of the diffraction / spectrum pattern.

    Parameters
    ----------
    pat
        Python type    : dict
        Fortran type   : diffpat_e_type
        Fortran intent : in
        Description    : diffraction pattern
    x1
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : minimum value of x
    x2
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : maximum value of x
    mode
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : 'peak' | 'background' | 'satellites'

    Returns
    -------
    pkb
        Python type    : dict
        Fortran type   : pkb_type
        Fortran intent : inout
        Description    : not documented yet
    """

    pkb = crysfml08lib.f_automatic_peak_background_search(pat,x1,x2,mode)[0]
    return pkb

def get_pkb_conditions():
    """
    Description not yet available

    Returns
    -------
    pkbc
        Python type    : dict
        Fortran type   : peak_search_cond_type
        Fortran intent : inout
        Description    : not documented yet
    """

    pkbc = crysfml08lib.f_get_pkb_conditions()[0]
    return pkbc

def set_pkb_conditions(**kwargs):
    """
    Set parameters used in the automatic detection of peaks and background

    Parameters
    ----------
    kwargs:
        pk_th
            Python type    : float
            Fortran type   : real
            Fortran intent : inout
            Description    : peak threshold
        sh_th
            Python type    : float
            Fortran type   : real
            Fortran intent : inout
            Description    : shoulder threshold
        bg_th
            Python type    : float
            Fortran type   : real
            Fortran intent : inout
            Description    : background threshold
        peak_kind
            Python type    : int
            Fortran type   : integer
            Fortran intent : inout
            Description    : 1: single peak | 2: doublet (cu-ka) | 3: doublet (mo-ka) | 4: doublet (co-ka)
        iter
            Python type    : int
            Fortran type   : integer
            Fortran intent : inout
            Description    : number of iterations
    """

    if kwargs:
        crysfml08lib.f_set_pkb_conditions(kwargs)
    else:
        crysfml08lib.f_set_pkb_conditions()