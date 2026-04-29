from . import crysfml08lib
import numpy as np

def get_shubnikov_info(r : int):
    """
    Returns the row r from the shubnikov_info table

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
        Fortran type   : shub_spgr_info_type
        Fortran intent : inout
        Description    : tabulated data of a shubnikov group
    """

    info = crysfml08lib.f_get_shubnikov_info(r)[0]
    return info

def get_spgr_info(r : int):
    """
    Returns the row r from the spgr_info table

    Parameters
    ----------
    r
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : not documented yet

    Returns
    -------
    info
        Python type    : dict
        Fortran type   : spgr_info_type
        Fortran intent : inout
        Description    : not documented yet
    """

    info = crysfml08lib.f_get_spgr_info(r)[0]
    return info