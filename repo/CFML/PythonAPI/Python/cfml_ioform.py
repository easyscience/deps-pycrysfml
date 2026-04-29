from . import crysfml08lib
import numpy as np

def read_xtal_structure(filenam : str,**kwargs):
    """
    Description not yet available

    Parameters
    ----------
    filenam
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : not documented yet
    kwargs:
        atm_typ
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : not documented yet
        mgp
            Python type    : dict
            Fortran type   : magsymm_k_type
            Fortran intent : out
            Description    : not documented yet
        matm
            Python type    : dict
            Fortran type   : matom_list_type
            Fortran intent : out
            Description    : not documented yet
        mag_dom
            Python type    : dict
            Fortran type   : magnetic_domain_type
            Fortran intent : out
            Description    : not documented yet
        iphase
            Python type    : int
            Fortran type   : integer
            Fortran intent : in
            Description    : not documented yet
        ftype
            Python type    : dict
            Fortran type   : file_type
            Fortran intent : out
            Description    : not documented yet
        filelist
            Python type    : dict
            Fortran type   : file_list_type
            Fortran intent : out
            Description    : not documented yet
        database_path
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : not documented yet

    Returns
    -------
    cell
        Python type    : dict
        Fortran type   : cell_g_type
        Fortran intent : out
        Description    : not documented yet
    spg
        Python type    : dict
        Fortran type   : spg_type
        Fortran intent : out
        Description    : not documented yet
    atm
        Python type    : dict
        Fortran type   : atlist_type
        Fortran intent : out
        Description    : not documented yet
    """

    if kwargs:
        cell,spg,atm = crysfml08lib.f_read_xtal_structure(filenam,kwargs)
    else:
        cell,spg,atm = crysfml08lib.f_read_xtal_structure(filenam)
    return cell,spg,atm