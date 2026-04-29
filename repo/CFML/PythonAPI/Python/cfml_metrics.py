from . import crysfml08lib
import numpy as np

def set_cell(vcell : np.ndarray,vang : np.ndarray,**kwargs):
    """
    Set crystal cell from cell parameters

    Parameters
    ----------
    vcell
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : in
        Description    : cell parameters a,b,c
    vang
        Python type    : np.ndarray
        Fortran type   : real
        Fortran intent : in
        Description    : cell parameters alpha,beta,gamma
    kwargs:
        carttype
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : cartesian coordinate system: 'ca' | 'bc' | 'ba' | 'cb'
        vscell
            Python type    : np.ndarray
            Fortran type   : real
            Fortran intent : in
            Description    : standard deviations of vcell
        vsang
            Python type    : np.ndarray
            Fortran type   : real
            Fortran intent : in
            Description    : standard deviations of vang

    Returns
    -------
    cell
        Python type    : dict
        Fortran type   : cell_g_type
        Fortran intent : inout
        Description    : crystal cell
    """

    if kwargs:
        cell = crysfml08lib.f_set_cell(vcell,vang,kwargs)[0]
    else:
        cell = crysfml08lib.f_set_cell(vcell,vang)[0]
    return cell