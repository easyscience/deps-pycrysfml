from . import crysfml08lib
import numpy as np

def load_pattern(filename : str,**kwargs):
    """
    Read a diffraction pattern from a file.

    Parameters
    ----------
    filename
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : full path of the file
    kwargs:
        mode
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : mode: cif | dmc | d1a | d1aold | d1b | d2b | d2bold | d20 | gsas | gsastof | g41 | g42 | hrpt | panalytical | nls |  socabim | timevariable | xysigma | 3t2
        sig
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if sig is false, pat%sigma will be the standard deviation. otherwise, it will be the variance.
        header
            Python type    : str
            Fortran type   : character
            Fortran intent : out
            Description    : header

    Returns
    -------
    pat
        Python type    : dict
        Fortran type   : diffpat_e_type
        Fortran intent : inout
        Description    : diffraction pattern
    """

    if kwargs:
        pat = crysfml08lib.f_load_pattern(filename,kwargs)[0]
    else:
        pat = crysfml08lib.f_load_pattern(filename)[0]
    return pat