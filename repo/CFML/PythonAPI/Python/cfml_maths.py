from . import crysfml08lib
import numpy as np

def set_eps_math(**kwargs):
    """
    Description not yet available

    Parameters
    ----------
    kwargs:
        neweps
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : not documented yet
    """

    if kwargs:
        crysfml08lib.f_set_eps_math(kwargs)
    else:
        crysfml08lib.f_set_eps_math()