from . import crysfml08lib
import numpy as np

def get_maxnumref(sintlmax : float,volcell : float,**kwargs):
    """
    Provides un upper limit of the expected maximum number of
    reflections up to SinTLMax for a volume VolCell of the
    primitive cell. If SinTLMin is given, the result is the
    number of reflections in the interval [SinTLMin,SinTLMax].
    If Mult is provided the result is divided by half this multiplicity,
    so we obtain an estimation of the expected mumber of unique reflections.

    Parameters
    ----------
    sintlmax
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : maximum value of sin(theta) / lambda
    volcell
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : volume of the primitive cell
    kwargs:
        sintlmin
            Python type    : float
            Fortran type   : real
            Fortran intent : in
            Description    : minimum value of sin(theta) / lambda
        mult
            Python type    : int
            Fortran type   : integer
            Fortran intent : in
            Description    : multiplicity

    Returns
    -------
    numref
        Python type    : int
        Fortran type   : integer
        Fortran intent : inout
        Description    : number of reflections
    """

    if kwargs:
        numref = crysfml08lib.f_get_maxnumref(sintlmax,volcell,kwargs)[0]
    else:
        numref = crysfml08lib.f_get_maxnumref(sintlmax,volcell)[0]
    return numref

def generate_reflections(cell : dict,slmin : float,slmax : float,spg : dict,**kwargs):
    """
    Calculate reflections between the sin_theta/lambda shells defined by (Slmin,Slmax)
    Valid for all type of space groups.

    Parameters
    ----------
    cell
        Python type    : dict
        Fortran type   : cell_g_type
        Fortran intent : in
        Description    : unit cell object
    slmin
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : minimum sintheta/lambda
    slmax
        Python type    : float
        Fortran type   : real
        Fortran intent : in
        Description    : maximum sintheta/lambda
    spg
        Python type    : dict
        Fortran type   : spg_type
        Fortran intent : in
        Description    : general space group
    kwargs:
        magext
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : magnetic extinctions if magext = true
        kinfo
            Python type    : dict
            Fortran type   : kvect_info_type
            Fortran intent : in
            Description    : modulation vector information
        order
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if true, reflections ordered by increasing sintheta/lambda
        unique
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : ordered unique reflections are generated
        seqindx
            Python type    : np.ndarray
            Fortran type   : integer
            Fortran intent : in
            Description    : sequence of indices change
        hlim
            Python type    : np.ndarray
            Fortran type   : integer
            Fortran intent : in
            Description    : index limits
        mag_only
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : only magnetic reflections are generated
        friedel
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : apply friedel law if true
        ref_typ
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : reflection type: refl | srefl | mrefl

    Returns
    -------
    reflex
        Python type    : dict
        Fortran type   : reflist_type
        Fortran intent : inout
        Description    : reflection list
    """

    if kwargs:
        reflex = crysfml08lib.f_generate_reflections(cell,slmin,slmax,spg,kwargs)[0]
    else:
        reflex = crysfml08lib.f_generate_reflections(cell,slmin,slmax,spg)[0]
    return reflex

def init_reflist(n : int,ctype : str,**kwargs):
    """
    Returns a reflection list object

    Parameters
    ----------
    n
        Python type    : int
        Fortran type   : integer
        Fortran intent : in
        Description    : number of reflections in the list
    ctype
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : reflection type: refl | srefl | mrefl
    kwargs:
        d
            Python type    : int
            Fortran type   : integer
            Fortran intent : in
            Description    : dimension of the magnetic hkl

    Returns
    -------
    reflex
        Python type    : dict
        Fortran type   : reflist_type
        Fortran intent : inout
        Description    : reflection list
    """

    if kwargs:
        reflex = crysfml08lib.f_init_reflist(n,ctype,kwargs)[0]
    else:
        reflex = crysfml08lib.f_init_reflist(n,ctype)[0]
    return reflex