from . import crysfml08lib
import numpy as np

def set_spacegroup_from_dbase(mystr : str,mode : str,**kwargs):
    """
    Set a Shubnikov or a superspacegroup from the database

    Parameters
    ----------
    mystr
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : string specifying the shubnikov or superspace group
    mode
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : type of group: 'shubn' | 'super'
    kwargs:
        xyz_type
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : coordinates notation: 'xyz' (default) | 'abc' | 'x1x2x3'
        setting
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : setting transformation
        keepdb
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if not given, deallocate the database after the group generation
        parent
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : if given, set the parent group symbol
        database_path
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : full path of the database file
        trn_to
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if given, transformation matrices between standard and new setting (if given) are computed

    Returns
    -------
    spaceg
        Python type    : dict
        Fortran type   : spg_type
        Fortran intent : inout
        Description    : shubnikov or superspace group
    """

    if kwargs:
        spaceg = crysfml08lib.f_set_spacegroup_from_dbase(mystr,mode,kwargs)[0]
    else:
        spaceg = crysfml08lib.f_set_spacegroup_from_dbase(mystr,mode)[0]
    return spaceg

def set_spacegroup_from_generators(mystr : str,**kwargs):
    """
    Set a Shubnikov or a superspacegroup from a list of generators

    Parameters
    ----------
    mystr
        Python type    : str
        Fortran type   : character
        Fortran intent : in
        Description    : generators list separated by semicolons: "x,-y,z,t,1;x,y,z,t+1/2,-1"
    kwargs:
        set_inv
            Python type    : bool
            Fortran type   : logical
            Fortran intent : in
            Description    : if given, the pointer to the inverse of each operator will be set
        database_path
            Python type    : str
            Fortran type   : character
            Fortran intent : in
            Description    : full path of the database file

    Returns
    -------
    spaceg
        Python type    : dict
        Fortran type   : spg_type
        Fortran intent : inout
        Description    : shubnikov or superspace group
    """

    if kwargs:
        spaceg = crysfml08lib.f_set_spacegroup_from_generators(mystr,kwargs)[0]
    else:
        spaceg = crysfml08lib.f_set_spacegroup_from_generators(mystr)[0]
    return spaceg