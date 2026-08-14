"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
get_def_pointer_for_ndarray(v)
get_def_var_array_unwrap(v)
get_def_var_scalar_unwrap(v)
get_number_of_arguments(p)
get_uses_crysfml08lib(modules)
wrap(buid_dir : str,modules : dict,lucy : dict,classes : list)
write_crysfml08lib(build_dir : str,modules : dict,lucy : dict,classes : list)
write_def_var_args(f)
write_def_var_in_args_ptr(p,f)
write_def_var_in_ret(p,f)
write_def_var_local(p,f)
write_def_var_unwraps(p,f)
write_docu_args(args,f)
write_docu_ret(args,f)
write_error_check(p,f)
write_fortran_call(p,f)
write_function_init(modules : dict,f)
write_function_pyinit(f)
write_header_crysfml08lib(modules : dict,f)
write_procedures_crysfml08lib(modules : dict,lucy : dict,classes : list,f)
write_python_module(build_dir : str,m : cfml_objects.Module)
write_python_function(m : cfml_objects.Module,p : str,docu : list,f)
write_return_section(p,f)
write_unwrapping_section_mandatory(p,f)
write_unwrapping_section_optionals(p,c,f,n)
write_uses(uses : list,f)
write_wrapping_function(m,p,lucy,c,f)
write_wrapping_initialization(f)
write_wrapping_section(p,lucy,f)
"""
import cfml_objects
import os
import sys

PRIMITIVES = ['integer','real','logical','character','complex','list','dict']
NUMERICALS = ['integer','real','rational','complex']
LEN_DEFAULT = 256
PROCS_TO_WRAP = {
'CFML_BckPeaks':['automatic_peak_background_search',
                 'get_pkb_conditions',
                 'set_pkb_conditions'],
'CFML_DiffPatt':['load_pattern'],
'CFML_gSpaceGroups':['set_spacegroup_from_dbase',
                     'set_spacegroup_from_generators'],
'CFML_IOForm':['read_xtal_structure'],
'CFML_Maths':['set_eps_math'],
'CFML_Metrics':['set_cell'],
'CFML_Py_Utilities': ['calculate_laue_image',
                      'cw_powder_pattern_from_dict',
                      'magnetic_structure_factors_from_mcif',
                      'patterns_simulation',
                      'read_crystal_structure',
                      'set_boundary',
                      'set_crystal_coordination',
                      'set_instrument_esmeralda',
                      'set_mask_atoms',
                      'set_spg_esmeralda',
                      'set_ub_esmeralda',
                      'structure_factors_from_cif',
                      'tof_powder_pattern_from_dict',
                      'update_global_phase'],
'CFML_Reflections':['generate_reflections',
                    'get_maxnumref',
                    'init_reflist'],
'CFML_Scattering_Tables':['get_abs_xs',
                          'get_anomalous_scfac',
                          'get_atomic_mass',
                          'get_atomic_vol',
                          'get_chem_info',
                          'get_chem_symb',
                          'get_covalent_radius',
                          'get_fermi_length',
                          'get_inc_xs',
                          'get_ionic_radius',
                          'get_magnetic_form',
                          'get_magnetic_j2',
                          'get_magnetic_j4',
                          'get_magnetic_j6',
                          'get_xray_form',
                          'get_xray_wavelengths',
                          'get_z_symb'],
'CFML_Symmetry_Tables':['get_shubnikov_info',
                        'get_spgr_info']}

def get_def_pointer_for_ndarray(v):
    """
    Build the Fortran statement for definition of a pointer
    for a ndarray.

    Parameters
    ----------
    var : cfml_objects.FortranVar
          Fortran variable

    Returns
    -------
    p_def : str
            pointer statement
    """
    if v.ftype == 'integer' or v.ftype == 'real' or v.ftype == 'complex':
        p_def = v.ftype
        if v.kind:
            p_def = p_def + '(kind=' + v.kind + ')'
    else:
        print(f'Error! Variable {v.name} is not a np.ndarray')
        sys.exit()
    dim = [':' for i in range(v.ndim)]
    p_def = p_def + ', dimension(' + ','.join(dim) + '), pointer'
    return p_def

def get_def_var_array_unwrap(v):
    """
    Build the Fortran statement for definition of an array variable v
    that must be unwrapped

    Parameters
    ----------
    var : cfml_objects.FortranVar
          Fortran variable

    Returns
    -------
    var_def : str
        Fortran statement
    """
    if v.ftype == 'integer' or v.ftype == 'real' or v.ftype == 'logical' or v.ftype == 'complex':
        var_def = v.ftype
        if v.kind:
            var_def = var_def + '(kind=' + v.kind + ')'
    elif v.ftype == 'character':
        if v.len.isdigit():
            var_def = v.ftype +'(len='+v.len+')'
        else:
            var_def = v.ftype +f'(len={LEN_DEFAULT})'
    elif v.is_class:
        var_def = 'class('+v.ftype+')'
    else:
        var_def = 'type('+v.ftype+')'
    check = [d.isdigit() for d in v.dim]
    if False in check:
        dim = [':' for i in range(v.ndim)]
    else:
        dim = v.dim.copy()
        if v.is_class:
            print('Error! A class array must have undefined dimensions')
            sys.exit()
    var_def = var_def + ', dimension(' + ','.join(dim) + ')'
    if ':' in dim:
        var_def = var_def + ', allocatable'
    if v.optional:
        var_def = var_def + ', target'
    return var_def

def get_def_var_scalar_unwrap(v):
    """
    Build the Fortran statement for definition of a scalar variable v
    that must be unwrapped

    Parameters
    ----------
    var : cfml_objects.FortranVar
          Fortran variable

    Returns
    -------
    var_def : str
        Fortran statement
    """
    if v.ftype in PRIMITIVES:
        return None
    elif v.is_class:
        var_def = 'class('+v.ftype+'), allocatable'
    else:
        var_def = 'type('+v.ftype+')'
    if v.optional:
        var_def = var_def + ', target'
    return var_def

def get_uses_crysfml08lib(modules):
    """
    Determine all the modules used in crysfml08lib

    Parameters
    ----------
    modules : dict
              modules of CrysFML08

    Returns
    -------
    uses : list
           list containing the modules used
    """
    uses = []
    for m in modules:
        use = m.lower()
        if use not in uses:
            uses.append(use)
        for u in modules[m].uses:
            try:
                use = u.split(',')[0].strip().split()[1].lower()
                if use in uses:
                    cycle
                else:
                    uses.append(use)
            except:
                pass
    uses.sort()
    return uses

def set_number_of_arguments(p):

    p.nwarg = 0
    p.nwman = 0
    p.nwopt = 0
    d = p.arguments.copy()
    for name in d:
        if name in p.warguments:
            p.nwarg = p.nwarg + 1
            a = d[name]
            if a.optional:
                p.nwopt = p.nwopt + 1
            else:
                p.nwman = p.nwman + 1

def wrap(build_dir : str,modules : dict,lucy : dict,classes : list):
    """
    Build the wrapper of CrysFML08

    Parameters
    ----------
    build_dir     : str
                    building directory
    modules       : dict
                    CrysFML08 modules
    lucy          : dict
                    base class for every CrysFML08 type
    classes       : list
                    CrysFML08 classes
    """
    write_crysfml08lib(build_dir,modules,lucy,classes)
    for m in modules:
        if m not in PROCS_TO_WRAP.keys(): # do not wrap module CFML_Strings
            continue
        write_python_module(build_dir,modules[m])
    # Create an empty __init__.py
    with open(os.path.join(build_dir,'Python','__init__.py'),'w') as f:
        pass

def write_crysfml08lib(build_dir : str,modules : dict,lucy : dict,
    classes : list):
    """
    Write the module crysfml08lib.f90

    Parameters
    ----------
    build_dir : str
                building directory
    modules   : dict
                CrysFML08 modules
    lucy      : dict
                base class for every CrysFML08 type
    classes   : list
                CrysFML08 classes
    """
    fname = os.path.join(build_dir,'Fortran','crysfml08lib.f90')
    with open(fname,'w') as f:
        f.write(f"module crysfml08lib")
        # Uses
        uses = get_uses_crysfml08lib(modules)
        write_uses(uses,f)
        # Header
        write_header_crysfml08lib(modules,f)
        # Procedures
        write_procedures_crysfml08lib(modules,lucy,classes,f)
        f.write(f"\n\nend module crysfml08lib")

def write_def_var_args(f):
    """
    Write the arguments of the wrapping function.
    They are the same for all functions.

    Parameters
    ----------
    f : _io.TextIOWrapper
        file
    """
    f.write(f"\n\n{'':>8}! Arguments")
    f.write(f"\n{'':>8}type(c_ptr), value :: self_ptr")
    f.write(f"\n{'':>8}type(c_ptr), value :: args_ptr")
    f.write(f"\n{'':>8}type(c_ptr)        :: resul")

def write_def_var_in_args_ptr(p,f):
    """
    Write the variables that should be unwrapped.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        Fortran procedure
    f : _io.TextIOWrapper
        file
    """
    f.write(f"\n\n{'':>8}! Variables in args_ptr")
    d = p.arguments.copy()
    for name in d:
        if name in p.warguments:
            a = d[name]
            if a.ptype == 'int':
                if a.optional:
                    if a.kind:
                        f.write(f"\n{'':>8}integer(kind={a.kind}), target :: "\
                            f"{a.name}")
                        f.write(f"\n{'':>8}integer(kind={a.kind}), pointer ::"\
                            f" ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}integer, target :: {a.name}")
                        f.write(f"\n{'':>8}integer, pointer :: ptr_{a.name}"\
                            f" => null()")
                else:
                    if a.kind:
                        f.write(f"\n{'':>8}integer(kind={a.kind}), target ::"\
                            f" {a.name}")
                    else:
                        f.write(f"\n{'':>8}integer, target :: {a.name}")
            elif a.ptype == 'float':
                if a.optional:
                    if a.kind:
                        f.write(f"\n{'':>8}real(kind={a.kind}), target :: "\
                            f"{a.name}")
                        f.write(f"\n{'':>8}real(kind={a.kind}), pointer :: "\
                            f"ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}real, target :: {a.name}")
                        f.write(f"\n{'':>8}real, pointer :: "\
                            f"ptr_{a.name} => null()")
                else:
                    if a.kind:
                        f.write(f"\n{'':>8}real(kind={a.kind}) :: {a.name}")
                    else:
                        f.write(f"\n{'':>8}real :: {a.name}")
            elif a.ptype == 'complex':
                if a.optional:
                    if a.kind:
                        f.write(f"\n{'':>8}complex(kind={a.kind}), "\
                            f"target :: {a.name}")
                        f.write(f"\n{'':>8}complex(kind={a.kind}), "\
                            f"pointer :: ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}complex, target :: {a.name}")
                        f.write(f"\n{'':>8}complex, pointer :: "\
                            f"ptr_{a.name} => null()")
                else:
                    if a.kind:
                        f.write(f"\n{'':>8}complex(kind={a.kind}) :: {a.name}")
                    else:
                        f.write(f"\n{'':>8}complex :: {a.name}")
            elif a.ptype == 'bool':
                if a.optional:
                    f.write(f"\n{'':>8}logical, target :: {a.name}")
                    f.write(f"\n{'':>8}logical, pointer :: ptr_{a.name}"\
                        f" => null()")
                else:
                    f.write(f"\n{'':>8}logical :: {a.name}")
            elif a.ptype == 'str':
                if a.optional:
                    f.write(f"\n{'':>8}character(len=:), allocatable, "\
                        f"target :: {a.name}")
                    f.write(f"\n{'':>8}character(len=:), pointer :: "\
                        f"ptr_{a.name} => null()")
                else:
                    f.write(f"\n{'':>8}character(len=:), allocatable "\
                        f":: {a.name}")
            elif a.ptype == 'np.ndarray':
                if a.ftype == 'np.ndarray':
                    if a.optional:
                        f.write(f"\n{'':>8}type(ndarray), target :: nd_{a.name}")
                        f.write(f"\n{'':>8}type(ndarray), pointer :: "\
                            f"ptr_nd_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}type(ndarray) :: nd_{a.name}")
                else:
                    f.write(f"\n{'':>8}type(ndarray) :: nd_{a.name}")
                    if a.optional:
                        var_def = a.ftype
                        if a.kind:
                            var_def = var_def + '(kind=' + a.kind + ')'
                        var_def = var_def + ', dimension('
                        for i in range(a.ndim):
                            if i > 0:
                                var_def = var_def + ','
                            var_def = var_def + ':'
                        var_def = var_def + ')'
                        f.write(f"\n{'':>8}{var_def}"\
                            f", pointer :: ptr_{a.name} => null()")
            elif a.ptype == 'dict':
                if a.ftype == 'dict':
                    if a.optional:
                        f.write(f"\n{'':>8}type(dict), target :: {a.name}")
                        f.write(f"\n{'':>8}type(dict), pointer :: "\
                            f"ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}type(dict) :: {a.name}")
                else:
                    if a.optional:
                        f.write(f"\n{'':>8}type(dict) :: "\
                            f"di_{a.name}")
                        f.write(f"\n{'':>8}type({a.ftype}), pointer :: "\
                            f"ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}type(dict) :: di_{a.name}")
            elif a.ptype == 'list':
                if a.ftype == 'list':
                    if a.optional:
                        f.write(f"\n{'':>8}type(list), target :: {a.name}")
                        f.write(f"\n{'':>8}type(list), pointer :: "\
                            f"ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}type(list) :: {a.name}")
                else:
                    if a.optional:
                        f.write(f"\n{'':>8}type(list) :: li_{a.name}")
                        f.write(f"\n{'':>8}type(list), pointer :: "\
                            f"ptr_{a.name} => null()")
                    else:
                        f.write(f"\n{'':>8}type(list) :: li_{a.name}")
        else:
            i = name.find('_type_spec')
            if i > -1:
                if a.optional:
                    f.write(f"\n{'':>8}character(len=:), allocatable,"\
                        f"target :: {name}")
                    f.write(f"\n{'':>8}character(len=:), pointer"\
                        f" :: ptr_{name} => null()")
                else:
                    f.write(f"\n{'':>8}character(len=:), allocatable :: {name}")
    if p.has_optionals:
        f.write(f"\n{'':>8}type(dict) :: di_kwargs")

def write_def_var_in_ret(p,f):
    """
    Write the variables that should be in the returned tuple.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        Fortran procedure
    f : _io.TextIOWrapper
        file
    """
    f.write(f"\n\n{'':>8}! Variables in returned tuple")
    d = p.arguments.copy()
    if isinstance(p,cfml_objects.Function):
        d[p.xreturn.name] = p.xreturn
    for name in d:
        if name in p.wreturn:
            a = d[name]
            if a.ptype == 'int':
                if a.kind:
                    f.write(f"\n{'':>8}integer(kind={a.kind}) :: {a.name}")
                else:
                    f.write(f"\n{'':>8}integer :: {a.name}")
            elif a.ptype == 'float':
                if a.kind:
                    f.write(f"\n{'':>8}real(kind={a.kind}) :: {a.name}")
                else:
                    f.write(f"\n{'':>8}real :: {a.name}")
            elif a.ptype == 'complex':
                if a.kind:
                    f.write(f"\n{'':>8}complex(kind={a.kind}) :: {a.name}")
                else:
                    f.write(f"\n{'':>8}complex :: {a.name}")
            elif a.ptype == 'bool':
                f.write(f"\n{'':>8}logical :: {a.name}")
            elif a.ptype == 'str':
                f.write(f"\n{'':>8}character(len=:), allocatable :: {a.name}")
            elif a.ptype == 'np.ndarray':
                f.write(f"\n{'':>8}type(ndarray) :: nd_{a.name}")
            elif a.ptype == 'dict':
                f.write(f"\n{'':>8}type(dict) :: di_{a.name}")
            elif a.ptype == 'list':
                f.write(f"\n{'':>8}type(list) :: li_{a.name}")

def write_def_var_local(p,f):
    """
    Write the definition of the local variables of the
    wrapping function.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        Fortran procedure
    f : _io.TextIOWrapper
        file
    """
    is_order = False
    ii_added = False
    for name in p.arguments:
        a = p.arguments[name]
        if a.ptype == 'np.ndarray' and a.ndim > 1:
            is_order = True
            break
    if not is_order and isinstance(p,cfml_objects.Function):
        a = p.xreturn
        if a.ptype == 'np.ndarray' and a.ndim > 1:
            is_order = True
    f.write(f"\n\n{'':>8}! Local parameters")
    f.write(f"\n{'':>8}integer, parameter :: NMANDATORY = {p.nwman}")
    f.write(f"\n\n{'':>8}! Local variables")
    for name in p.wreturn + p.inout:
        a = None
        if name in p.arguments:
            a = p.arguments[name]
        else:
            if isinstance(p,cfml_objects.Function):
                if name == p.xreturn.name:
                    a = p.xreturn
        if a is None:
            raise Exception(f"write_def_var_local: {name} not found"\
                f" in {p.name}")
        if a.ptype == 'list' and a.ftype != 'list':
            if not ii_added:
                f.write(f"\n{'':>8}integer :: ii")
                ii_added = True
            f.write(f"\n{'':>8}type(dict) :: di_{a.name}")
    f.write(f"\n{'':>8}integer :: nargs")
    if p.has_optionals:
        f.write(f"\n{'':>8}integer :: ierror,ierror2")
    else:
        f.write(f"\n{'':>8}integer :: ierror")
    if is_order:
        f.write(f"\n{'':>8}character(len=1) :: array_order")
    f.write(f"\n{'':>8}type(object) :: item")
    if len(p.wreturn) > 0:
        f.write(f"\n{'':>8}type(tuple) :: args,ret")
    else:
        f.write(f"\n{'':>8}type(tuple) :: args")
    f.write(f"\n{'':>8}type(nonetype) :: nret")

def write_def_var_unwraps(v,f):
    """
    Write the definition of the variables that must be unwrapped.

    Parameters
    ----------
    v : cfml_objects.FortranVar
        Fortran variable
    f : _io.TextIOWrapper
        file
    """
    if v.ndim == 0:
        vdef = get_def_var_scalar_unwrap(v)
        if vdef:
            f.write(f"\n{'':>8}{vdef} :: {v.name}")
    else:
        vdef = get_def_var_array_unwrap(v)
        f.write(f"\n{'':>8}{vdef} :: {v.name}")
        if v.ptype == 'np.ndarray' and v.intent.find('in') > -1:
            pdef = get_def_pointer_for_ndarray(v)
            f.write(f"\n{'':>8}{pdef} :: p_{v.name}")

def write_docu_args(args,f) -> None:
    """"
    Documentation of arguments of the python function.

    Parameters
    ----------
    args : list
           list of arguments. Every element of the list
           contains the name,info,ptype and intent values
           of the variable.
    f    : _io.TextIOWrapper
           file
    """
    n = 0
    optional = False
    for a in args:
        if a[5] and not optional:
            optional = True
            f.write(f'\n{"":>{4}}kwargs:')
            n = 4
        f.write(f'\n{"":>{n+4}}{a[0]}')
        f.write(f'\n{"":>{n+8}}Python type    : {a[2]}')
        f.write(f'\n{"":>{n+8}}Fortran type   : {a[4]}')
        f.write(f'\n{"":>{n+8}}Fortran intent : {a[3]}')
        if a[1]:
            f.write(f'\n{"":>{n+8}}Description    : {a[1]}')
        else:
            f.write(f'\n{"":>{n+8}}Description    : not documented yet')

def write_docu_ret(p,f):
    """
    Documentation of the return value of the python function.

    Parameters
    ----------
    p : cfml_objects.Subroutine | cfml_objects.Function
        Fortran variable
    f : _io.TextIOWrapper
        file
    """
    for name in p.wreturn:
        if name in p.arguments:
            a = p.arguments[name]
        elif isinstance(p,cfml_objects.Function):
            if name == p.xreturn.name:
                a = p.xreturn
        else:
            raise Exception(f'write_docu_ret: return variable '\
                f'{name} not found in {p.name}')
        f.write(f'\n{"":>{4}}{a.name}')
        f.write(f'\n{"":>{8}}Python type    : {a.ptype}')
        f.write(f'\n{"":>{8}}Fortran type   : {a.ftype}')
        f.write(f'\n{"":>{8}}Fortran intent : {a.intent}')
        if a.info:
            f.write(f'\n{"":>{8}}Description    : {a.info}')
        else:
            f.write(f'\n{"":>{8}}Description    : not documented yet')

def write_error_check(p,f):
    """
    Write code for checking possible errors and raising an exception
    if an error is detected.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        CrysFML08 procedure
    f : _io.TextIOWrapper
        file where the wrapping function is written
    """
    f.write(f"\n\n{'':>8}! Check errors")
    f.write(f"\n{'':>8}if (ierror /= 0 .or. err_cfml%ierr /= 0) then")
    f.write(f"\n{'':>12}ierror = EXCEPTION_ERROR")
    f.write(f"\n{'':>12}if (err_cfml%ierr /= 0) then")
    f.write(f"\n{'':>16}call raise_exception(RuntimeError,"\
        f"'f_{p.name}: '//trim(err_cfml%msg))")
    f.write(f"\n{'':>12}else")
    f.write(f"\n{'':>16}call raise_exception(RuntimeError,"\
        f"'f_{p.name}: '//'error parsing arguments')")
    f.write(f"\n{'':>12}end if")
    f.write(f"\n{'':>8}end if")

def write_fortran_call(p,f):
    """
    Write the call to the fortran function of crysfml08 that is
    being wrapped.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        CrysFML08 procedure
    f : _io.TextIOWrapper
        file where the wrapping function is written
    """
    f.write(f"\n\n{'':>8}! Call to CrysFML08 procedure")
    # Build the call
    n = 0
    arg = '('
    for name in p.arguments:
        a = p.arguments[name]
        if a.optional:
            var = f'ptr_{a.name}'
        else:
            var = a.name
        if n == 0:
            arg = arg + f'{var}'
        else:
            arg = arg + f',{var}'
        n += 1
    arg = arg + ')'
    f.write(f"\n{'':>8}if (ierror == 0) then")
    if isinstance(p,cfml_objects.Function):
        if p.is_overload:
            f.write(f"\n{'':>12}{p.xreturn.name} = "\
                f"{p.overload}{arg}")
        else:
            f.write(f"\n{'':>12}{p.xreturn.name} = "\
                f"{p.name}{arg}")
    else:
        if p.is_overload:
            f.write(f"\n{'':>12}call {p.overload}{arg}")
        else:
            f.write(f"\n{'':>12}call {p.name}{arg}")
    f.write(f"\n{'':>12}if (err_cfml%ierr /= 0) then")
    f.write(f"\n{'':>16}ierror = EXCEPTION_ERROR")
    f.write(f"\n{'':>16}call raise_exception(RuntimeError,"\
        f"'f_{p.name}: '//trim(err_cfml%msg))")
    f.write(f"\n{'':>12}end if")
    f.write(f"\n{'':>8}end if")

def write_function_init(modules : dict,f) -> None:

    # Count number of procedures
    nproc = 0
    for m in modules:
        if m not in PROCS_TO_WRAP.keys():
            continue
        for proc in modules[m].procedures:
            if proc not in PROCS_TO_WRAP[m]:
                continue
            p = modules[m].procedures[proc]
            modules[m].wraps.append(p)
            nproc = nproc + 1
    # Write function
    f.write(f"\n\n{'':>4}function Init() result(m)")
    f.write(f"\n\n{'':>8}! Local variables")
    f.write(f"\n{'':>8}type(c_ptr) :: m")
    f.write(f"\n{'':>8}integer :: ierror")
    f.write(f"\n\n{'':>8}ierror = Forpy_Initialize()")
    f.write(f"\n\n{'':>8}! Build method table")
    f.write(f"\n{'':>8}call table_crysfml08lib%init({nproc})")
    for m in modules:
        for p in modules[m].wraps:
            f.write(f"\n{'':>8}"\
                f"call table_crysfml08lib%add_method('f_{p.name}',"\
                f"'wrapper of function {m.lower()}.{p.name}', &")
            f.write(f"\n{'':>12}METH_VARARGS,c_funloc(f_{p.name}))")
    f.write(f"\n\n{'':>8}m = mod_crysfml08lib%init"\
        f"('crysfml08lib','A Python API for CrysFML08',table_crysfml08lib)")
    f.write(f"\n\n{'':>4}end function Init")

def write_function_pyinit(f) -> None:

    f.write(f"\n\n{'':>4}function PyInit_crysfml08lib() "\
        f"bind(c,name='PyInit_crysfml08lib') result(m)")
    f.write(f"\n{'':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml08lib")
    f.write(f"\n\n{'':>8}type(c_ptr) :: m")
    f.write(f"\n\n{'':>8}m = Init()")
    f.write(f"\n\n{'':>4}end function PyInit_crysfml08lib")

def write_header_crysfml08lib(modules : dict,f) -> None:

    f.write(f"\n\n{'':>4}implicit none")
    f.write(f"\n\n{'':>4}type(PythonModule), save :: mod_crysfml08lib")
    f.write(f"\n{'':>4}type(PythonMethodTable), save :: table_crysfml08lib")
    f.write(f"\n\n{'':>4}contains")
    write_function_pyinit(f)
    write_function_init(modules,f)

def write_procedures_crysfml08lib(modules : dict,lucy : dict,classes : list,f):

    for m in modules:
        if m == 'CFML_Strings':
            continue
        for p in modules[m].wraps:
            write_wrapping_function(m,p,lucy,classes,f)

def write_patterns_simulation_wrapping_function(m,p,lucy,c,f):

    f.write(f"\n\n{'':>4}"\
        f"function f_{p.name}(self_ptr,args_ptr) result(resul) bind(c)")
    f.write(f"\n{'':>8}! Wrapper for procedure {p.name} of module {m}")
    f.write(f"\n\n{'':>8}! Arguments")
    f.write(f"\n{'':>8}type(c_ptr), value :: self_ptr")
    f.write(f"\n{'':>8}type(c_ptr), value :: args_ptr")
    f.write(f"\n{'':>8}type(c_ptr)        :: resul")
    f.write(f"\n\n{'':>8}! Variables in args_ptr")
    f.write(f"\n{'':>8}type(list) :: strings")
    f.write(f"\n{'':>8}type(ndarray) :: nd_x")
    f.write(f"\n{'':>8}real(kind=cp), dimension(:), pointer :: ptr_x => null()")
    f.write(f"\n\n{'':>8}! Variables in returned tuple")
    f.write(f"\n{'':>8}type(list) :: li_patterns")
    f.write(f"\n\n{'':>8}! Unwrapped variables")
    f.write(f"\n{'':>8}real(kind=cp), dimension(:), allocatable, target :: x")
    f.write(f"\n{'':>8}real(kind=cp), dimension(:), pointer :: p_x")
    f.write(f"\n{'':>8}type(xy_pattern_type), dimension(:), allocatable :: patterns")
    f.write(f"\n\n{'':>8}! Local parameters")
    f.write(f"\n{'':>8}integer, parameter :: NMANDATORY = 1")
    f.write(f"\n\n{'':>8}! Local variables")
    f.write(f"\n{'':>8}integer :: ii")
    f.write(f"\n{'':>8}type(dict) :: di_patterns")
    f.write(f"\n{'':>8}integer :: nargs")
    f.write(f"\n{'':>8}integer :: ierror")
    f.write(f"\n{'':>8}type(object) :: item")
    f.write(f"\n{'':>8}type(tuple) :: args,ret")
    f.write(f"\n{'':>8}type(nonetype) :: nret")
    write_wrapping_initialization(f)
    f.write(f"\n\n{'':>8}! Unwrapping")
    f.write(f"\n{'':>8}ptr_x => null()")
    f.write(f"\n{'':>8}if (ierror == 0) ierror = args%getitem(item,0)")
    f.write(f"\n{'':>8}if (ierror == 0) call get_var_from_item('f_patterns_simulation','strings',item,strings,ierror)")
    f.write(f"\n{'':>8}if (ierror == 0 .and. nargs > 1) then")
    f.write(f"\n{'':>12}ierror = args%getitem(item,1)")
    f.write(f"\n{'':>12}if (ierror == 0) call get_var_from_item('f_patterns_simulation','x',item,nd_x,ierror)")
    f.write(f"\n{'':>12}if (ierror == 0) call ndarray_to_pointer('patterns_simulation','x',nd_x,p_x,ierror)")
    f.write(f"\n{'':>12}if (ierror == 0) call pointer_to_alloc_array('patterns_simulation','x',p_x,x,ierror)")
    f.write(f"\n{'':>12}if (ierror == 0) ptr_x => x")
    f.write(f"\n{'':>8}end if")
    write_error_check(p,f)
    f.write(f"\n\n{'':>8}! Call to CrysFML08 procedure")
    f.write(f"\n{'':>8}if (ierror == 0) then")
    f.write(f"\n{'':>12}patterns = patterns_simulation(strings,ptr_x)")
    f.write(f"\n{'':>12}if (err_cfml%ierr /= 0) then")
    f.write(f"\n{'':>16}ierror = EXCEPTION_ERROR")
    f.write(f"\n{'':>16}call raise_exception(RuntimeError,'f_patterns_simulation: '//trim(err_cfml%msg))")
    f.write(f"\n{'':>12}end if")
    f.write(f"\n{'':>8}end if")
    write_wrapping_section(p,lucy,f)
    write_return_section(p,f)
    f.write(f"\n\n{'':>4}end function f_{p.name}")

def write_python_function(m : cfml_objects.Module,p : str,f) -> None:

    if m.name == 'CFML_Py_Utilities' and p == 'patterns_simulation':
        write_patterns_simulation_python_function(m,p,f)
        return

    args = []
    args_py = '('
    args_for = '('
    is_kwargs = False
    narg = 0

    for name in m.procedures[p].warguments:
        try:
            a = m.procedures[p].arguments[name]
            if a.name == 'str':
                a.name = 'mystr'
            args.append([a.name.strip(),a.info.strip(),a.ptype,a.intent,a.ftype,a.optional])
            if a.optional:
                is_kwargs = True
            else:
                if narg > 0:
                    args_py = args_py + ','
                    args_for = args_for + ','
                args_py = args_py + a.name.strip() + ' : ' + a.ptype
                args_for = args_for + a.name.strip()
                narg += 1
        except:
            i = name.find('_type_spec')
            if i > -1:
                # We are assuming here that a type_spec argument never appears
                # as an optional argument. Take care.
                args.append([name.strip(),f'type specification for allocate class '\
                    f'{name.strip()[:i]}','str','in','character(len=:)',False])
                if narg > 0:
                    args_py = args_py + ',' + name.strip() + ' : ' + 'str'
                    args_for = args_for + ',' + name.strip()
                else:
                    args_py = args_py + name.strip() + ' : ' + 'str'
                    args_for = args_for + name.strip()
                narg += 1
            else:
                raise Exception(f'write_python_function: variable {name}'\
                    f' not found in {p}')
    if is_kwargs:
        if narg > 0:
            args_py = args_py + ',**kwargs'
            args_for_opt = args_for + ',kwargs'
        else:
            args_py = args_py + '**kwargs'
            args_for_opt = args_for + 'kwargs'
    args_py = args_py + ')'
    args_for = args_for + ')'
    if is_kwargs:
        args_for_opt = args_for_opt + ')'
    f.write(f"\n\ndef {p}{args_py}:")
    f.write(f'\n{"":>4}"""')
    if m.procedures[p].docu:
        for l in m.procedures[p].docu:
            f.write(f'\n{"":>4}{l}')
    else:
        f.write(f'\n{"":>4}Description not yet available')
    if args:
        f.write(f'\n\n{"":>4}Parameters')
        f.write(f'\n{"":>4}----------')
        write_docu_args(args,f)
    if m.procedures[p].wreturn:
        f.write(f'\n\n{"":>4}Returns')
        f.write(f'\n{"":>4}-------')
        write_docu_ret(m.procedures[p],f)
    f.write(f'\n{"":>4}"""')
    if len(m.procedures[p].wreturn) > 1:  # returns a tuple in case of multiple return values
        r = ','.join(m.procedures[p].wreturn)
        if is_kwargs:
            f.write(f"\n\n{'':>4}if kwargs:")
            f.write(f"\n{'':>8}{r} = crysfml08lib.f_{p}{args_for_opt}")
            f.write(f"\n{'':>4}else:")
            f.write(f"\n{'':>8}{r} = crysfml08lib.f_{p}{args_for}")
        else:
            f.write(f"\n\n{'':>4}{r} = crysfml08lib.f_{p}{args_for}")
        f.write(f"\n{'':>4}return {r}")
    elif len(m.procedures[p].wreturn) == 1:  # returns a value (instead of a tuple) in case of a single return value
        r = ','.join(m.procedures[p].wreturn)
        if is_kwargs:
            f.write(f"\n\n{'':>4}if kwargs:")
            f.write(f"\n{'':>8}{r} = crysfml08lib.f_{p}{args_for_opt}[0]")
            f.write(f"\n{'':>4}else:")
            f.write(f"\n{'':>8}{r} = crysfml08lib.f_{p}{args_for}[0]")
        else:
            f.write(f"\n\n{'':>4}{r} = crysfml08lib.f_{p}{args_for}[0]")
        f.write(f"\n{'':>4}return {r}")
    else:
        if is_kwargs:
            f.write(f"\n\n{'':>4}if kwargs:")
            f.write(f"\n{'':>8}crysfml08lib.f_{p}{args_for_opt}")
            f.write(f"\n{'':>4}else:")
            f.write(f"\n{'':>8}crysfml08lib.f_{p}{args_for}")
        else:
            f.write(f"\n\n{'':>4}crysfml08lib.f_{p}{args_for}")
    args.clear()

def write_patterns_simulation_python_function(m : cfml_objects.Module,p : str,f) -> None:

    proc = m.procedures[p]
    strings = proc.arguments['strings']
    x = proc.arguments['x']
    f.write(f"\n\ndef {p}(strings : list,x=None):")
    f.write(f'\n{"":>4}"""')
    if proc.docu:
        for l in proc.docu:
            f.write(f'\n{"":>4}{l}')
    else:
        f.write(f'\n{"":>4}Description not yet available')
    f.write(f'\n\n{"":>4}Parameters')
    f.write(f'\n{"":>4}----------')
    for a in (strings, x):
        f.write(f'\n{"":>{4}}{a.name}')
        f.write(f'\n{"":>{8}}Python type    : {a.ptype}')
        f.write(f'\n{"":>{8}}Fortran type   : {a.ftype}')
        f.write(f'\n{"":>{8}}Fortran intent : {a.intent}')
        if a.info:
            f.write(f'\n{"":>{8}}Description    : {a.info}')
        else:
            f.write(f'\n{"":>{8}}Description    : not documented yet')
    if proc.wreturn:
        f.write(f'\n\n{"":>4}Returns')
        f.write(f'\n{"":>4}-------')
        write_docu_ret(proc,f)
    f.write(f'\n{"":>4}"""')
    f.write(f"\n\n{'':>4}if x is None:")
    f.write(f"\n{'':>8}patterns = crysfml08lib.f_patterns_simulation(strings)[0]")
    f.write(f"\n{'':>4}else:")
    f.write(f"\n{'':>8}x = np.asarray(x, dtype=np.float32)")
    f.write(f"\n{'':>8}patterns = crysfml08lib.f_patterns_simulation(strings,x)[0]")
    f.write(f"\n{'':>4}return patterns")

def write_python_module(build_dir : str,m : cfml_objects.Module) -> None:

    if not m.wraps:
        return
    filename = os.path.join(build_dir,'Python',m.name.lower()+'.py')
    with open(filename,'w') as f:
        f.write(f"from . import crysfml08lib")
        f.write(f"\nimport numpy as np")
        for p in m.wraps:
            write_python_function(m,p.name,f)

def write_return_section(p,f):
    """
    Write the wrapping section of the wrapping function.

    Parameters
    ----------
    p    : cfml_objects.Subroutine | cfml_objects.Function
           CrysFML08 procedure
    f    : _io.TextIOWrapper
           file where the wrapping function is written
    """
    f.write(f"\n\n{'':>8}! Return")
    if len(p.wreturn) > 0:
        f.write(f"\n{'':>8}if (ierror == 0) then")
        f.write(f"\n{'':>12}ierror = tuple_create(ret,{len(p.wreturn)})")
        n = 0
        for name in p.wreturn:
            a = None
            if name in p.arguments:
                a = p.arguments[name]
            else:
                if isinstance(p,cfml_objects.Function):
                    if name == p.xreturn.name:
                        a = p.xreturn
            if a is None:
                raise Exception(f"write_result_section: {name} not found"\
                    f" in {p.name}")
            if a.ptype in ['int','float','complex','bool','str']:
                w_name = a.name
            elif a.ptype == 'dict':
                if a.ftype == 'dict':
                    w_name = a.name
                else:
                    w_name = f'di_{a.name}'
            elif a.ptype == 'list':
                w_name = f'li_{a.name}'
            elif a.ptype == 'np.ndarray':
                w_name = f'nd_{a.name}'
            f.write(f"\n{'':>12}ierror = ret%setitem({n},{w_name})")
            n += 1
        f.write(f"\n{'':>12}resul = ret%get_c_ptr()")
        f.write(f"\n{'':>8}else")
        f.write(f"\n{'':>12}resul = C_NULL_PTR")
        f.write(f"\n{'':>8}end if")
    else:
        f.write(f"\n{'':>8}ierror = nonetype_create(nret)")
        f.write(f"\n{'':>8}resul = nret%get_c_ptr()")

def write_unwrapping_section_mandatory(p,c,f):
    """
    Write the unwrapping section of the wrapping function.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        CrysFML08 procedure
    c : list
        list of CrysFML08 classes
    f : _io.TextIOWrapper
        file where the wrapping function is written

    Return
    ------
    n : int
        number of mandatory arguments read (starting from 0)
    """
    f.write(f"\n\n{'':>8}! Unwrapping")
    n = 0
    if p.has_optionals:
        f.write(f"\n{'':>8}if (ierror == 0 .and. nargs == NMANDATORY) then")
        for name in p.warguments:
            a = p.arguments[name]
            if a.optional:
                f.write(f"\n{'':>12}ptr_{a.name} => null()")
        f.write(f"\n{'':>8}end if")
    for name in p.warguments:
        try:
            a = p.arguments[name]
        except:
            # Here variables of the type xxx_type_spec
            # must be unwrapped and the corresponding
            # class allocated
            continue
        if a.optional:
            continue
        if a.ptype in ['int','float','complex','bool','str']:
            w_name = a.name
        elif a.ptype == 'np.ndarray':
            w_name = f'nd_{a.name}'
        elif a.ptype == 'dict':
            if a.ftype == 'dict':
                w_name = a.name
            else:
                w_name = f'di_{a.name}'
        elif a.ptype == 'list':
            if a.ftype == 'list':
                w_name = a.name
            else:
                w_name = f'li_{a.name}'
        f.write(f"\n{'':>8}if (ierror == 0) ierror = args%getitem(item,{n})")
        f.write(f"\n{'':>8}if (ierror == 0) call get_var_from_item"\
            f"('f_{p.name}','{a.name}',item,{w_name},ierror)")
        if a.ptype == 'np.ndarray':
            if a.ndim == 1:
                f.write(f"\n{'':>8}if (ierror == 0) call ndarray_to_pointer"\
                    f"('{p.name}','{a.name}',nd_{a.name},p_{a.name},ierror)")
            else:
                f.write(f"\n{'':>8}if (ierror == 0) call ndarray_to_pointer"\
                    f"('{p.name}','{a.name}',nd_{a.name},p_{a.name},ierror,"\
                    f"array_order)")
            check = [d.isdigit() for d in a.dim]
            if False in check:
                if a.ndim == 1:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call pointer_to_alloc_array"\
                        f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror)")
                else:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call pointer_to_alloc_array"\
                        f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror,"\
                        f"array_order)")
            else:
                if a.ndim == 1:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call pointer_to_array"\
                        f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror)")
                else:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call pointer_to_array"\
                        f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror,"\
                        f"array_order)")
        elif a.ptype == 'dict' and a.ftype != 'dict':
            if a.ftype in c:
                if a.is_class: # and a.allocatable:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call unwrap_class_{a.ftype}"\
                        f"(di_{a.name},{a.name},ierror)")
                else:
                    f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call unwrap_class_{a.ftype}_no_alloc"\
                        f"(di_{a.name},{a.name},ierror)")
            else:
                f.write(f"\n{'':>8}if (ierror == 0) "\
                    f"call unwrap_type(di_{a.name},{a.name},ierror)")
        elif a.ptype == 'list':
            if a.ftype == 'character' or a.ftype == 'logical':
                f.write(f"\n{'':>8}if (ierror == 0) "\
                        f"call List_to_Alloc_Array_Primitive"\
                        f"(li_{a.name},{a.name},ierror)")
        n += 1
    return n

def write_unwrapping_section_optionals(p,c,f,n):
    """
    Write the unwrapping section of the wrapping function.

    Parameters
    ----------
    p : cfml_objects.Subroutine, cfml_objects.Function
        CrysFML08 procedure
    c : list
        list of CrysFML08 classes
    f : _io.TextIOWrapper
        file where the wrapping function is written
    n : int
        position of the argument kwargs
    """
    f.write(f"\n{'':>8}if (ierror == 0 .and. nargs > {p.nwman}) then")
    f.write(f"\n{'':>12}! Optional arguments")
    f.write(f"\n{'':>12}ierror2 = args%getitem(item,{n})")
    f.write(f"\n{'':>12}if (ierror2 == 0) call get_var_from_item"\
            f"('f_{p.name}','kwargs',item,di_kwargs,ierror)")
    f.write(f"\n{'':>12}if (ierror == 0) then")
    for name in p.warguments:
        try:
            a = p.arguments[name]
        except:
            # Here variables of the type xxx_type_spec
            # must be unwrapped and the corresponding
            # class allocated
            continue
        if not a.optional:
            continue
        f.write(f"\n{'':>16}ierror2 ="
            f" di_kwargs%getitem(item,'{a.name}')")
        f.write(f"\n{'':>16}if (ierror2 == 0) then")
        if a.ptype in ['int','float','complex','bool','str']:
            w_name = a.name
        elif a.ptype == 'np.ndarray':
            w_name = f'nd_{a.name}'
        elif a.ptype == 'dict':
            if a.ftype == 'dict':
                w_name = a.name
            else:
                w_name = f'di_{a.name}'
        elif a.ptype == 'list':
            w_name = f'li_{a.name}'
        j = a.intent.find('in')
        if j > -1:
            f.write(f"\n{'':>20}if (ierror == 0) call get_var_from_item"\
                f"('f_{p.name}','{a.name}',item,{w_name},ierror)")
            if a.ptype == 'np.ndarray':
                if a.ndim == 1:
                    f.write(f"\n{'':>20}if (ierror == 0) call ndarray_to_pointer"\
                        f"('{p.name}','{a.name}',nd_{a.name},p_{a.name},ierror)")
                else:
                    f.write(f"\n{'':>20}if (ierror == 0) call ndarray_to_pointer"\
                        f"('{p.name}','{a.name}',nd_{a.name},p_{a.name},ierror,"\
                        f"array_order)")
                check = [d.isdigit() for d in a.dim]
                if False in check:
                    if a.ndim == 1:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call pointer_to_alloc_array"\
                            f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror)")
                    else:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call pointer_to_alloc_array"\
                            f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror,"\
                            f"array_order)")
                else:
                    if a.ndim == 1:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call pointer_to_array"\
                            f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror)")
                    else:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call pointer_to_array"\
                            f"('{p.name}','{a.name}',p_{a.name},{a.name},ierror,"\
                            f"array_order)")
            elif a.ptype == 'dict' and a.ftype != 'dict':
                if a.ftype in c:
                    if a.is_class:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call unwrap_class_{a.ftype}"\
                            f"(di_{a.name},{a.name},ierror)")
                    else:
                        f.write(f"\n{'':>20}if (ierror == 0) "\
                            f"call unwrap_class_{a.ftype}_no_alloc"\
                            f"(di_{a.name},{a.name},ierror)")
                else:
                    f.write(f"\n{'':>20}if (ierror == 0) "\
                        f"call unwrap_type(di_{a.name},{a.name},ierror)")
            elif a.ptype == 'list':
                pass
            f.write(f"\n{'':>20}if (ierror == 0) ptr_{a.name} => {a.name}")
        else:
            f.write(f"\n{'':>20}ptr_{a.name} => {a.name}")
        f.write(f"\n{'':>16}else")
        f.write(f"\n{'':>20}ptr_{a.name} => null()")
        f.write(f"\n{'':>20}call err_clear")
        f.write(f"\n{'':>16}end if")
    f.write(f"\n{'':>12}end if")
    f.write(f"\n{'':>12}if (ierror2 == 0) call err_clear")
    f.write(f"\n{'':>8}end if")

def write_uses(uses : list,f) -> None:

    f.write(f"\n\n{'':>4}use forpy_mod, str_forpy => str")
    f.write(f"\n{'':>4}use iso_c_binding")
    for u in uses:
        if u == 'forpy_mod':
            continue
        f.write(f"\n{'':>4}use {u}")
    f.write(f"\n{'':>4}use cfml_wraps")
    f.write(f"\n{'':>4}use cfml_wraps_utils")

def write_wrapping_function(m,p,lucy,c,f):
    """
    Write the wrapping function for a procedure of CrysFML08

    Parameters
    ----------
    m    : str
           module name
    p    : cfml_objects.Subroutine, cfml_objects.Function
           CrysFML08 procedure
    lucy : dict
         : base class for all CrysFML08 types
    c    : list
           list of CrysFML08 classes
    f    : _io.TextIOWrapper
           file where the wrapping function is written
    """
    if m == 'CFML_Py_Utilities' and p.name == 'patterns_simulation':
        write_patterns_simulation_wrapping_function(m,p,lucy,c,f)
        return

    f.write(f"\n\n{'':>4}"\
        f"function f_{p.name}(self_ptr,args_ptr) result(resul) bind(c)")
    f.write(f"\n{'':>8}! Wrapper for procedure {p.name} of module {m}")
    set_number_of_arguments(p)
    # Write definitions for variables in args (common for all functions)
    write_def_var_args(f)
    # Build the wrapper only for documented procedures
    #if not p.docu:
    #    f.write(f"\n\n{'':>8}! Function not documented yet.")
    #    f.write(f"\n{'':>8}! Not implemented yet")
    #    f.write(f"\n\n{'':>4}end function f_{p.name}")
    #    return
    write_def_var_in_args_ptr(p,f)
    write_def_var_in_ret(p,f)
    f.write(f"\n\n{'':>8}! Unwrapped variables")
    for name in p.arguments:
        a = p.arguments[name]
        write_def_var_unwraps(a,f)
    if isinstance(p,cfml_objects.Function):
        write_def_var_unwraps(p.xreturn,f)
    write_def_var_local(p,f)
    write_wrapping_initialization(f)
    n = write_unwrapping_section_mandatory(p,c,f)
    if p.has_optionals:
        write_unwrapping_section_optionals(p,c,f,n)
    write_error_check(p,f)
    write_fortran_call(p,f)
    write_wrapping_section(p,lucy,f)
    write_return_section(p,f)
    f.write(f"\n\n{'':>4}end function f_{p.name}")

def write_wrapping_initialization(f):
    """
    Write the initialization common to all wrapping functions.

    Parameters
    ----------
    f : _io.TextIOWrapper
        file
    """
    f.write(f"\n\n{'':>8}ierror = 0")
    f.write(f"\n{'':>8}call err_clear ! Reset Python error")
    f.write(f"\n{'':>8}call clear_error() ! Reset CrysFML08 error")
    f.write(f"\n\n{'':>8}! Use unsafe_cast_from_c_ptr to cast"\
        f"from c_ptr to tuple/dict")
    f.write(f"\n{'':>8}call unsafe_cast_from_c_ptr(args,args_ptr)")
    f.write(f"\n{'':>8}ierror = args%len(nargs)")
    f.write(f"\n{'':>8}if (ierror == 0) then")
    f.write(f"\n{'':>12}if (nargs < NMANDATORY) then")
    f.write(f"\n{'':>16}ierror = 1")
    f.write(f"\n{'':>16}err_cfml%ierr = 1")
    f.write(f"\n{'':>16}err_cfml%msg = 'Wrong number of arguments'")
    f.write(f"\n{'':>12}end if")
    f.write(f"\n{'':>8}end if")

def write_wrapping_section(p,lucy,f):
    """
    Write the wrapping section of the wrapping function.

    Parameters
    ----------
    p    : cfml_objects.Subroutine, cfml_objects.Function
           CrysFML08 procedure
    lucy : dict
           base class for every CrysFML08 type
    f    : _io.TextIOWrapper
           file where the wrapping function is written
    """
    f.write(f"\n\n{'':>8}! Wrapping")
    n = 0
    for name in p.wreturn + p.inout:
        a = None
        if name in p.arguments:
            a = p.arguments[name]
        else:
            if isinstance(p,cfml_objects.Function):
                if name == p.xreturn.name:
                    a = p.xreturn
        if a is None:
            raise Exception(f"write_wrapping section: {name} not found"\
                f" in {p.name}")
        if a.ptype in ['int','float','complex','bool','str']:
            continue
        if a.ptype == 'dict' and a.ftype == 'dict':
            continue
        if a.ptype == 'list' and a.ftype == 'list':
            continue
        if a.ptype == 'np.ndarray':
            if a.optional:
                f.write(f"\n{'':>8}if (associated(ptr_{a.name})) then")
                f.write(f"\n{'':>12}if (ierror == 0) ierror = ndarray_create"\
                    f"(nd_{a.name},{a.name})")
            else:
                f.write(f"\n{'':>8}if (ierror == 0) then")
                f.write(f"\n{'':>12}ierror = ndarray_create"\
                    f"(nd_{a.name},{a.name})")
        elif a.ptype == 'dict':
            if a.optional:
                f.write(f"\n{'':>8}if (associated(ptr_{a.name})) then")
                f.write(f"\n{'':>12}if (ierror == 0) call wrap_type"\
                    f"({a.name},di_{a.name},ierror)")
            else:
                f.write(f"\n{'':>8}if (ierror == 0) then")
                if name in p.wreturn:
                    f.write(f"\n{'':>12}ierror = dict_create(di_{a.name})")
                f.write(f"\n{'':>12}if (ierror == 0) call wrap_type"\
                    f"({a.name},di_{a.name},ierror)")
        elif a.ptype == 'list':
            if a.optional:
                raise Exception(f"Optional list not supported yet as returned variables")
            else:
                f.write(f"\n{'':>8}if (ierror == 0) then")
                if a.ndim == 1:
                    f.write(f"\n{'':>12}if (ierror == 0) ierror = list_create(li_{a.name})\n")
                    f.write(f"{'':>12}if (ierror == 0) then\n")
                    f.write(f"{'':>12}    do ii = 1 , size({a.name})\n")
                    f.write(f"{'':>12}        ierror = dict_create(di_{a.name})\n")
                    f.write(f"{'':>12}        if (ierror == 0) call wrap_type({a.name}(ii),di_{a.name},ierror)\n")
                    f.write(f"{'':>12}        if (ierror == 0) ierror = li_{a.name}%append(di_{a.name})\n")
                    f.write(f"{'':>12}        if (ierror == 0) call di_{a.name}%destroy\n")
                    f.write(f"{'':>12}    end do\n")
                    f.write(f"{'':>12}end if")
                else:
                    raise Exception(f"Returned list with ndim > 1 not supported yet")
                #if name in p.wreturn:
                #    f.write(f"\n{'':>12}ierror = list_create(li_{a.name})")
                #f.write(f"\n{'':>12}if (ierror == 0) call wrap_type"\
                #    f"({a.name},li_{a.name},ierror)")
        f.write(f"\n{'':>12}if (err_cfml%ierr /= 0) then")
        f.write(f"\n{'':>16}ierror = EXCEPTION_ERROR")
        f.write(f"\n{'':>16}call raise_exception(RuntimeError,"\
            f"'f_{p.name}: '//trim(err_cfml%msg))")
        f.write(f"\n{'':>12}end if")
        f.write(f"\n{'':>8}end if")
