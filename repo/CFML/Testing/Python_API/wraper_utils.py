"""
---------
Functions
---------
end_function(f,p) -> None
end_module(m : cfml_objects.Module) -> None
fortran_procedure(f,p) -> None
init_module(m : cfml_objects.Module) -> None
init_procedure(f,p_name : str) -> None
set_fortran_procedures_args(f,p) -> None
set_local_variables(f,p,rets : list) -> None
set_local_variables_charray(f,var : str,ftype : str, dim : str) -> None
set_local_variables_primarray(f,var : str,ftype_s : str,dimen : str) -> None
set_rets(p) -> list
tune_fortran_type(var : cfml_objects.FortranVar) -> cfml_objects.FortranVar
unwrap_arguments(f,p) -> None
wrap_arguments(f,p,rets : list) -> None
wrap_procedure(p) -> None
write_api_init(procs : dict,nprocs : int) -> None
write_charlist() -> None
write_license(f) -> None
"""
import cfml_objects
import colorama
import copy
import parser_utils

w_name = ''
w_file = ''

def end_function(f,p) -> None:

    f.write(f"{' ':>8}if (ierror == 0) then \n")
    f.write(f"{' ':>12}resul = ret%get_c_ptr()\n")
    f.write(f"{' ':>8}else\n")
    f.write(f"{' ':>12}ierror = nonetype_create(nret)\n")
    f.write(f"{' ':>12}resul = nret%get_c_ptr()\n")
    f.write(f"{' ':>8}end if\n")
    f.write(f"\n{' ':>4}end function py_{p.name}\n")

def end_module(m : cfml_objects.Module) -> None:

    with open(w_file,'a') as f:
        f.write(f"\nend module {w_name}\n")

def fortran_procedure(f,p) -> None:

    f.write(f"\n{' ':>8}! Call CrysFML procedure\n")
    f.write(f"{' ':>8}if (ierror == 0) then\n")
    f.write(f"{' ':>12}call clear_error()\n")
    arguments = '('
    for arg in p.arguments.keys():
        arguments = arguments + arg + ','
    arguments = arguments[:-1] + ')'
    if type(p) == cfml_objects.Subroutine:
        f.write(f"{' ':>12}call {p.name}{arguments}\n")
    else:
        f.write(f"{' ':>12}{p.xreturn.name} = {p.name}{arguments}\n")
    f.write(f"{' ':>12}if (err_cfml%flag) then\n")
    f.write(f"{' ':>16}ierror = EXCEPTION_ERROR\n")
    f.write(f"{' ':>16}call raise_exception(RuntimeError,\"{p.name}: \"//trim(err_cfml%msg))\n")
    f.write(f"{' ':>12}end if\n")
    f.write(f"{' ':>8}end if\n")

def init_module(m : cfml_objects.Module) -> None:

    global w_name
    global w_file
    w_name = 'py_' + m.name
    w_file = w_name + '.f90'

    with open(w_file,'w') as f:
        write_license(f)
        f.write(f"module {w_name}\n")
        f.write(f"\n{' ':>4}use forpy_mod, str_fp => str  \n")
        f.write(f"{' ':>4}use iso_c_binding \n")
        #f.write(f"{' ':>4}use wraps\n")
        f.write(f"\n{' ':>4}use cfml_globaldeps \n")
        f.write(f"{' ':>4}use {m.name}\n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}contains\n")

def init_procedure(f,p_name : str) -> None:

    f.write(f"\n{' ':>4}function py_{p_name}(self_ptr,args_ptr) result(resul) bind(c)\n")
    f.write(f"\n{' ':>8}! Arguments\n")
    f.write(f"{' ':>8}type(c_ptr), value :: self_ptr\n")
    f.write(f"{' ':>8}type(c_ptr), value :: args_ptr\n")
    f.write(f"{' ':>8}type(c_ptr)        :: resul\n")

def set_fortran_procedures_args(f,p) -> None:

    f.write(f"\n{' ':>8}! Arguments for the Fortran procedure\n")
    for arg in p.arguments.keys():
        ftype = tune_fortran_type(p.arguments[arg])
        s = parser_utils.build_var_def(ftype)
        f.write(f"{' ':>8}{s} :: {p.arguments[arg].name} ! CrysFML type: {p.arguments[arg].ftype}\n")
    if type(p) == cfml_objects.Function:
        ftype = tune_fortran_type(p.xreturn)
        s = parser_utils.build_var_def(ftype)
        f.write(f"{' ':>8}{s} :: {p.xreturn.name} ! CrysFML type: {p.xreturn.ftype}\n")

def set_local_variables(f,p,rets) -> None:
    """
    Local variables are needed for fortran arrays and derived types
    """
    f.write(f"\n{' ':>8}! Local variables\n")
    f.write(f"{' ':>8}integer :: ierror,ii\n")
    f.write(f"{' ':>8}type(object) :: item\n")
    f.write(f"{' ':>8}type(tuple) :: args,ret\n")
    f.write(f"{' ':>8}type(nonetype) :: nret\n")
    for arg in p.arguments.keys():
        if p.arguments[arg].intent == 'in':
            ndim = p.arguments[arg].ndim
            ftype = p.arguments[arg].ftype
            if parser_utils.is_primitive(ftype) and ndim > 0:
                ftype_s = p.arguments[arg].fortran_type_short
                if ftype_s == 'character':
                    set_local_variables_charray(f,arg,ftype,dim)
                else:
                    set_local_variables_primarray(f,arg,ftype_s,dim)
    for r in rets: # arguments with intent = 'inout' or 'out' and, if p is a function, the result
        if hasattr(r,'intent'):
            ftype = tune_fortran_type(r)
        else:
            ftype = tune_fortran_type(r)
        #ftype_s = r.fortran_type_short
        #if parser_utils.is_primitive(ftype):
        #    if parser_utils.is_array(r.dim):
        #        if ftype_s == 'character':
        #            set_local_variables_charray(f,r.name,ftype,r.dim)
        #        else:
        #            set_local_variables_primarray(f,r.name,ftype_s,r.dim)

def set_local_variables_charray(f,var : str,ftype : str, dim : str) -> None:

    f.write(f"{' ':>8}type(list) :: li_{var}\n")
    l = parser_utils.get_len(ftype)
    if l == '*' or l == ':':
        f.write(f"{' ':>8}integer :: maxlen_{var}\n")
    elif l.isdigit():
        f.write(f"{' ':>8}integer :: maxlen_{var} = {l}\n")
    else:
        print(f"{colorama.Fore.RED}{'Error: '}Character variable with len = {l}, it is neither \"*\" nor a digit{colorama.Style.RESET_ALL}")
        raise IOError
    if dim.find(':') > -1:
        f.write(f"{' ':>8}integer :: size_{var}\n")
    else:
        f.write(f"{' ':>8}integer :: size_{var} = {dim}\n")

def set_local_variables_primarray(f,var : str,ftype_s : str,dimen : str) -> None:
    """
    For integer, real and logical types
    """
    dim = len(dimen.split(','))-1
    d = '(:'
    for i in range(dim):
        d = d + ',:'
    d = d + ')'
    f.write(f"{' ':>8}type(ndarray) :: nd_{var}\n")
    f.write(f"{' ':>8}{ftype_s}, dimension{d}, pointer :: pointer_{var}\n")

def set_rets(p) -> list:
    """
    Variables that must be returned. Arguments with intent = 'inout' and 'out',
    and, if p is a function, the function result
    """
    rets = []
    for arg in p.arguments.keys():
        if p.arguments[arg].intent != 'in':
            rets.append(p.arguments[arg])
    if type(p) == cfml_objects.Function:
        rets.append(p.xreturn)
    return rets

def tune_fortran_type(var : cfml_objects.FortranVar) -> cfml_objects.FortranVar:

    tvar = copy.copy(var)
    if tvar.ftype == 'character':
        # forpy does not understand (len=*), must be replaced by (len=:)
        if tvar.len == '*':
            tvar.len = ':'
            if tvar.ndim == 0:
                tvar.allocatable = True
        elif tvar.len.isdigit() or tvar.len == ':':
            pass
        else:
            print(f"{colorama.Fore.RED}Error: Cannot tune character variable{colorama.Style.RESET_ALL}")
            raise IOError
    if tvar.ndim > 0:
        # Arrays with dimensions specified as ':', must have the 'allocatable' attribute
        if ':' in tvar.dim and not tvar.allocatable:
            tvar.allocatable = True
    return tvar

def unwrap_arguments(f,p) -> None:
    """
    Only arguments with intent = 'in' or 'inout' must be unwrapped
    """
    f.write(f"\n{' ':>8}! Reset error variable\n")
    f.write(f"{' ':>8}ierror = 0\n")
    f.write(f"\n{' ':>8}! Unwrap_arguments\n")
    f.write(f"{' ':>8}call unsafe_cast_from_c_ptr(args,args_ptr)\n")
    i = 0
    for arg in p.arguments.keys():
        a = p.arguments[arg]
        dim = a.dim
        ftype = a.ftype
        #ftype_s = a.fortran_type_short
        if a.intent.find('in') > -1 or ':' in a.dim > -1 and a.is_allocatable:
            f.write(f"{' ':>8}if (ierror == 0) ierror = args%getitem(item,{i})\n")
            i += 1
            if parser_utils.is_primitive(ftype):
                if a.ndim > 0:
                    pass
                    #if ftype_s == 'character':
                    #    unwrap_charray(f,p,arg,ftype,dim)
                    #else:
                    #    unwrap_primarray(f,p,arg,ftype,dim)
                else:
                    f.write(f"{' ':>8}if (ierror == 0) ierror = cast({arg},item)\n")

def unwrap_charray(f,p,var : str,ftype : str,dim : str) -> None:

    f.write(f"{' ':>8}if (ierror == 0) ierror = cast(li_{var},item)!{dim} {ftype}\n")
    l = parser_utils.get_len(ftype)
    if l == '*': # Set len to the maximum length of an element in the list
        f.write(f"{' ':>8}if (ierror == 0) call maxlen_from_li_charr(li_{var},maxlen_{var},ierror)\n")
        f.write(f"{' ':>8}if (ierror /= 0) then\n")
        f.write(f"{' ':>12}ierror = EXCEPTION_ERROR\n")
        f.write(f"{' ':>12}call raise_exception(RuntimeError,\"{p.name}: Error unwrapping python list\")\n")
        f.write(f"{' ':>8}end if\n")
    if dim.find(':') > -1: # Set size = number of elements of the list
        f.write(f"{' ':>8}if (ierror == 0) ierror = li_{var}%len(size_{var})\n")
    else:
        f.write(f"{' ':>8}if (ierror == 0) ierror = li_{var}%len(size_aux)\n")
        f.write(f"{' ':>8}if (size_aux /= size_{var}) then\n")
        f.write(f"{' ':>12}ierror = EXCEPTION_ERROR\n")
        f.write(f"{' ':>12}call raise_exception(RuntimeError,\"{p.name}: Error in list size of argument {var}\")\n")
        f.write(f"{' ':>8}end if\n")
    f.write(f"{' ':>8}if (ierror == 0) allocate(character(len=maxlen_{var})::{var}(size_{var}))\n")
    f.write(f"{' ':>8}if (ierror == 0) call list_to_charray(li_{var},{var},ierror)\n")

def unwrap_primarray(f,p,var : str,ftype : str,dim : str) -> None:

    f.write(f"{' ':>8}if (ierror == 0) ierror = cast(nd_{var},item)\n")
    f.write(f"{' ':>8}if (ierror == 0) ierror = nd_{var}%get_data(pointer_{var},order='F')\n")
    ndim = len(dim.split(','))
    if dim.find(':') < 0:
        f.write(f"{' ':>8}if (ierror == 0) then\n")
        f.write(f"{' ':>12}do ii = 1 , {ndim}\n")
        f.write(f"{' ':>16}if (size({var},ii) /= size(pointer_{var},ii)) then\n")
        f.write(f"{' ':>20}ierror = EXCEPTION_ERROR\n")
        f.write(f"{' ':>20}call raise_exception(RuntimeError,\"{p.name}: Size of array {var} differs from that of its pointer\")\n")
        f.write(f"{' ':>20}exit\n")
        f.write(f"{' ':>16}end if\n")
        f.write(f"{' ':>12}end do\n")
        f.write(f"{' ':>8}end if\n")
        f.write(f"{' ':>8}if (ierror == 0) {var} = pointer_{var}\n")
    else:
        f.write(f"{' ':>8}allocate({var}(")
        for i in range(ndim):
            f.write(f"size(pointer_{var},{i+1})")
            if i < ndim - 1:
                f.write(f",")
            else:
                f.write(f"))\n")

def wrap_arguments(f,p,rets : list) -> None:

    n = len(rets)
    f.write(f"\n{' ':>8}! Return tuple\n")
    f.write(f"{' ':>8}if (ierror == 0) ierror = tuple_create(ret,{n})\n")
    i = 0
    for r in rets:
        if parser_utils.is_primitive(r.ftype):
            if r.ndim > 0:
                if r.fortran_type.startswith('character'):
                    if r.intent == 'out':
                        f.write(f"{' ':>8}ierror = list_create(li_{r.name})\n")
                    f.write(f"{' ':>8}call charray_to_list({r.name},li_{r.name},ierror)\n")
                    f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},li_{r.name})\n")
                else:
                    f.write(f"{' ':>8}if (ierror == 0) ierror = ndarray_create(nd_{r.name},{r.name})\n")
                    f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},nd_{r.name})\n")
            else:
                f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},{r.name})\n")
        i += 1

def wrap_procedure(p) -> None:

    with open(w_file,'a') as f:
        init_procedure(f,p.name)
        set_fortran_procedures_args(f,p)
        rets = set_rets(p)
        set_local_variables(f,p,rets)
        unwrap_arguments(f,p)
        fortran_procedure(f,p)
        wrap_arguments(f,p,rets)
        end_function(f,p)

def write_api_init(procs : dict,nprocs : int) -> None:

    with open('api_init.f90','w') as f:
        write_license(f)
        f.write(f"\nmodule api_init\n")
        f.write(f"\n{' ':>4}use forpy_mod, str_fp => str \n")
        f.write(f"{' ':>4}use iso_fortran_env\n")
        f.write(f"\n")
        for m in procs.keys():
            f.write(f"{' ':>4}use py_{m}\n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}type(PythonModule), save :: mod_Def\n")
        f.write(f"{' ':>4}type(PythonMethodTable), save :: method_Table\n")
        f.write(f"\n{' ':>4}contains\n")
        f.write(f"\n{' ':>4}! Initialization function for Python 3\n")
        f.write(f"{' ':>4}! Called when importing module\n")
        f.write(f"{' ':>4}! Must use bind(c, name=\"PyInit_<module name>\")\n")
        f.write(f"{' ':>4}! Return value must be type(c_ptr),\n")
        f.write(f"{' ':>4}! use the return value of PythonModule%init\n")
        f.write(f"{' ':>4}function PyInit_crysfml_api() bind(c,name=\"PyInit_crysfml_api\") result(m)\n")
        f.write(f"{' ':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml_api\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}type(c_ptr) :: m\n")
        f.write(f"\n{' ':>8}m = Init()\n")
        f.write(f"\n{' ':>4}end function PyInit_crysfml_api\n")
        f.write(f"\n{' ':>4}function Init() result(m)\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}type(c_ptr) :: m\n")
        f.write(f"{' ':>8}integer :: ierror\n")
        f.write(f"\n{' ':>8}ierror = Forpy_Initialize()\n")
        f.write(f"\n{' ':>8}! Build method table\n")
        f.write(f"{' ':>8}call method_Table%init({nprocs})\n")
        for m in procs.keys():
            for p in procs[m]:
                f.write(f"{' ':>8}call method_Table%add_method(\"{p}\",&\n")
                f.write(f"{' ':>12}\"{p}\",METH_VARARGS,&\n")
                f.write(f"{' ':>12}c_funloc(py_{p}))\n")

        f.write(f"\n{' ':>8}! Build mod_Def\n")
        f.write(f"{' ':>8}m = mod_Def%init(\"pycrysfml\",&\n")
        f.write(f"{' ':>12}\"A Python API for CrysFML08\",method_Table)")
        f.write(f"\n{' ':>4}end function Init\n")
        f.write(f"\nend module api_init\n")

def write_interconversion() -> None:

    with open('wraps.f90','w') as f:
        write_license(f)
        f.write(f"\nmodule wraps\n")
        f.write(f"\n{' ':>4}use forpy_mod, str_fp => str \n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}contains\n")
        # Subroutine charray_to_list
        f.write(f"\n{' ':>4}subroutine charray_to_list(charr,li_charr,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}character(len=*), dimension(:), intent(in)    :: charr\n")
        f.write(f"{' ':>8}type(list),                     intent(inout) :: li_charr\n")
        f.write(f"{' ':>8}integer,                        intent(out)   :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i,n\n")
        f.write(f"\n{' ':>8}ierror = 0\n")
        f.write(f"\n{' ':>8}ierror = li_charr%len(n)\n")
        f.write(f"{' ':>8}do i = 1 , size(charr)\n")
        f.write(f"{' ':>12}if (ierror == 0) then\n")
        f.write(f"{' ':>16}if (i <= n) then\n")
        f.write(f"{' ':>20}ierror = li_charr%setitem(i-1,charr(i))\n")
        f.write(f"{' ':>16}else\n")
        f.write(f"{' ':>20}ierror = li_charr%append(charr(i))\n")
        f.write(f"{' ':>16}end if\n")
        f.write(f"{' ':>12}end if\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine charray_to_list\n")
        # Subroutine list_to_charray
        f.write(f"\n{' ':>4}subroutine list_to_charray(li_charr,charr,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}type(list),                     intent(inout) :: li_charr\n")
        f.write(f"{' ':>8}character(len=*), dimension(:), intent(inout) :: charr\n")
        f.write(f"{' ':>8}integer,                        intent(out)   :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i,n\n")
        f.write(f"{' ':>8}character(len=:), allocatable :: mystr\n")
        f.write(f"{' ':>8}type(object) :: item\n")
        f.write(f"\n{' ':>8}ierror = li_charr%len(n)\n")
        f.write(f"{' ':>8}do i = 1 , n\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = li_charr%getitem(item,i-1)\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = cast(mystr,item)\n")
        f.write(f"{' ':>12}if (ierror == 0) charr(i) = mystr\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine list_to_charray\n")
        # Subroutine maxlen_from_li_charr
        f.write(f"\n{' ':>4}subroutine maxlen_from_li_charr(li_charr,l,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}type(list),                     intent(inout) :: li_charr\n")
        f.write(f"{' ':>8}integer,                        intent(out)   :: l\n")
        f.write(f"{' ':>8}integer,                        intent(out)   :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i,n\n")
        f.write(f"{' ':>8}character(len=:), allocatable :: mystr\n")
        f.write(f"{' ':>8}type(object) :: item\n")
        f.write(f"\n{' ':>8}l = 0\n")
        f.write(f"{' ':>8}ierror = li_charr%len(n)\n")
        f.write(f"{' ':>8}do i = 1 , n\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = li_charr%getitem(item,i-1)\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = cast(mystr,item)\n")
        f.write(f"{' ':>12}if (ierror == 0) then \n")
        f.write(f"{' ':>16}if (len(trim(mystr)) > l) l = len(trim(mystr)) \n")
        f.write(f"{' ':>12}end if\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine maxlen_from_li_charr\n")
        f.write(f"\nend module wraps\n")

def write_license(f) -> None:

    f.write(f"! ------------------------------------------------------------\n")
    f.write(f"! CrysFML08 API\n")
    f.write(f"!\n")
    f.write(f"{'! @license':<14}GNU LGPL (see LICENSE)\n")
    f.write(f"{'! @copyright':<14}Institut Laue Langevin 2020-now\n")
    f.write(f"{'! @authors':<14}Scientific Computing Group at ILL (see AUTHORS),\n")
    f.write(f"!{' ':>13}based on Elias Rabel work for Forpy\n")
    f.write(f"! ------------------------------------------------------------\n")
    f.write(f"\n")
