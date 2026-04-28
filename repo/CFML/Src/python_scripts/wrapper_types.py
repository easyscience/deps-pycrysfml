"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
local_variables_unwrap(tipos : list) -> list
local_variables_wrap(tipos : list) -> list
wrap(modules : dict,lucy : dict) -> None
wrap_cfml_module_types(m : cfml_objects.Module,m_name : str,lucy : dict) -> None
write_cfml_wraps(modules : dict) -> None
write_unwrap_proc(f,t : dict,s : str,lucy : dict) -> None
write_unwrap_proc_no_alloc(f,t : dict,s : str,lucy : dict) -> None
write_unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict)
write_wraps_primitives()
write_wrap_proc(f,t : dict,s : str,lucy : dict) -> None
write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict)
"""
import cfml_objects
import os

publics_unwrap = []
publics_unwrap_no_alloc = []
publics_wrap = []

PRIMITIVES = ['integer','real','logical','character','complex']
NUMERICALS = ['integer','real']

def local_variables_unwrap(tipos : list) -> list:

    p_int_1D = False
    p_int_2D = False
    p_int_3D = False
    p_real_1D = False
    p_real_2D = False
    p_real_3D = False
    my_list = False
    my_dicts = []
    for t in tipos:
        for c in t.components:
            var = t.components[c]
            if var.ndim == 0:
                if var.ftype.lower() not in PRIMITIVES:
                    my_dicts.append(f"dict_{var.name}")
            else:
                if var.ftype.lower() == 'integer':
                    if var.ndim == 1:
                        p_int_1D = True
                    elif var.ndim == 2:
                        p_int_2D = True
                    elif var.ndim == 3:
                        p_int_3D = True
                elif var.ftype.lower() == 'real':
                    if var.ndim == 1:
                        p_real_1D = True
                    elif var.ndim == 2:
                        p_real_2D = True
                    elif var.ndim == 3:
                        p_real_3D = True
                else:
                    my_list = True
    lv = []
    order = False
    if p_int_1D:
        lv.append('integer, dimension(:), pointer :: p_int_1d')
    if p_int_2D:
        lv.append('integer, dimension(:,:), pointer :: p_int_2d')
        order = True
    if p_int_3D:
        lv.append('integer, dimension(:,:,:), pointer :: p_int_3d')
        order = True
    if p_real_1D:
        lv.append('real, dimension(:), pointer :: p_real_1d')
    if p_real_2D:
        lv.append('real, dimension(:,:), pointer :: p_real_2d')
        order = True
    if p_real_3D:
        lv.append('real, dimension(:,:,:), pointer :: p_real_3d')
        order = True
    if order:
        lv.append('character(len=1) :: order')
    if my_list:
        lv.append('type(list) :: my_list')
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    return lv

def local_variables_wrap(tipos : list) -> list:

    my_dicts = []
    my_lists = []
    my_ndarrays = []
    for t in tipos:
        for c in t.components:
            var = t.components[c]
            if var.ndim == 0:
                if var.ftype.lower() not in PRIMITIVES:
                    my_dicts.append(f"di_{var.name}")
            else:
                if var.ftype.lower() in NUMERICALS:
                    my_ndarrays.append(f"nd_{var.name}")
                else:
                    my_lists.append(f"li_{var.name}")
    lv = []
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    if my_lists:
        lv.append('type(list) :: '+','.join(my_lists))
    if my_ndarrays:
        lv.append('type(ndarray) :: '+','.join(my_ndarrays))
    return lv

def wrap(modules : dict,lucy : dict) -> None:

    if not os.path.isdir('../Python_API'):
        os.mkdir('../Python_API')
    if not os.path.isdir('../Python_API/CFML_Wraps'):
        os.mkdir('../Python_API/CFML_Wraps')
    for m in modules:
        wrap_cfml_module_types(modules[m],m,lucy)
    write_wraps_primitives()
    write_cfml_wraps(modules)
    return None

def wrap_cfml_module_types(m : cfml_objects.Module,m_name : str,lucy : dict) -> None:

    w_name = 'Wraps_'+m_name[5:]
    os.path.join('../Python_API',w_name+'.f90')
    w_file = os.path.join('../Python_API/CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"{'':>4}contains\n")
        t = m.types
        for s in t:
            if not t[s].parent:
                write_unwrap_proc(f,t,s,lucy)
                if t[s].childs:
                    write_unwrap_proc_no_alloc(f,t,s,lucy)
                write_wrap_proc(f,t,s,lucy)
                write_list_to_array(f,t,s,lucy)
        f.write(f"\nend submodule")

def write_cfml_wraps(modules : dict) -> None:

    w_file = os.path.join('../Python_API/CFML_Wraps.f90')
    with open(w_file,'w') as f:
        f.write(f"\nModule CFML_Wraps\n")
        f.write(f"\n{'':>4}use cfml_globaldeps\n")
        for m in modules:
            if modules[m].types:
                f.write(f"{'':>4}use {m}, only: ")
                n = 0
                for s in modules[m].types:
                    if n == 0:
                        f.write(f"{s}")
                    else:
                        f.write(f",{s}")
                    n += 1
                f.write(f"\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"\n{'':>4}private\n")
        f.write(f"\n{'':>4}public :: ")
        n = 0
        for p in publics_unwrap:
            if n == 3:
                f.write(f",&\n{'':>14}")
                n = 0
            if n == 0:
                f.write(f"unwrap_{p[0]}")
            else:
                f.write(f",unwrap_{p[0]}")
            n += 1
        f.write(f"\n{'':>4}public :: ")
        n = 0
        for p in publics_unwrap_no_alloc:
            if n == 3:
                f.write(f",&\n{'':>14}")
                n = 0
            if n == 0:
                f.write(f"unwrap_{p}_no_alloc")
            else:
                f.write(f",unwrap_{p}_no_alloc")
            n += 1
        f.write(f"\n{'':>4}public :: ")
        n = 0
        for p in publics_wrap:
            if n == 3:
                f.write(f",&\n{'':>14}")
                n = 0
            if n == 0:
                f.write(f"wrap_{p[0]}")
            else:
                f.write(f",wrap_{p[0]}")
            n += 1
        f.write(f"\n{'':>4}public :: list_to_array\n")
        f.write(f"\n{'':>4}interface list_to_array\n")
        f.write(f"{'':>8}module procedure list_to_array_character\n")
        f.write(f"{'':>8}module procedure list_to_array_logical\n")
        for m in modules:
            t = modules[m].types
            for s in t:
                if not t[s].parent:
                    f.write(f"{'':>8}module procedure list_to_array_{s.lower()}\n")
        f.write(f"{'':>4}end interface\n")
        f.write(f"\n{'':>4}interface\n")
        for p in publics_unwrap:
            f.write(f"\n{'':>8}module subroutine unwrap_{p[0]}(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            if p[1] == 'class':
                f.write(f"{'':>12}class({p[0]}), allocatable, intent(out) :: for_var\n")
            else:
                f.write(f"{'':>12}type({p[0]}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p[0]}\n")
        for p in publics_unwrap_no_alloc:
            f.write(f"\n{'':>8}module subroutine unwrap_{p}_no_alloc(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}class({p}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p}_no_alloc\n")
        for p in publics_wrap:
            f.write(f"\n{'':>8}module subroutine wrap_{p[0]}(for_var,py_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            if p[1] == 'class':
                f.write(f"{'':>12}class({p[0]}), intent(out) :: for_var\n")
            else:
                f.write(f"{'':>12}type({p[0]}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine wrap_{p[0]}\n")
        f.write(f"\n{'':>8}module subroutine list_to_array_character(procedure_name,var_name,my_list,arr,ierror)\n")
        f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
        f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
        f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
        f.write(f"{'':>12}character(len=*), dimension(:), allocatable, intent(out) :: arr\n")
        f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
        f.write(f"{'':>8}end subroutine list_to_array_character\n")
        f.write(f"\n{'':>8}module subroutine list_to_array_logical(procedure_name,var_name,my_list,arr,ierror)\n")
        f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
        f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
        f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
        f.write(f"{'':>12}logical, dimension(:), allocatable, intent(out) :: arr\n")
        f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
        f.write(f"{'':>8}end subroutine list_to_array_logical\n")
        for m in modules:
            t = modules[m].types
            for s in t:
                if not t[s].parent:
                    f.write(f"\n{'':>8}module subroutine list_to_array_{s.lower()}(procedure_name,var_name,my_list,arr,ierror)\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                    f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                    if t[s].childs:
                        f.write(f"{'':>12}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
                    else:
                        f.write(f"{'':>12}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
                    f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                    f.write(f"{'':>8}end subroutine list_to_array_{s.lower()}\n")
        f.write(f"\n{'':>4}end interface\n")
        f.write(f"\nEnd Module CFML_Wraps\n")

def write_list_to_array(f,t : dict,s : str,lucy : dict) -> None:

    f.write(f"\n{'':>4}Module Subroutine List_to_Array_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    if t[s].childs:
        f.write(f"{'':>8}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
    else:
        f.write(f"{'':>8}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,n\n")
    if t[s].childs:
        f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
        tipos = [t[s]]
        f.write(f"{'':>8}type({s}) :: src_1\n")
        n = 2
        for ch in t[s].childs:
            f.write(f"{'':>8}type({ch[0]}) :: src{n}\n")
            n += 1
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
    f.write(f"{'':>8}if (n > 0) then\n")
    if not t[s].childs:
        f.write(f"{'':>12}allocate(arr(n))\n")
    f.write(f"{'':>12}do i = 0 , n-1\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_dict,item)\n")
    if t[s].childs:
        f.write(f"{'':>16}if (ierror == 0) then\n")
        f.write(f"{'':>20}ierror = my_dict%getitem(fortran_type,'fortran_type')\n")
        f.write(f"{'':>20}if (ierror /= 0) then\n")
        f.write(f"{'':>24}err_cfml%flag = .true.\n")
        f.write(f"{'':>24}err_cfml%ierr = -1\n")
        f.write(f"{'':>24}err_cfml%msg  = 'List_To_Array_{s}: Cannot determine fortran type'\n")
        f.write(f"{'':>20}else if (fortran_type == '{s}') then\n")
        f.write(f"{'':>24}allocate(arr(n),source=src1)\n")
        n = 2
        for ch in t[s].childs:
            f.write(f"{'':>20}else if (fortran_type == '{ch[0]}') then\n")
            f.write(f"{'':>24}allocate(arr(n),source=src{n})\n")
            n += 1
        f.write(f"{'':>20}else\n")
        f.write(f"{'':>24}ierror = -1\n")
        f.write(f"{'':>24}err_cfml%flag = .true.\n")
        f.write(f"{'':>24}err_cfml%ierr = -1\n")
        f.write(f"{'':>24}err_cfml%msg  = 'List_To_Array_{s}: Wrong fortran type'\n")
        f.write(f"{'':>20}end if\n")
        f.write(f"{'':>16}end if\n")
        f.write(f"{'':>16}if (ierror == 0) call unwrap_{s}_no_alloc(my_dict,arr(i+1),ierror)\n")
    else:
        f.write(f"{'':>16}if (ierror == 0) call unwrap_{s}(my_dict,arr(i+1),ierror)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>12}end do\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine List_to_Array_{s}\n")

def write_unwrap_proc(f,t : dict,s : str,lucy : dict) -> None:

    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    if t[s].childs:
        publics_unwrap.append([s,'class'])
        f.write(f"{'':>8}class({s}), allocatable, intent(out) :: for_var\n")
    else:
        publics_unwrap.append([s,'type'])
        f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = [t[s]]
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_unwrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    # Procedure
    f.write(f"{'':>8}ierror = py_var%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = ierror\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    f.write(f"{'':>12}if (fortran_type == '{s}') then\n")
    f.write(f"{'':>16}allocate({s} :: for_var)\n")
    childs = t[s].childs
    if childs:
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    f.write(f"{'':>12}else if (fortran_type == '{ch[0]}') then\n")
                    f.write(f"{'':>16}allocate({ch[0]} :: for_var)\n")
                    n += 1
            level += 1
    f.write(f"{'':>12}else\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t[s],8,'for_var',lucy)
    if childs:
        f.write(f"{'':>8}if (ierror == 0) then\n")
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    if (n == 0):
                        f.write(f"{'':>12}select type (A => for_var)\n")
                    f.write(f"{'':>16}class is ({ch[0]})\n")
                    write_unwrap_type(f,t[ch[0]],20,'A',lucy)
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}: Unwrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}\n")

def write_unwrap_proc_no_alloc(f,t : dict,s : str,lucy : dict) -> None:

    publics_unwrap_no_alloc.append(s)
    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}_no_alloc(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    if t[s].childs:
        f.write(f"{'':>8}class({s}), intent(out) :: for_var\n")
    else:
        f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = [t[s]]
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_unwrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    # Procedure
    f.write(f"{'':>8}ierror = py_var%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = ierror\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_no_alloc: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    f.write(f"{'':>12}if (fortran_type /= '{s}' &\n")
    childs = t[s].childs
    for ch in childs:
        f.write(f"{'':>16}.and. fortran_type /= '{ch[0]}' &\n")
    f.write(f"{'':>16}) then\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t[s],8,'for_var',lucy)
    if childs:
        f.write(f"{'':>8}if (ierror == 0) then\n")
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    if (n == 0):
                        f.write(f"{'':>12}select type (A => for_var)\n")
                    f.write(f"{'':>16}class is ({ch[0]})\n")
                    write_unwrap_type(f,t[ch[0]],20,'A',lucy)
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_no_alloc: Unwrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}_no_alloc\n")

def write_unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{forvar}%{var.name},ierror)\n")
            else:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,dict_{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call unwrap_{lucy[var.ftype]}('Unwrap_{t.name}','{var.name}',dict_{var.name},{forvar}%{var.name},ierror)\n")
        else:
            if var.ftype.lower() in NUMERICALS:
                if var.ftype.lower() == 'integer':
                    pointer = 'p_int_'+str(var.ndim)+'d'
                else:
                    pointer = 'p_real_'+str(var.ndim)+'d'
                if var.allocatable:
                    func = 'pointer_to_array_alloc'
                else:
                    func = 'pointer_to_array'
                if var.ndim == 1:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror)\n")
                else:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror,order)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror,order)\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(my_list)\n")
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,my_list,ierror)\n")
                f.write(f"{tab}if (ierror == 0) call list_to_array('Unwrap_{t.name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call my_list%destroy\n")

def write_wraps_primitives():

    w_name = 'Wraps_Primitives'
    w_file = os.path.join('../Python_API/CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"\n{'':>4}contains\n")
        f.write(f"\n{'':>4}subroutine list_to_array_character(procedure_name,var_name,my_list,arr,ierror)\n")
        f.write(f"\n{'':>8}! Arguments\n")
        f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
        f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
        f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
        f.write(f"{'':>8}character(len=*), dimension(:), allocatable, intent(out) :: arr\n")
        f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
        f.write(f"\n{'':>8}! Local variables\n")
        f.write(f"{'':>8}integer :: i,n\n")
        f.write(f"{'':>8}character(len=:), allocatable :: my_str\n")
        f.write(f"{'':>8}type(object) :: item\n")
        f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
        f.write(f"{'':>8}if (n > 0) then\n")
        f.write(f"{'':>12}allocate(arr(n))\n")
        f.write(f"{'':>12}do i = 0 , n-1\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_str,item)\n")
        f.write(f"{'':>16}if (ierror == 0) arr(i+1) = my_str\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = err_cfml%ierr\n")
        f.write(f"{'':>12}end do\n")
        f.write(f"{'':>8}end if\n")
        f.write(f"\n{'':>4}end subroutine list_to_array_character\n")
        f.write(f"\n{'':>4}subroutine list_to_array_logical(procedure_name,var_name,my_list,arr,ierror)\n")
        f.write(f"\n{'':>8}! Arguments\n")
        f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
        f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
        f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
        f.write(f"{'':>8}logical, dimension(:), allocatable, intent(out) :: arr\n")
        f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
        f.write(f"\n{'':>8}! Local variables\n")
        f.write(f"{'':>8}integer :: i,n\n")
        f.write(f"{'':>8}type(object) :: item\n")
        f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
        f.write(f"{'':>8}if (n > 0) then\n")
        f.write(f"{'':>12}allocate(arr(n))\n")
        f.write(f"{'':>12}do i = 0 , n-1\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = cast(arr(i+1),item)\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = err_cfml%ierr\n")
        f.write(f"{'':>12}end do\n")
        f.write(f"{'':>8}end if\n")
        f.write(f"\n{'':>4}end subroutine list_to_array_logical\n")
        f.write(f"\nend submodule")

def write_wrap_proc(f,t : dict,s : str,lucy : dict) -> None:

    f.write(f"\n{'':>4}Module Subroutine Wrap_{s}(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    if t[s].childs:
        publics_wrap.append([s,'class'])
        f.write(f"{'':>8}class({s}), intent(in) :: for_var\n")
    else:
        publics_wrap.append([s,'type'])
        f.write(f"{'':>8}type({s}), intent(in) :: for_var\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = [t[s]]
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_wrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    # Procedure
    write_wrap_type(f,t[s],8,'for_var',lucy)
    childs = t[s].childs
    if childs:
        f.write(f"{'':>8}if (ierror == 0) then\n")
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    if (n == 0):
                        f.write(f"{'':>12}select type (A => for_var)\n")
                    f.write(f"{'':>16}class is ({ch[0]})\n")
                    write_wrap_type(f,t[ch[0]],20,'A',lucy)
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Wrap_{s}: Wrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Wrap_{s}\n")

def write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',{forvar}%{var.name})\n")
            else:
                f.write(f"{tab}if (ierror == 0) call wrap_{lucy[var.ftype]}(for_var%{var.name},di_{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',di_{var.name})\n")
        else:
            if var.ftype.lower() in NUMERICALS:
                f.write(f"{tab}if (ierror == 0) ierror = ndarray_create(nd_{var.name},for_var%{var.name})\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',nd_{var.name})\n")
            elif var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                f.write(f"{tab}if (ierror == 0) then\n")
                f.write(f"{tab}    do i = 1 , size(for_var%{var.name})\n")
                f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append(for_var%{var.name}(i))\n")
                f.write(f"{tab}    end do\n")
                f.write(f"{tab}end if\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                f.write(f"{tab}if (ierror == 0) allocate(di_{var.name}(size(for_var%{var.name})))\n")
                f.write(f"{tab}if (ierror == 0) then\n")
                f.write(f"{tab}    do i = 1 , size(for_var%{var.name})\n")
                f.write(f"{tab}        ierror = dict_create(di_{var.name}(i))\n")
                f.write(f"{tab}        if (ierror == 0) call wrap_{lucy[var.ftype]}(for_var%{var.name},(di_{var.name}(i),ierror))\n")
                f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append(di_{var.name}(i))\n")
                f.write(f"{tab}    end do\n")
                f.write(f"{tab}end if\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")