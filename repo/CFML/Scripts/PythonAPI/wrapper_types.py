"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
delete_previous_build(build_dir : str)
local_variables_subroutine_unwrap(tipos : list) -> list
local_variables_subroutine_wrap(tipos : list) -> list
unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,classes : list)
wrap(build_dir : str,modules : dict,lucy : dict,classes : list) -> None
write_module_cfml_wraps(build_dir : str,modules : dict,classes : list) -> None
write_submodule(build_dir : str,m : cfml_objects.Module,m_name : str,
    lucy : dict,classes : list) -> None
write_subroutine_list_to_array1d(f,t : dict,s : str,classes : list,
    is_class : bool=True,is_alloc : bool=True) -> None
write_subroutine_list_to_array2d(f,t : dict,s : str,classes : list,
    is_class : bool=True,is_alloc : bool=True) -> None
write_subroutine_unwrap(f,t : dict,s : str,classes : list) -> None
write_subroutine_unwrap_no_alloc(f,t : dict,s : str,classes : list) -> None
write_subroutine_wrap(f,t : dict,s : str,lucy : dict) -> None
write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict)
"""
import cfml_objects
import glob
import os

publics_unwrap = []
publics_unwrap_no_alloc = []
publics_wrap = []
wrap_files = []

PRIMITIVES = ['integer','real','logical','character','complex']
NUMERICALS = ['integer','real','rational','complex']

def delete_previous_build(build_dir : str):

    files = glob.glob(os.path.join(build_dir,'Python/*.py'))
    for f in files:
        os.remove(f)
    files = glob.glob(os.path.join(build_dir,'Fortran/*.f90'))
    for f in files:
        os.remove(f)
    files = glob.glob(os.path.join(build_dir,'Fortran/CFML_Wraps/*.f90'))
    for f in files:
        os.remove(f)
    return None

def local_variables_subroutine_unwrap(tipos : list) -> list:

    p_int_1D = False
    p_int_2D = False
    p_int_3D = False
    p_int_4D = False
    p_real_1D = False
    p_real_2D = False
    p_real_3D = False
    p_real_4D = False
    p_complex_1D = False
    p_complex_2D = False
    p_complex_3D = False
    p_complex_4D = False
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
                    elif var.ndim == 4:
                        p_int_4D = True
                elif var.ftype.lower() == 'real' or var.ftype.lower() == 'rational':
                    if var.ndim == 1:
                        p_real_1D = True
                    elif var.ndim == 2:
                        p_real_2D = True
                    elif var.ndim == 3:
                        p_real_3D = True
                    elif var.ndim == 4:
                        p_real_4D = True
                elif var.ftype.lower() == 'complex':
                    if var.ndim == 1:
                        p_complex_1D = True
                    elif var.ndim == 2:
                        p_complex_2D = True
                    elif var.ndim == 3:
                        p_complex_3D = True
                    elif var.ndim == 4:
                        p_complex_4D = True
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
    if p_int_4D:
        lv.append('integer, dimension(:,:,:,:), pointer :: p_int_4d')
        order = True
    if p_real_1D:
        lv.append('real, dimension(:), pointer :: p_real_1d')
    if p_real_2D:
        lv.append('real, dimension(:,:), pointer :: p_real_2d')
        order = True
    if p_real_3D:
        lv.append('real, dimension(:,:,:), pointer :: p_real_3d')
        order = True
    if p_real_4D:
        lv.append('real, dimension(:,:,:,:), pointer :: p_real_4d')
        order = True
    if p_complex_1D:
        lv.append('complex, dimension(:), pointer :: p_complex_1d')
    if p_complex_2D:
        lv.append('complex, dimension(:,:), pointer :: p_complex_2d')
        order = True
    if p_complex_3D:
        lv.append('complex, dimension(:,:,:), pointer :: p_complex_3d')
        order = True
    if p_complex_4D:
        lv.append('complex, dimension(:,:,:,:), pointer :: p_complex_4d')
        order = True
    if order:
        lv.append('character(len=1) :: order')
    if my_list:
        lv.append('type(list) :: my_list')
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    return lv

def local_variables_subroutine_wrap(tipos : list) -> list:

    my_arrays = []
    my_dicts = []
    #my_dicts_alloc = []
    my_lists = []
    my_ndarrays = []
    iterator = False
    iterator_2 = False
    for t in tipos:
        for c in t.components:
            var = t.components[c]
            if var.ndim == 0:
                if var.ftype.lower() not in PRIMITIVES:
                    if f"di_{var.name}" not in my_dicts:
                        my_dicts.append(f"di_{var.name}")
            else:
                if var.ftype.lower() in NUMERICALS:
                    if f"nd_{var.name}" not in my_ndarrays:
                        my_ndarrays.append(f"nd_{var.name}")
                        if var.ftype.lower() == 'rational':
                            if var.ndim == 1:
                                my_arrays.append(f"real, dimension(:), allocatable :: {var.name}_real")
                            elif var.ndim == 2:
                                my_arrays.append(f"real, dimension(:,:), allocatable :: {var.name}_real")
                            elif var.ndim == 3:
                                my_arrays.append(f"real, dimension(:,:,:), allocatable :: {var.name}_real")
                else:
                    if f"li_{var.name}" not in my_lists:
                        my_lists.append(f"li_{var.name}")
                        if var.ndim == 2: # This is here for 2d logical or character arrays
                            my_lists.append(f"li_{var.name}_2")
                            iterator_2 = True
                        #if f"di_{var.name}" not in my_dicts_alloc:
                        #    my_dicts_alloc.append(f"di_{var.name}")
                        if f"di_{var.name}" not in my_dicts:
                            my_dicts.append(f"di_{var.name}")
                        iterator = True
    lv = []
    if iterator:
        if iterator_2:    
            lv.append('integer :: i,j')
        else:
            lv.append('integer :: i')
    for arr in my_arrays:
        lv.append(arr)
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    #if my_dicts_alloc:
    #    lv.append('type(dict), dimension(:), allocatable :: '+','.join(my_dicts_alloc))
    if my_lists:
        lv.append('type(list) :: '+','.join(my_lists))
    if my_ndarrays:
        lv.append('type(ndarray) :: '+','.join(my_ndarrays))
    return lv

def unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,classes : list):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                if var.ftype.lower() == 'character' and var.allocatable:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item_string_alloc("
                        f"'Unwrap_{t.name}','{var.name}',py_var,{forvar}%{var.name},ierror)\n")
                else:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{forvar}%{var.name},ierror)\n")
            else:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,dict_{var.name},ierror)\n")
                if not var.ftype.lower() in classes:
                    f.write(f"{tab}if (ierror == 0) call unwrap_type(dict_{var.name},{forvar}%{var.name},ierror)\n")
                else:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror == 0) call unwrap_class_{var.ftype.lower()}(dict_{var.name},{forvar}%{var.name},ierror)\n")
                    else:
                        f.write(f"{tab}if (ierror == 0) call unwrap_class_{var.ftype.lower()}_no_alloc(dict_{var.name},{forvar}%{var.name},ierror)\n")
        else:
            if var.ftype.lower() in NUMERICALS:
                if var.ftype.lower() == 'integer':
                    pointer = 'p_int_'+str(var.ndim)+'d'
                elif var.ftype.lower() == 'complex':
                    pointer = 'p_complex_'+str(var.ndim)+'d'
                else:
                    pointer = 'p_real_'+str(var.ndim)+'d'
                if var.allocatable:
                    func = 'pointer_to_alloc_array'
                else:
                    func = 'pointer_to_array'
                if var.ndim == 1:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror2)\n")
                        f.write(f"{tab}if (ierror2 == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror)\n")
                        f.write(f"{tab}if (ierror2 /= 0) then\n")
                        f.write(f"{tab}    call err_clear\n")
                        f.write(f"{tab}    call clear_error()\n")
                        f.write(f"{tab}end if\n")
                    else:
                        f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror)\n")
                        f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror)\n")
                else:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror2,order)\n")
                        f.write(f"{tab}if (ierror2 == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror,order)\n")
                        f.write(f"{tab}if (ierror2 /= 0) then\n")
                        f.write(f"{tab}    call err_clear\n")
                        f.write(f"{tab}    call clear_error()\n")
                        f.write(f"{tab}end if\n")
                    else:
                        f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror,order)\n")
                        f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror,order)\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(my_list)\n")
                if var.allocatable:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,my_list,ierror2)\n")
                else:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,my_list,ierror)\n")
                if var.is_class:
                    f.write(f"{tab}if (ierror == 0) call list_to_class_array_{var.ftype.lower()}('Unwrap_{t.name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                else:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror2 == 0) then\n")
                        if var.ftype.lower() == 'logical' or var.ftype.lower() == 'character':
                            f.write(f"{tab}    if (ierror == 0) call list_to_alloc_array_primitive(my_list,{forvar}%{var.name},ierror)\n")
                        else:
                            f.write(f"{tab}    if (ierror == 0) call list_to_type_array('Unwrap_{t.name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                        f.write(f"{tab}else\n")
                        f.write(f"{tab}    call err_clear\n")
                        f.write(f"{tab}    call clear_error()\n")
                        f.write(f"{tab}end if\n")
                    else:
                        if var.ftype.lower() == 'logical' or var.ftype.lower() == 'character':
                            f.write(f"{tab}if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,{forvar}%{var.name},ierror)\n")
                        else:
                            f.write(f"{tab}if (ierror == 0) call list_to_type_array_no_alloc('Unwrap_{t.name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call my_list%destroy\n")

def wrap(build_dir : str,modules : dict,lucy : dict,classes : list) -> None:

    if not os.path.isdir(os.path.join(build_dir,'Python')):
        os.mkdir(os.path.join(build_dir,'Python'))
    if not os.path.isdir(os.path.join(build_dir,'Fortran')):
        os.mkdir(os.path.join(build_dir,'Fortran'))
    if not os.path.isdir(os.path.join(build_dir,'Fortran/CFML_Wraps')):
        os.mkdir(os.path.join(build_dir,'Fortran/CFML_Wraps'))
    delete_previous_build(build_dir)
    for m in modules:
        write_submodule(build_dir,modules[m],m,lucy,classes)
    write_module_cfml_wraps(build_dir,modules,classes)
    return None

def write_module_cfml_wraps(build_dir : str,modules : dict,classes : list):

    w_file = os.path.join(build_dir,'Fortran/CFML_Wraps.f90')
    with open(w_file,'w') as f:
        # Module name
        f.write(f"\nModule CFML_Wraps")
        # Uses
        f.write(f"\n\n{'':>4}use forpy_mod")
        f.write(f"\n{'':>4}use CFML_GlobalDeps\n")
        for m in modules:
            if m.lower() == 'cfml_rational':
                f.write(f"{'':>4}use {m}\n") # We need the operator =
            else:
                if modules[m].types: # Use only types
                    f.write(f"{'':>4}use {m}, only: ")
                    n = 0
                    for s in modules[m].types:
                        if n == 0:
                            f.write(f"{s}")
                        else:
                            f.write(f",{s}")
                        n += 1
                    f.write(f"\n")
        f.write(f"{'':>4}use CFML_Wraps_Utils")
        f.write(f"\n\n{'':>4}implicit none")
        f.write(f"\n\n{'':>4}private")
        # Public procedures list_to_class_array
        for m in modules:
            t = modules[m].types
            for s in t:
                if s in classes:
                    f.write(f"\n{'':>4}public :: list_to_class_array_{s.lower()}")
        # Public procedures list_to_type_array
        f.write(f"\n{'':>4}public :: list_to_type_array") 
        f.write(f"\n{'':>4}public :: list_to_type_array_no_alloc") 
        # Public procedures unwrap
        for p in publics_unwrap:
            if p[1] == 'class':
                f.write(f"\n{'':>4}public :: unwrap_{p[1]}_{p[0]}")
        for p in publics_unwrap_no_alloc:
            f.write(f"\n{'':>4}public :: unwrap_class_{p}_no_alloc")
        f.write(f"\n{'':>4}public :: unwrap_type") 
        # Public procedures wrap
        f.write(f"\n{'':>4}public :: wrap_type\n")
        # Overloads for list_to_class_array
        for m in modules:
            t = modules[m].types
            for s in t:
                if s in classes:
                    f.write(f"\n{'':>4}interface list_to_class_array_{s.lower()}")
                    f.write(f"\n{'':>8}module procedure list_to_class_array1d_{s.lower()}")
                    f.write(f"\n{'':>8}module procedure list_to_class_array2d_{s.lower()}")
                    f.write(f"\n{'':>4}end interface\n")
        # Overloads for list_to_type_array
        f.write(f"\n{'':>4}interface list_to_type_array\n")
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"{'':>8}module procedure list_to_type_array1d_{s.lower()}\n")
                f.write(f"{'':>8}module procedure list_to_type_array2d_{s.lower()}\n")
        f.write(f"{'':>4}end interface\n")
        # Overloads for list_to_type_array_no_alloc
        f.write(f"\n{'':>4}interface list_to_type_array_no_alloc\n")
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"{'':>8}module procedure list_to_type_array1d_{s.lower()}_no_alloc\n")
                f.write(f"{'':>8}module procedure list_to_type_array2d_{s.lower()}_no_alloc\n")
        f.write(f"{'':>4}end interface\n")
        # Overloads for unwrap_type
        f.write(f"\n{'':>4}interface unwrap_type\n")
        for p in publics_unwrap:
            if p[1] == 'type':
                f.write(f"{'':>8}module procedure unwrap_{p[1]}_{p[0]}\n")
        f.write(f"{'':>4}end interface\n") 
        # Overloads for wrap_type
        f.write(f"\n{'':>4}interface wrap_type\n")
        for p in publics_wrap:
            f.write(f"{'':>8}module procedure wrap_{p[0]}\n")
        f.write(f"{'':>4}end interface\n")
        f.write(f"\n{'':>4}interface\n")
        # Interface list to class_array
        for m in modules:
            t = modules[m].types
            for s in t:
                if s in classes:
                    f.write(f"\n{'':>8}module subroutine list_to_class_array1d_{s.lower()}(procedure_name,var_name,my_list,arr,ierror)\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                    f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                    f.write(f"{'':>12}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
                    f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                    f.write(f"{'':>8}end subroutine list_to_class_array1d_{s.lower()}\n")
                    f.write(f"\n{'':>8}module subroutine list_to_class_array2d_{s.lower()}(procedure_name,var_name,my_list,arr,ierror)\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                    f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                    f.write(f"{'':>12}class({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
                    f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                    f.write(f"{'':>8}end subroutine list_to_class_array2d_{s.lower()}\n")
        # Interface List to array_type
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"\n{'':>8}module subroutine list_to_type_array1d_{s.lower()}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_type_array1d_{s.lower()}\n")
                f.write(f"\n{'':>8}module subroutine list_to_type_array2d_{s.lower()}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_type_array2d_{s.lower()}\n")
        # Interface List to array_type_no_alloc
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"\n{'':>8}module subroutine list_to_type_array1d_{s.lower()}_no_alloc(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(*), intent(inout) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_type_array1d_{s.lower()}_no_alloc\n")
                f.write(f"\n{'':>8}module subroutine list_to_type_array2d_{s.lower()}_no_alloc(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:,:), intent(inout) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_type_array2d_{s.lower()}_no_alloc\n")  
        # Interface unwraps procedures
        for p in publics_unwrap:
            f.write(f"\n{'':>8}module subroutine unwrap_{p[1]}_{p[0]}(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            if p[1] == 'class':
                f.write(f"{'':>12}class({p[0]}), allocatable, intent(out) :: for_var\n")
            else:
                f.write(f"{'':>12}type({p[0]}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p[1]}_{p[0]}\n")
        # Interface unwraps_no_alloc procedures
        for p in publics_unwrap_no_alloc:
            f.write(f"\n{'':>8}module subroutine unwrap_class_{p}_no_alloc(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}class({p}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_class_{p}_no_alloc\n")
        # Interface wraps procedures
        for p in publics_wrap:
            f.write(f"\n{'':>8}module subroutine wrap_{p[0]}(for_var,py_var,ierror)\n")
            if p[1] == 'class':
                f.write(f"{'':>12}class({p[0]}), intent(inout) :: for_var\n")
            else:
                f.write(f"{'':>12}type({p[0]}), intent(inout) :: for_var\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine wrap_{p[0]}\n")
        f.write(f"\n{'':>4}end interface\n")
        f.write(f"\nEnd Module CFML_Wraps\n")

def write_submodule(build_dir : str,m : cfml_objects.Module,m_name : str,
    lucy : dict,classes : list) -> None:

    w_name = 'Wraps_'+m_name[5:]
    os.path.join(build_dir,'Fortran',w_name+'.f90')
    wrap_files.append(w_name+'.f90')
    w_file = os.path.join(build_dir,'Fortran/CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"{'':>4}contains\n")
        t = m.types
        for s in t:
            if not t[s].parent:
                write_subroutine_wrap(f,t,s,lucy)
            if s in classes:
                write_subroutine_unwrap_no_alloc(f,t,s,classes)
                write_subroutine_list_to_array1d(f,t,s,classes,is_class=True)
                write_subroutine_list_to_array2d(f,t,s,classes,is_class=True)
            write_subroutine_unwrap(f,t,s,classes)
            write_subroutine_list_to_array1d(f,t,s,classes,is_class=False,is_alloc=True)
            write_subroutine_list_to_array1d(f,t,s,classes,is_class=False,is_alloc=False) 
            write_subroutine_list_to_array2d(f,t,s,classes,is_class=False,is_alloc=True)
            write_subroutine_list_to_array2d(f,t,s,classes,is_class=False,is_alloc=False)
        f.write(f"\nend submodule")

def write_subroutine_list_to_array1d(f,t : dict,s : str,classes : list,is_class : bool=True,is_alloc : bool=True) -> None:

    if is_class:
        name = f'List_to_class_array1d_{s}'
    else:
        name = f'list_to_type_array1d_{s}'
        if not is_alloc:
            name = name + '_no_alloc'
    f.write(f"\n{'':>4}Module Subroutine {name}(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    if is_class:
        f.write(f"{'':>8}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
    else:
        if is_alloc:
            f.write(f"{'':>8}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
        else:
            f.write(f"{'':>8}type({s}), dimension(*), intent(inout) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,n\n")
    if is_class:
        f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
    f.write(f"{'':>8}if (ierror == 0 .and. n > 0) then\n")
    if is_class:
        f.write(f"{'':>12}! Get the fortran type from the first element of the list\n")
        f.write(f"{'':>12}ierror = my_list%getitem(item,0)\n")
        f.write(f"{'':>12}if (ierror == 0) ierror = cast(my_dict,item)\n")
        f.write(f"{'':>12}if (ierror == 0) ierror = my_dict%getitem(fortran_type,'fortran_type')\n")
        f.write(f"{'':>12}if (ierror /= 0) then\n")
        f.write(f"{'':>16}err_cfml%flag = .true.\n")
        f.write(f"{'':>16}err_cfml%ierr = -1\n")
        f.write(f"{'':>16}err_cfml%msg  = '{name}: Cannot determine fortran type'\n")
        f.write(f"{'':>12}else if (fortran_type == '{s}') then\n")
        f.write(f"{'':>16}allocate({s} :: arr(n))\n")
        n = 2
        for ch in t[s].childs:
            f.write(f"{'':>12}else if (fortran_type == '{ch[0]}') then\n")
            f.write(f"{'':>16}allocate({ch[0]} :: arr(n))\n")
            n += 1
        f.write(f"{'':>12}else\n")
        f.write(f"{'':>16}ierror = -1\n")
        f.write(f"{'':>16}err_cfml%flag = .true.\n")
        f.write(f"{'':>16}err_cfml%ierr = -1\n")
        f.write(f"{'':>16}err_cfml%msg  = '{name}: Wrong fortran type'\n")
        f.write(f"{'':>16}return\n")
        f.write(f"{'':>12}end if\n")
    elif is_alloc:
        f.write(f"{'':>12}allocate(arr(n))\n")
    f.write(f"{'':>12}do i = 0 , n-1\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_dict,item)\n")
    if s in classes:
        f.write(f"{'':>16}if (ierror == 0) call unwrap_class_{s}_no_alloc(my_dict,arr(i+1),ierror)\n")
    else:
        f.write(f"{'':>16}if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>12}end do\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine {name}\n")

def write_subroutine_list_to_array2d(f,t : dict,s : str,classes : list,is_class : bool=True,is_alloc : bool=True) -> None:

    if is_class:
        name = f'List_to_class_array2d_{s}'
    else:
        name = f'list_to_type_array2d_{s}'
        if not is_alloc:
            name = name + '_no_alloc'
    f.write(f"\n{'':>4}Module Subroutine {name}(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    if is_class:
        f.write(f"{'':>8}class({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
    else:
        if is_alloc:
            f.write(f"{'':>8}type({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
        else:
            f.write(f"{'':>8}type({s}), dimension(:,:), intent(inout) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,j,n,m\n")
    if is_class:
        f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"{'':>8}type(list) :: li\n")
    f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
    f.write(f"{'':>8}if (ierror == 0 .and. n > 0) then\n")
    f.write(f"{'':>12}ierror = my_list%getitem(item,0)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = cast(li,item)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = li%len(m)\n")
    f.write(f"{'':>12}if (ierror == 0 .and. m > 0) then\n")
    if is_class:
        f.write(f"{'':>16}ierror = li%getitem(item,0)\n")
        f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_dict,item)\n")
        f.write(f"{'':>16}if (ierror == 0) then\n")
        f.write(f"{'':>20}ierror = my_dict%getitem(fortran_type,'fortran_type')\n")
        f.write(f"{'':>20}if (ierror /= 0) then\n")
        f.write(f"{'':>24}err_cfml%flag = .true.\n")
        f.write(f"{'':>24}err_cfml%ierr = -1\n")
        f.write(f"{'':>24}err_cfml%msg  = '{name}: Cannot determine fortran type'\n")
        f.write(f"{'':>20}else if (fortran_type == '{s}') then\n")
        f.write(f"{'':>24}allocate({s} :: arr(n,m))\n")
        n = 2
        for ch in t[s].childs:
            f.write(f"{'':>20}else if (fortran_type == '{ch[0]}') then\n")
            f.write(f"{'':>24}allocate({ch[0]} :: arr(n,m))\n")
            n += 1
        f.write(f"{'':>20}else\n")
        f.write(f"{'':>24}ierror = -1\n")
        f.write(f"{'':>24}err_cfml%flag = .true.\n")
        f.write(f"{'':>24}err_cfml%ierr = -1\n")
        f.write(f"{'':>24}err_cfml%msg  = '{name}: Wrong fortran type'\n")
        f.write(f"{'':>24}return\n")
        f.write(f"{'':>20}end if\n")
        f.write(f"{'':>16}end if\n")
    elif is_alloc:
        f.write(f"{'':>16}allocate(arr(n,m))\n")
    f.write(f"{'':>16}if (ierror == 0) then\n")
    f.write(f"{'':>20}do i = 0 , n-1\n")
    f.write(f"{'':>24}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>24}if (ierror == 0) ierror = cast(li,item)\n")
    f.write(f"{'':>24}do j = 0 , m-1\n")
    f.write(f"{'':>28}if (ierror == 0) ierror = li%getitem(item,j)\n")
    f.write(f"{'':>28}if (ierror == 0) ierror = cast(my_dict,item)\n")
    if s in classes:
        f.write(f"{'':>28}if (ierror == 0) call unwrap_class_{s}_no_alloc(my_dict,arr(i+1,j+1),ierror)\n")
    else:
        f.write(f"{'':>28}if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)\n")
    f.write(f"{'':>28}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>24}end do\n")
    f.write(f"{'':>20}end do\n")
    f.write(f"{'':>16}end if\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine {name}\n")

def write_subroutine_unwrap(f,t : dict,s : str,classes : list) -> None:

    if s in classes:
        name = f'Unwrap_class_{s}'
    else:
        name = f'Unwrap_type_{s}'
    f.write(f"\n{'':>4}Module Subroutine {name}(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    if s in classes:
        publics_unwrap.append([s,'class'])
        f.write(f"{'':>8}class({s}), allocatable, intent(out) :: for_var\n")
    else:
        publics_unwrap.append([s,'type'])
        f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = []
    padre = t[s].parent
    while padre:
        tipos.append(t[padre])
        padre = t[padre].parent
    tipos.append(t[s])
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_subroutine_unwrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: ierror2\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    f.write(f"{'':>8}ierror2 = 0\n")
    # Procedure
    f.write(f"{'':>8}ierror = py_var%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = ierror\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    childs = t[s].childs
    if childs:
        f.write(f"{'':>12}if (fortran_type == '{s}') then\n")
        f.write(f"{'':>16}allocate({s} :: for_var)\n")
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
        f.write(f"{'':>16}return\n")
        f.write(f"{'':>12}end if\n")
    else:
        f.write(f"{'':>12}if (fortran_type /= '{s}') then\n")
        f.write(f"{'':>16}ierror = -1\n")
        f.write(f"{'':>16}err_cfml%flag = .true.\n")
        f.write(f"{'':>16}err_cfml%ierr = ierror\n")
        f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
        f.write(f"{'':>16}return\n")
        f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    padre = t[s].parent
    while padre:
        unwrap_type(f,t[padre],8,'for_var',classes)
        padre = t[padre].parent
    unwrap_type(f,t[s],8,'for_var',classes)
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
                    unwrap_type(f,t[ch[0]],20,'A',classes)
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}if (err_cfml%ierr == 0) then\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = -1\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}: Unwrapping failed'\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine {name}\n")

def write_subroutine_unwrap_no_alloc(f,t : dict,s : str,classes : list) -> None:

    publics_unwrap_no_alloc.append(s)
    f.write(f"\n{'':>4}Module Subroutine Unwrap_class_{s}_no_alloc(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    if s in classes:
        f.write(f"{'':>8}class({s}), intent(out) :: for_var\n")
    else:
        f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = []
    padre = t[s].parent
    while padre:
        tipos.append(t[padre])
        padre = t[padre].parent
    tipos.append(t[s])
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_subroutine_unwrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: ierror2\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    f.write(f"{'':>8}ierror2 = 0\n")
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
    f.write(f"{'':>16}return\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    padre = t[s].parent
    while padre:
        unwrap_type(f,t[padre],8,'for_var',classes)
        padre = t[padre].parent
    unwrap_type(f,t[s],8,'for_var',classes)
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
                    unwrap_type(f,t[ch[0]],20,'A',classes)
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}if (err_cfml%ierr == 0) then\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = -1\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}_no_alloc: Unwrapping failed'\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_class_{s}_no_alloc\n")

def write_subroutine_wrap(f,t : dict,s : str,lucy : dict) -> None:

    f.write(f"\n{'':>4}Module Subroutine Wrap_{s}(for_var,py_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    if t[s].childs:
        publics_wrap.append([s,'class'])
        f.write(f"{'':>8}class({s}), intent(inout) :: for_var\n")
    else:
        publics_wrap.append([s,'type'])
        f.write(f"{'':>8}type({s}), intent(inout) :: for_var\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    tipos = [t[s]]
    for ch in t[s].childs:
        tipos.append(t[ch[0]])
    local_var = local_variables_subroutine_wrap(tipos)
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
    f.write(f"{'':>12}if (err_cfml%ierr == 0) then\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = -1\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Wrap_{s}: Wrapping failed'\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Wrap_{s}\n")

def write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str,lucy : dict):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('fortran_type','{t.name}')\n")
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                if var.ftype.lower() == 'character':
                    f.write(f"{tab}if (ierror == 0) then\n")
                    f.write(f"{tab}    ierror = py_var%setitem('{var.name}',{forvar}%{var.name})\n")
                    f.write(f"{tab}    if (ierror /= 0) then\n")
                    f.write(f"{tab}        {forvar}%{var.name} = ''\n")
                    f.write(f"{tab}        ierror = py_var%setitem('{var.name}',{forvar}%{var.name})\n")
                    f.write(f"{tab}    end if\n")
                    f.write(f"{tab}end if\n")
                else:
                    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',{forvar}%{var.name})\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = dict_create(di_{var.name})\n")
                f.write(f"{tab}if (ierror == 0) call wrap_type({forvar}%{var.name},di_{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',di_{var.name})\n")
        else:
            if var.allocatable:
                tab2 = tab + '    '
                f.write(f"{tab}if (allocated({forvar}%{var.name})) then\n")
            else:
                tab2 = tab
            if var.ftype.lower() in NUMERICALS:
                if var.ftype.lower() == 'rational':
                    if var.ndim == 1:
                        f.write(f"{tab2}if (ierror == 0) allocate({var.name}_real(size({forvar}%{var.name})))\n")
                    elif var.ndim == 2:
                        f.write(f"{tab2}if (ierror == 0) allocate({var.name}_real(size({forvar}%{var.name},1),size({forvar}%{var.name},2)))\n")
                    elif var.ndim == 3:
                        f.write(f"{tab2}if (ierror == 0) allocate({var.name}_real(size({forvar}%{var.name},1),size({forvar}%{var.name},2),size({forvar}%{var.name},3)))\n")
                    f.write(f"{tab2}if (ierror == 0) {var.name}_real = {forvar}%{var.name}\n")
                    f.write(f"{tab2}if (ierror == 0) ierror = ndarray_create(nd_{var.name},{var.name}_real)\n")
                else:
                    f.write(f"{tab2}if (ierror == 0) ierror = ndarray_create(nd_{var.name},{forvar}%{var.name})\n")
                f.write(f"{tab2}if (ierror == 0) ierror = py_var%setitem('{var.name}',nd_{var.name})\n")
            elif var.ftype.lower() in PRIMITIVES:
                if var.ndim == 1:
                    f.write(f"{tab2}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab2}if (ierror == 0) then\n")
                    f.write(f"{tab2}    do i = 1 , size({forvar}%{var.name})\n")
                    if var.ftype.lower() == 'character':
                        f.write(f"{tab2}        if (ierror == 0) then\n")
                        f.write(f"{tab2}            ierror = li_{var.name}%append({forvar}%{var.name}(i))\n")
                        f.write(f"{tab2}            if (ierror /= 0) then\n")
                        f.write(f"{tab2}                {forvar}%{var.name}(i) = ''\n")
                        f.write(f"{tab2}                ierror = li_{var.name}%append({forvar}%{var.name}(i))\n")
                        f.write(f"{tab2}            end if\n")
                        f.write(f"{tab2}        end if\n")
                    else:
                        f.write(f"{tab2}        if (ierror == 0) ierror = li_{var.name}%append({forvar}%{var.name}(i))\n")
                    f.write(f"{tab2}    end do\n")
                    f.write(f"{tab2}end if\n")
                    f.write(f"{tab2}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
                elif var.ndim == 2:
                    f.write(f"{tab2}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab2}if (ierror == 0) then\n")
                    f.write(f"{tab2}    do i = 1 , size({forvar}%{var.name},1)\n")
                    f.write(f"{tab2}        if (ierror == 0) ierror = list_create(li_{var.name}_2)\n")
                    f.write(f"{tab2}        do j = 1 , size({forvar}%{var.name},2)\n")
                    if var.ftype.lower() == 'character':
                        f.write(f"{tab2}        if (ierror == 0) then\n")
                        f.write(f"{tab2}            ierror = li_{var.name}_2%append({forvar}%{var.name}(i,j))\n")
                        f.write(f"{tab2}            if (ierror /= 0) then\n")
                        f.write(f"{tab2}                {forvar}%{var.name}(i,j) = ''\n")
                        f.write(f"{tab2}                ierror = li_{var.name}_2%append({forvar}%{var.name}(i,j))\n")
                        f.write(f"{tab2}            end if\n")
                        f.write(f"{tab2}        end if\n")
                    else:
                        f.write(f"{tab2}        if (ierror == 0) ierror = li_{var.name}_2%append({forvar}%{var.name}(i,j))\n")
                    f.write(f"{tab2}        end do\n")
                    f.write(f"{tab2}        if (ierror == 0) ierror = li_{var.name}%append(li_{var.name}_2)\n")
                    f.write(f"{tab2}        call li_{var.name}_2%destroy\n")
                    f.write(f"{tab2}    end do\n")
                    f.write(f"{tab2}end if\n")
                    f.write(f"{tab2}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
            else:
                if var.ndim == 1:
                    f.write(f"{tab2}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab2}if (ierror == 0) then\n")
                    f.write(f"{tab2}    do i = 1 , size({forvar}%{var.name})\n")
                    f.write(f"{tab2}        ierror = dict_create(di_{var.name})\n")
                    f.write(f"{tab2}        if (ierror == 0) call wrap_type({forvar}%{var.name}(i),di_{var.name},ierror)\n")
                    f.write(f"{tab2}        if (ierror == 0) ierror = li_{var.name}%append(di_{var.name})\n")
                    f.write(f"{tab2}        if (ierror == 0) call di_{var.name}%destroy\n")
                    f.write(f"{tab2}    end do\n")
                    f.write(f"{tab2}end if\n")
                    f.write(f"{tab2}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
                elif var.ndim == 2:
                    f.write(f"{tab2}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab2}if (ierror == 0) then\n")
                    f.write(f"{tab2}    do i = 1 , size({forvar}%{var.name},1)\n")
                    f.write(f"{tab2}        if (ierror == 0) ierror = list_create(li_{var.name}_2)\n")
                    f.write(f"{tab2}        do j = 1 , size({forvar}%{var.name},2)\n")
                    f.write(f"{tab2}            ierror = dict_create(di_{var.name})\n")
                    f.write(f"{tab2}            if (ierror == 0) call wrap_type({forvar}%{var.name}(i,j),di_{var.name},ierror)\n")                    
                    f.write(f"{tab2}            if (ierror == 0) ierror = li_{var.name}_2%append(di_{var.name})\n")
                    f.write(f"{tab2}            if (ierror == 0) call di_{var.name}%destroy\n")
                    f.write(f"{tab2}        end do\n")
                    f.write(f"{tab2}        if (ierror == 0) ierror = li_{var.name}%append(li_{var.name}_2)\n")
                    f.write(f"{tab2}        call li_{var.name}_2%destroy\n")
                    f.write(f"{tab2}    end do\n")
                    f.write(f"{tab2}end if\n")
                    f.write(f"{tab2}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
            if var.allocatable:
                f.write(f"{tab}end if\n")
