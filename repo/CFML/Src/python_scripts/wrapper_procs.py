"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
write_header(f,m : cfml_objects.Module)
write_py_init(f,m : cfml_objects.Module)
write_uses(f,uses : list)
wrap(modules : dict,lucy : dict) -> None
wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str,lucy : dict) -> None
"""
import cfml_objects
import os

def write_header(f,m : cfml_objects.Module):

    f.write(f"\n\n{'':>4}implicit none")
    f.write(f"\n\n{'':>4}type(PythonModule), save :: mod_{m.name[5:].lower()}")
    f.write(f"\n{'':>4}type(PythonMethodTable), save :: table_{m.name[5:].lower()}")
    f.write(f"\n\n{'':>4}contains")
    write_py_init(f,m)
    write_init(f,m)

def write_init(f,m : cfml_objects.Module):

    # Count number of procedures
    nproc = 0
    for p in m.procedures:
        if m.procedures[p].is_overload:
            continue
        nproc = nproc + 1
    nproc = nproc + len(m.interface.keys())
    # Write function
    f.write(f"\n\n{'':>4}function Init() result(m)")
    f.write(f"\n\n{'':>8}! Local variables")
    f.write(f"\n{'':>8}type(c_ptr) :: m")
    f.write(f"\n{'':>8}integer :: ierror")
    f.write(f"\n\n{'':>8}ierror = Forpy_Initialize()")
    f.write(f"\n\n{'':>8}! Build method table")
    f.write(f"\n{'':>8}call table_{m.name[5:].lower()}%init({nproc})")
    for p in m.procedures:
        if m.procedures[p].is_overload:
            continue
        f.write(f"\n{'':>8}call table_{m.name[5:].lower()}%add_method('{p}','py_{p}',METH_VARARGS,c_funloc(py_{p}))")
    for i in m.interface:
        f.write(f"\n{'':>8}call table_{m.name[5:].lower()}%add_method('{m.interface[i].name}','py_{m.interface[i].name}',METH_VARARGS,c_funloc(py_{m.interface[i].name}))")
    f.write(f"\n\n{'':>8}m = mod_{m.name[5:].lower()}%init('py_{m.name.lower()}','A Python API for CrysFML08',table_{m.name[5:].lower()})")
    f.write(f"\n\n{'':>4}end function Init")

def write_py_init(f,m : cfml_objects.Module):

    w_name = 'py_'+m.name.lower()
    f.write(f"\n\n{'':>4}function PyInit_{w_name.lower()}() bind(c,name='PyInit_{w_name.lower()}') result(m)")
    f.write(f"\n{'':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_{w_name.lower()}")
    f.write(f"\n\n{'':>8}type(c_ptr) :: m")
    f.write(f"\n\n{'':>8}m = Init()")
    f.write(f"\n\n{'':>4}end function PyInit_{w_name.lower()}")

def write_uses(f,uses : list):

    f.write(f"\n\n{'':>4}use forpy_mod")
    f.write(f"\n{'':>4}use iso_c_binding")
    for u in uses:
        u = u.replace('Use','use')
        f.write(f"\n{'':>4}{','.join([s.strip() for s in u.split(',')])}")

def wrap(modules : dict,lucy : dict) -> None:

    for m in modules:
        wrap_cfml_module_procs(modules[m],lucy)

def wrap_cfml_module_procs(m : cfml_objects.Module,lucy : dict) -> None:

    w_name = 'py_'+m.name.lower()
    w_file = os.path.join('../Python_API',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"module {w_name}")
        # Uses
        write_uses(f,m.uses)
        # Header
        write_header(f,m)
        # Procedures
        for p in m.procedures:
            if m.procedures[p].is_overload:
                continue
            wrap_procedure(f,m,p)
        for i in m.interface:
            wrap_interface(f,m,i)
        f.write(f"\n\nend module {w_name}")

def wrap_interface(f,m : cfml_objects.Module,i : str):

    f.write(f"\n\n{'':>4}function py_{i}(self_ptr,args_ptr) result(resul) bind(c)")
    f.write(f"\n\n{'':>8}!Arguments")
    f.write(f"\n{'':>8}type(c_ptr), value :: self_ptr")
    f.write(f"\n{'':>8}type(c_ptr), value :: args_ptr")
    f.write(f"\n{'':>8}type(c_ptr)        :: resul")
    f.write(f"\n\n{'':>4}end function py_{i}")

def wrap_procedure(f,m : cfml_objects.Module,p : str):

    f.write(f"\n\n{'':>4}function py_{p}(self_ptr,args_ptr) result(resul) bind(c)")
    f.write(f"\n\n{'':>8}!Arguments")
    f.write(f"\n{'':>8}type(c_ptr), value :: self_ptr")
    f.write(f"\n{'':>8}type(c_ptr), value :: args_ptr")
    f.write(f"\n{'':>8}type(c_ptr)        :: resul")
    f.write(f"\n\n{'':>4}end function py_{p}")