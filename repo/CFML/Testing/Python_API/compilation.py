"""
---------
Functions
---------
create_scripts(directory : str) -> None
create_script_windows() -> None
"""

import colorama
import os
import platform

system = platform.system() # Windows, Linux, Darwin
def create_scripts(directory : str) -> None:

    print(f"{colorama.Fore.GREEN}{'Writing compilation scripts'}{colorama.Style.RESET_ALL}")
    os.chdir(directory)
    if not os.path.isdir('scripts'):
        os.makedirs('scripts')
    script_loc = os.path.join('scripts', system)
    print(f'script_loc : {script_loc}')
    if not os.path.isdir(script_loc):
        os.makedirs(script_loc)
    if system == 'Linux':
        print("calling linux")
        create_script_linux()
    elif system == 'Windows':
        create_script_windows()
    # else:
    #     create_script_macos()

def create_script_windows() -> None:
    opt="/I%CRYSFML08_INSTALL%\\include /fpp /libs:dll /threads /c"
    with open('scripts/windows/make_ifort_crysfml08api.bat','w') as f:
        f.write(f"{'@echo off'}\n")
        f.write(f"\nifort %FORPY%\\forpy_mod.f90 /fpp /libs:dll /threads /c\n")
        f.write(f"ifort ..\\..\\src\\fortran\\wraps.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_atoms.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_eos.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_extincorr.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_geom.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_gspacegroups.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_keycodes.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_magnetic_database.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_maths.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_profiles.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_random.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_reflections.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_scattering_tables.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_superspace_database.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_sxtal_geom.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_symmetry_tables.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\API_init.f90 {opt}\n")
        f.write(f"\nlink *.obj /out:\"crysfml_api.dll\" /libpath:%CRYSFML08_INSTALL%\\lib /dll libCrysFML.a %LIBPYTHON% /NODEFAULTLIB:libcmt.lib\n")
        f.write(f"\nmove crysfml_api.dll ..\\..\\src\\python\\crysfml_api.pyd\n")
        f.write(f"del *.obj *.mod *.exp *.lib")

def create_script_linux() -> None:
    opt="-I$CRYSFML08_INSTALL -fpp -libs:dll -threads -c -fPIC"

    with open('scripts/Linux/make_ifort_crysfml08api.sh','w') as f:
        print("opened")
        f.write(f"\nifort $FORPY/forpy_mod.F90 -fpp -libs:dll -threads -c -fPIC\n")
        f.write(f"ifort ../../src/fortran/wraps.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_atoms.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_eos.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_extincorr.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_geom.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_gspacegroups.f90 {opt}\n")
        #f.write(f"ifort ../../src/fortran/py_cfml_keycodes.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_magnetic_database.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_maths.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_profiles.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_random.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_reflections.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_scattering_tables.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_superspace_database.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_sxtal_geom.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/py_cfml_symmetry_tables.f90 {opt}\n")
        f.write(f"ifort ../../src/fortran/api_init.f90 {opt}\n")
        f.write(f"\nifort -o crysfml_api.so -shared -fPIC *.o -L$CRYSFML08_INSTALL -lcrysfml\n")
        f.write(f"rm *.o *.mod")

if __name__ == '__main__':
    CRYSFML08 = os.getenv('CRYSFML08')
    create_scripts(os.path.join(CRYSFML08,'API'))
