import os

def cmake(build_dir : str,wrap_files : list,modules : dict):

    with open(os.path.join(build_dir,f"Fortran/CMakeLists.txt"),'w') as f:
        f.write("file(GLOB SUBMOD_WRAPS_SRC CFML_Wraps/*.f90)")
        f.write("\n")
        f.write("\nset(PYCFML_SRC")
        f.write("\n    crysfml08lib.f90")
        f.write("\n    CFML_Wraps.f90")
        f.write("\n    ${SUBMOD_WRAPS_SRC})")
        f.write("\n")
        f.write("\nadd_library(crysfml08lib SHARED ${PYCFML_SRC})")
        f.write("\n")
        f.write("\nif(WIN32)")
        f.write("\n    set_target_properties(crysfml08lib PROPERTIES")
        f.write('\n        PREFIX ""         # Remove prefix "lib" from library name')
        f.write('\n        SUFFIX ".pyd"     # Extension .pyd instead of .dll)')
        f.write("\n    )")
        f.write("\nelseif(APPLE)")
        f.write("\n    set_target_properties(crysfml08lib PROPERTIES")
        f.write('\n        PREFIX ""         # Remove prefix "lib" from library name')
        f.write('\n        SUFFIX ".so"      # Extension .so instead of .dylib)')
        f.write("\n    )")
        f.write("\nelseif(UNIX)")
        f.write("\n    set_target_properties(crysfml08lib PROPERTIES")
        f.write('\n        PREFIX ""         # Remove prefix "lib" from library name')
        f.write("\n    )")
        f.write("\nendif()")
        f.write("\n")
        f.write("\ntarget_include_directories(crysfml08lib PRIVATE") 
        f.write("\n    ${CMAKE_INSTALL_PREFIX}/include")
        f.write("\n)")
        f.write("\n")
        f.write('\nif (CMAKE_SYSTEM_NAME STREQUAL "Windows" OR CMAKE_SYSTEM_NAME STREQUAL "Darwin")')
        f.write("\n    target_link_libraries(crysfml08lib PRIVATE") 
        f.write("\n        CrysFML08")
        f.write("\n        ${PYTHON_LIBRARY_PATH}")
        f.write("\n    )")
        f.write("\nelse()")
        f.write("\n    target_link_libraries(crysfml08lib PRIVATE") 
        f.write("\n        CrysFML08")
        f.write("\n    )")
        f.write("\nendif()")

def make_intel_windows(compiler : str,build_dir : str,wrap_files : list,modules : dict):

    OPT="/c /fpp /nologo"
    if compiler == 'ifort':
        OPT = OPT + " /Qdiag-disable:10448"
    with open(os.path.join(build_dir,f"scripts/windows/make_{compiler}.bat"),'w') as f:
        f.write(f"@echo off")
        f.write(f"\n\nset INSTALLATION_DIR=..\\..\\")
        f.write(f"\nset CRYSFML08_INSTALL=")
        f.write(f"\nset LIBPYTHON=")
        f.write(f'\nset OPT="{OPT}"')
        f.write(f"\nif not exist %INSTALLATION_DIR% (")
        f.write(f"\n    mkdir %INSTALLATION_DIR%")
        f.write(f"\n)")
        f.write(f"\n\nrem CFML_Wraps")
        f.write(f"\necho Compiling CFML_Wraps.f90")
        f.write(f"\n{compiler} {OPT} ..\\..\\Fortran\\CFML_Wraps.f90 /I%CRYSFML08_INSTALL%\\include")
        for w in wrap_files:
            f.write(f"\nrem {w}")
            f.write(f"\necho Compiling {w}")
            f.write(f"\n{compiler} {OPT} ..\\..\\Fortran\\CFML_Wraps\\{w} /I%CRYSFML08_INSTALL%\\include")
        f.write(f"\n\nrem crysfml08lib")
        f.write(f"\necho Building crysfml08lib.pyd")
        f.write(f"\n{compiler} {OPT} ..\\..\\Fortran\\crysfml08lib.f90 /I%CRYSFML08_INSTALL%\\include")
        f.write(f"\nlink *.obj /out:crysfml08lib.dll /libpath:%CRYSFML08_INSTALL%\\lib /dll %LIBPYTHON% CrysFML08.lib")
        f.write(f"\nmove crysfml08lib.dll %INSTALLATION_DIR%\\crysfml08lib.pyd")
        f.write(f"\n\ndel *.obj *.mod *.smod *.exp *.lib")

def make_intel_linux(compiler : str,build_dir : str,wrap_files : list,modules : dict):

    OPT="-fPIC -fpp -c"
    if compiler == 'ifort':
        OPT = OPT + " -diag-disable=10448"
    with open(os.path.join(build_dir,f'scripts/linux/make_{compiler}.sh'),'w') as f:
        f.write(f"INSTALLATION_DIR=../../Python")
        f.write(f'\nCRYSFML08_INSTALL=""')
        f.write(f'\nOPT="{OPT}"')
        f.write(f"\n\nif [ ! -d $INSTALLATION_DIR ]; then")
        f.write(f"\n    mkdir $INSTALLATION_DIR")
        f.write(f"\nfi")
        f.write(f"\n\n# CFML_Wraps")
        f.write(f"\necho Compiling CFML_Wraps.f90")
        f.write(f"\n{compiler} $OPT ../../Fortran/CFML_Wraps.f90 -I$CRYSFML08_INSTALL/include")
        for w in wrap_files:
            f.write(f"\n# {w}")
            f.write(f"\necho Compiling {w}")
            f.write(f"\n{compiler} $OPT ../../Fortran/CFML_Wraps/{w} -I$CRYSFML08_INSTALL/include")
        f.write(f"\n\n# crysfml08lib")
        f.write(f"\necho Building crysfml08lib.so")
        f.write(f"\n{compiler} $OPT ../../Fortran/crysfml08lib.f90 -I$CRYSFML08_INSTALL/include")
        if compiler == 'ifort':
            f.write(f"\n{compiler} -diag-disable=10448 -shared -o crysfml08lib.so *.o -L $CRYSFML08_INSTALL/lib -l CrysFML08")
        else:
            f.write(f"\n{compiler} -shared -o crysfml08lib.so *.o -L $CRYSFML08_INSTALL/lib -l CrysFML08")
        f.write(f"\nmv crysfml08lib.so $INSTALLATION_DIR/")
        f.write(f"\n\nrm *.o *.mod *.smod")

def scripts(build_dir : str,wrap_files : list,modules : dict):

    if not os.path.isdir(os.path.join(build_dir,'scripts')):
        os.mkdir(os.path.join(build_dir,'scripts'))
    if not os.path.isdir(os.path.join(build_dir,'scripts/windows')):
        os.mkdir(os.path.join(build_dir,'scripts/windows'))
    if not os.path.isdir(os.path.join(build_dir,'scripts/linux')):
        os.mkdir(os.path.join(build_dir,'scripts/linux'))
    make_intel_windows('ifort',build_dir,wrap_files,modules)
    make_intel_windows('ifx',build_dir,wrap_files,modules)
    make_intel_linux('ifort',build_dir,wrap_files,modules)
    make_intel_linux('ifx',build_dir,wrap_files,modules)

