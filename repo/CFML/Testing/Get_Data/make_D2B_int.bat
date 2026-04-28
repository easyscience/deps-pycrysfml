@echo off
   (set _DEBUG=N)
   (set _COMP=ifort)
   (set SRC_NXS=%CRYSFML08%\HDF5)
:LOOP
    if [%1]==[debug]  (set _DEBUG=Y)
    if [%1]==[ifort]  (set _COMP=ifort)
    if [%1]==[gfortran] (
       (set _COMP=gfortran)
       (set _VER=m64)
    )
    shift
    if not [%1]==[] goto LOOP

    if [%_COMP%]==[ifort] (
        if [%_DEBUG%]==[Y] (
           (set DIRECTORY=ifort_debug)
           (set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn)
           (set OPT2=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /heap-arrays /Warn)
        ) else (
           (set DIRECTORY=ifort_release)
           (set OPT=/c /O3 /nologo /nologo /Warn)
           (set OPT2=/c /O3 /nologo /nologo /heap-arrays /Warn)
        )
      (set INCLUDE=/I %CRYSFML08%\%DIRECTORY%\include /I %HDF5_INSTALL%\include\static)
      (set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\%DIRECTORY%\Lib ^
           libCrysFML08.a libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
           /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)

    )

    ifort %SRC_NXS%\Nexus_Mod.f90 %OPT% %INCLUDE%
    ifort D2B_read_mod.f90        %OPT% %INCLUDE%
    ifort D2B_data_mod.f90        %OPT% %INCLUDE%
    ifort D2B_int_mod.f90         %OPT% %INCLUDE%
    ifort D2B_int.f90             %OPT% %INCLUDE%
    link /out:D2B_int.exe *.obj   %liblink%

del *.obj *.mod
if exist %FULLPROF% move D2B_int.exe %FULLPROF%\.
rem option /subsystem: console is used to prevent LINK : fatal error LNK1561: entry point must be defined
rem options /NODEFAULTLIB is used to prevent linker errors due to multiple references to the same function.