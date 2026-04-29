@echo off
setlocal EnableExtensions EnableDelayedExpansion
   (set _DEBUG=N)
   (set _COMP=ifort)
   (set SRC_NXS=%CRYSFML08%\HDF5)
   (set SRC=%CRYSFML08%\Testing\Get_Data)
   (set EX=get_data.exe)
   (set DistFPS=%PROGCFML%\DistFPS_64b)

:LOOP
    if [%1]==[debug]    (set _DEBUG=Y)
    if [%1]==[ifort]    (set _COMP=ifort)
    if [%1]==[ifx]      (set _COMP=ifx)
    shift
    if not [%1]==[] goto LOOP

    
  echo ....
  echo Program    : %EX%
  echo Compiler   : %_COMP%
  echo Debug      : %_DEBUG%
  echo ....
    if [%_COMP%]==[ifort] (
        (set EXE=get_data_if.exe)
        if [%_DEBUG%]==[Y] (
           (set INCLUD=/I%CRYSFML08%\ifort64_debug\include /I%HDF5_INSTALL%\include\static)
           (set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn /Qdiag-disable:10448 /heap-arrays )
           (set OPT2=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /heap-arrays /Warn /Qdiag-disable:10448 /heap-arrays )
           (set liblink=/nologo /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64_debug\lib ^
             CrysFML08.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
             /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)
        ) else (
           (set INCLUD=/I%CRYSFML08%\ifort64\include /I%HDF5_INSTALL%\include\static)
           (set OPT=/c /O3 /nologo /nologo  /heap-arrays /Warn /Qdiag-disable:10448)
           (set OPT2=/c /O3 /nologo /nologo /heap-arrays /Warn /Qdiag-disable:10448)
           (set liblink=/nologo /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64\lib ^
             CrysFML08.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
             /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)
        )

    )

    if [%_COMP%]==[ifx] (
        (set EXE=get_data_ifx.exe)
        if [%_DEBUG%]==[Y] (
           (set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn /heap-arrays )
           (set OPT2=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /heap-arrays /Warn /Qdiag-disable:10448 /heap-arrays )
           (set INCLUD=/I%CRYSFML08%\ifx_debug\include /I%HDF5_INSTALL%\include\static)
           (set liblink=/nologo /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifx_debug\lib ^
             CrysFML08.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
             /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)
         ) else (
           (set OPT=/c /O3 /nologo /nologo  /heap-arrays /Warn /Qdiag-disable:10448)
           (set OPT2=/c /O3 /nologo /nologo /heap-arrays /Warn /Qdiag-disable:10448)
           (set INCLUD=/I%CRYSFML08%\ifx_release\include /I%HDF5_INSTALL%\include\static)
           (set liblink=/nologo /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifx_release\lib ^
             CrysFML08.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
             /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)
        )
    )

rem Intel Compilation
rem Compiling the source files
    echo .... Compiling {%_COMP%} Nexus_Mod.f90
    %_COMP% %SRC_NXS%\Nexus_Mod.f90       %OPT% %INCLUD%
    echo .... Compiling {%_COMP%} D2B_read_mod.f90
    %_COMP% %SRC%\D2B_read_mod.f90        %OPT% %INCLUD%
    echo .... Compiling {%_COMP%} D2B_data_mod.f90
    %_COMP% %SRC%\D2B_data_mod.f90        %OPT% %INCLUD%
    echo .... Compiling {%_COMP%} D2B_int_mod.f90
    %_COMP% %SRC%\D2B_int_mod.f90         %OPT% %INCLUD%
    echo .... Compiling {%_COMP%} GetData_Globals.f90
    %_COMP% %SRC%\GetData_Globals.f90     %OPT% %INCLUD%
    echo .... Compiling {%_COMP%} Get_data.f90
    %_COMP% %SRC%\Get_data.f90            %OPT% %INCLUD%
    link /out:%EXE% *.obj %liblink%
rem
rem Compress executable
rem
   upx %EXE%
rem
rem
rem Update FullProf Distribution
rem
   if exist %FULLPROF% copy %EXE% %FULLPROF% 
   if exist %PROGCFML% copy %EXE% %DistFPS%\%_COMP%\get_data.exe
rem
rem Clean several files
rem
   del *.obj *.o *.mod *.map *.bak *.pdb *.exe > nul
:ENDT
