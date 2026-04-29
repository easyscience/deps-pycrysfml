@echo off
if not x%1 == x goto CONT
cls
echo    MAKE_GET_DATA64: Make Get_Data Compilation for 64 bits
echo    Syntax: make_get_data [ifort/ifortd/gfortran]
goto ENDT
:CONT
rem
if x%1 == xifort goto IFORT
if x%1 == xifortd goto IFORTD
if x%1 == xgfortran goto GFORTRAN
goto FIN
rem
rem Gfortran Compilation
rem
:GFORTRAN
   gfortran get_data.f90 -c -O3  -funroll-loops -msse2 -I"%CRYSFML08%"\gfortran64_release\libc
   gfortran -o get_data *.o -L"%CRYSFML08%"\gfortran64_release\libc -lcrysfml
   goto FIN
rem
rem Intel Compilation
rem
:IFORT
set SRC_NXS=%CRYSFML08%\HDF5
set INCLUDE=/I %CRYSFML08%\ifort64\LibC /I %HDF5_INSTALL%\include\static
set OPT=/c /O3 /Warn
set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64\LibC crysfml.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
    ifort %SRC_NXS%\Nexus_Mod.f90 %OPT% %INCLUDE%
    ifort get_data.f90  %OPT% %INCLUDE%
    link /out:get_data_nxs.exe *.obj %liblink%
    goto FIN
:IFORTD
set SRC_NXS=%CRYSFML08%\HDF5
rem set INCLUDE=/I %CRYSFML08%\ifort64_debug\LibC /I %HDF5_INSTALL%\include\static
set INCLUDE=/I %CRYSFML08%\ifort_debug\include /I %HDF5_INSTALL%\include\static
set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn
rem set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64_debug\LibC crysfml.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort_debug\Lib libCrysFML08.a libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
    ifort %SRC_NXS%\Nexus_Mod.f90 %OPT% %INCLUDE%
    ifort get_data.f90  %OPT% %INCLUDE%
    link /out:get_data_nxs.exe *.obj %liblink%
:FIN
rem
rem Compress executable
rem
   upx get_data_nxs.exe
rem   copy get_data.exe ..\..\DistFPS_64b
rem
rem Update FullProf Distribution
rem
   if exist %FULLPROF% copy get_data_nxs.exe %FULLPROF%
rem
rem Clean several files
rem
   del *.obj *.o *.mod *.map *.bak *.pdb
:ENDT
