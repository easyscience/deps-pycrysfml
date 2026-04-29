@echo off
set SRC_NXS=%CRYSFML08%\HDF5
rem set INCLUDE=/I %CRYSFML08%\ifort_release\include /I %HDF5_INSTALL%\include\static
rem set OPT=/c /O3 /nologo
rem set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort_release\Lib libCrysFML08.a libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
 set INCLUDE=/I %CRYSFML08%\ifort_debug\include /I %HDF5_INSTALL%\include\static
 set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn
 set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort_debug\Lib libCrysFML08.a libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
    ifort %SRC_NXS%\Nexus_Mod.f90 %OPT% %INCLUDE%
    ifort D2B_data_mod_old.f90        %OPT% %INCLUDE%
    ifort D2B_read_mod_old.f90        %OPT% %INCLUDE%
    ifort D2B_int_old.f90             %OPT% %INCLUDE%
    link /out:D2B_int_old.exe *.obj   %liblink%

del *.obj *.mod
if exist %FULLPROF% move D2B_int_old.exe %FULLPROF%\.
rem option /subsystem: console is used to prevent LINK : fatal error LNK1561: entry point must be defined
rem options /NODEFAULTLIB is used to prevent linker errors due to multiple references to the same function.