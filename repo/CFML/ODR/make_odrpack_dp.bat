@echo off
   ifort dp_precision.f    /c 
   ifort lpkbls.f          /c /O2 /nologo 
   ifort odr.f             /c /O2 /nologo 
rem  ifort ODR_wrapper.f90   /c /O2 /nologo /I%CRYSFML%\ifort64\LibC (this is single precision it does not work!)
rem
rem Library
rem
   lib /out:odr_dp.lib *.obj
rem
   move *.mod %CRYSFML%\ifort64\ODR_dp\.
   move *.lib %CRYSFML%\ifort64\ODR_dp\.
del *.obj *.mod *.o *.map *.bak > nul