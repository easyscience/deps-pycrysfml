@echo off
rem ****
rem ****---- Compilation for MCMAG Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: Jan-2015
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MCMAG: Make MCMAG Compilation
   echo    Syntax: make_mcmag [gfortran/ifort]
   goto END
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd     goto IFORTD
   echo    Unknown compiler!
   goto END
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c mc_mag.f90   /O2 /nologo
   link /subsystem:console  /out:mcmag.exe *.obj
   del *.obj *.mod
   upx mcmag.exe
   if exist %FULLPROF% move mcmag.exe %FULLPROF% > nul
   goto END
rem
:IFORTD
   ifort /c mc_mag.f90 /debug:full /check /traceback  /nologo
   link /subsystem:console /out:mcmagd.exe *.obj
   del *.obj *.mod
   upx mcmagd.exe
   if exist %FULLPROF% move mcmagd.exe %FULLPROF% > nul
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c mc_mag.f90
   gfortran *.o -o mcmag_gf
   upx mcmag_gf.exe
   del *.o *.mod
   if exist %FULLPROF% move mcmag_gf.exe %FULLPROF% > nul
   goto END
rem
:END
