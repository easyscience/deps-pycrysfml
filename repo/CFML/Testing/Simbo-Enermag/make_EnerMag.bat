@echo off
rem ****
rem ****---- Compilation for ENERMAG Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: Jan-2011
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_EnerMag: Make ENERMAG Compilation
   echo    Syntax: make_EnerMag [f95/lf95/g95/gfortran/ifort]
   goto FIN
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd     goto IFORTD
   echo    Unknown compiler!
   goto FIN
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c Sup_Exc.f90 /O2 /nologo /I%CRYSFML08%\ifort_release\include
   ifort /c EnerMag.f90   /O2 /nologo /I%CRYSFML08%\ifort_release\include
   link /subsystem:console /stack:64000000 /out:EnerMag.exe *.obj %CRYSFML08%\ifort_release\lib\libcrysfml08.a
   goto END
rem
:IFORTD
   ifort /c Sup_Exc.f90 /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /I%CRYSFML08%\ifort_debug\include
   ifort /c EnerMag.f90   /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /I%CRYSFML08%\ifort_debug\include
   link /subsystem:console /stack:64000000 /out:EnerMag.exe *.obj %CRYSFML08%\ifort_debug\lib\libcrysfml08.a
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c Sup_Exc.f90    -I../../gfortran/include
   gfortran -c EnerMag.f90    -I../../gfortran/include
   gfortran *.o -o EnerMag    -L../../gfortran/lib   -lcrysfml08
   goto END
rem
:END
rem
rem  Comment the following lines if upx or %FULLPROF% are not available
rem  or if you want to conserve the object files
rem  Compression of executable
        upx EnerMag.exe
rem  Move the excutable to a directory in the Path
        if exist %FULLPROF% move EnerMag.exe %FULLPROF% > nul
rem  Remove unnecessary files
        del *.obj *.mod *.o *.map *.bak > nul
:FIN