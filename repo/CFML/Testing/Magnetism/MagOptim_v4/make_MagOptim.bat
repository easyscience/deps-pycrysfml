@echo off
rem ****
rem ****---- Compilation for MagOptim Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: June-2012
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_MagOptim: Make MagOptim Compilation
   echo    Syntax: make_MagOptim [gfortran/ifort/ifortd]
   goto END
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd    goto IFORTD
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c Prep_Input.f90              /O2 /nologo /I. /I%CRYSFML%\ifort_release\include
   ifort /c Cost_MagFunctions.f90       /O2 /nologo /I. /I%CRYSFML%\ifort_release\include
   ifort /c MagOptim.f90                /O2 /nologo /I. /I%CRYSFML%\ifort_release\include
   link  /subsystem:console /stack:64000000 /out:MagOptim.exe *.obj  %CRYSFML%\ifort_release\Lib\libCrysFML08.a
   goto END
rem
:IFORTD
   ifort /c Prep_Input.f90               /debug:full /check /traceback /nologo /I. /I%CRYSFML%\ifort_debug\include
   ifort /c Cost_MagFunctions.f90        /debug:full /check /traceback /nologo /I. /I%CRYSFML%\ifort_debug\include
   ifort /c MagOptim.f90                 /debug:full /check /traceback /nologo /I. /I%CRYSFML%\ifort_debug\include
   link  /subsystem:console /stack:64000000 /out:MagOptim.exe *.obj  %CRYSFML%\ifort_debug\Lib\libCrysFML08.a
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c Prep_Input.f90           -O3 -funroll-loops  -msse2  -I%CRYSFML%\gfortran_release\include
   gfortran -c Cost_MagFunctions.f90    -O3 -funroll-loops  -msse2  -I%CRYSFML%\gfortran_release\include
   gfortran -c MagOptim.f90             -O3 -funroll-loops  -msse2  -I%CRYSFML%\gfortran_release\include
   gfortran  *.o -o  MagOptim_gf  -L%CRYSFML%\gfortran_release\Lib -lcrysfml08  -Wl,--heap=0x01000000
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
