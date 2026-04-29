@echo off
rem ****
rem ****---- Compilation for Simple_HKL_GEN Program ----****
rem ****
rem **** Author: JRC + JGP
rem **** Revision: Nov-2008
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_sHKL_GEN: Make simple_hkl_gen Compilation
   echo    Syntax: make_shkl_gen [gfortran/gfortrand/ifort/ifortd]
   goto END
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xgfortrand goto GFORD
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd    goto IFORTD
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c simple_hkl_gen.f90 /O2 /nologo /I%CRYSFML%\ifort_release\include
   link /subsystem:console /out:simple_hkl_gen.exe *.obj %CRYSFML%\ifort_release\lib\libcrysfml08.a
   goto END
:IFORTD
   ifort /c simple_hkl_gen.f90 /debug:full /check /CB /check:noarg_temp_created /traceback /nologo  /I%CRYSFML%\ifort_debug\include
   link /subsystem:console /out:simple_hkl_gen.exe *.obj %CRYSFML%\ifort_debug\lib\libcrysfml08.a
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c -O3 -funroll-loops  -msse2   simple_hkl_gen.f90     -I%CRYSFML%\gfortran_release\include
   gfortran  *.o -o simple_hkl_gen -O3  -funroll-loops  -msse2  -L%CRYSFML%\gfortran_release\lib -lcrysfml08
   goto END
:GFORD
   gfortran -c -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   simple_hkl_gen.f90     -I%CRYSFML%\gfortran_debug\include
   gfortran  *.o -o simple_hkl_gen  -L%CRYSFML%\gfortran_debug\lib -lcrysfml08
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
