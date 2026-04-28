@echo off
rem ****
rem ****---- Compilation for Simple_Calculation_of_Powder_Patterns Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: June-2023
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_SIMILAR_POWDER_PATTERN:  
   echo    Syntax: make_simil [gfortran/ifort]
   goto END
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd     goto IFORTD
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c Convolutions_powder.f90 /O3 /nologo /I. /I..\..\ifort64\LibC
   link /subsystem:console /stack:102400000 /out:convol_pow.exe *.obj ..\..\ifort64\LibC\CrysFML.lib
   goto END
:IFORTD
   ifort /c Convolutions_powder.f90 /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /I. /I..\..\ifort64_debug\LibC
   link /subsystem:console /stack:102400000 /out:convol_pow.exe *.obj ..\..\ifort64_debug\LibC\CrysFML.lib
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c -O3  -std=f2003  -funroll-loops  -msse2 -ffree-line-length-none  Convolutions_powder.f90   -I..\..\GFortran\LibC
   gfortran  *.o -o convol_pow_gf -O3  -funroll-loops  -msse2  -L..\..\GFortran\LibC -lcrysfml  -Wl,--heap=0x01000000
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
