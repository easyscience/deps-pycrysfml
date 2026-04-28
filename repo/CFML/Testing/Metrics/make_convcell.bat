@echo off
rem ****
rem ****---- Compilation for GET_UNIT_CELL Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: Jan-2011
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_convcell: Makes GET_Conventional_UNIT_CELL Compilation
   echo    Syntax: make_convcell [f95/lf95/g95/gfortran/ifort]
   goto END
rem
:CONT
   if x%1 == xf95       goto F95
   if x%1 == xlf95      goto LF95
   if x%1 == xg95       goto G95
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   goto END
rem
rem ****---- Absoft Compiler ----****
:F95
   goto END
rem
rem ****---- Lahey Compiler ----****
:LF95
   lf95 -c Get_Conv_Cell.f90  -tp -nomap -stchk -nchk -o1 -mod ".;c:\CrysFML\lahey\LibC"
   lf95  *.obj -out Get_Conv_Cell -tp -nomap -stchk -nchk -o1 -mod ".;c:\CrysFML\lahey\LibC" -lib c:\CrysFML\lahey\LibC\CrysFML
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c Get_Conv_Cell.f90   /O2 /nologo /IC:\CrysFML\ifort\LibC
   rem ifort /exe:Get_Conv_Cell *.obj C:\CrysFML\ifort\LibC\crysfml.lib
   link /subsystem:console /out:Get_Conv_Cell.exe *.obj C:\CrysFML\ifort\LibC\crysfml.lib
   goto END
rem
rem **---- G95 Compiler ----**
:G95
   g95 -c Get_Conv_Cell.f90    -I../../G95/LibC
   g95 *.o -o Get_Conv_Cell    -L../../G95/LibC   -lcrysfml
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c Get_Conv_Cell.f90    -I../../GFortran/LibC
   gfortran *.o -o Get_Conv_Cell    -L../../GFortran/LibC   -lcrysfml
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
