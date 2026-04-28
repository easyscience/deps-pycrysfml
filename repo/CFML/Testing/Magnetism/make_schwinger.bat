@echo off
rem ****
rem ****---- Compilation for Schwinger Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: October-2015
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_MAGREF: Make Schwinger Compilation
   echo    Syntax: make_Schwinger [gfortran/ifort/ifortd]
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
   ifort /c Schwinger.f90 /Ox /nologo /I. /I %CrysFML%\ifort_release\include
   link /subsystem:console /stack:102400000 /out:Schwinger.exe *.obj %CrysFML%\ifort_release\lib\libCrysFML08.a
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORTD
   ifort /c Schwinger.f90 /debug /nologo /I. /I %CrysFML%\ifort_debug\include
   link /subsystem:console /stack:102400000 /out:Schwinger.exe *.obj %CrysFML%\ifort_debug\lib\libCrysFML08.a
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c -O3  -std=f2003  -funroll-loops  -msse2   Schwinger.f90   -I %CrysFML%\gfortran_release\include
   gfortran  *.o -o Schwinger_gf -O3  -funroll-loops  -msse2  -L %CrysFML%\gfortran_release\lib -lcrysfml08  -Wl,--heap=0x01000000
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
