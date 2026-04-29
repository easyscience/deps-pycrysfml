@echo off
rem
rem GFortran Compilation
rem
   set OPT1=-O3 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics
   gfortran -c keycod.f90       %OPT1% -I%CRYSFML%\gfortran\LibC08
   gfortran -o keycod.exe keycod.o   -L%CRYSFML%\gfortran\LibC08 -lcrysfml 
  
rem
   del *.obj *.mod *.o *.map *.bak > nul