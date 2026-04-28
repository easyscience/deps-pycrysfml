@echo off
rem
rem GFortran Compilation
rem
   set OPT1=-O2 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics
   gfortran -c test_missbehaviour_gfortran.f90   %OPT1%
   gfortran -c test_public_proc.f90              %OPT1%
   gfortran -c test_gfortran.f90                 %OPT1%
   gfortran -o test_gfortran.exe *.o

rem
rem   del *.obj *.mod *.o *.map *.bak > nul