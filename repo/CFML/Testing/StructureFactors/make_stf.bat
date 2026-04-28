@echo off
rem ****
rem ****---- Compilation for CALC_SFAC Program ----****
rem ****
rem **** Author: JRC + JGP
rem **** Revision: Nov-2008
rem ****
rem
    set INC=/I"%CRYSFML%"\ifort64\LibC 
    set CRYSLIB="%CRYSFML%"\ifort64\LibC\crysfml.lib
    set INCD=/I"%CRYSFML%"\ifort64_debug\LibC
    set CRYSLIBD="%CRYSFML%"\ifort64_debug\libC\crysfml.lib
rem
:CONT
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd     goto IFORTD
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c calc_sfac.f90 /O2 /nologo %INC%
   link /subsystem:console /out:calc_sfac.exe *.obj %CRYSLIB%
   goto END
:IFORTD
   ifort /c calc_sfac.f90 /O2 /nologo %INCD%
   link /subsystem:console /out:calc_sfac.exe *.obj %CRYSLIBD%
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
