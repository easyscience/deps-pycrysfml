@echo off
rem ****
rem ****---- Compilation for IO_Files Program ----****
rem ****
rem > INIT 
   (set _DEBUG=N)
   (set _COMP=ifort)
rem
rem > Arguments ----
:LOOP
    if [%1]==[debug]  (set _DEBUG=Y)
    if [%1]==[ifort]  (set _COMP=ifort)
    if [%1]==[gfortran64] (
       (set _COMP=gfortran)
       (set _VER=m64)
    )   
    shift
    if not [%1]==[] goto LOOP
rem
rem > Compilers
rem
   if [%_COMP%]==[ifort] (
      if [%_DEBUG%]==[Y] (
         (set DIRECTORY=ifort64_debug)
         (set OPT0=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Qdiag-disable:10448)
         (set OPT1=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Qdiag-disable:10448)
      ) else (
         (set DIRECTORY=ifort64)
         (set OPT0=/Od)
         (set OPT1=/O2)
      )
      (set OPT2=/fpp /Qopt-report:0)
   )
rem   
   if [%_COMP%]==[gfortran] (
      if [%_DEBUG%]==[Y] (
         (set DIRECTORY=gfortran_debug)
         (set OPT0=-g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
         (set OPT1=-g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
      ) else (
         (set DIRECTORY=gfortran)
         (set OPT0=-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
         (set OPT1=-O3 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
      )
      (set OPT2=)
   )
rem
rem > Compilation
   if [%_COMP%]==[ifort] (
      ifort /c io_files.f90  /nologo %OPT1% /I%CRYSFML08%\%DIRECTORY%\include
      ifort /exe:io_files_if *.obj /nologo /Qdiag-disable:10448 %CRYSFML08%\%DIRECTORY%\lib\crysfml08.lib /link /nologo /stack:3000000000
      copy io_files_if.exe %FULLPROF%\.      
   )
rem   
   if [%_COMP%]==[gfortran] (
      gfortran -c io_files.f90           %OPT1% -I%CRYSFML08%\%DIRECTORY%\LibC
      gfortran -o io_files_gf.exe *.o -L%CRYSFML08%\%DIRECTORY%\LibC -lcrysfml08
      copy io_files_gf.exe %FULLPROF%\.      
   )
rem   
   del *.obj *.mod *.o *.map *.bak > nul
   
