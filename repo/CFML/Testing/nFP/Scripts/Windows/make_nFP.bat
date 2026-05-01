@echo off
echo    MAKE_nFP: Make nFP Compilation
echo    Syntax: make_nFP [gfortran/ifort/ifx ] [debug]
echo    By default: ifort and no debug

(set _DEBUG=N)
(set Compiler=ifort)
rem
rem Compiling MagStREXS using gfortran or ifort, ifx (64 bits).
rem The environment variable CRYSFML08 (or CRYSFML) is defined in the system.
rem The environment variable RXMS must be changed to the local repository path.
rem
:LOOP
    if [%1]==[debug]    (set _DEBUG=Y)
    if [%1]==[ifort]    (set Compiler=ifort)
    if [%1]==[ifx]      (set Compiler=ifx)
    if [%1]==[gfortran] (set Compiler=gfortran)
    shift
    if not [%1]==[] goto LOOP
    
set nFPdir=%CRYSFML08%\Testing\nFP
set SRC=%nFPdir%\src
set DistnFP=%nFPdir%\DistnFP\Windows\%Compiler%
set exec=nFP.exe
rem
rem Intel ifort Compilation of nFP for 64 bits
rem
  if [%Compiler%]==[ifort] (
       if [%_DEBUG%]==[Y] (
        
          set INC=/I%CRYSFML08%\ifort64_debug\include
          set LIBC=%CRYSFML08%\ifort64_debug\lib\CrysFML08.lib
          set OPT=/c /fpp /free /debug:full /check /check:noarg_temp_created /warn /traceback /nologo /Qdiag-disable:10448 /heap-arrays %INC%
          set LINK_ARG=/exe:%exec% *.obj %LIBC% /Qdiag-disable:10448 /link /nologo /stack:64000000

       ) else (
     
          set INC=/I%CRYSFML08%\ifort64\include   
          set LIBC=%CRYSFML08%\ifort64\lib\CrysFML08.lib
          set OPT=/c /fpp /free /O2 /Qm64 /nologo /warn:all /Qopt-report:0 /Qdiag-disable:10448 %INC%
          set LINK_ARG=/exe:%exec% *.obj %LIBC% /Qdiag-disable:10448 /link /nologo /stack:64000000
       ) 
  )   
rem
rem Intel ifx Compilation of MagStREXS for 64 bits
rem
  if [%Compiler%]==[ifx] (
  
       if [%_DEBUG%]==[Y] (
        
          set INC=/I%CRYSFML08%\ifx_debug\include
          set LIBC=%CRYSFML08%\ifx_debug\lib\CrysFML08.lib
          set OPT=/c /fpp /free /Od /debug:full /check /check:noarg_temp_created /warn /traceback /nologo /heap-arrays %INC%
          set LINK_ARG=/exe:%exec% *.obj "%LIBC%" /link /nologo /stack:64000000
                   
       ) else (
     
          set INC=/I%CRYSFML08%\ifx_release\include   
          set LIBC=%CRYSFML08%\ifx_release\lib\CrysFML08.lib
          set OPT=/c /fpp /free /O2 /Qm64 /nologo /warn:all /Qopt-report:0 %INC%
          set LINK_ARG=/exe:%exec% *.obj "%LIBC%" /link /nologo /stack:64000000
       )
     )

rem
rem Gfortran Compilation of TOF_Fit_LM console for 64 bits
rem
  if [%Compiler%]==[gfortran] (
     
       if [%_DEBUG%]==[Y] (
        
          set INC=-I%CRYSFML08%\gfortran_debug\include  
          set LIBC=%CRYSFML08%\gfortran_debug\libC\CrysFML08.a
          set OPT=-c %INC% -g -O0 -Wall -Wno-conversion -Wno-character-truncation -Wno-maybe-uninitialized -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics 
          set LINK_ARG=-out %exec% *.o  %LIBC%  
                   
       ) else (
     
          set INC=-I%CRYSFML08%\gfortran_release\include  
          set LIBC=%CRYSFML08%\gfortran_release\libC\CrysFML08.a
          set OPT=-c %INC% -O3 -ffree-line-length-0 -fdec-math -fall-intrinsics    
          set LINK_ARG=-out %exec% *.o  %LIBC%  
       )
    )
echo ...
echo Compiler %Compiler%
echo Source %SRC%
echo Distribution %DistnFP%
echo Link Arguments %LINK_ARG%  
echo Executable %exec%
echo ... 
   echo  .... Compiling nFP_globals.f90
   %Compiler% %SRC%\nFP_globals.f90 %OPT%
   echo  .... Compiling nFP_read_files.f90
   %Compiler% %SRC%\nFP_read_files.f90 %OPT%
   echo  .... Compiling nFP_Reflections.f90
   %Compiler% %SRC%\nFP_Reflections.f90 %OPT%
   echo  .... Compiling nFP_Simulation.f90
   %Compiler% %SRC%\nFP_Simulation.f90 %OPT%
   echo  .... Compiling nFP_Optimization.f90
   %Compiler% %SRC%\nFP_Optimization.f90 %OPT%
   echo  .... Compiling nFP_main.f90
   %Compiler% %SRC%\nFP_main.f90 %OPT%
   echo  .... Linking everything
   %Compiler% %LINK_ARG%

   echo  .... Copying nFP_main.exe to %DistnFP% directory
   copy %exec% %DistnFP%\.
   del *.obj *.mod *.smod *.o *.map *.bak *.pdb > nul