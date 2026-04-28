@echo off
rem ---------------------------------------
rem ---- Compilation for TOF_Fit_LM Program ----
rem ---------------------------------------
rem
rem Intel Classic Compilation of TOF_Fit_LM console for 64 bits
rem
rem
(set _DEBUG=N)
   (set _COMP=ifort)
rem > Arguments ----
:LOOP
    if [%1]==[debug]    (set _DEBUG=Y)
    if [%1]==[ifort]    (set _COMP=ifort)
    if [%1]==[ifx]      (set _COMP=ifx)
       if [%1]==[gfortran] (set _COMP=gfortran)
    shift
    if not [%1]==[] goto LOOP
   
   (set TOF=..\..\TOF-fit)
   (set EX=TOF_Fit_LM.exe)
   (set DistFPS=%PROGCFML%\DistFPS_64b)
  echo ....
  echo Program    : %EX%
  echo Compiler   : %_COMP%
  echo Debug      : %_DEBUG%
  echo ....
  if [%_COMP%]==[ifort] (
       set EXE=TOF_Fit_LM_if.exe
rem Console ifort       
       if [%_DEBUG%]==[Y] (
        
          set INCLUD=/I%CRYSFML08%\ifort64_debug\include /I%CRYSFML08%\ifort64\ODR_sp  
          set LIBCRYS=/nologo %CRYSFML08%\ifort64_debug\Lib\CrysFML08.lib %CRYSFML08%\ifort64\ODR_sp\odr_sp.lib /Qdiag-disable:10448
          set OPT=/c /debug:full /check /check:noarg_temp_created /warn /traceback /nologo /assume:byterecl /Qdiag-disable:10448 /heap-arrays
                   
       ) else (
     
          set INCLUD=/I%CRYSFML08%\ifort64\include   /I%CRYSFML08%\ifort64\ODR_sp 
          set LIBCRYS=/nologo %CRYSFML08%\ifort64\Lib\CrysFML08.lib %CRYSFML08%\ifort64\ODR_sp\odr_sp.lib  /Qdiag-disable:10448
          set OPT=/c /O2 /nologo /warn /assume:byterecl /Qdiag-disable:10448 /heap-arrays
       )
 
  )   
rem
rem Intel ifx Compilation of TOF_Fit_LM console for 64 bits
rem
  if [%_COMP%]==[ifx] (
       set EXE=TOF_Fit_LM_ifx.exe
       if [%_DEBUG%]==[Y] (
        
          set INCLUD=/I%CRYSFML08%\ifx_debug\include  /I%CRYSFML08%\ifx_debug\ODR_sp  
          set LIBCRYS=/nologo %CRYSFML08%\ifx_debug\Lib\CrysFML08.lib %CRYSFML08%\ifx_debug\ODR_sp\odr_sp.lib  
          set OPT=/c /debug:full /warn /check /check:noarg_temp_created /traceback /nologo /assume:byterecl  /heap-arrays
                   
       ) else (
     
          set INCLUD=/I%CRYSFML08%\ifx_release\include  /I%CRYSFML08%\ifx_release\ODR_sp  
          set LIBCRYS=/nologo %CRYSFML08%\ifx_release\Lib\CrysFML08.lib %CRYSFML08%\ifx_release\ODR_sp\odr_sp.lib  
          set OPT=/c /O3 /nologo /warn /assume:byterecl  /heap-arrays
       )
     )
rem
rem Gfortran Compilation of TOF_Fit_LM console for 64 bits
rem
  if [%_COMP%]==[gfortran] (
       set EXE=TOF_Fit_LM_gf.exe
       if [%_DEBUG%]==[Y] (
        
          set INCLUD=-I%CRYSFML08%\gfortran_debug\include -I%CRYSFML08%\gfortran_debug\ODR_sp
          set LIBCRYS=%CRYSFML08%\gfortran_debug\lib\libCrysFML08.a    %CRYSFML08%\gfortran_debug\ODR_sp\libodr_sp.a
          set OPT=-c -g -O0 -Wall -Wno-conversion -Wno-character-truncation -Wno-maybe-uninitialized -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics 
                   
       ) else (
     
          set INCLUD=-I%CRYSFML08%\gfortran_release\include -I%CRYSFML08%\gfortran_release\ODR_sp
          set LIBCRYS=%CRYSFML08%\gfortran_release\Lib\libCrysFML08.a %CRYSFML08%\gfortran_release\ODR_sp\libodr_sp.a
          set OPT=-c -O3 -ffree-line-length-0 -fdec-math -fall-intrinsics    
       )
    )

rem
rem Intel Compilation
rem
   echo ... Compiling TOF_Fit_LM.f90
   %_comp% %TOF%\TOF_module_LM.f90   %OPT% %INCLUD%
   %_comp% %TOF%\TOF_fitting_LM.f90  %OPT% %INCLUD%
   
rem
rem Linking
rem
   echo ... Linking to %EXE%
   if [%_comp%] == [gfortran] (
    %_comp% -o %EXE% *.o %LIBCRYS%  
   ) else (
    %_comp% /exe:%EXE% *.obj %LIBCRYS%  
   ) 
   upx %EXE%
   if exist %FULLPROF% copy %EXE% %FULLPROF% > nul
   if exist %PROGCFML% copy %EXE% %DistFPS%\%_comp%\%EX% > nul
rem
rem Clean several files
rem
   del *.obj *.o *.mod *.exe *.map *.bak *_genmod.f90 > nul rem
rem > Compilers
rem
