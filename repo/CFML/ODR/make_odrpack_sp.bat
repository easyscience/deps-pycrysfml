@echo off
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
   
  if [%_COMP%]==[ifort] (
       if [%_DEBUG%]==[Y] (
        
          set INCLUD=/I%CRYSFML08%\ifort64_debug\include 
          (set DIREC=%CRYSFML08%\ifort64_debug\ODR_sp)  
          set OPT=/c /debug:full /check /check:noarg_temp_created /warn /traceback /nologo /assume:byterecl /Qdiag-disable:10448 /heap-arrays
                   
       ) else (
     
          set INCLUD=/I%CRYSFML08%\ifort64\include   /I%CRYSFML08%\ifort64\ODR_sp 
          (set DIREC=%CRYSFML08%\ifort64\ODR_sp)  
          set OPT=/c /O2 /nologo /assume:byterecl /Qdiag-disable:10448 /heap-arrays
       )
 
  )   
rem
rem Intel ifx Compilation of TOF_Fit_LM console for 64 bits
rem
  if [%_COMP%]==[ifx] (
  
       if [%_DEBUG%]==[Y] (
        
          (set DIREC=%CRYSFML08%\ifx_debug\ODR_sp)  
          set INCLUD=/I%CRYSFML08%\ifx_debug\include   
          set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /assume:byterecl  /heap-arrays
                   
       ) else (
     
          (set DIREC=%CRYSFML08%\ifx_release\ODR_sp)  
          set INCLUD=/I%CRYSFML08%\ifx_release\include  
          set OPT=/c /O3 /nologo /assume:byterecl  /heap-arrays
       )
     )
rem
rem Gfortran Compilation of TOF_Fit_LM console for 64 bits
rem
  if [%_COMP%]==[gfortran] (
     
       if [%_DEBUG%]==[Y] (
        
          (set DIREC=%CRYSFML08%\gfortran_debug\ODR_sp)  
          set INCLUD=-I%CRYSFML08%\gfortran_debug\include  
          set OPT=-c -g -O0 -Wall -Wno-conversion -Wno-character-truncation -Wno-maybe-uninitialized -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics 
                   
       ) else (
     
          (set DIREC=%CRYSFML08%\gfortran_release\ODR_sp)  
          set INCLUD=-I%CRYSFML08%\gfortran_release\include 
          set OPT=-c -O3 -ffree-line-length-0 -fdec-math -fall-intrinsics    
       )
    )

rem
rem Intel Compilation
rem
    echo .... Compiling{%_COMP%} sp_precision.f
   %_COMP% sp_precision.f    %OPT%  %INCLUD%
    echo .... Compiling{%_COMP%} lpkbls.f
   %_COMP% lpkbls.f          %OPT%  %INCLUD%
    echo .... Compiling{%_COMP%} odr.f
   %_COMP% odr.f             %OPT%  %INCLUD%
    echo .... Compiling{%_COMP%} ODR_wrapper.f90
   %_COMP% ODR_wrapper.f90   %OPT%  %INCLUD%
rem
rem Library
rem
   lib /nologo /out:odr_sp.lib *.obj
rem
   echo .... Moving *.mod and *.lib to %DIREC%\.
   move *.mod %DIREC%\. > nul
   move *.lib %DIREC%\. > nul
   echo .... cleaning the directory *.mod and *.lib
  del *.obj *.mod *.o *.map *.bak *_genmod.f90 > nul