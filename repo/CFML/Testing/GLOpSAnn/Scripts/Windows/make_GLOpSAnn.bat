@echo off
rem ****
rem ****---- Compilation of GlOpSAnn Program ----****
rem ****
rem **** Author: JRC + JGP
rem **** Revision: Nov-2008
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_GLOpSAnn: Make GLOpSAnn Compilation
   echo    Syntax: make_GLOpSAnn [f95/lf95/g95/gfortran/ifort]
   goto END
rem
:CONT
   set SRCD=..\..\Src
   if x%1 == xgfortran  goto GFOR
   if x%1 == xgfortrand  goto GFORD
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd    goto IFORTD
   goto END
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c %SRCD%\GLS_observ.f90            /O3 /Qparallel /nologo  /I%CRYSFML08%\ifort64\LibC
   ifort /c %SRCD%\GLS_cost_functions.f90    /O3 /Qparallel /nologo  /I%CRYSFML08%\ifort64\LibC
   ifort /c %SRCD%\GLOpSAnn.f90              /O3 /Qparallel /nologo  /I%CRYSFML08%\ifort64\LibC
   ifort /exe:GLOpSAnn *.obj  %CRYSFML08%\ifort64\LibC\CrysFML.lib /link /stack:64000000
rem   ifort  /exe:GLOpSAnn.exe *.obj  C:\CrysFML\ifort64\LibC\CrysFML.lib
   goto END
:IFORTD
   ifort /c %SRCD%\GLS_observ.f90           /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /c %SRCD%\GLS_cost_functions.f90   /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /c %SRCD%\GLOpSAnn.f90             /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /exe:GLOpSAnn.exe *.obj  %CRYSFML08%\ifort64_debug\LibC\CrysFML.lib
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c %SRCD%\GLS_observ.f90          -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran -c %SRCD%\GLS_cost_functions.f90  -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran -c %SRCD%\GLOpSAnn.f90            -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran  *.o -o  GLOpSAnn_gf  -L%CRYSFML08%\gfortran\LibC -lcrysfml  -Wl,--heap=0x01000000
   upx GLOpSAnn_gf.exe
   if exist %FULLPROF%  copy GLOpSAnn_gf.exe %FULLPROF%\nGLOpSAnn_gf.exe
   goto FIN
:GFORD
   gfortran -c %SRCD%\GLS_observ.f90          -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran -c %SRCD%\GLS_cost_functions.f90  -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran -c %SRCD%\GLOpSAnn.f90            -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran  *.o -o  GLOpSAnn_gf  -L%CRYSFML08%\gfortran_debug\LibC -lcrysfml  -Wl,--heap=0x01000000
   upx GLOpSAnn_gf.exe
   if exist %FULLPROF%  copy GLOpSAnn_gf.exe %FULLPROF%\nGLOpSAnn_gf.exe
   goto FIN
rem
:END
   upx GLOpSAnn.exe
 if exist %FULLPROF%  copy GLOpSAnn.exe %FULLPROF%\nGLOpSAnn.exe
rem if exist %PROGCFML%  move GLOpSAnn.exe %PROGCFML%\DistFPS_64b\.
rem if exist %PROGCFML%  copy ..\Docs\GLOpSAnn.pdf %PROGCFML%\DistFPS\Docs\.
:FIN
   del *.obj *.mod *.o *.map *.bak *.exe *.pdb > nul
