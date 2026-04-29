@echo off
rem ****
rem ****---- Compilation for PHAS_DIAG Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: Jan-2011
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_Phas: Make PHAS_DIAG Compilation
   echo    Syntax: make_phas_diag [gfortran/gfortrand/ifort/ifortd]
   goto FIN
rem
:CONT
   if x%1 == xgfortran   goto GFOR
   if x%1 == xgfortrand  goto GFORD
   if x%1 == xifort      goto IFORT
   if x%1 == xifortd     goto IFORTD
   echo    Unknown compiler!
   goto FIN
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c Sup_Exc.f90 /O2 /nologo /I%CRYSFML08%\ifort_release\include
   ifort /c Phase_Diagram.f90   /O2 /nologo /I%CRYSFML08%\ifort_release\include
   link /subsystem:console /stack:64000000 /out:Phase_Diagram.exe *.obj %CRYSFML08%\ifort_release\lib\libcrysfml08.a
   goto END
:IFORTD
   ifort /c Sup_Exc.f90 /debug:full /check /traceback /nologo /I%CRYSFML08%\ifort_debug\include
   ifort /c Phase_Diagram.f90   /debug:full /check /traceback /nologo /I%CRYSFML08%\ifort_debug\include
   link /subsystem:console /stack:64000000 /out:Phase_Diagram.exe *.obj %CRYSFML08%\ifort_debug\lib\libcrysfml08.a
   goto END
rem
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c Sup_Exc.f90  -I../../GFortran/LibC
   gfortran -c Phase_Diagram.f90    -I../../GFortran/LibC
   gfortran *.o -o Phase_Diagram_gf    -L../../GFortran/LibC   -lcrysfml
   goto END
:GFORD
   gfortran -c Sup_Exc.f90  -I../../gfortran_debug/include
   gfortran -c Phase_Diagram.f90    -I../../gfortran_debug/include
   gfortran *.o -o Phase_Diagram_gf    -L../../gfortran_debug/lib   -lcrysfml08
   goto END
rem
:END
rem
rem  Comment the following lines if upx or %FULLPROF% are not available
rem  or if you want to conserve the object files
rem  Compression of executable
        upx Phase_Diagram.exe
rem  Move the excutable to a directory in the Path
        if exist %FULLPROF% move Phase_Diagram.exe %FULLPROF% > nul
rem        if exist %FULLPROF% move Phase_Diagram_gf.exe %FULLPROF% > nul
rem  Remove unnecessary files
        del *.obj *.mod *.o *.map *.bak > nul
:FIN