@echo off
   ifort sp_precision.f  /c  /nologo 
   ifort lpkbls.f        /c  /debug:full /check /check:noarg_temp_created /traceback /nologo /CB 
   ifort odr.f           /c  /debug:full /check /check:noarg_temp_created /traceback /nologo /CB
rem
rem Library
rem
   lib /out:odr_sp.lib *.obj
rem
   move *.mod %CRYSFML%\ifort64_debug\ODR_sp\.
   move *.lib %CRYSFML%\ifort64_debug\ODR_sp\.
rem
del *.obj *.mod *.o *.map *.bak > nul