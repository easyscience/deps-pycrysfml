@echo off
rem
rem Intel Compilation
rem
   (set OPT0=/O2 /check /heap-arrays /check:noarg_temp_created /traceback /nologo /CB /Qdiag-disable:10448 )
   ifort /c keycod.f90  %OPT0% /I"%CRYSFML08%"\ifort64\include
   ifort /exe:keycod keycod.obj  /Qdiag-disable:10448  "%CRYSFML08%"\ifort64\lib\crysfml08.lib /link /stack:300000000
rem
   del *.obj *.mod  *.map *.bak > nul
