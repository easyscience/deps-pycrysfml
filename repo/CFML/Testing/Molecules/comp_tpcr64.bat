@echo off
rem
rem Intel Compilation
rem
   ifort mol_tpcr.f90 /c /Ox /nologo /I"%CRYSFML%"\ifort64\libc08  /warn
   ifort /exe:mol_tpcr mol_tpcr.obj "%CRYSFML%"\ifort64\libc08\crysfml.lib
rem
rem Compress executable
rem
rem upx mol_tpcr.exe
rem
rem Clean several files
rem
   del *.obj *.mod  *.map *.bak > nul
