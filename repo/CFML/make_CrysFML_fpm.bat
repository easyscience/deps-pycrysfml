@echo off
rem .
rem  Attempt to create a unified build method for CrysFML using fmp
rem .
echo ---- Construction of the CrysFML library for 64 bits using gfortran, ifort or ifx (oneAPI) ----
echo ---- The building procedure installs also some executable programs of the Testing subdirectory
echo      Default: ifort compiler in release mode. Equivalent to the first example below
echo      Examples of using the script:
echo             make_CrysFML_fpm  ifort
echo             make_CrysFML_fpm  ifort debug
echo             make_CrysFML_fpm  gfortran
echo             make_CrysFML_fpm  gfortran debug
echo             make_CrysFML_fpm  ifx
echo             make_CrysFML_fpm  ifx debug
echo     For using the Winteracter library, add the word "win" as the last argument without quotes
echo     For using the Program_Examples (only in console mode), add the keyword "prog" without quotes  
echo ----
   (set _DEBUG=N)
   (set _COMP=ifort)
   (set _WINT=N)
   (set _PROG=N)
rem > Arguments ----
:LOOP
    if [%1]==[debug]    (set _DEBUG=Y)
    if [%1]==[ifort]    (set _COMP=ifort)
    if [%1]==[ifx]      (set _COMP=ifx)
    if [%1]==[gfortran] (set _COMP=gfortran)
    if [%1]==[win]      (set _WINT=win)
    if [%1]==[prog]     (set _PROG=Y)
    shift
    if not [%1]==[] goto LOOP
rem .
rem  Select the proper fpm.toml file depending on use for console of winteracter (two options com and win)
rem  The console toml file construct also the executables in Testing/... if the
rem  argument "prog" is provided 
  if [%_WINT%]==[win] (
          echo Copying .\toml\fpm_windows_win.toml to fpm.toml
          copy .\toml\fpm_windows_win.toml  fpm.toml
   ) else (
          if [%_PROG%]==[Y] ( 
            echo Copying .\toml\fpm_windows_con_prog.toml to fpm.toml
            copy .\toml\fpm_windows_con_prog.toml  fpm.toml     
          ) else ( 
            echo Copying .\toml\fpm_windows_con.toml to fpm.toml
            copy .\toml\fpm_windows_con.toml  fpm.toml  
          )            
   )
rem .
rem  First change the extensions of files that are optionally used in fpm to "xxx" by
rem  invoking the tochange.bat script in the Src directory.
rem .
cd .\Src
   if [%_WINT%]==[win] (
          call tochange xxx win
   ) else (
          call tochange xxx
          )
   )
cd ..
rem .
rem Select now the compilers and execute the appropriate response file in rsp directory
rem .
    if [%_COMP%]==[ifort] (
      cd .\Src
      ren  CFML_GlobalDeps_Windows_INTEL.xxx  CFML_GlobalDeps.f90
      cd ..
      if [%_WINT%]==[win] (
          if [%_DEBUG%]==[Y] (
             fpm @./rsp/ifort_win_debug_win.rsp
          ) else (
             fpm @./rsp/ifort_win_release_win.rsp
          )
      ) else (
          if [%_DEBUG%]==[Y] (
             fpm @./rsp/ifort_win_debug_con.rsp
          ) else (
             fpm @./rsp/ifort_win_release_con.rsp
          )
      )
      cd .\Src
      ren CFML_GlobalDeps.f90 CFML_GlobalDeps_Windows_INTEL.xxx
      cd ..
    )
rem .
rem With the compiler ifx only console modes are available
rem .
    if [%_COMP%]==[ifx] (
      cd .\Src
      ren  CFML_GlobalDeps_Windows_INTEL.xxx  CFML_GlobalDeps.f90
      cd ..
      if [%_DEBUG%]==[Y] (
         fpm @./rsp/ifx_win_debug_con.rsp
      ) else (
         fpm @./rsp/ifx_win_release_con.rsp
      )
      cd .\Src
      ren CFML_GlobalDeps.f90 CFML_GlobalDeps_Windows_INTEL.xxx
      cd ..
    )
    if [%_COMP%]==[gfortran] (
      cd .\Src
      ren  CFML_GlobalDeps_Windows_GFOR.xxx  CFML_GlobalDeps.f90
      cd ..
      if [%_WINT%]==[win] (
          if [%_DEBUG%]==[Y] (
             fpm @./rsp/gf_debug_win.rsp
          ) else (
             fpm @./rsp/gf_release_win.rsp
          )
      ) else (
          if [%_DEBUG%]==[Y] (
             fpm @./rsp/gf_debug_con.rsp
          ) else (
             fpm @./rsp/gf_release_con.rsp
          )
      )
      cd .\Src
      ren  CFML_GlobalDeps.f90 CFML_GlobalDeps_Windows_GFOR.xxx
      cd ..
    )
rem .
rem  Undo the changes of extensions to be compatilbe with Cmake
rem .
cd .\Src
   if [%_WINT%]==[win] (
          call tochange f90 win
   ) else (
          call tochange f90
          )
   )
cd ..
