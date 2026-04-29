#!/bin/bash

# ----------------------------------------
# Script to compile  nDataRed
# Authors: JGP, JRC
# Date: March 2022
# Compiler: ifort, gfortran
# ----------------------------------------

# Checking CrySFML Environment Variable
if [ -z "$CRYSFML" ]; then
   echo "****"
   echo "**** Please, set the environment variable CRYSFML in your .bash_profile"
   echo "****"
   exit
fi

# Checking PROGCFML Environment Variable
if [ -z "$PROGCFML" ]; then
   echo "****"
   echo "**** Please, set the environment variable PROGCFML in your .bash_profile"
   echo "****"
   exit
fi

#
# Default Options
#
compiler="ifort"
debug="N"
#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "ifort")
         compiler=$arg
         ;;
      "gfortran")
         compiler=$arg
         ;;
      "debug"*)
         debug="Y"
         ;;
   esac
done

#
# Intel compiler
#
if [ $compiler == "ifort" ]; then
    
   if [ $debug == "Y" ]; then
     inc="-I$CRYSFML/ifort64_debug/LibC"
     libc="-L$CRYSFML/ifort64_debug/LibC"
     libstatic="-lcrysfml"
     mode="-static-intel"
     exe=nDataRed
     
     opt1="-c -g -warn -m64 -traceback -check noarg_temp_created -fp-stack-check"
     opt0="-c -g -warn -m64 -traceback -check noarg_temp_created -fp-stack-check"
     optd="-c -g -warn -m64 -traceback -check noarg_temp_created -fp-stack-check"
   else
     inc="-I$CRYSFML/ifort64/LibC"
     libc="-L$CRYSFML/ifort64/LibC"
     libstatic="-lcrysfml"
     mode="-static-intel"
     exe=nDataRed_deb

     opt1="-c -O3 -m64 -qopt-report=0"
     opt0="-c -O0 -m64 -qopt-report=0"
     optd="-c -O1 -m64 -qopt-report=0"
   fi
fi

#
# GFortran compiler
#
if [ $compiler == "gfortran" ]; then
   if [ $debug == "Y" ]; then
       inc="-I$CRYSFML/GFortran64_debug/LibC"
      libc="-L$CRYSFML/GFortran64_debug/LibC"
      libstatic="-lcrysfml"
      mode="-static-libgfortran"
      exe=nDataRed
      opt1="-c -g -m64 -ffree-line-length-none"
      opt0="-c -g -m64 -ffree-line-length-none"
      optd="-c -g -m64 -ffree-line-length-none"
   else
       inc="-I$CRYSFML/GFortran64/LibC"
      libc="-L$CRYSFML/GFortran64/LibC"
      libstatic="-lcrysfml"
      mode="-static-libgfortran"
      exe=nDataRed
      opt1="-c -O3 -m64 -ffree-line-length-none -funroll-loops -msse2"
      opt0="-c -O0 -m64 -ffree-line-length-none -funroll-loops -msse2"
      optd="-c -O1 -m64 -ffree-line-length-none -funroll-loops -msse2"
   fi
fi

#
# Compilation Process
#
echo " ########################################################"
echo " #### nDataRed (Console) Program                     ####"
echo " #### JRC - JGP                                 2022 ####"
echo " ########################################################"

  $compiler $opt1 twin_mod.f90                   $inc
  $compiler $opt1 datared_mod.f90                $inc
  $compiler $opt1 datared_rnw_reflections.f90    $inc
  $compiler $opt1 datared_treat_reflections.f90  $inc
  $compiler $opt1 datared.f90                    $inc

$compiler -m64 -o $exe  *.o $mode $libc $libstatic 
#
# Final process
#
rm -rf *.o *.mod *_genmod.f90
#
if [ -d $FULLPROF ]; then
   mv $exe $FULLPROF
fi
# END

