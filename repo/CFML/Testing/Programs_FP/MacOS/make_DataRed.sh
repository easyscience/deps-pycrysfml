#!/bin/bash
# ------------------------------
# Script to compile the program: nDataRed
# Authors: JGP, JRC
# Date: January 2021
# ------------------------------
# General Options
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
   inc="-I$CRYSFML08/ifort64/include"
   lib="-L$CRYSFML08/ifort64/lib"
   mode="-static-intel"
   if [ $debug == "Y" ]; then
      opt1="-c -g -warn -arch x86_64 -heap-arrays"
   else
      opt1="-c -O2 -arch x86_64 -qopt-report=0 -heap-arrays"
   fi
fi

#
# GFortran compiler
#
if [ $compiler == "gfortran" ]; then
   inc="-I$CRYSFML08/GFortran64/LibC"
   lib="-L$CRYSFML08/GFortran64/LibC"
   mode="-static-libgfortran"
   if [ $debug == "Y" ]; then
      opt1="-c -g -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   else
      opt1="-c -O2 -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   fi
fi
#
# Compilation Process
#
cd ../../DataRed
echo " ########################################################"
echo " ####  nDataRed Program                        (1.0) ####"
echo " ####  JRC                             CopyLeft-2022 ####"
echo " ########################################################"
$compiler $opt1 twin_mod.f90                  $inc
$compiler $opt1 datared_mod.f90               $inc
$compiler $opt1 datared_rnw_reflections.f90   $inc
$compiler $opt1 datared_treat_reflections.f90 $inc
$compiler $opt1 datared.f90                   $inc
$compiler -arch x86_64 *.o -o nDataRed $lib $mode -lcrysfml
#
# Final process
#
rm -rf *.o *.mod *_genmod.f90

if [ ! -d $PROGCFML/DistFPS/MacOS ]; then
   mkdir $PROGCFML/DistFPS/MacOS
fi
cp nDataRed $PROGCFML/DistFPS/MacOS
#
if [ -d $FULLPROF ]; then
   mv -f nDataRed $FULLPROF
fi

cd ../Programs_FP/MacOS
#
