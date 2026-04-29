#!/bin/bash
# ----------------------------------------
# Script to compile the program: MHall
# Authors: JGP, JRC
# Date: January 2021
# ----------------------------------------
# General Options
compiler="ifort"
debug="N"
ARCH="m64"
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
   libstatic="-lCrysFML08"
   VERS="MacOS"
   ln="-diag-disable=10448"
   if [ $debug == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH -diag-disable=10448"
   else
      OPT1="-c -heap-arrays -$ARCH -O2 -qopt-report=0 -diag-disable=10448"
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
cd ../../Hall_Symbols
echo " ########################################################"
echo " #### Program MHall                            (1.0) ####"
echo " #### JRC                              CopyLeft-2024 ####"
echo " ########################################################"
$compiler $opt1 MHall.f90 $inc
$compiler -$ARCH *.o -o MHall -static-intel $ln $lib  $libstatic
#
# Final process
#
rm -rf *.o *.mod *_genmod.f90
cp MHall $FULLPROF/.
if [ ! -d $PROGCFML/DistFPS/$VERS ]; then
   mkdir $PROGCFML/DistFPS/$VERS
fi
mv MHall $PROGCFML/DistFPS/$VERS/.
cd ../Programs_FP/MacOS

# END


