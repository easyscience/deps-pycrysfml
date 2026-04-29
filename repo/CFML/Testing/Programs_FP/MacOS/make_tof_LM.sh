#!/bin/bash
# ------------------------------
# Script to compile the program: TOF_fit_LM
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
   inc="-I$CRYSFML08/ifort64/include -I$CRYSFML08/ifort64/ODR_sp"
   lib="-L$CRYSFML08/ifort64/lib -L$CRYSFML08/ifort64/ODR_sp"
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
   inc="-I$CRYSFML08/GFortran64/LibC -I$CRYSFML08/gfortran64/ODR_sp"
   lib="-L$CRYSFML08/GFortran64/LibC -L$CRYSFML08/gfortran64/ODR_sp"
   mode="-static-libgfortran"
   if [ $debug == "Y" ]; then
      opt1="-c -g -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   else
      opt1="-c -O2 -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   fi
fi
#
#
# Compilation Process
#
cd ../../TOF-fit
echo " ########################################################"
echo " #### TOF_fit_LM   Program                     (1.0) ####"
echo " #### JRC                              CopyLeft-2021 ####"
echo " ########################################################"
$compiler $opt1 TOF_module_LM.f90 $inc
$compiler $opt1 TOF_fitting_LM.f90 $inc
$compiler -arch x86_64 *.o -o TOF_fit_LM $lib $mode -lcrysfml -lodr_sp
#
# Final process
#
rm -rf *.o *.mod *_genmod.f90

if [ ! -d $PROGCFML/DistFPS/MacOS ]; then
   mkdir $PROGCFML/DistFPS/MacOS
fi
mv TOF_fit_LM $PROGCFML/DistFPS/MacOS

if [ -d $FULLPROF ]; then
    cp TOF_fit_LM $FULLPROF/.
fi

cd ../Programs_FP/MacOS
#
