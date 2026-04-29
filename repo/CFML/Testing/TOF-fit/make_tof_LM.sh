#!/bin/bash
# ------------------------------
# Script to compile the program: TOF_fit_LM
# Authors: JGP, JRC
# Date: January 2021
# ------------------------------
# General Options
COMP="ifort"
DEBUG="N"
ARCH="m64"
#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "m32")
         ARCH=$arg
         ;;
      "m64")
         ARCH=$arg
         ;;
      "debug"*)
         DEBUG="Y"
         ;;
   esac
done
#
# Settings
#
if [ $ARCH == "m32" ]; then
   INC="-I$CRYSFML/ifort/LibC"
   LIB="-L$CRYSFML/ifort/LibC"
   LIBSTATIC="-lcrysfml"
   VERS="Linux"
else
   INC="-I$CRYSFML/ifort64/LibC -I$CRYSFML/ifort64/ODR "
   LIB="-L$CRYSFML/ifort64/LibC -L$CRYSFML/ifort64/ODR"
   LIBSTATIC="-lcrysfml -lodr_sp"
   VERS="Linux64"
fi
if [ $DEBUG == "Y" ]; then
   OPT1="-c -g -$ARCH"
else
   OPT1="-c -warn -$ARCH -O2 -qopt-report=0"
fi
#
#
# Compilation Process
#
echo " ########################################################"
echo " #### TOF_fit_LM   Program                     (1.0) ####"
echo " #### JRC                              CopyLeft-2021 ####"
echo " ########################################################"
$COMP $OPT1 TOF_module_LM.f90 $INC
$COMP $OPT1 TOF_fitting_LM.f90 $INC
$COMP -$ARCH *.o -o TOF_fit_LM -static-intel $LIB  $LIBSTATIC
#
# Final process
#
upx TOF_fit_LM
rm -rf *.o *.mod
cp TOF_fit_LM $FULLPROF/.
mv TOF_fit_LM $PROGCFML/DistFPS/$VERS/.
#
