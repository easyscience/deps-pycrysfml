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
      "ifx")
         COMP=$arg
         ;;
       "debug"*)
         DEBUG="Y"
         ;;
   esac
done
#
# Settings
#
if [ $COMP == "ifort" ]; then
   INC="-I$CRYSFML08_INSTALL/ifort64/include"
   INCODR="-I$CRYSFML08_INSTALL/ifort64/ODR_sp"
   LIB="-L$CRYSFML08_INSTALL/ifort64/lib"
   LIBSTATIC="-lCrysFML08"
   LIBODR="-L$CRYSFML08_INSTALL/ifort64/ODR_sp"
   LIBSTATIC_ODR="-lodr_sp"
   VERS="Linux64"
   ln="-diag-disable=10448"
   if [ $DEBUG == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH -diag-disable=10448"
   else
      OPT1="-c -heap-arrays -$ARCH -O2 -qopt-report=0 -diag-disable=10448"
   fi
fi
if [ $COMP == "ifx" ]; then
   INC="-I$CRYSFML08_INSTALL/ifx_release/include"
   INCODR="-I$CRYSFML08_INSTALL/ifx_release/ODR_sp"
   LIB="-L$CRYSFML08_INSTALL/ifx_release/lib"
   LIBSTATIC="-lCrysFML08"
   LIBODR="-L$CRYSFML08_INSTALL/ifx_release/ODR_sp"
   LIBSTATIC_ODR="-lodr_sp"
   VERS="Linux64"
   ln=""
   if [ $DEBUG == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH"
   else
      OPT1="-c -heap-arrays -$ARCH -O3 -qopt-report=0"
   fi
fi
#
# Compilation Process
#
cd ../../TOF-fit
echo " ########################################################"
echo " #### Program TOF_fit_LM                       (1.0) ####"
echo " #### JRC                              CopyLeft-2024 ####"
echo " ########################################################"
$COMP $OPT1 TOF_module_LM.f90 $INCODR $INC
$COMP $OPT1 TOF_fitting_LM.f90 $INCODR $INC
$COMP -$ARCH *.o -o TOF_fit_LM -static-intel $ln $LIBODR $LIBSTATIC_ODR $LIB  $LIBSTATIC
#
# Final process
#
upx TOF_fit_LM
rm -rf *.o *.mod
mv TOF_fit_LM $PROGCFML/DistFPS/$VERS/$COMP/.
cd ../Programs_FP/Linux
#
