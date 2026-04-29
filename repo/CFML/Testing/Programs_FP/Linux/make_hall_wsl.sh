#!/bin/bash
# ----------------------------------------
# Script to compile the program: MHall
# Authors: JGP, JRC
# Date: January 2021
# ----------------------------------------
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
   INC="-I$CRYSFML08_INSTALL/ifort64/include -I$HDF5_INSTALL/mod/static"
   LIB="-L$CRYSFML08_INSTALL/ifort64/lib"
   LIBSTATIC="-lCrysFML08"
   VERS="Linux64"
   ln="-diag-disable=10448"
   if [ $DEBUG == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH -diag-disable=10448"
   else
      OPT1="-c -heap-arrays -$ARCH -O2 -qopt-report=0 -diag-disable=10448"
   fi
fi
if [ $COMP == "ifx" ]; then
   INC="-I$CRYSFML08_INSTALL/ifx_release/include -I$HDF5_INSTALL/mod/static"
   LIB="-L$CRYSFML08_INSTALL/ifx_release/lib"
   LIBSTATIC="-lCrysFML08"
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
cd ../../Hall_Symbols
echo " ########################################################"
echo " #### Program MHall                            (1.0) ####"
echo " #### JRC                              CopyLeft-2024 ####"
echo " ########################################################"
$COMP $OPT1 MHall.f90 $INC
$COMP -$ARCH *.o -o MHall -static-intel $ln $LIB  $LIBSTATIC
#
# Final process
#
upx MHall
rm -rf *.o *.mod
mv MHall $PROGCFML/DistFPS/$VERS/$COMP/.
cd ../Programs_FP/Linux
#

