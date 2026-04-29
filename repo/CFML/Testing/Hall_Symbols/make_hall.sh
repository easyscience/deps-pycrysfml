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
   INC="-I$CRYSFML/ifort64/LibC"
   LIB="-L$CRYSFML/ifort64/LibC"
   LIBSTATIC="-lcrysfml"
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
echo " #### MHall       Program                      (1.0) ####"
echo " #### JRC                              CopyLeft-2020 ####"
echo " ########################################################"
$COMP $OPT1 MHall.f90 $INC
$COMP -$ARCH *.o -o MHall -static-intel $LIB  $LIBSTATIC
#
# Final process
#
upx MHall
rm -rf *.o *.mod
cp MHall $FULLPROF/.
mv MHall $PROGCFML/DistFPS/$VERS
#

