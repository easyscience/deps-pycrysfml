#!/bin/bash
# ------------------------------
# Script to compile the program: Get_Data
# Authors: NAK, JGP, JRC
# Date: March 2023
# ------------------------------
# General Options
COMP="ifort"
DEBUG="N"
ARCH="m64"
mode=_Release
# The HDF5_INSTALL variables should be locally adapted
# below it is adapted to Linux computer at ILL and Grenoble at home
HDF5_INSTALL=/usr/local/HDF_Group/HDF5/1.14.0
#  below it is adapted to Linux computer at Barcelona
#HDF5_INSTALL=/usr/local/hdf5-ifx-static-cmake
ZLIB_DIR=/usr/lib/x86_64-linux-gnu/
SRC_NXS=$CRYSFML08/HDF5
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
   INC="-I$CRYSFML08/ifort64/include -I$HDF5_INSTALL/mod/static"
   LIB="-L$CRYSFML08/ifort64/lib"
   LIBSTATIC="-lCrysFML08"
   VERS="Linux"
   ln="-diag-disable=10448"
   if [ $DEBUG == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH -diag-disable=10448"
   else
      OPT1="-c -heap-arrays -$ARCH -O2 -qopt-report=0 -diag-disable=10448"
   fi
fi
if [ $COMP == "ifx" ]; then
   INC="-I$CRYSFML08/ifx_release/include -I$HDF5_INSTALL/mod/static"
   LIB="-L$CRYSFML08/ifx_release/lib"
   LIBSTATIC="-lCrysFML08"
   VERS="Linux"
   ln=""
   if [ $DEBUG == "Y" ]; then
      OPT1="-c -warn -g -heap-arrays -$ARCH"
   else
      OPT1="-c -heap-arrays -$ARCH -O3 -qopt-report=0"
   fi
fi
#
#
# Compilation Process
#
cd ../../Get_Data
echo " ########################################################"
echo " ####  Program  Get_Data                       (2.0) ####"
echo " ####  JRC                             CopyLeft-2024 ####"
echo " ########################################################"
$COMP $OPT1 $SRC_NXS/Nexus_Mod.f90    $INC
$COMP $OPT1 D2B_read_mod.f90          $INC
$COMP $OPT1 D2B_read_mod.f90          $INC
$COMP $OPT1 D2B_data_mod.f90          $INC
$COMP $OPT1 D2B_int_mod.f90           $INC
$COMP $OPT1 GetData_Globals.f90       $INC
$COMP $OPT1 Get_data.f90              $INC
$COMP -$ARCH *.o -o get_data -static-intel $ln $LIB  $LIBSTATIC  -L $HDF5_INSTALL/lib -L  $ZLIB_DIR -l hdf5_fortran -l hdf5_f90cstub -l hdf5  -l z

#
# Final process
#
upx get_data
rm -rf *.o *.mod
cp get_data $FULLPROF/.
mv get_data $PROGCFML/DistFPS/$VERS/$COMP$mode/.
cd ../Programs_FP/Linux
#
