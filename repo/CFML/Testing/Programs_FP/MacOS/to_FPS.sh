#!/bin/sh
# Script to compile the FullProf Suite Programs on Linux
#
echo "to_FPS : Adding programs to the FullProf Suite (ifort only)"
# It is assumed that the environment variable CRYSFML08 has been
# defined before executing this script
#
# Compiler Name
#
compiler="ifort"
for arg in "$@"
do
   case "$arg" in
      "ifort")
         compiler=$arg
         ;;
      "gfortran")
         compiler=$arg
         ;;
   esac
done

# "#### MHall Program ####"
chmod +x make_hall.sh
./make_hall.sh $compiler
#
# "#### TOF_fit_LM Program ####"
chmod +x make_tof_LM.sh
./make_tof_LM.sh $compiler
#
# "#### nDataRed Program ####"
chmod +x make_DataRed.sh
./make_DataRed.sh $compiler
