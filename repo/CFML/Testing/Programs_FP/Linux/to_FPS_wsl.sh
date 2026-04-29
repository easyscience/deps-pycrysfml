#!/bin/sh
# Script to compile the FullProf Suite Programs on Linux
#
echo "to_FPS : Adding programs to the FullProf Suite (ifort/ifx only)"
# It is assumed that the environment variable CRYSFML08 has been
# defined before executing this script
COMP="ifort"
DEBUG="N"
#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "ifort")
         COMP=$arg
         ;;
      "ifx")
         COMP=$arg
         ;;
      "debug"*)
         DEBUG="Y"
         ;;
   esac
done
# "#### MHall Program ####"
chmod +x make_hall_wsl.sh
./make_hall_wsl.sh $COMP $DEBUG
#
# "#### TOF_fit_LM Program ####"
chmod +x make_tof_LM_wsl.sh
./make_tof_LM_wsl.sh $COMP $DEBUG
#
# "#### nDataRed Program ####"
chmod +x make_DataRed_wsl.sh
./make_DataRed_wsl.sh $COMP $DEBUG
#
# "#### Get_Data Program ####"
chmod +x make_get_data_wsl.sh
./make_get_data_wsl.sh $COMP $DEBUG
