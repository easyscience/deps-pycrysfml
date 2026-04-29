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
      "ifx")
         COMP=$arg
         ;;
      "debug"*)
         DEBUG="Y"
         ;;
   esac
done
# "#### MHall Program ####"
chmod +x make_hall.sh
./make_hall.sh $COMP $DEBUG
#
# "#### TOF_fit_LM Program ####"
chmod +x make_tof_LM.sh
./make_tof_LM.sh $COMP $DEBUG
#
# "#### nDataRed Program ####"
chmod +x make_DataRed.sh
./make_DataRed.sh $COMP $DEBUG
#
# "#### Get_Data Program ####"
chmod +x make_get_data.sh
./make_get_data.sh $COMP $DEBUG
