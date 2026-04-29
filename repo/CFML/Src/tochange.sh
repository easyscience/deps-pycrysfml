#!/bin/bash
#
if [ -z "$1" ]; then
cat << !
Syntax : tochange.sh xxx  or tochange.sh f90
         or tochange.sh xxx win or tochange.sh f90 win
!
exit
fi
#
# Arguments
#
_WIN="win"
_CHANGEX="xxx"
_CHANGEF="f90"
to_change="xxx"
win="N"
for arg in "$@"
do
   case "$arg" in
      "xxx")
         to_change=$arg
         ;;
      "f90")
         to_change=$arg
         ;;
      "win")
         win=$arg
         ;;
   esac
done
# Space between [ and  comparison are essential!
if [ $to_change == $_CHANGEX ]
then
   echo "---- Changing the extension of some *.f90 files to *.xxx to maintain compatibility with FPM"
   mv  CFML_GlobalDeps_Linux_GFOR.f90         CFML_GlobalDeps_Linux_GFOR.xxx
   mv  CFML_GlobalDeps_Linux_INTEL.f90         CFML_GlobalDeps_Linux_INTEL.xxx
   mv  CFML_GlobalDeps_MacOS_GFOR.f90         CFML_GlobalDeps_MacOS_GFOR.xxx
   mv  CFML_GlobalDeps_MacOs_INTEL.f90         CFML_GlobalDeps_MacOs_INTEL.xxx
   mv  CFML_GlobalDeps_Windows_GFOR.f90       CFML_GlobalDeps_Windows_GFOR.xxx
   mv  CFML_GlobalDeps_Windows_INTEL.f90       CFML_GlobalDeps_Windows_INTEL.xxx

   if [ $win  ==  $_WIN ]
   then
     mv CFML_Messages.f90         CFML_Messages.xxx
     cd CFML_Messages
     mv Con_Err_Message.f90       Con_Err_Message.xxx
     mv Con_Info_Message.f90      Con_Info_Message.xxx
     mv Con_Print_Message.f90     Con_Print_Message.xxx
     mv Con_Wait_Message.f90      Con_Wait_Message.xxx
     mv Con_Write_ScrollMsg.f90   Con_Write_ScrollMsg.xxx
     cd ..
   else
     mv CFML_Messages_Win.f90       CFML_Messages_Win.xxx
     cd CFML_Messages
     mv Win_Err_Message.f90         Win_Err_Message.xxx
     mv Win_Info_Message.f90        Win_Info_Message.xxx
     mv Win_Question_Message.f90    Win_Question_Message.xxx
     mv Win_Stop_Message.f90        Win_Stop_Message.xxx
     mv Win_Warning_Message.f90     Win_Warning_Message.xxx
     mv Win_Write_ScrollMsg.f90     Win_Write_ScrollMsg.xxx
     cd ..
   fi
   exit
fi
#
if [ $to_change  ==  $_CHANGEF ]
then
   echo "---- Changing the extension of *.xxx files to *.f90 to maintain compatibility with CMake"
   mv  CFML_GlobalDeps_Linux_GFOR.xxx         CFML_GlobalDeps_Linux_GFOR.f90
   mv  CFML_GlobalDeps_Linux_INTEL.xxx         CFML_GlobalDeps_Linux_INTEL.f90
   mv  CFML_GlobalDeps_MacOS_GFOR.xxx         CFML_GlobalDeps_MacOS_GFOR.f90
   mv  CFML_GlobalDeps_MacOs_INTEL.xxx         CFML_GlobalDeps_MacOs_INTEL.f90
   mv  CFML_GlobalDeps_Windows_GFOR.xxx       CFML_GlobalDeps_Windows_GFOR.f90
   mv  CFML_GlobalDeps_Windows_INTEL.xxx       CFML_GlobalDeps_Windows_INTEL.f90
   if [ $win ==  $_WIN ]
   then
     mv CFML_Messages.xxx         CFML_Messages.f90
     cd CFML_Messages
     mv CFML_Messages.xxx         CFML_Messages.f90
     mv Con_Err_Message.xxx       Con_Err_Message.f90
     mv Con_Info_Message.xxx      Con_Info_Message.f90
     mv Con_Print_Message.xxx     Con_Print_Message.f90
     mv Con_Wait_Message.xxx      Con_Wait_Message.f90
     mv Con_Write_ScrollMsg.xxx   Con_Write_ScrollMsg.f90
     cd ..
   else
      mv CFML_Messages_Win.xxx       CFML_Messages_Win.f90
      cd CFML_Messages
      mv Win_Err_Message.xxx         Win_Err_Message.f90
      mv Win_Info_Message.xxx        Win_Info_Message.f90
      mv Win_Question_Message.xxx    Win_Question_Message.f90
      mv Win_Stop_Message.xxx        Win_Stop_Message.f90
      mv Win_Warning_Message.xxx     Win_Warning_Message.f90
      mv Win_Write_ScrollMsg.xxx     Win_Write_ScrollMsg.f90
      cd ..
   fi
else
   echo "---- NOTHING DONE! This script should be invoked with argument xxx or f90 and optionally a second argument win!"
fi
