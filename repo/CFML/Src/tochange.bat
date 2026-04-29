@echo off
(set _WINT=no)
if [%1]==[xxx]  (set _CHANGE=to_xxx)
if [%1]==[f90]  (set _CHANGE=to_f90)
if [%2]==[win]  (set _WINT=win)
if [%_CHANGE%]==[to_xxx] (
   echo ---- Changing the extension of some *.f90 files to *.xxx to maintain compatibility with FPM
rem   echo "CFML_GlobalDeps_Linux_GFOR.f90         CFML_GlobalDeps_Linux_GFOR.xxx"
   ren  CFML_GlobalDeps_Linux_GFOR.f90         CFML_GlobalDeps_Linux_GFOR.xxx
rem   echo "ren  CFML_GlobalDeps_Linux_GFOR.f90         CFML_GlobalDeps_Linux_GFOR.xxx"
   ren  CFML_GlobalDeps_Linux_INTEL.f90         CFML_GlobalDeps_Linux_INTEL.xxx
rem   echo "ren  CFML_GlobalDeps_MacOS_GFOR.f90         CFML_GlobalDeps_MacOS_GFOR.xxx"
   ren  CFML_GlobalDeps_MacOS_GFOR.f90         CFML_GlobalDeps_MacOS_GFOR.xxx
rem   echo "ren CFML_GlobalDeps_MacOs_INTEL.f90         CFML_GlobalDeps_MacOs_INTEL.xxx"
   ren  CFML_GlobalDeps_MacOs_INTEL.f90         CFML_GlobalDeps_MacOs_INTEL.xxx
rem   echo "ren  CFML_GlobalDeps_Windows_GFOR.f90       CFML_GlobalDeps_Windows_GFOR.xxx"
   ren  CFML_GlobalDeps_Windows_GFOR.f90       CFML_GlobalDeps_Windows_GFOR.xxx
rem   echo "ren  CFML_GlobalDeps_Windows_INTEL.f90       CFML_GlobalDeps_Windows_INTEL.xxx"
   ren  CFML_GlobalDeps_Windows_INTEL.f90       CFML_GlobalDeps_Windows_INTEL.xxx

   if [%_WINT%]==[win] (
     
rem     echo "ren CFML_Messages.f90         CFML_Messages.xxx"
     ren CFML_Messages.f90         CFML_Messages.xxx
     cd CFML_Messages
rem     echo "ren Con_Err_Message.f90       Con_Err_Message.xxx"
     ren Con_Err_Message.f90       Con_Err_Message.xxx
rem     echo "ren Con_Info_Message.f90      Con_Info_Message.xxx"
     ren Con_Info_Message.f90      Con_Info_Message.xxx
rem     echo "ren Con_Print_Message.f90     Con_Print_Message.xxx"
     ren Con_Print_Message.f90     Con_Print_Message.xxx
rem     echo "ren Con_Wait_Message.f90      Con_Wait_Message.xxx"
     ren Con_Wait_Message.f90      Con_Wait_Message.xxx
rem     echo "ren Con_Write_ScrollMsg.f90   Con_Write_ScrollMsg.xxx" 
     ren Con_Write_ScrollMsg.f90   Con_Write_ScrollMsg.xxx
     cd ..
   
   ) else (
     
rem     echo "ren CFML_Messages_Win.f90       CFML_Messages_Win.xxx"
     ren CFML_Messages_Win.f90       CFML_Messages_Win.xxx
     cd CFML_Messages
rem     echo "ren Win_Err_Message.f90         Win_Err_Message.xxx"
     ren Win_Err_Message.f90         Win_Err_Message.xxx
rem     echo "ren Win_Info_Message.f90        Win_Info_Message.xxx"
     ren Win_Info_Message.f90        Win_Info_Message.xxx
rem     echo "ren Win_Question_Message.f90    Win_Question_Message.xxx"
     ren Win_Question_Message.f90    Win_Question_Message.xxx
rem     echo "ren Win_Stop_Message.f90        Win_Stop_Message.xxx"
     ren Win_Stop_Message.f90        Win_Stop_Message.xxx
rem     echo "ren Win_Warning_Message.f90     Win_Warning_Message.xxx"
     ren Win_Warning_Message.f90     Win_Warning_Message.xxx
rem     echo "ren Win_Write_ScrollMsg.f90     Win_Write_ScrollMsg.xxx"
     ren Win_Write_ScrollMsg.f90     Win_Write_ScrollMsg.xxx
     cd ..
   
   )
)

if [%_CHANGE%]==[to_f90] (
   echo ---- Changing the extension of *.xxx files to *.f90 to maintain compatibility with CMake   
   ren  CFML_GlobalDeps_Linux_GFOR.xxx         CFML_GlobalDeps_Linux_GFOR.f90
   ren  CFML_GlobalDeps_Linux_INTEL.xxx         CFML_GlobalDeps_Linux_INTEL.f90
   ren  CFML_GlobalDeps_MacOS_GFOR.xxx         CFML_GlobalDeps_MacOS_GFOR.f90
   ren  CFML_GlobalDeps_MacOs_INTEL.xxx         CFML_GlobalDeps_MacOs_INTEL.f90
   ren  CFML_GlobalDeps_Windows_GFOR.xxx       CFML_GlobalDeps_Windows_GFOR.f90
   ren  CFML_GlobalDeps_Windows_INTEL.xxx       CFML_GlobalDeps_Windows_INTEL.f90
   
   if [%_WINT%]==[win] (
     
     ren CFML_Messages.xxx         CFML_Messages.f90
     cd CFML_Messages
     ren CFML_Messages.xxx         CFML_Messages.f90
     ren Con_Err_Message.xxx       Con_Err_Message.f90
     ren Con_Info_Message.xxx      Con_Info_Message.f90
     ren Con_Print_Message.xxx     Con_Print_Message.f90
     ren Con_Wait_Message.xxx      Con_Wait_Message.f90
     ren Con_Write_ScrollMsg.xxx   Con_Write_ScrollMsg.f90
     cd ..
   
   ) else (
     
     ren CFML_Messages_Win.xxx     CFML_Messages_Win.f90
     cd CFML_Messages
     ren Win_Err_Message.xxx       Win_Err_Message.f90
     ren Win_Info_Message.xxx      Win_Info_Message.f90
     ren Win_Question_Message.xxx  Win_Question_Message.f90
     ren Win_Stop_Message.xxx      Win_Stop_Message.f90
     ren Win_Warning_Message.xxx   Win_Warning_Message.f90
     ren Win_Write_ScrollMsg.xxx   Win_Write_ScrollMsg.f90
     cd ..
   
   )
)
