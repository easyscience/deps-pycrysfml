@echo off
setlocal EnableExtensions EnableDelayedExpansion
rem
   (set _DEBUG=N)
   (set _COMP=ifort)
rem > Arguments ----
:LOOP
    if [%1]==[debug]    (set _DEBUG=Y)
    if [%1]==[ifort]    (set _COMP=ifort)
    if [%1]==[ifx]      (set _COMP=ifx)
    if [%1]==[gfortran] (set _COMP=gfortran)
    shift
    if not [%1]==[] goto LOOP
@echo "                                           "
@echo "-------------------------------------------"
@echo "           Building MHall                  "
@echo "-------------------------------------------"
@echo "                                           "
   call make_MHall %_COMP%
rem
@echo "                                           "
@echo "-------------------------------------------"
@echo "   Building TOF_fit_LM                     "
@echo "-------------------------------------------"
@echo "                                           "
   call make_TOF_LM %_COMP%
rem
rem
@echo "                                           "
@echo "-------------------------------------------"
@echo "        Building nDataRed                  "
@echo "-------------------------------------------"
@echo "                                           "
   call make_DataRed %_COMP%
rem
rem
@echo "                                           "
@echo "-------------------------------------------"
@echo "        Building Get_Data                  "
@echo "-------------------------------------------"
@echo "                                           "
   call make_get_data %_COMP%
