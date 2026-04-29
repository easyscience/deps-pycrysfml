SubModule (CFML_gSpaceGroups) gS_Get_HM_Standard
   implicit none
   Contains

   !!----
   !!---- GET_HM_STANDARD
   !!----
   !!---- Returns the Herman-Maugin symbol for the standard
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_HM_Standard(NumSpg) Result(SymbolHM)
       !---- Arguments ----!
       integer,           intent(in) :: numSpg
       character(len=:), allocatable :: symbolHM

       !---- Local variables ----!
       integer               :: i, n
       integer, dimension(1) :: posSep

       !> Init
       SymbolHM=" "
       if (numSpg < 0 .or. numSpg > 230) return

       !> Load Table
       call Set_Spgr_Info()

       i=1
       do
          if (spgr_info(i)%n == numSpg) then
             call Get_Separator_Pos(spgr_info(i)%HM,':',posSep,n)
             if (n == 0) then
                symbolHM = spgr_info(i)%HM

             else ! Origin choice 2
                symbolHM = spgr_info(i+1)%HM
             end if
             exit
          end if
          i = i + 1
       end do
   End Function Get_HM_Standard

End Submodule gS_Get_HM_Standard