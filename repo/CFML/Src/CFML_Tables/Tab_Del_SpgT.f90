!!----
!!----
!!----
SubModule (CFML_Symmetry_Tables) TAB_Del_SpgT
   Implicit none
   Contains
   !!----
   !!---- Remove_Shubnikov_Info()
   !!----
   !!----    Deallocating SPGR_INFO Data
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_Shubnikov_Info()

      if (allocated(shubnikov_info)) deallocate(shubnikov_info)
      Shubnikov_Info_loaded=.false.

   End Subroutine Remove_Shubnikov_Info

   !!----
   !!---- Remove_Spgr_Info()
   !!----
   !!----    Deallocating SPGR_INFO Data
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_Spgr_Info()

      if (allocated(spgr_info)) deallocate(spgr_info)
      spgr_info_loaded=.false.

   End Subroutine Remove_Spgr_Info

   !!----
   !!---- Remove_System_Equiv()
   !!----
   !!----    Deallocating SPGR_INFO Data
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_System_Equiv()

      if (allocated(System_Equiv)) deallocate(System_Equiv)
      System_Equiv_loaded=.false.

   End Subroutine Remove_System_Equiv

   !!----
   !!---- Remove_Wyckoff_Info()
   !!----
   !!----    Deallocating WYCKOFF_INFO Data
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_Wyckoff_Info()

      if (allocated(wyckoff_info)) deallocate(wyckoff_info)
      wyckoff_info_loaded=.false.

   End Subroutine Remove_Wyckoff_Info

End SubModule TAB_Del_SpgT