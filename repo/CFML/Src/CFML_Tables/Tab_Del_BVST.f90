!!----
!!----
!!----
SubModule (CFML_BVS_Tables) TAB_Del_BVST
   Implicit none
  Contains

   !!----
   !!---- REMOVE_AP_TABLE
   !!----
   !!----    Deallocating Ap_Table
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_Atomic_Properties_Table()

      if (allocated(Ap_Table)) deallocate(Ap_Table)

   End Subroutine Remove_Atomic_Properties_Table

   !!----
   !!---- REMOVE_BVEL_TABLE
   !!----
   !!----    Deallocating BVEL_Table
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_BVEL_Table()

      if (allocated(BVEL_Table)) deallocate(BVEL_Table)

   End Subroutine Remove_BVEL_Table

   !!----
   !!---- REMOVE_BVS_TABLE
   !!----
   !!----    Deallocating BVS_Table
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_BVS_Table()

      if (allocated(BVS_Table)) deallocate(BVS_Table)

   End Subroutine Remove_BVS_Table

   !!----
   !!---- REMOVE_SBVS_TABLE
   !!----
   !!----    Deallocating sBVS_Table
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Remove_sBVS_Table()

      if (allocated(sBVS_Table)) deallocate(sBVS_Table)

   End Subroutine Remove_sBVS_Table

End SubModule TAB_Del_BVST