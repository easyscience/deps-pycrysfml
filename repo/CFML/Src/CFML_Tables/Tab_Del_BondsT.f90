!!----
!!----
!!----
SubModule (CFML_Bonds_Tables) TAB_Del_Bonds
   Implicit none
  Contains

   !!----
   !!---- REMOVE_BONDS_TABLE
   !!----    Deallocating Bond_Length_Table
   !!----
   !!---- 15/04/2019
   !!
   Module Subroutine Remove_Bonds_Table()

      if (allocated(Bond_Length_Table)) deallocate(Bond_Length_Table)
      Set_BT_Variable =.false.

   End Subroutine Remove_Bonds_Table

End SubModule TAB_Del_Bonds