!----
!!----
!!----
SubModule (CFML_Bonds_Tables) TAB_Get_BondsT
   Implicit none
  Contains

   !!--++
   !!--++ GET_BONDS_TABLE_SYMBOL
   !!--++    Fills the components of the Bond_Length_Table variable
   !!--++
   !!--++ 15/04/2019
   !!
   Module Function Get_Bonds_Table_Symbol(Symb1,Symb2) Result(Bonds)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: Symb1
      character(len=*),           intent(in)  :: Symb2
      real(kind=cp), dimension(3)             :: Bonds

      !---- Local Variables ----!
      integer :: Z1,Z2

      z1=Get_Z_Symb(Symb1)
      z2=Get_Z_Symb(Symb2)

      if (.not. Set_BT_Variable) call Set_Bonds_Table()
      Bonds=Get_Bonds_Table_Z(Z1,Z2)

   End Function Get_Bonds_Table_Symbol

   !!--++
   !!--++ GET_BONDS_TABLE_Z
   !!--++    Fills the components of the Bond_Length_Table variable
   !!--++
   !!--++ 15/04/2019
   !!
   Module Function Get_Bonds_Table_Z(Z1,Z2) Result(Bonds)
      !---- Arguments ----!
      integer,                    intent(in)  :: Z1
      integer,                    intent(in)  :: Z2
      real(kind=cp),dimension(3)              :: Bonds

      Bonds=0.0_cp
      if (z1 <= 0 .or. z1 > 103) return
      if (z2 <= 0 .or. z2 > 103) return

      if (.not. Set_BT_Variable) call Set_Bonds_Table()

      bonds=bond_length_table(:,z1,z2)

   End Function Get_Bonds_Table_Z

End SubModule TAB_Get_BondsT