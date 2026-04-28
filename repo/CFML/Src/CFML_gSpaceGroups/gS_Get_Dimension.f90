!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Dimension
   implicit none
   Contains
   !!----
   !!---- Get_Dimension_SymmOp
   !!----
   !!---- 19/04/2019
   !!
   Module Function Get_Dimension_SymmOp(Symb) Result(d)
      !---- Arguments ----!
      character(len=*), intent(in) :: Symb
      integer                      :: d

      !---- Local Variables ----!
      integer, dimension(20) :: Pos
      integer                :: np,i,j

      !> Init
      d=0

      call Get_Separator_Pos(symb, ",", pos, np)
      if (np <= 0) return

      !> Verify if time reversal symbol is provided
      !> reading an integer supposed to be invt
      read(unit=symb(pos(np)+1:),fmt=*, iostat=i) j

      if (i == 0) then
         d=np+1     !> time_inv provided
      else
         d=np+2     !>time_inv not-provided
      end if
   End Function Get_Dimension_SymmOp

End SubModule gS_Get_Dimension

