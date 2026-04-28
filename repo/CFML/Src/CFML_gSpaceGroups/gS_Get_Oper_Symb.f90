!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Oper_Symb
   implicit none
   Contains

   !!----
   !!---- GET_OP_FROM_SYMB
   !!----
   !!---- 19/04/19
   !!
   Module Function Get_Op_from_Symb(Symb) Result(Op)
      !---- Arguments ----!
      character(len=*),     intent(in) :: symb
      type(Symm_Oper_Type)             :: Op

      !---- Local variables ----!
      integer :: d

      d=Get_Dimension_SymmOp(Symb)

      allocate(Op%Mat(d,d))
      call Get_Mat_From_Symb_Int(Symb, Op%Mat, Op%time_inv)
      Op%dt=rational_determ(Op%Mat(1:3,1:3))

   End Function Get_Op_from_Symb

End SubModule gS_Get_Oper_Symb

