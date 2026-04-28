!!----
!!----
!!----
!!----
SubModule (CFML_GSPACEGROUPS) gS_Inverse_OP
   implicit none
   Contains
   !!----
   !!---- INVERSE_OP_SYMM
   !!----    Return the inverse symmetry operator
   !!----
   !!---- 12/05/2019 10:52:02
   !!
   Module Function Inverse_OP(Op) Result(i_OP)
      !---- Arguments ----!
      type(Symm_Oper_Type), intent(in) :: Op
      type(Symm_Oper_Type)             :: i_Op

      !---- Local Variables ----!
      integer                         :: d
      type(rational), dimension(3,3)  :: r
      type(rational), dimension(3)    :: t


      !> Init
      d=size(Op%Mat,dim=1)
      call allocate_op(d, i_Op)

      call Rational_Identity_Matrix(r)
      r=-rational_inverse_matrix(Op%Mat(1:3,1:3))
      if (err_CFML%Ierr /= 0) return
      t=matmul(r,Op%Mat(1:3,4))
      t=rational_modulo_lat(t)

      i_Op%mat(1:3,1:3)=-r(1:3,1:3)
      i_Op%mat(1:3,4)=t

   End Function Inverse_OP

End SubModule gS_Inverse_OP