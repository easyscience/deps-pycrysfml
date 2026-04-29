!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_is_InversionCentre
   implicit none
   Contains

   !!----
   !!---- Is_OP_Inversion_Centre
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_OP_Inversion_Centre(Op) Result(Info)
      !---- Arguments ----!
      type(Symm_Oper_Type),intent(in) :: Op
      logical                         :: info

      !---- Local Variables ----!
      integer :: dr

      !> Init
      info=.false.

      if (.not. allocated(identity_matrix)) return
      dr=size(Op%Mat(:,1))-1
      if (Rational_Equal(-identity_matrix(1:dr,1:dr),Op%Mat(1:dr,1:dr)) .and. Op%time_inv == 1) info=.true.
   End Function Is_OP_Inversion_Centre

End SubModule gS_is_InversionCentre

