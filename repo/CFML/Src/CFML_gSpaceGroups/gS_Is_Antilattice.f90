!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_is_AntiLattice
   implicit none
   Contains

   !!----
   !!---- IS_ANTILATTICE
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_OP_Anti_Lattice(Op) Result(Info)
      !---- Arguments ----!
      type(Symm_Oper_Type),intent(in) :: Op
      logical                         :: info

      !---- Local Variables ----!
      integer                                     :: d
      type(rational), dimension(:,:), allocatable :: Mat

      !> Init
      info=.false.

      d=size(Op%Mat(:,1))-1

      if (allocated(Mat)) deallocate(Mat)
      allocate(Mat(d,d))
      call Rational_Identity_Matrix(Mat)

      if (Rational_Equal(Op%Mat(1:d,1:d),Mat) .and. Op%time_inv == -1) then
         if (.not. Rational_Is_NullVector(Op%Mat(1:d, d+1))) info=.true.
      end if

   End Function Is_OP_Anti_Lattice

End SubModule gS_is_AntiLattice