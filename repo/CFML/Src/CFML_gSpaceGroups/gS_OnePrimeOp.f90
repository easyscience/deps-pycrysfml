!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_OnePrimeOp
   implicit none
   Contains
   !!----
   !!---- IS_ONEPRIME
   !!----   .True. is the operator is a 1'
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_OP_1_Prime(Op) Result(Info)
      !---- Arguments ----!
      type(Symm_Oper_Type),intent(in) :: Op
      logical                         :: info

      !---- Local Variables ----!
      integer                                     :: d
      type(rational), dimension(:,:), allocatable :: Mat

      !> Init
      info=.false.

      d=size(Op%Mat(:,1))-1

      !> Traslational have to be zero
      if (.not. Rational_Is_NullVector(Op%Mat(1:d, d+1))) return

      if (allocated(Mat)) deallocate(Mat)
      allocate(Mat(d,d))
      call Rational_Identity_Matrix(Mat)

      if (Rational_Equal(Op%Mat(1:d,1:d),Mat) .and. Op%time_inv == -1) info=.true.
   End Function Is_OP_1_Prime

   !!----
   !!---- IS_MINUS_ONEPRIME
   !!----   .True. is the operator is -1'
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_OP_Minus_1_Prime(Op) Result(Info)
      !---- Arguments ----!
      type(Symm_Oper_Type),intent(in) :: Op
      logical                         :: info

      !---- Local Variables ----!
      integer                                     :: d
      type(rational), dimension(:,:), allocatable :: Mat

      !> Init
      info=.false.

      d=size(Op%Mat(:,1))-1

      !> Traslational have to be zero
      if (.not. Rational_Is_NullVector(Op%Mat(1:d, d+1))) return

      if (allocated(Mat)) deallocate(Mat)
      allocate(Mat(d,d))
      call Rational_Identity_Matrix(Mat)

      if (Rational_Equal(Op%Mat(1:d,1:d),-Mat) .and. Op%time_inv == -1) info=.true.
   End Function Is_OP_Minus_1_Prime

   !!----
   !!---- SEARCH_ONEPRIME_OPERATOR
   !!----   Search 1' or -1' in the Space group operators. This function return:
   !!----       0 if not found 1' or -1'
   !!----       1 for 1'
   !!----      -1 for -1'
   !!---
   !!---- 25/05/2019
   !!
   Module Function Search_OnePrime_Operator(G) Result(Prime)
      !---- Arguments ----!
      class(spg_type), intent(in) :: G
      integer                     :: Prime

      !---- Local Variables ----!
      integer                        :: n
      type(rational), dimension(3)   :: tr
      type(rational), dimension(3,3) :: Identidad

      !> Init
      Prime=0

      call Rational_Identity_Matrix(identidad)

      do n=1,G%multip
         !> Only need to find 1' or -1'
         if (G%op(n)%Time_Inv /= -1) cycle
         tr=G%op(n)%Mat(1:3,4)
         if (.not. rational_is_NullVector(tr) ) cycle

         if (rational_equal(G%op(n)%Mat(1:3,1:3), identidad)) then
            prime=1
            exit
         end if

         if (rational_equal(G%op(n)%Mat(1:3,1:3),-identidad)) then
            prime=-1
            exit
         end if
      end do

   End Function Search_OnePrime_Operator


End SubModule gS_OnePrimeOp