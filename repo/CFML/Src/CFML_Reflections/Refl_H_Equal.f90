!!----
!!----
!!----
!!
SubModule (CFML_Reflections) Refl_H_Equal
   implicit none
   Contains
   !!----
   !!---- H_EQUAL_Int
   !!----     .True. if both reflections are equal
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_Equal_Int(H,K) Result (Info)
      !---- Arguments ----!
      integer, dimension(:), intent(in) :: h
      integer, dimension(:), intent(in) :: k
      logical                           :: info

      !---- Local Variables ----!
      integer :: i

      !> Init
      info=.true.
      do i=1,size(H)
         if (h(i) /= k(i)) then
            info=.false.
            return
         end if
      end do
   End Function H_Equal_Int
   !!----
   !!---- H_EQUAL_Real
   !!----     .True. if both reflections are equal
   !!----
   !!---- 14/03/2023
   !!
   Module Function H_Equal_Real(H,K) Result (Info)
      !---- Arguments ----!
      real(kind=cp), dimension(:), intent(in) :: h
      real(kind=cp), dimension(:), intent(in) :: k
      logical                                 :: info

      !---- Local Variables ----!
      integer :: i

      !> Init
      info=.true.
      do i=1,size(H)
         if (abs(h(i) - k(i)) > EPS_REF) then
            info=.false.
            return
         end if
      end do
   End Function H_Equal_real

End SubModule Refl_H_Equal