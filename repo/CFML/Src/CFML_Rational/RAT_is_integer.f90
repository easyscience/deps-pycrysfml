!!----
!!----
!!----
!!----
!!
SubModule (CFML_Rational) RAT_Is_Integer
 implicit none
 Contains

   !!----
   !!---- IS_INTEGER_RATIONAL_MATRIX
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Is_Integer_Rational_Matrix(Mat) Result(OK)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: Mat
      logical                                    :: OK

      !---- Local Variables ----!
      integer:: i,j,n1,n2

      n1=size(Mat,dim=1); n2=size(Mat,dim=2)
      OK=.false.

      do j=1,n1
         do i=1,n2
            if (Mat(i,j)%denominator /= 1_LI ) return
         end do
      end do
      OK=.true.

   End Function Is_Integer_Rational_Matrix

   !!----
   !!---- IS_INTEGER_RATIONAL_VECTOR
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Is_Integer_Rational_Vector(Vec) Result(OK)
      !---- Arguments ----!
      type(rational), dimension(:), intent(in) :: vec
      logical                                  :: ok

      !---- Local Variables ----!
      integer :: i,n

      n=size(vec)
      OK=.false.

      do i=1,n
         if (vec(i)%denominator /= 1_LI ) return
      end do
      OK=.true.

   End Function Is_Integer_Rational_Vector

   !!----
   !!---- IS_INTEGER_RATIONAL_SCALAR
   !!----
   !!---- 08/04/2019
   !!
   Elemental Module Function Is_Integer_Rational_Scalar(R) Result(OK)
      !---- Arguments ----!
      type(rational), intent(in) :: r
      logical                    :: OK

      OK=.false.
      if (r%denominator == 1_LI ) ok=.true.

   End Function Is_Integer_Rational_Scalar

End SubModule RAT_Is_Integer