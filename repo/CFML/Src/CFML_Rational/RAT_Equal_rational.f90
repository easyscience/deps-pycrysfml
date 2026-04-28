!!----
!!----
!!----
!!----
!!
SubModule (CFML_Rational) RAT_Equal_Rational
 implicit none
 Contains

   !!----
   !!---- RATIONAL_EQUAL_VECTOR
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Equal_Vector(Vec1,Vec2) Result(Equal)
      !---- Arguments ----!
      type(rational), dimension(:), intent(in) :: vec1, vec2
      logical                                  :: equal

      !---- Local Variables ----!
      integer:: i,n1,n2

      n1=size(vec1); n2=size(vec2)

      equal=.false.
      if (n1 /= n2) return

      do i=1,n1
         if (vec1(i) /= vec2(i)) return
      end do
      equal=.true.

   End Function Rational_Equal_Vector

   !!----
   !!---- RATIONAL_EQUAL_MATRIX
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Equal_Matrix(Mat1,Mat2) Result(Equal)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: Mat1,Mat2
      logical                                    :: equal

      !---- Local Variables ----!
      integer:: i,j,n11,n12,n21,n22

      n11=size(Mat1,dim=1); n12=size(Mat1,dim=2)
      n21=size(Mat2,dim=1); n22=size(Mat2,dim=2)

      equal=.false.
      if(n11 /= n21 .or. n12 /= n22) return

      do j=1,n12
         do i=1,n11
            if (Mat1(i,j) /= Mat2(i,j)) return
         end do
      end do
      equal=.true.

   End Function Rational_Equal_Matrix

End SubModule RAT_Equal_Rational