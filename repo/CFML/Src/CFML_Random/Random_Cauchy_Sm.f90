!!----
!!----
!!----
SubModule (CFML_Random) Random_Cauchy_Sm
  implicit none
   Contains
   !!----
   !!---- RANDOM_CAUCHY
   !!----
   !!----    Generate a random deviate from the standard Cauchy distribution
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Cauchy() Result(Fn_Val)
      !---- Arguments ----!
      real(kind=cp) :: fn_val

      !---- Local variables ----!
      real(kind=cp),dimension(2) :: v

      do
         call random_number(v)
         v = two*(v - half)
         if (abs(v(2)) < vsmall) cycle               ! test for zero
         if (v(1)**2 + v(2)**2 < one) exit
      end do

      fn_val = v(1) / v(2)

   End Function Random_Cauchy

End SubModule Random_Cauchy_Sm