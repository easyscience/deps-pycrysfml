Submodule (CFML_Structure_Factors) SF_AtomicFactors
   !---- Variables ----!
   implicit none

 Contains

   !!--++
   !!--++ FUNCTION Fj(s,a,b,c)
   !!--++    Atomic scattering factor calculation according to:
   !!--++       Fj(s)=Sum_i[Ai*exp(-Bi*s*s)] + C (i=1..4)
   !!--++
   !!--++ Update: April 2022
   !!
   Module Function Fj(s,a,b,c) Result(res)
      !---- Arguments ----!
      real(kind=cp),             intent(in) :: s
      real(kind=cp),dimension(4),intent(in) :: a
      real(kind=cp),dimension(4),intent(in) :: b
      real(kind=cp),             intent(in) :: c
      real(kind=cp)                         :: res

      !---- Local variables ----!
      integer :: i

      res=0.0_cp
      do i=1,4
         res=res + a(i)*exp(-b(i)*s*s)
      end do
      res=res+c

   End Function Fj

End Submodule SF_AtomicFactors