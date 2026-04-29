!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_BacktoBack
  implicit none
   Contains
   !!----
   !!---- BACK_TO_BACK_EXP
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Function Back_To_Back_Exp(X,Par) Result (Bb_Val)
      !---- Arguments ----!
      real(kind=cp),              intent(in) :: x
      real(kind=cp), dimension(:),intent(in) :: par
      real(kind=cp)                          :: bb_val

      !--- Local variables ---!
      real(kind=cp) :: alfa,beta,N

      alfa=par(1)
      beta=par(2)
      N= 0.5*alfa*beta/(alfa+beta)
      if ( x < 0.0) then
         bb_val =  N*exp(alfa*x)
      else
         bb_val =  N*exp(-beta*x)
      end if

   End Function Back_To_Back_Exp

   !!----
   !!---- BACK_TO_BACK_EXP_DER
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Subroutine Back_To_Back_Exp_Der(X,Par,Bb_Val,Dpar)
      !---- Arguments ----!
      real(kind=cp),                        intent(in)  :: x
      real(kind=cp),           dimension(:),intent(in)  :: par
      real(kind=cp),                        intent(out) :: bb_val
      real(kind=cp), optional, dimension(:),intent(out) :: dpar

      !--- Local variables ---!
      real(kind=cp) :: alfa,beta,N,derX,derAlf,derBet

      alfa=par(1)
      beta=par(2)
      N= 0.5*alfa*beta/(alfa+beta)   !dN/da= 2(N/bet)**2 , dN/db= 2(N/alf)**2

      if ( x < 0.0) then
         bb_val =  N*exp(alfa*x)
         if (present(dpar)) then
            derX=bb_val*alfa
            derAlf=bb_val*(2.0*N/beta/beta+x)    !df/da= dN/da*exp(a*x)+Nexp(a*x)*(x)= (2N/bet**2+x)* f
            derBet=bb_val* 2.0*N/alfa/alfa       !df/db= dN/db * exp(-a*x) = 2N/alf**2 *f
            dpar(1:3) = [derX,derAlf,derBet]
         end if
      else
         bb_val =  N*exp(-beta*x)
         if (present(dpar)) then
            derX=bb_val*beta
            derAlf=bb_val*2.0*N/beta/beta      !df/da= dN/da*exp(-b*x)= 2N/bet**2 * f
            derBet=bb_val*(2.0*N/alfa/alfa-x)  !df/db= dN/db * exp(-b*x)+Nexp(-b*x)*(-x) = (2N/alf**2 -x) *f
            dpar(1:3) = [derX,derAlf,derBet]
         end if
      end if

   End Subroutine Back_To_Back_Exp_Der

End SubModule Profile_BacktoBack