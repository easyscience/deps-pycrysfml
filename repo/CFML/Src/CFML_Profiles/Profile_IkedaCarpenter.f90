!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_IkedaCarpenter
  implicit none
   Contains
   !!----
   !!---- IKEDA_CARPENTER
   !!----
   !!----
   !!---- U21/04/2019
   !!
   Pure Module Function Ikeda_Carpenter(X,Par) Result (Ik_Val)
      !---- Arguments ---!
      real(kind=cp),              intent(in) :: x
      real(kind=cp), dimension(:),intent(in) :: par
      real(kind=cp)                          :: ik_val

      !--- Local variables ---!
      real :: alfa,beta,R,amb,a3,x2,exb,exa,ramb,poly

      if (x < 0.0) then
         ik_val=0.0
      else
         x2=x*x
         alfa=par(1)
         beta=par(2)
         R   =par(3)
         amb=alfa-beta
         ramb=1.0/(amb*amb*amb)
         a3=alfa*alfa*alfa
         exb=exp(-beta*x)
         exa=exp(-alfa*x)
         poly=1.0+(1.0+0.5*amb*x)*amb*x
         ik_val = 0.5*a3*((1.0-R)*x2*exa+2.0*R*beta*ramb*(exb-exa*poly))
      end if

   End Function Ikeda_Carpenter

   !!----
   !!---- IKEDA_CARPENTER_DER
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Subroutine Ikeda_Carpenter_Der(X,Par,Ik_Val,Dpar)
      !---- Arguments ----!
      real(kind=cp),                        intent(in)  :: x
      real(kind=cp),           dimension(:),intent(in)  :: par
      real(kind=cp),                        intent(out) :: ik_val
      real(kind=cp), optional, dimension(:),intent(out) :: dpar

      !--- Local variables ---!
      real(kind=cp) :: alfa,beta,R,amb,a3,x2,exb,exa,ramb,poly
      real(kind=cp) :: derX, derAlf, derBet, derR

      if (x < 0.0) then
         ik_val=0.0
         if (present(dpar)) then
            derX=0.0
            derAlf=0.0
            derBet=0.0
            derR=0.0
            dpar(1:4) = (/derX,derAlf,derBet,derR/)
         end if
      else
         x2=x*x
         alfa=par(1)
         beta=par(2)
         R   =par(3)
         amb=alfa-beta         !damb/da=1; damb/db=-1; d(b*amb^(-3))/db=amb^(-3)-3b amb^-3/amb= ramb*(1+3b/amb)
         ramb=1.0/(amb*amb*amb)!dramb/da=-3 amb^(-4);  dramb/db= 3 amb^(-4)
         a3=alfa*alfa*alfa
         exb=exp(-beta*x)
         exa=exp(-alfa*x)                 !dexa/da= exa * (-x)
         poly=1.0+(1.0+0.5*amb*x)*amb*x   !dpoly/dx= (1+amb*x)*amb ; dpoly/da= x+0.5*x*x*2*amb = (1+x*amb)*x
         ik_val = 0.5*a3*((1.0-R)*x2*exa+2.0*beta*ramb*(exb-exa*poly)*R)
         if (present(dpar)) then
            derX=0.5*a3*((1.0-R)*(2.0*x*exa-x2*exa*alfa)+2.0*beta*ramb*R*(-exb*beta+exa*alfa*poly-exa*(1.0+amb*x)*amb))
            derAlf=3.0*poly/a3 + 0.5*a3*(-(1.0-R)*x2*exa*x + 2.0*beta*R*ramb*( -3.0/amb *(exb-exa*poly) + &
                   exa*( x*poly-(1.0+x*amb)*x )))
            derBet=a3*R * ( ramb*(1.0+3.0*beta/amb)*(exb-exa*poly) + beta*ramb*(-exb*x + exa*(1.0+x*amb)*x))
            derR=0.5*a3*(-x2*exa+2.0*beta*ramb*(exb-exa*poly))
            dpar(1:4) = (/derX,derAlf,derBet,derR/)
         end if
      end if

   End Subroutine Ikeda_Carpenter_Der


End SubModule Profile_IkedaCarpenter