!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_APL
   implicit none
   Contains

   !!--++
   !!--++ GET_APL
   !!--++
   !!--++ Returns a,b,c APL parameters and their derivatives in vectors.
   !!--++
   !!--++ output values:
   !!--++      a(1,j) are a,b,c
   !!--++      a(2,j) are first derivs
   !!--++      a(3,j) are second derivs of the a,b,c
   !!--++
   !!--++ Explicit values of VV0 etc used as input, because this depends on thermal model etc
   !!--++ not just on the PV model parameters
   !!--++
   !!--++ Date: 20/11/2019 RJA
   !!
   Module Function Get_APL(VV0, V0, K0, Kp, Kpp, Z, Iorder) Result(A)
      !---- Arguments ----!
      real(kind=cp),               intent(in)  :: VV0, V0, K0, Kp, Kpp, Z   !input parameters: VV0 is V/V0
      integer,                     intent(in)  :: iorder
      real(kind=cp),dimension(3,3)             :: a

      !---- Local Variables ----!
      real(kind=cp) :: x,c0,c2,c3,pFG0

      !> init
      a=0.0_cp

      x=vv0**0.333333_cp
      pFG0=AFERMIGAS*(Z/v0)**1.66666667_cp
      c0=-1.0_cp*log(3.0_cp*K0/pFG0)         ! assumes V in A^3

      select case(iorder)
         case(2) !AP1
            c2=0._cp
            c3=0._cp

         case(3) !AP2
            c2=1.5_cp*(Kp-3.0_cp)-c0
            c3=0._cp

         case(4) !AP3
            c2=1.5_cp*(Kp-3.0_cp)-c0
            !c3 in steps, using the Holzapfel expression for Kpp0
            c3=-9._cp*kpp*k0
            c3=(20._cp + 12._cp*c0 + c0*c0 + 2.0_cp*c2*(9.0_cp+c0) + 4.0_cp*c2*c2 - c3)/6.0_cp
      end select

      !> terms in pressure expression up to AP3
      a(1,1)=1.0_cp/x**5.0_cp*(1.0_cp-x)
      a(1,2)=exp(c0*(1.0_cp-x))
      a(1,3)=1.0_cp+c2*x*(1.0_cp-x) + c3*x*(1.0_cp-x)**2.0_cp     !Only one with extra term for AP3

      !> first derivatives wrt x up to AP2
      a(2,1)= -5.0_cp/x**6.0_cp +4.0_cp/x**5.0_cp
      a(2,2)= -1.0_cp*c0*a(1,2)
      a(2,3)= c2*(1.0_cp-2.0_cp*x) + c3*(1.0_cp-4.0_cp*x+3.0_cp*x*x) !Only one with extra term for AP3

      !> second derivatives wrt x up to AP2
      a(3,1)= 30.0_cp/x**7.0_cp - 20.0_cp/x**6.0_cp
      a(3,2)=c0*c0*a(1,2)
      a(3,3)=-2.0_cp*c2 + c3*(-4.0_cp*x+6.0_cp*x) !Only one with extra term for AP3

   End Function Get_APL

End SubModule EoS_Get_APL