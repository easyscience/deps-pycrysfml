!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Strain
   implicit none
   Contains

   !!----
   !!---- FUNCTION STRAIN
   !!----
   !!---- Returns the value of Strain (S) at this V according with the EoS model
   !!----
   !!--.. NOTE:
   !!--..    The values of EosPar are NOT used. Only the type of Eos function is needed!
   !!--..    For linear eos, the strain is that of a^3
   !!--..    If V/V0 = 0. then an error is set
   !!----
   !!---- Date: 15/02/2013
   !!
   Module Function Strain(VV0, Eos) Result(S)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: VV0  ! Volume divided by V0 at this temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: s

      !---- Local Variables ----!
      real(kind=cp) :: cvv0

      !> Init
      s=0.0_cp
      if (vv0 <= 0.00001) then
         call set_error(1,"Strain calculation called with invalid V/V0 =< 0: strain set zero")
         return
      end if

      !> Local copy
      cvv0=vv0
      if (eos%linear) cvv0=vv0**3.0_cp

      select case (eos%imodel)
         case (1,5,6,7) ! Murnaghan, Tait, APL, Kumar
            s=0.0_cp

         case (2) ! Birch-Murnaghan
            s=(cvv0**(-2.0_cp/3.0_cp) - 1.0_cp)/2.0_cp

         case (3) ! Vinet: new definition RJA 28/03/2013
            s= 1.0_cp - cvv0**(1.0_cp/3.0_cp)

         case (4) ! Natural Strain: the original definition of strain was wrong in v5 by a change in sign
            s= -1.0_cp*log(cvv0)/3.0_cp
      end select

   End Function Strain

   !!----
   !!---- FUNCTION STRAIN_EOS
   !!----
   !!---- Returns the value of Strain (S) at this V and T according with the EoS parameters
   !!----
   !!---- Date: 05/09/2013
   !!
   Module Function Strain_EoS(V, T,Eos) Result(S)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: V    ! Volume
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameters
      real(kind=cp)              :: s

      !---- Local Variables ----!
      real(kind=cp) :: vvo

      !> Init
      s=0.0_cp
      if (v <= 0.00001) then
         call set_error(1,"Strain calculation called with invalid V =< 0: strain set zero")
         return
      end if

      !> Calculate
      vvo=v/get_volume(0.0_cp,t,eos)     ! vv0 is V(P,T)/V(0,T) or a(P,T)/a(0,T)
      s=strain(vvo,eos)                  ! cubes vv0 on input if linear

   End Function Strain_EOS

End SubModule EoS_Strain