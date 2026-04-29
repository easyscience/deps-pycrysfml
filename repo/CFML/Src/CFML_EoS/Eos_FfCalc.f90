!!----
!!----
!!----
SubModule (CFML_EoS) EoS_FfCalc
   implicit none
   Contains

   !!----
   !!---- SUBROUTINE FFCAL_DAT
   !!----
   !!---- Return Normalized pressure and/or Strain at V and P
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine FfCal_Dat(V, V0, P, EoS, F, S)
      !---- Arguments ----!
      real(kind=cp),           intent(in)  :: V    ! Volume
      real(kind=cp),           intent(in)  :: V0   ! Volume at zero pressure
      real(kind=cp),           intent(in)  :: P    ! Pressure
      type(Eos_Type),          intent(in)  :: EoS  ! Eos Parameter: only imodel and linear used
      real(kind=cp), optional, intent(out) :: F    ! Normalised pressure
      real(kind=cp), optional, intent(out) :: S    ! Strain

      !---- Local Variables ----!
      real(kind=cp) :: vv0, sc, fc

      !> Check
      if (present(f)) f=0.0_cp
      if (present(s)) s=0.0_cp

      if (abs(v0) <= 0.00001) then
         call set_error(1,'V0 is zero - No strain can be calculated!')
         return
      end if

      !> Calculate VV0 or a/a0
      vv0=v/v0
      sc=Strain(VV0,Eos)                      ! returns volume like strain for linear
      fc=NormPressure_P(sc,P,EoS%imodel)

      if (present(f)) f=fc
      if (present(s)) s=sc

   End Subroutine FfCal_Dat

   !!----
   !!---- SUBROUTINE FFCAL_DAT_ESD
   !!----
   !!---- Calculate the Normalised Pressure (F) and Strain (S) value at
   !!---- Volume (V) and Pressure (P) and also their ESD
   !!----
   !!--.. IMPORTANT: Eosparameters are not used in this calculation!
   !!--.. The only element of esopar that is used is the eos model type
   !!--.. and the linear flag when the strain function is called
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine FfCal_Dat_Esd(V, SigV, V0, SigV0, P, SigP, Eos, F, SigF, S, SigS)
      !---- Arguments ----!
      real(kind=cp),  intent(in)  :: V       ! Volume
      real(kind=cp),  intent(in)  :: SigV    ! Sigma (V)
      real(kind=cp),  intent(in)  :: V0      ! Vo
      real(kind=cp),  intent(in)  :: SigV0   ! Sigma (Vo)
      real(kind=cp),  intent(in)  :: P       ! Pressure
      real(kind=cp),  intent(in)  :: SigP    ! Sigma (P)
      type(Eos_Type), intent(in)  :: EoS     ! Eos Parameter
      real(kind=cp),  intent(out) :: F       ! Normalised pressure
      real(kind=cp),  intent(out) :: SigF    ! Sigma (F)
      real(kind=cp),  intent(out) :: S       ! Strain
      real(kind=cp),  intent(out) :: SigS    ! Sigma (S)

      !---- Local Variables ----!
      real(kind=cp) :: vv0,vv0_esd, sigmap,e

      !> Init
      f=0.0_cp; sigf=0.0_cp
      s=0.0_cp; sigs=0.0_cp

      if (abs(v0) <= 0.00001) then
         call set_error(1,'V0 is zero - No strain can be calculated!')
         return
      end if

      !> Note that V0 in call is the V0 at this temperature
      vv0=v/v0
      vv0_esd=vv0*sqrt( (sigv/abs(v))**2.0_cp + (sigv0/abs(v0))**2.0_cp )

      !> Strain
      s=Strain(vv0,Eos)      ! input a/a0 or v/v0 to strain. It always returns volume strain

      !> Normalized Pressure
      f=NormPressure_P(s,p,EoS%imodel)

      !> Check Pressure values
      if (abs(p) <= 0.0001) then
         e=0.0_cp
      else
         e=sigp/p
      end if

      !> If the finite strain is zero, the esd of the normalised pressure is not defined
      if (s < tiny(0.0)) return

      !> ESD Calculations: all done in volume terms
      if (eos%linear) then
         vv0_esd=3.0*vv0**2.0_cp*vv0_esd
         vv0=vv0**3.0_cp
      end if

      select case (eos%imodel)
         case (1,5,6,7) ! Murnaghan, Tait, APL, Kumar
            ! Nothing to do

         case (2) ! Birch-Murnaghan
            sigmap=((7.0_cp * vv0**(-2.0_cp/3.0_cp) -5.0_cp)*vv0_esd) / &           ! this is sigma_prime in texts
                   (3.0_cp*(1.0_cp - vv0**(-2.0_cp/3.0_cp))*vv0)

            sigs=vv0**(-5.0_cp/3.0_cp) * vv0_esd / 3.0_cp
            sigf=f*sqrt(e**2.0_cp + sigmap**2.0_cp)

         case (3) ! Vinet:
            sigs=vv0**(-2.0_cp/3.0_cp) * vv0_esd/3.0_cp
            sigmap= (s*s-1.0_cp)/s/(1.0_cp-s)**2.0_cp
            sigmap=sigmap*sigs

            sigf=f*sqrt(e**2.0_cp + sigmap**2.0_cp)

         case (4) ! Natural
            sigmap=(1.0_cp - 1.0_cp/log(vv0))*vv0_esd/vv0

            sigs=vv0_esd/3.0_cp/vv0
            sigf=f*sqrt(e**2.0_cp + sigmap**2.0_cp)
      end select

   End Subroutine FfCal_Dat_Esd

   !!----
   !!---- FFCAL_EOS
   !!----  Returns normalised pressure and Strain at this P,T,
   !!----  calculated from the eos parameters
   !!----
   !!---- 17/07/2015
   !!
   Module Subroutine FfCal_EoS(P,T,EoS,F,S)
      !---- Arguments ----!
      real(kind=cp),           intent(in)     :: P       ! Pressure
      real(kind=cp),           intent(in)     :: T       ! Temperature
      type(Eos_Type),          intent(in)     :: EoS     ! Eos Parameter
      real(kind=cp),           intent(out)    :: F       ! Normalised pressure
      real(kind=cp),           intent(out)    :: S       ! Strain

      !---- Local Variables ----!
      real(kind=cp) :: v

      !> Init
      f=0.0_cp
      s=0.0_cp

      v=get_volume(p,t,eos)         ! V at this P,T from eos parameters
      s=strain_eos(v,t,eos)         ! finite strain at this P,T
      f=normpressure_eos(s,t,eos)   ! Norm pressure

   End Subroutine FfCal_EoS

End SubModule EoS_FfCalc