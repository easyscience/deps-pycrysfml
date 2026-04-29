!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Calc
   implicit none

   Contains

   !!----
   !!---- EOS_CAL
   !!----
   !!---- Returns elastic properties (not the parameter values) at this P,T for EoS
   !!----
   !!---- Date: 17/07/2015
   !!---- Revision: OK
   !!
   Module Function EoS_Cal(P, T, EoS) Result(Parvals)
      !---- Arguments ----!
      real(kind=cp),                intent(in)  :: P       ! Pressure
      real(kind=cp),                intent(in)  :: T       ! Temperature
      type(Eos_Type),               intent(in)  :: EoS     ! Eos Parameter
      real(kind=cp), dimension(6)               :: Parvals ! Output parameter values

      !> Init
      parvals=0.0_cp

      call physical_check(eos,Pin=p,Tin=t)   ! produce warnings based on P,T
      if (err_CFML%Flag) return

      parvals(1)=get_volume(p,t,eos)
      parvals(2)=k_cal(parvals(1),t,eos,P=p)
      parvals(3)=kp_cal(parvals(1),t,eos,P=p)
      parvals(4)=kpp_cal(parvals(1),t,eos)
      parvals(5)=dKdT_cal(p,t,eos)           ! dK/dT at this P,T
      parvals(6)=alpha_cal(p,t,eos)          ! 1/V.dV/dT at this T

   End Function EoS_Cal

   !!----
   !!---- EOS_CAL_ESD
   !!----
   !!---- Returns esd's of the elastic properties (not the parameter values) at this P and T for EoS
   !!----
   !!---- Date: 17/07/2015
   !!---- Revision: OK
   !!
   Module Function EoS_Cal_Esd(P, T, EoS) Result(Esd)
      !---- Arguments ----!
      real(kind=cp),                intent(in)  :: P       ! Pressure
      real(kind=cp),                intent(in)  :: T       ! Temperature
      type(Eos_Type),               intent(in)  :: EoS     ! Eos Parameter
      real(kind=cp),  dimension(6)              :: Esd     ! Output esd values

      !---- Local Variables ----!
      real(kind=cp),dimension(N_EOSPAR) :: esdfull

      !> Init
      esd=0.0_cp
      esdfull=0.0_cp

      !> calculate parameter esd's transformed to this P,T
      esdfull= transform_Esd(P,T,EoS)
      esd(1:5)=esdfull(1:5)
      esd(6)=esdfull(10)

      !> make adjustment for only using alpha0
      select case(EoS%itherm)
          case(5)               ! Salje
            esd(6)=esd(6)/3.0_cp            ! alpha is 1/3 of p1
      end select

   End Function EoS_Cal_Esd

End SubModule EoS_Calc