!!----
!!----
!!----
SubModule (CFML_EoS) EoS_dKdTCalc
   implicit none

   Contains

   !!----
   !!---- FUNCTION DKDT_CAL
   !!----
   !!---- Calculate the derivative dK/dt (or dM/dt in the linear case) at P and T
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Function dKdT_Cal(Pin, Tin, EoS, DeltaT) Result(dKdT)
      !---- Arguments ----!
      real(kind=cp),            intent(in) :: Pin       ! Pressure
      real(kind=cp),            intent(in) :: Tin       ! Temperature
      type(Eos_Type),           intent(in) :: EoS     ! Eos Parameter
      real(kind=cp),  optional, intent(in) :: DeltaT  ! Delta T
      real(kind=cp)                        :: dKdT

      !---- Local Variables ----!
      integer                      :: j
      real(kind=cp)                :: p,t,del,tlimit,tcal,Ttr !,vlimit
      real(kind=cp),dimension(-2:2):: kpt


      !> Init: set step size for numerical differentiation
      p=pin
      t=tin
      del=30.0_cp                        ! good number for accuracy
      if (present(deltat)) del=deltat
      if (t-2.0*del < 0.0_cp) del=t/2.1  ! prevents going to negative T

      !> Code to prevent crossing a phase boundary
      if (eos%itran > 0) then
         Ttr = Get_Transition_Temperature(P,eos)
         if (transition_phase(P,T+2.0*del,eos) .neqv. transition_phase(P,T,eos)) del=0.4*abs(Ttr-T)
         if (transition_phase(P,T-2.0*del,eos) .neqv. transition_phase(P,T,eos)) del=0.4*abs(Ttr-T)
      end if

      !> Code to stop some Pthermal EoS going into illegal large volume above T
      if (eos%itherm == 7 .or. eos%itherm == 8) then
         tlimit=t+2.0*del
         do                                        ! search for positive K at this P
            if (get_K(p,tlimit,eos) > 0.0_cp .and. (.not. err_cfml%flag)) exit
            call clear_error()
            tlimit=tlimit-0.1_cp*(tlimit-t)
            if (tlimit < t)exit                    ! should never happen because P,T is valid
         end do
         del=0.4*abs(tlimit-t)
      end if

      !>Code to stop ptvtable eos going beyond the table edge:
      if(eos%imodel == -1)then
          if (p < eos%table%pmin) p=eos%table%pmin   !because beyond table in P, dK/dT same as at edge
          if (p > eos%table%pmax) p=eos%table%pmax
          if (t-2.0*del < eos%table%tmin) del=(t-eos%table%tmin)/2.0_cp -1.0_cp
          if (t+2.0*del > eos%table%tmax) del=(eos%table%tmax - t)/2.0_cp -1.0_cp
      endif

      !> Trap close to zero K
      if (t < 1.0_cp) then
         dKdT=Get_K(P,1.0,eos)-Get_K(P,0.0,eos)

      else
         do j=-2,2,1
            tcal=t+real(j)*del              ! apply shift to t
            kpt(j)=Get_K(P,tcal,eos)        ! calc resulting K
         end do
         dKdT=(kpt(-2)+8.0_cp*(kpt(1)-kpt(-1))-kpt(2))/(12.0_cp*del)     ! Derivative to second order approximation
      end if

      !> No linear conversion is required because get_K returns values for "linear Kp" = Mp,
      !> so kppc is already dMp/dP = Mpp

   End Function dKdT_Cal

End SubModule EoS_dKdTCalc