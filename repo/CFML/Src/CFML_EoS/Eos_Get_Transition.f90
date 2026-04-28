!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Transition
   implicit none

   Contains

   !!----
   !!---- FUNCTION GET_TRANSITION_PRESSURE
   !!----
   !!---- Returns the transition pressure at this T
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Function Get_Transition_Pressure(T, Eos) Result(Ptr)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: Ptr  ! The transition T at this P

      !---- Local Variables ----!
      real(kind=cp) :: sqroot

      !> Init
      ptr=0.0_cp

      !> Check for valid model number. If not valid, return with zero Tr
      if (EoS%itran < 1 .or. EoS%itran > N_TRANS_MODELS) return

      !> Now determine if in high-symmetry or low field
      select case(EoS%itran)
         case(1) ! Landau PV
            ptr=EoS%params(21)

         case(3) ! Landau PVT
            !ptr = (T-EoS%params(21))/EoS%params(22) original linear
            if (abs(EoS%params(23)) < tiny(0.0) ) then
               ptr = (T-EoS%params(21))/EoS%params(22)        ! linear
            else
               sqroot=EoS%params(22)*EoS%params(22)-4.0_cp*EoS%params(23)*(EoS%params(21)-T)
               if (sqroot > tiny(0.0) ) then
                  ptr=2.0_cp*(T-EoS%params(21))/(EoS%params(22)+sqrt(sqroot))        ! Viet/Muller formula for root
               else if(sqroot > -1.0*tiny(0.0) ) then
                  ptr=-2.0_cp*(T-EoS%params(21))/EoS%params(22)
               else
                  call set_error(1,' ')
                  write(Err_cfml%msg,'(a,f8.3,a)')'No real solution for Ptr at T = ',T,'K'
               end if
            end if

      end select

   End Function Get_Transition_Pressure

   !!----
   !!---- FUNCTION GET_TRANSITION_STRAIN
   !!----
   !!---- Returns the strain at P,T due to the transition, including any softening
   !!---- in the high-symm phase for Volume eos returns the volume strain term, for
   !!---- linear eos it returns the linear term!!
   !!---- Vs is defined relative to the 'bare' eos of the high phase (ie the high
   !!---- phase without transition effects)Returns the transition pressure at this T
   !!----
   !!---- Date: 16/02/2015
   !!
   Module Function Get_Transition_Strain(P, T, Eos) Result(VS)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P    ! Pressure
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: Vs   ! The volume strain

      !----Local Variables ----!
      real(kind=cp) :: Ttr , a ! transition temperature at this pressure

      !> init
      vs=0._cp

      !> Check for valid model number. If not valid, return with zero Vs
      if (EoS%itran < 1 .or. EoS%itran > N_TRANS_MODELS) return

      !> Now determine if in high-symmetry or low field
      if (Transition_phase(P,T,EoS)) then
         !> This section for being in the low field
         select case(EoS%itran)
            case(1) ! Landau PV
               vs=EoS%params(24)*abs(EoS%params(21)-P)**EoS%params(25)

            case(2) ! Landau TV
               vs=EoS%params(24)*abs(EoS%params(21)-T)**EoS%params(25)

            case(3) ! Landau PVT
               Ttr = Get_Transition_Temperature(P,EoS)
               a=EoS%params(24)
               vs=a*abs(Ttr-T)**EoS%params(25)            !abs function to handle highT being low sym
         end select

      else
         !> This section for being in the high field
         select case(EoS%itran)
            case(1) ! Landau PV
               vs=EoS%params(26)*abs(EoS%params(21)-P)**EoS%params(27)

            case(2) ! Landau TV
               vs=EoS%params(26)*abs(EoS%params(21)-T)**EoS%params(27)

            case(3) ! Landau PVT:  Note no da/dP for highP phase
               Ttr = Get_Transition_Temperature(P,EoS)
               vs=EoS%params(26)*abs(Ttr-T)**EoS%params(27)            !abs function to handle highT being low sym
         end select
      end if

   End Function Get_Transition_Strain

   !!----
   !!---- FUNCTION GET_TRANSITION_TEMPERATURE
   !!----
   !!---- Returns the transition temperature at this pressure
   !!----
   !!---- Date: 16/02/2015
   !!
   Module Function Get_Transition_Temperature(P, Eos) Result(Tr)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P    ! Pressure
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: Tr   ! The transition T at this P

      !---- Local Variables ----!

      !>init
      tr=0._cp

      !> Check for valid model number. If not valid, return with zero Tr
      if (eos%itran < 1 .or. eos%itran > N_TRANS_MODELS) return

      !> Now determine if in high-symmetry or low field
      select case(eos%itran)
         case(2) ! Landau TV
            Tr=eos%params(21)

         case(3) ! Landau PVT: with a curved phase boundary
            Tr = eos%params(21)+p*eos%params(22)+p*p*eos%params(23)
      end select

   End Function Get_Transition_Temperature

   !!----
   !!---- LOGICAL FUNCTION TRANSITION_PHASE
   !!----
   !!---- Returns .true. if P and T are in the low phase stability field
   !!---- and .false. (default) if in the high-symm field, or exactly on
   !!---- the boundary.
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Function Transition_Phase(P, T, Eos) Result(Ip)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P    ! Pressure
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      logical                    :: Ip

      !---- Local Variables ----!
      real(kind=cp) :: Ttr      ! transition temperature at this pressure

      !> default to 'high' phase for safety
      ip=.false.

      !> Check for valid model number. If not valid, return (with high phase indicated).
      if (eos%itran < 1 .or. eos%itran > N_TRANS_MODELS) return

      !> Test P and T against the Tr, set ip=.true. if in the low phase field for highT =high symm
      select case(eos%itran)
         case (1) ! Landau PV
            if (P < eos%params(21)) ip=.true.

         case (2) ! Landau TV
            if (T < eos%params(21)) ip=.true.

         case (3) ! Landau PVT
            !Ttr = eos%params(21)+p*eos%params(22)   changed from this on 18/12/2015
            Ttr = Get_Transition_Temperature(P,Eos)  ! general case
            if ( T < Ttr ) ip=.true.
      end select

      !> Now invert result if the lowT phase is high symm phase:
      if (eos%params(20) < 0) ip = .not. ip

   End Function Transition_Phase

End SubModule EoS_Get_Transition