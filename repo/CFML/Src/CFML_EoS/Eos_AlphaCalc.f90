!!----
!!----
!!----
SubModule (CFML_EoS) Eos_AlphaCalc
   implicit none

   Contains

   !!----
   !!---- ALPHA_CAL
   !!----   Calculate the alpha parameter in thermal case
   !!----   For linear case alpha is correct as 1/a da/dT
   !!----
   !!---- 17/07/2015
   !!
   Module Function Alpha_Cal(P, T, EoS, DeltaT) Result(Alpha)
      !---- Arguments ----!
      real(kind=cp),            intent(in) :: P       ! Pressure
      real(kind=cp),            intent(in) :: T       ! Temperature
      type(Eos_Type),           intent(in) :: EoS     ! Eos Parameter
      real(kind=cp),  optional, intent(in) :: DeltaT  ! Delta T value
      real(kind=cp)                        :: alpha

      !---- Local Variables ----!
      integer                        :: j
      real(kind=cp)                  :: del, tt, delmin, tlimit, tr, alphaest, vol
      real(kind=cp), dimension(-2:2) :: v  ! array for calc v values

      !> Init
      alpha=0.0_cp
      Tlimit=0.0_cp

      !> PTV table
      if (eos%imodel == -1) then
         alpha=get_props_ptvtable(p, t, 0.0, eos, 'AL')
         return
      end if

      !>No thermal model
      if (eos%itherm < 0 .or. eos%itherm > N_THERM_MODELS) return

      !> Oscillator based eos (itherm=7,8), with no phase transition
      !> Alpha calculated direct from Cv based on equation 2.83 of Anderson (1995)
      !> Cv is returned in J/mol/K by direct algebra expression, and using R=8.314
      if (eos%Osc_allowed .and. eos%itran == 0) then
         vol=get_volume(p,t,eos)
         alpha=get_grun_th(p,t,eos)*get_cv(p,t,eos)/k_cal(vol,t,eos,p=p)/vol

         !> scaling
         alpha=alpha*EPThermal_factor(eos)
         return
      end if

      !> Need to trap numerical problems with Kroll, Salje, Pthermal at low T
      select case(eos%itherm)
         case(0) ! no thermal parameters
            return

         case(4:6) ! Kroll, Salje, and HP Pthermal: For T < 0.05T(Einstein) alpha=0. Same for Salje but test is 0.05Tsat
            if (t < 0.05_cp*eos%params(11) ) return
      end select

      !> Numerical solutions: set step size (not used in MGD)
      del =abs(0.001_cp/eos%params(10))      ! Set step in T to get about 0.1% shift in V
      if (del > 80.0_cp) del=80.0_cp         ! otherwise for small alpha, del is too big and alpha is inaccurate
      if (present(deltaT)) del=deltaT

      select case(eos%itherm)           ! adjustment of step size
         !> Fei, HP98
         case(2,3)
            !> T ok, but do not step down into invalid region
            if (abs(eos%params(12)) > tiny(0.0_cp) ) then
               delmin=abs(t-tlimit)/2.1_cp
               if (del > delmin) del=delmin
            end if

         !> Kroll, Salje, HP and linear Thermal pressure
         case(4:6)
            delmin=(t-0.025_cp*eos%params(11))/2.0_cp     ! do not allow step in to area where alpha=0
            if (del > delmin) del=delmin                  ! ensures T at all steps is positive

         !> MGD Pthermal and q-compromise
         case(7:8)
            ! so no alpha available for estimation: changed from T+100 to T-100 to avoid going
            ! into area where large V invalid
            alphaest=(get_volume(p,t,eos)-get_volume(p,t-100._cp,eos))/get_volume(p,t-50._cp,eos)/100._cp
            del=abs(0.001_cp/alphaest)
            delmin=(t-0.025_cp*eos%params(11))/2.0_cp     ! do not allow step in to area where alpha=0
            if (del > delmin) del=delmin                  ! ensures T at all steps is positive

            ! now stop the del taking us into illegal area
            tlimit=t+2.0*del
            do
               ! search for positive K at this P
               if (get_K(p,tlimit,eos) > 0.0_cp .and. (.not. err_cfml%flag)) exit
               call clear_error()
               tlimit=tlimit-0.1_cp*(tlimit-t)
               if (tlimit < t) exit                    ! should never happen because P,T is valid
            end do
            del=0.4*abs(tlimit-t)
      end select

      !> Stop calculation going across a phase boundary
      if (eos%itran > 0) then
         Tr=get_transition_temperature(p,eos)
         if (transition_phase(P,T,eos) .neqv. transition_phase(P,T+2.0*del,eos)) del=abs(T-Tr)/2.1_cp
         if (transition_phase(P,T,eos) .neqv. transition_phase(P,T-2.0*del,eos)) del=abs(T-Tr)/2.1_cp
         if (del < 1.0) del=1.0
      end if

      !> Do the numerical solution
      do j=-2,2,1
         tt=t+real(j)*del              ! apply shift to temp
         v(j)=get_volume(p,tt,eos)     ! calc resulting V
      end do

      alpha=(v(-2)+8.0_cp*(v(1)-v(-1))-v(2))/(12.0_cp*del)/v(0)     ! Derivative to second order approximation

   End Function Alpha_Cal

   !!--++
   !!--++ FUNCTION GET_ALPHA_AXIS
   !!--++
   !!--++ Returns the value of alpha of principal axis (ieos) in unit cell
   !!--++ in cell_eos at P,T
   !!--++ Call this Function directly when the calling routine  knows that the direction is a principal axis
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Alpha_Axis(P, T, Cell_EoS, Ieos) Result(Alpha)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: P, T
      type(eos_cell_type),intent(in)  :: Cell_EoS
      integer,            intent(in)  :: Ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                   :: Alpha     !returned thermal expansion

      !---- Local Variables ----!

      !> init
      Alpha=0.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            Alpha=alpha_cal(p,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            Alpha=alpha_cal(p,t,cell_eos%eos(1))

         case(3)
            Alpha=get_alpha_third(p,T,cell_eos,ieos)

         case(4)
            alpha=alpha_cal(p,t,cell_eos%eos(cell_eos%unique))
      end select

   End Function Get_Alpha_Axis

   !!----
   !!---- FUNCTION GET_ALPHA_CELL
   !!----
   !!---- Returns the value of alpha of any axis in unit cell in cell_eos at P,T
   !!----
   !!---- Call this Function when the calling routine does not know if the direction
   !!---- is a principal axis or not.
   !!---- If a principal direction is requested, only axis%ieos is required
   !!---- axis%v and axis%atype only used if axis%ieos=-2
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Alpha_Cell(P, T, Cell_EoS, Axis) Result(Alpha)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: alpha !returned thermal expansion

      !---- Local Variables ----!

      !> init
      alpha=0.0_cp

      select case(axis%ieos)
         case(0:6)
            !> principal direction for which eos exists, or can be calculated
            alpha=get_alpha_axis(p,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            alpha=get_alpha_general(p,T,cell_eos,axis)
      end select

   End Function Get_Alpha_Cell

   !!--++
   !!--++ FUNCTION GET_ALPHA_GENERAL
   !!--++
   !!--++ Returns the value of alpha of any axis in unit cell in cell_eos at P,T
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Alpha_General(P, T, Cell_Eos, Axis) Result(Alpha)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: alpha

      !---- Local Variables ----!
      integer         :: i
      real(kind=cp)   :: tstep,tcal

      !> for spline
      integer, parameter             :: NSTEP=21   !must be odd
      integer                        :: imid
      real(kind=cp), dimension(NSTEP):: x,y,dy !,d2y

      tstep=20.
      tcal=t-int(NSTEP/2)*tstep
      do i=1,NSTEP
         x(i)=tcal
         y(i)=get_Volume_general(P,Tcal,cell_eos,axis)
         tcal=tcal+tstep
      end do

      !call Second_Derivative(x, y, NSTEP, d2y)
      dy= First_Derivative(x, y, NSTEP)

      imid=int(NSTEP/2) + 1
      alpha=dy(imid)/y(imid)

   End Function Get_Alpha_General

   !!--++
   !!--++ FUNCTION GET_ALPHA_THIRD
   !!--++
   !!--++ Returns the value of alpha of a principal axis ieos in unit cell in cell_eos at P,T
   !!--++ when it can be calculated from others
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Alpha_Third(P, T, Cell_Eos, Ieos) Result(Alpha)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: cell_eos
      integer,            intent(in) :: ieos     ! the alpha of the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                  :: alpha

      !---- Local Variables ----!
      integer         :: i
      real(kind=cp)   :: alpha_ang

      !> init
      alpha=0._cp

      !> safety check: if mono or triclinic, should only be called if angle poly used
      if (U_case(cell_eos%system(1:4)) == 'TRIC' .or. U_case(cell_eos%system(1:3)) == 'MONO')then
         if (cell_eos%eosang%iangle == 0) then
            call set_error(1,'Get_Alpha_Third called for mono or triclinic, without angle poly set')
         end if
      end if

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC','MONO','ORTH')
            alpha_ang=Get_Angle_Volfactor_Deriv(P,T,cell_eos,'T')/Get_Angle_Volfactor(P,T,cell_eos)  !1/A dA/dT

            select case(ieos)
               case(0)
                  alpha=Alpha_Cal(P,T,cell_eos%eos(1))+ Alpha_Cal(P,T,cell_eos%eos(2)) + &
                        Alpha_Cal(P,T,cell_eos%eos(3)) + alpha_ang

               case default
                  alpha=Alpha_Cal(P,T,cell_eos%eos(0))
                  do i=1,3
                     if (i == ieos)cycle
                     alpha=alpha- Alpha_Cal(P,T,cell_eos%eos(i))
                  end do
                  alpha=alpha-alpha_ang
            end select

         case('TRIG','HEXA','TETR')
            select case(ieos)
               case(0)     ! calc volumes from a and c
                  alpha= 2.0_cp*Alpha_Cal(P,T,cell_eos%eos(1))+Alpha_Cal(P,T,cell_eos%eos(3))

               case(1)     ! a from V and c
                  alpha= (Alpha_Cal(P,T,cell_eos%eos(0))-Alpha_Cal(P,T,cell_eos%eos(3)))/2.0_cp

               case(3)     ! c from a and V
                  alpha=  Alpha_Cal(P,T,cell_eos%eos(0))-2.0_cp*Alpha_Cal(P,T,cell_eos%eos(1))
            end select

         case('CUBI','ISOT')
            select case(ieos)
               case(0)     ! calc volume from a
                  alpha=Alpha_Cal(P,T,cell_eos%eos(1))*3.0_cp

               case(1,2,3)     ! a,b, or c from V
                  alpha=Alpha_Cal(P,T,cell_eos%eos(0))/3.0_cp
            end select
      end select

   End Function Get_Alpha_Third

End SubModule Eos_AlphaCalc