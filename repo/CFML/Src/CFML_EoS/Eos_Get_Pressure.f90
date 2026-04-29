!!----
!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Pressure
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_PRESS_AXIS
   !!----
   !!---- Returns the value of pressure at input length or volume of principal axis
   !!---- ioes in unit cell in cell_eos
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Press_Axis(A, T, Cell_eos, Ieos)  Result(P)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: a,T       ! a is volume or linear value, as appropriate
      type(eos_cell_type),intent(in) :: cell_eos  !the eos for the cell axes
      integer,            intent(in) :: ieos      ! the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                  :: p

      !---- Local Variables ----!

      !>init
      p=0.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            p=get_pressure(a,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            p=get_pressure(a,t,cell_eos%eos(1))

         case(3)
            p=get_press_third(a,T,cell_eos,ieos)

         case(4)
            p=get_pressure(p,t,cell_eos%eos(cell_eos%unique))
      end select

   End Function Get_Press_Axis

   !!----
   !!---- FUNCTION GET_PRESS_CELL
   !!----
   !!---- Returns the value of pressure at input length or volume of axis in unit cell in cell_eos
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Press_Cell(A, T, Cell_eos, Axis) Result(P)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: a,t      !a is in length not length cubed
      type(eos_cell_type),intent(in) :: cell_eos !the eos for the cell axes
      type(axis_type),    intent(in) :: axis  ! The direction. if a cell axis then only axis%ieos is required
      real(kind=cp)                  :: p

      !---- Local Variables ----!

      !>init
      p=0.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            P=get_press_axis(a,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            P=get_press_general(a,T,cell_eos,axis)

      end select

   End Function Get_Press_Cell

   !!--++
   !!--++ FUNCTION GET_PRESS_GENERAL
   !!--++
   !!--++ Returns the value of pressure at input length of a general direction
   !!--++ (axis) in unit cell in cell_eos
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Press_General(A, T, Cell_Eos, Axis) Result(P)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: a,t      !a is in length not length cubed
      type(eos_cell_type),intent(in) :: cell_eos
      type(axis_type),    intent(in) :: axis
      real(kind=cp)                  :: p

      !---- Local Variables ----!
      real(kind=cp) :: del,delprev,step,tol,acalc
      integer       :: ic

      !> init
      p=0.0_cp

      del=0.1
      delprev=del
      step=-1.0_cp*del
      tol=0.0005_cp*a/get_mod_general(P,T,cell_eos,axis) !tolerance in V scaled by M to give 0.0005 error in P

      ic=0
      do while (ic <= 1000)
         ic=ic+1

         !> calc the ratio at the current p
         acalc=get_Volume_general(P,T,cell_eos,axis)
         del=acalc-a

         if (ic > 1)then                      ! need to get two calcs before adjusting step size and dirn
            if(abs(del) .lt. tol) exit

            if (del*delprev < 0._cp)then     ! over-stepped: reverse with half the step size
               step=-0.5_cp*step

            else                            ! same signs
               if (abs(del) > abs(delprev))then ! going the wrong direction
                  step=-1.0_cp*step
               end if
            end if
            if (abs(step) < 0.000001) exit  ! step in p got too small
         end if
         delprev=del
         p=p+step
      end do

   End Function Get_Press_General

   !!--++
   !!--++ FUNCTION GET_PRESS_THIRD
   !!--++
   !!--++ Returns the value of pressure at input length of a principal axis (ieos) in unit cell in cell_eos
   !!--++   when it must be calculated from other eos
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Press_Third(A, T, Cell_Eos, Ieos, Pguess) Result(P)
      !---- Arguments ----!
      real(kind=cp),         intent(in) :: a,T      ! v is volume or linear value, as appropriate
      type(eos_cell_type),   intent(in) :: cell_eos
      integer,               intent(in) :: ieos     ! the axis (1,2,3) or V (0) to be calculated
      real(kind=cp),optional,intent(in) :: pguess   !initial guess to P
      real(kind=cp)                     :: p

      !---- Local Variables ----!
      real(kind=cp) :: del,delprev,step,tol,vcalc
      integer       :: ic

      !> init
      p=0.0_cp
      if (present(pguess))p=pguess
      call clear_error()

      del=0.1
      delprev=del
      step=-1.0_cp*del

      tol=0.0005_cp*a/get_mod_third(P,T,cell_eos,ieos)   !tolerance in V scaled by M to give 0.0005 error in P

      ic=0
      do while (ic <= 1000)
         ic=ic+1

         ! calc the ratio at the current p
         vcalc=get_volume_third(P,T,cell_eos,ieos)
         del=vcalc-a

         if (ic > 1)then                      ! need to get two calcs before adjusting step size and dirn
            if (abs(del) < tol)return

            if (del*delprev < 0._cp)then     ! over-stepped: reverse with half the step size
               step=-0.5_cp*step

            else                            ! same signs
               if (abs(del) > abs(delprev))then ! going the wrong direction
                  step=-1.0_cp*step
               end if
            end if
            if (abs(step) < 0.000001)return  ! step in p got too small
         end if
         delprev=del
         p=p+step
      end do

   End Function Get_Press_Third

   !!----
   !!---- FUNCTION GET_PRESSURE
   !!----
   !!---- Returns value of pressure (P) at (V,T) for EoS Model defined in EoSPar variable
   !!---- In any error occurs then the final value is set to 0.0
   !!----
   !!--.. Note:
   !!--.. Validated code against Eosfit v5.2 for non-thermal EoS: RJA 22/02/2013
   !!--.. Changed 28/03/2013 RJA to use strain function to get f from Vo/V.
   !!--.. This will ensure better consistency
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Function Get_Pressure(V, T, Eos) Result(P)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: V    ! Volume
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: p

      !---- Local Variables ----!
      logical                           :: first
      integer                           :: i
      real(kind=cp)                     :: K0,Kp,kpp,vv0,x,u,vol,plast,vs,ptr,vtr,difp,volp
      real(kind=cp)                     :: a,b,c,f
      real(kind=cp),dimension(N_EOSPAR) :: ev
      real(kind=cp),dimension(3)        :: abc      ! for Tait parameters
      real(kind=cp),dimension(3,3)      :: apl      ! for APL parameters

      !> Init
      p=0.0_cp
      vol=v             ! needed to allow for transition strain: vol is the V of the bare phase
      vs=0.0_cp
      plast=0.0_cp
      first=.true.

      !> If PTV table, go directly
      if (EoS%imodel == -1) then
         p=get_props_ptvtable(0.0,t,v,EoS,'P')     ! get_props_ptvtable works in length if linear
         return
      end if

      !> copy Eos parameters to local
      ev= EoS_to_Vec(EoS)         ! Volume or linear case is covered

      !> These parameters depend only on T, not P or V
      k0=Get_K0_T(T,EoS)              ! Handles thermal pressure case, returns K0 or M0
      if (EoS%linear) k0=k0/3.0_cp

      kp=Get_Kp0_T(T,EoS)
      if (EoS%linear) kp=kp/3.0_cp
      kpp=Get_Kpp0_T(T,EoS)
      if (EoS%linear) kpp=kpp/3.0_cp

      !> Start increment loop to get transition factor
      do
         !> Thermal case
         select case (EoS%itherm)
            case (0,6,7,8,9) ! No thermal case, or  thermal pressure which uses params at Tref
               vv0=vol/EoS%params(1)      !vv0 is now V/V0 or a/a0

            case (1:5)
               vv0=vol/Get_V0_T(T,EoS)    ! In the case of Phase transition, Get_V0_T always returns V0 of high phase at this T: This is correct!
         end select

         f=strain(vv0,EoS)                ! use strain to get finite strain from v/v0 or a/a0
         if (err_cfml%flag) then
            call set_error(1,trim(err_cfml%msg)//' called from Get_Pressure')
            exit
         end if

         !> Using volume dimensions
         vv0=1.0_cp/vv0                    ! vv0 now V0/V for easy of use in rest of subprogram
         if (EoS%linear) vv0=vv0**(3.0_cp)

         select case (EoS%imodel)
            case (1) ! Murnaghan
               P=K0/Kp*(vv0**Kp - 1.0_cp)

            case (2) ! Birch-Murnaghan
               a=f*(1.0_cp+2.0_cp*f)**(2.5_cp)     ! changed expressions to use only f 04/04/2013 RJA
               b=0.0_cp
               c=0.0_cp
               if (EoS%iorder > 2)  b=1.5_cp*(Kp-4.0_cp)
               if (EoS%iorder == 4) c = 1.5_cp*(K0*Kpp + (Kp-4.0_cp)*(Kp-3.0_cp)+35.0_cp/9.0_cp)
               p=3.0_cp*K0*a*(1.0_cp + b*f + c*f*f)

            case (3) ! Vinet
               x=vv0**(-1.0_cp/3.0_cp)
               u=1.0_cp -x
               p=3.0_cp*K0*u/x/x*exp(1.5_cp*(Kp-1.0_cp)*u)

            case (4) ! Natural
               b=0.0_cp
               c=0.0_cp
               if (EoS%iorder > 2)  b=1.5_cp*(Kp-2.0_cp)
               if (EoS%iorder == 4) c =1.5_cp*(K0*Kpp + 1.0_cp +(Kp-2.0_cp)+(Kp-2.0_cp)**2.0_cp)
               p=3.0_cp*vv0*K0*f*(1.0_cp + b*f + c*f*f)

            case (5) ! Tait
               abc= get_tait(t,EoS)
               vv0=1.0_cp/vv0     ! back to vv0=v/v0
               p=(((vv0 +abc(1) -1.0_cp)/abc(1))**(-1.0_cp/abc(3)) - 1.0_cp)/abc(2)

            case(6) ! APL
               apl= Get_APL(1.0_cp/VV0,vv0*vol,K0,Kp,Kpp,ev(5),EoS%iorder)
               p=3.0_cp*k0*product(apl(1,:))

            case(7) !Kumar
               a=kp+1.0_cp
               vv0=1.0_cp/vv0     ! back to vv0=v/v0
               p=k0/a * (exp(a*(1.0_cp-vv0))-1)

         end select

         !> Handle pthermal EoS
         if (EoS%pthermaleos .and. EoS%imodel /= 0) p=p+pthermal(Vol,T,EoS)        !Vol is a or V

         !> Iteration required if there is a phase transition
         if (EoS%itran == 0) exit

         !> Determine and set the transition pressure at this T
         if (first) then
            i=0
            if (EoS%itran == 1)then
               ptr=ev(21)       !PV transition, so Ptr is the eos%param(21)

            else
               !> ptr=(T-ev(21))/ev(22)
               ptr=get_transition_pressure(T,EoS)
            end if
            vtr=huge(0.0_cp)
         end if

         i=i+1

         if (abs(plast-p) < 0.0001) exit         ! iteration has converged
         if (i > 1000)then
            call set_error(1,'No convergence in Transition loop in Get_Pressure')
            return
         endif

         !> Here if not converged:               ! works provided phase boundary is almost linear
         if (first .or. abs(plast-p) < difp) then
            vs=get_transition_strain(p,T,EoS)
            volp=vol
            vol=(v/(1.0+vs) +vol)/2.0
            difp=abs(plast-p)
            plast=p
         else            ! shift was too big
            vol=(vol+volp)/2.0
         end if

         first=.false.
      end do

   End Function Get_Pressure

   !!----
   !!---- FUNCTION GET_PRESSURE_ESD
   !!----
   !!---- Gets the partial derivatives of P with respect to the EoS
   !!---- parameters at a given v,t point in order to calculate from vcv
   !!---- matrix the esd(p)
   !!----
   !!--.. Rewritten 09/04/2013 RJA to use deriv_partial
   !!----
   !!---- Update: 17/07/2015
   !!----
   !!
   Module Function Get_Pressure_Esd(V, T, Eos) Result(Esd)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: V    ! Volume
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: esd

      !---- Local Variables ----!
      integer                           :: i,j
      real(kind=cp),dimension(n_eospar) :: td
      real(kind=cp)                     :: vol,temp
      type(Eos_Type)                    :: E  ! Eos Parameter local copy

      !> Init
      esd=0.0_cp

      !> local copies
      vol=v
      temp=t
      e=eos
      td= Deriv_Partial_P(vol,temp,e,xtype=0,calc='ALL')  ! gets all derivatives dP/dparam in array td

      !> esd
      do i=1,N_EOSPAR
         do j=1,N_EOSPAR
            esd=esd+eos%vcv(i,j)*td(i)*td(j)
         end do
      end do

      !> Final
      esd=sqrt(esd)

   End Function Get_Pressure_Esd

   !!--++
   !!--++ FUNCTION GET_PRESSURE_K
   !!--++   returns the pressure for a known bulk modulus K and temperature T
   !!--++
   !!--++ Date: 16/03/2017
   !!
   Module Function Get_Pressure_K(K, T, Eos, Itype, Pest) Result(P)
      !---- Arguments ----!
      real(kind=cp),           intent(in) :: K       ! Bulk modulus
      real(kind=cp),           intent(in) :: T       ! Temperature
      type(Eos_Type),          intent(in) :: EoS     ! Eos Parameter
      integer,                 intent(in) :: itype
      real(kind=cp), optional, intent(in) :: Pest    ! approx pressure: needed if transitions
      real(kind=cp)                       :: p

      !---- Local Variables ----!
      integer            :: ic,irev
      real(kind=cp)      :: temp,ktarget,kcalc,ktol,step,del,delprev,pprev,v0,step_prev
      real(kind=cp)      :: pmindiff
      real(kind=cp)      :: k0,steptest
      !character(len=255) :: ltext

      !> init
      ic=0
      irev=0      ! reversal counter
      temp=t
      ktarget=k

      !> initial guess...use Pest if present
      if (present(Pest)) then
         p=Pest

      else
         !> Murngahan guess based on T=Tref
         V0=get_volume(0.0_cp,eos%Tref,eos)
         K0=k_cal(V0,eos%Tref,eos)
         p=(ktarget-K0)/eos%params(3)
      end if

      pmindiff=p                  !> record p of best fit
      step=eos%params(2)/100.0    !> make initial step 0.01 K0 of first phase: not critical
      ktol=0.001*ktarget

      do
          !> counter
          ic=ic+1

          kcalc=get_property_x(p,t,eos,itype)
          del=ktarget-kcalc
          if (ic > 1) then                      ! need to get two calcs before adjusting step size and dirn
             if (abs(del) < ktol .or. abs(p-pprev) < 0.0005*P) return
             if (abs(step) < 0.0001) return                             ! step size getting down to precision

             if (abs(del) < abs(delprev) ) pmindiff=p
             if (abs(del-delprev) > 10.0*tiny(1.0_cp) ) then
                steptest= del/(del-delprev) *(pprev-p)  ! linear approx: newstep= dx/dy * (ytarget-yc)
                if (steptest < step) then
                   step=steptest
                else
                   step=sign(step,steptest)
                end if
             end if
          end if

          pprev=p          ! this x stored
          delprev=del      ! ytarget-yc stored
          if (eos%itran > 0) then
             !> trap jumping back and forth across phase boundary
             if (transition_phase(P,Temp,eos) .neqv. transition_phase(P+step,Temp,eos) ) then
                step=0.9*Get_Transition_Pressure(Temp,Eos)-p
                !write(ltext,'(''  Step limited by transition pressure of '',f7.4)')Get_Transition_Pressure(Temp,Eos)
                !if (idmode == 2)call write_out(ltext)
             end if
          end if

          if (ic > 2 .and. abs(step) > abs(step_prev) ) then
             step=sign(0.9*step_prev,step)
          end if

          !write(ltext,'('' P ='',f7.4,'' Step = '',f7.4,''  Kcalc ='',f10.5,'' Ktarget = '',f10.5,'' Ktol ='',f10.5)')p,step,kcalc,ktarget,ktol
          !if (idmode == 2)call write_out(ltext)

          p=p+step
          step_prev=step

          if (ic > 100) then
             !if(idmode == 2)call write_out('********Convergence failure in get_pressure_K')
             !write(ltext,'('' P ='',f7.4,'' Step = '',f7.4,''  Kcalc ='',f10.5,'' Ktarget = '',f10.5,'' Ktol ='',f10.5)')p,step,kcalc,ktarget,ktol

             !if(idmode == 2)call write_out(ltext)
             !if(idmode == 2)call write_out_blank(1)
             call set_error(-1,'Convergence failure in get_pressure_K')
             if (eos%itran > 0) then
                err_cfml%msg=trim(err_cfml%msg)//' probably due to transition giving max in K'
             end if
             p=pmindiff
             return
          end if
      end do

   End Function Get_Pressure_K

   !!----
   !!---- FUNCTION GET_PRESSURE_X
   !!----
   !!---- Gets ...
   !!----
   !!---- Date: 16/03/2017
   !!
   Module Function Get_Pressure_X(X, T, Eos, Xtype, Pest) Result(P)
      !---- Arguments ----!
      real(kind=cp),           intent(in) :: x       ! V or Bulk modulus
      real(kind=cp),           intent(in) :: T       ! Temperature
      type(Eos_Type),          intent(in) :: EoS     ! Eos Parameter
      integer,                 intent(in) :: Xtype   ! =0 when X=V, =1 for X=K (isothermal)  =2 for adiabatic
      real(kind=cp), optional, intent(in) :: Pest    ! Approx pressure: needed if transitions
      real(kind=cp)                       :: p

      !---- Local Variables ----!
      integer :: itype

      !> Init
      p=0.0_cp

      itype=xtype
      if (itype < 0 .or. itype > N_DATA_TYPES) itype=0               ! default

      select case(itype) ! Get calc pressure: depends on data type
         case(0)                 ! X is V
            p=get_pressure(x,t,eos)

         case(1)                 ! X is KT
            p=get_pressure_K(x,t,eos,itype,Pest)

         case(2)                 ! X is Ks:
            p=get_pressure_K(x,t,eos,itype,Pest)

         case default
            p=0.0_cp
      end select

   End Function Get_Pressure_X

End SubModule EoS_Get_Pressure