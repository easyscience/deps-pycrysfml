!!----
!!----
!!----
SubModule (CFML_EoS) Eos_Get_Volume
   implicit none

   Contains

   !!--++
   !!--++ FUNCTION GET_V0_AXIS
   !!--++
   !!--++ Returns the value of volume or length of principal axis (ieos) in unit cell
   !!--++ in cell_eos at Pref,Tref
   !!--++
   !!--++ Call this Function directly when the calling routine  knows that the direction
   !!--++ is a principal axis
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_V0_Axis(Cell_eos, Ieos) result(L)
      !---- Arguments ----!
      type(eos_cell_type), intent(in)  :: cell_eos
      integer,             intent(in)  :: ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                    :: L         !returned length or volume

      !---- Local Variables ----!

      !> init
      l=10.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            L=get_volume(cell_eos%eosc%pref,cell_eos%eosc%tref,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            L=get_volume(cell_eos%eosc%pref,cell_eos%eosc%tref,cell_eos%eos(1))

         case(3)
            L=get_volume_third(cell_eos%eosc%pref,cell_eos%eosc%tref,cell_eos,ieos)
      end select

   End Function Get_V0_Axis

   !!--++
   !!--++ FUNCTION GET_V0_CELL
   !!--++
   !!--++ Returns the value of volume or length of any axis in unit cell in cell_eos
   !!--++ at Pref,Tref
   !!--++ Call this Function when the calling routine does not know if the direction
   !!--++ is a principal axis or not
   !!--++ If a principal direction is requested, only axis%ieos is required
   !!--++ axis%v and axis%atype only used if axis%ieos=-2
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_V0_Cell(Cell_eos, Axis) result(L)
      !---- Arguments ----!
      type(eos_cell_type), intent(in)  :: cell_eos
      type(axis_type),     intent(in)  :: axis
      real(kind=cp)                    :: L !returned length or volume

      !---- Local Variables ----!

      !> init
      l=10.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            L=get_v0_axis(cell_eos,axis%ieos)

         case(-2)   !general direction
            L=get_volume_general(cell_eos%eosc%pref,cell_eos%eosc%tref,cell_eos,axis)
      end select

   End Function Get_V0_Cell

   !!--++
   !!--++ FUNCTION Get_V0_T
   !!--++
   !!--++ PRIVATE
   !!--++ Returns the volume at P=0 and T=T from V0 and Thermal expansion
   !!--++ Except for Pthermal, for which it returns V at P=0, T=Tref
   !!--++ It calculates V0 only for thermal expansion, not including transition effects!
   !!--++ Therfore this must remain PRIVATE
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Function Get_V0_T(T, EoS) Result(V)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: T        ! Temperature
      type(Eos_Type), intent(in) :: EoS   ! Eos Parameter
      real(kind=cp)              :: V

      !---- Local Variables ----!
      real(kind=cp)                      :: kp,AK
      real(kind=cp)                      :: Tref,A,B,C,Tn,tt
      real(kind=cp)                      :: delt,delt2
      real(kind=cp), dimension(n_eospar) :: ev

      !> Init
      Tref=EoS%tref

      !> Local copy EoS
      ev= EoS_to_Vec(EoS) ! Volume or linear case is covered
                                 ! all equations written for volume
      delt=T-Tref
      select case(EoS%itherm)
         case(0)
            v=ev(1)                 ! no thermal eos: V is V0

         case(1)                    ! Berman, works at all T
            V=ev(1)*(1.0_cp +ev(10)*delt+0.5_cp*ev(11)*delt*delt)

         case(2)                    ! Fei,
            TT=T
            delt2=t*t-tref*tref
            if (tt < tiny(0._cp)) tt=0.001        ! to prevent divide by zero

            if (abs(ev(12)) > tiny(0._cp)) then
               V=ev(1)*exp(ev(10)*delt + 0.5_cp*ev(11)*delt2 - ev(12)*(1.0_cp/TT - 1.0_cp/Tref))
            else
               V=ev(1)*exp(ev(10)*delt + 0.5_cp*ev(11)*delt2)    ! to protect from divide by zero when alpha2=0 and T=0
            end if

         case(3)                    ! HP 1998 modified
            tt=t
            if (t < tiny(0._cp)) tt=tiny(0._cp)      ! prevents sqrt(-ve T)
            V=ev(1)*(1.0_cp + ev(10)*(tt-tref) - 2.0_cp*(10.0_cp*ev(10)+ev(11))*(sqrt(tt) -sqrt(tref)))

         case(4)                    ! Holland-Powell 2011 in the Kroll form
            !>>>>>kp=ev(3)   : version before 11/11/2016
            if (EoS%icross == 2) then
               kp=ev(8)
            else
               kp=ev(3)
            end if
            if(abs(kp-1) < 0.0001 .or. abs(kp/(kp+2.0_cp)) < 0.0001)then
                V=ev(1)                             ! In these cases algebra shows V=V0
            else
                Tn= ev(11)/Tref                        ! theta/Tref
                C=Tn*Tn*exp(Tn)/(exp(tn)-1.0_cp)**2.0_cp
                B=-1.0/kp/(kp+2.0_cp)
                if (t > 0.05_cp*ev(11)) then                               ! to avoid numerical problems at T=0
                   A=ev(10)*ev(11)/C *(1.0_cp/(exp(ev(11)/T)-1.0_cp) - 1.0_cp/(exp(Tn)-1.0_cp) )
                else
                   A=ev(10)*ev(11)/C *(-1.0_cp/(exp(Tn)-1.0_cp) )          ! because when T=0 then 1.0/exp(Tein/T) = 0
                end if

                !  V=ev(1)*(-1.0_cp*kp + (1.0_cp+kp)*(1.0_cp - kp*(kp+2.0_cp)*A/(kp+1.0_cp))**B)
                AK=1.0_cp - kp*(kp+2.0_cp)*A/(kp+1.0_cp)
                if (AK < tiny(0._cp))then
                   V=ev(1)       ! for safe return
                   call set_error(1,'T exceeds valid limit for Kroll expansion in get_V0_T')
                else
                   V=ev(1)*(-1.0_cp*kp + (1.0_cp+kp)*AK**B)
                end if
            end if

         case(5)                    ! Salje, Tref fixed at zero
            A=T/ev(11)
            if (A < 0.001) then
               V=ev(1)                 ! ultra-low T: coth(theta_sat/T)=1.0
            else
               A=1.0_cp/tanh(1.0_cp/A) ! the coth(theta_sat/T)
               V=(ev(1)**(1.0_cp/3.0_cp) + ev(10)*ev(11)*(A-1.0_cp))**3.0_cp
            end if

         case(6:9)
            v=ev(1)         ! Pthermal needs V0 at Tref
      end select

      !> Linear
      if (EoS%linear) v=v**(1.0_cp/3.0_cp)

   End Function Get_V0_T

   !!----
   !!---- FUNCTION GET_VOLUME
   !!----
   !!---- Find volume from EoS at given P and T
   !!----
   !!---- Uses a spline to solve for volume if no analytical approach available
   !!---- Written 3/2019 RJA
   !!---- Under test
   !!----
   !!---- Date: 03/02/2021
   !!
   Module Function Get_Volume(P, T, Eos) Result(V)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P    ! Pressure
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: V

      !---- Local Variables ----!
      real(kind=cp), parameter          :: PREC=0.000001_cp  !precision to find V.

      integer,parameter                 :: nstep=30
      integer                           :: i,ic
      real(kind=cp),dimension(nstep)    :: x,y, d2y

      type(Eos_Type)                    :: EoSc  ! Eos copy
      real(kind=cp)                     :: V0,K0,Kp,strain,vfactor !,k
      real(kind=cp)                     :: Vol, vstep, delp_prev,delp,v_prev,a,logterm
      real(kind=cp),dimension(N_EOSPAR) :: ev
      real(kind=cp),dimension(3)        :: abc          ! Tait parameters
      real(kind=cp)                     :: pa           ! pa=p-pth

      logical                           :: reverse


      !> Init
      v=0.0_cp
      strain=0.0_cp   ! strain from transition: linear or volume to match eos type
      pa=p            ! local copy p

      !> If PTV table, go directly
      if (EoS%imodel == -1) then
         v=get_props_ptvtable(pa,t,0.0,EoS,'V')     ! get_props_ptvtable returns length if linear
         return
      end if

      !> Local copy EoS
      ev= EoS_to_Vec(EoS) ! Volume or linear case is covered

      !> Set appropriate V0, and adjust for thermal pressure:
      select case (EoS%itherm)
         case (0)                                   ! 0=no thermal,
            v0=ev(1)                                  ! v0 is volume eos, (a0)^3 for linear

         case (1:5)
            v0=get_v0_t(t,EoS)                     ! returns a0 for linear
            if (EoS%linear) v0=v0**3.0_cp

         case (6)                                     ! HP or linear thermal pressure
            v0=ev(1)
            pa=p-pthermal(0.0,t,EoS)               ! adjust pressure to isothermal pressure for murn and tait estimates

         case(7:9)                                    ! MGD - do a guess on basis parallel isochors of pa
            v0=ev(1)
            pa=p - EoS%params(2)*(t-EoS%tref)/100000.         ! have to guess an alpha because that requires V !!!
      end select

      !> set K0, for various purposes
      k0=Get_K0_T(T,EoS)              ! Handles thermal pressure case, returns K0 or M0
      if (EoS%linear) k0=k0/3.0_cp

      kp=Get_Kp0_T(T,EoS)
      if (EoS%linear) kp=kp/3.0_cp

      !> Get the volume strain due to transition: only a function of P,T NOT V!!
      if (EoS%itran > 0) then
         strain=get_transition_strain(P,T,EoS)     ! returns the linear or volume strain as appropriate
      end if

      !> If there is no eos model, we are finished because V0 is the V at this T
      if (EoS%imodel == 0) then
         if (EoS%linear) v0=v0**(1.0_cp/3.0_cp)
         v=v0*(1.0_cp + strain)
         return
      end if

      !> Analytic solution for Murnaghan:  use this for first guess for other EoS except Tait
      vfactor=(1.0_cp + kp*pa/k0)
      if (vfactor < 0.0)then
         v=v0        ! safe value for when 1+kp*pa/k0 is negative
      else
         v=v0*(vfactor)**(-1.0_cp/kp)
      end if

      !> Cannot do the following if MGD pthermal
      if (EoS%itherm /=7  .and. EoS%itherm /=8 .and. EoS%itherm /=9) then
         if (EoS%imodel ==1) then
            !> Exact solution for Murnaghan
            if (EoS%linear) v=v**(1.0_cp/3.0_cp)
            if (EoS%itran > 0) v=v*(1.0_cp + strain)     ! apply transition strain (already converted if linear)
            !if (vfactor < 0.0 .and. kp > 0.0) then
            if (vfactor < 0.0) then
               call set_error(1,'Pressure < -K0/Kp: yields meaningless volumes for Murnaghhan EoS')
            end if
            return
         end if

         !> Analytic solution for Tait
         if (EoS%imodel ==5) then
            abc= get_tait(t,EoS)                     ! get_tait returns volume-like parameters even for linear
            if (abc(2)*pa < -0.999999_cp) then
               call set_error(1,'Pressure yields infinite volume for Tait Eos')
               v=9999.0        ! safe value return
            else
               v=v0*(1.0_cp-abc(1)*(1.0_cp-(1.0_cp + abc(2)*pa)**(-1.0_cp*abc(3))))
               if (EoS%linear) v=v**(1.0_cp/3.0_cp)
               if (EoS%itran > 0) v=v*(1.0_cp + strain)     ! apply transition strain (already converted if linear)
            end if
            return
         end if

         !> Analytic solution for Kumar
         if (EoS%imodel ==7) then
             a=kp+1.0_cp
             logterm=a*pa/k0 +1.0_cp    !This is for safety: we should have checked with physical check
             if (logterm < tiny(0._cp)) then
                call set_error(1,' ')
                return
            else
               v=v0*(1.0_cp - log(logterm)/a)
               if (EoS%linear) v=v**(1.0_cp/3.0_cp)
               if (EoS%itran > 0) v=v*(1.0_cp + strain)     ! apply transition strain (already converted if linear)
            end if
            return
         end if
      end if

      !> Find iterative solution for the rest of functions: get_pressure includes the thermal pressure term
      !> But if there is a transition, we only want the P/V for the bare high-symm phase without softening

      !From here work with Vol, starting from Murnaghan estimate
      vol=v
      if (EoS%linear) vol=vol**(1.0_cp/3.0_cp)
      eosc=EoS        ! copy
      eosc%itran=0       ! turn off transition

      !> initial simple hunt
      delp_prev=huge(0._cp)
      ic = 0
      reverse=.false.
      Vstep=eosc%params(1)/100._cp
      do
         ic=ic+1
         if (ic > 1000)then
            call set_error(1,' *****No solution found in get_volume after 1000 cycles')
            return
         end if

         call clear_error()                   ! have to clear the previous errors, otherwise get_pressure will return 0
         delp=p-get_pressure(Vol,T,eosc)
         if (abs(delp) < 0.000001_cp)then  ! hit correct vol by accident. Happens if P=0 at Tref for MGD
            v=vol
            if (EoS%itran > 0) v=vol*(1.0_cp + strain)
            return
         end if

         if (delp*delp_prev < 0._cp .and. ic > 1)then     ! over-stepped solution: solution between v_prev and v
            vol=vol-delp*Vstep/(delp-delp_prev)                               ! best guess
            exit
         end if

         if (abs(delp) > abs(delp_prev))then ! delta-pressure getting bigger
            if (reverse) then               ! found a minimum between v_prev-vstep and v
               call set_error(1,' *****No volume found in get_volume')
               return
            else
               reverse=.true.            ! just going the wrong way
               vstep=-1.0_cp*vstep
            end if
         end if
         v_prev=vol         ! this volume stored
         delp_prev=delp  ! store delp
         Vol=Vol+Vstep
      end do

      ! now calculate PV around the solution: we want increasing P, so this means vstep < 0
      Vstep=-1.0_cp*abs(Vstep)
      Vol=Vol-2.0*Vstep
      Vstep=4.0*Vstep/nstep

      do i=1,nstep
         x(i)=get_pressure(vol,t,eosc)
         y(i)=vol
         vol=vol+vstep
      end do

      d2y= Second_Derivative(x, y, nstep)
      vol= Spline_Interpol(p,x,y,d2y,nstep)

      v=vol
      if (EoS%itran > 0) v=vol*(1.0_cp + strain)  ! apply transition strain ('vol' is actually linear if linear eos)

   End Function Get_Volume

   !!----
   !!---- FUNCTION GET_VOLUME_AXIS
   !!----
   !!---- Returns the value of volume or length of principal axis (ieos) in unit cell
   !!---- in cell_eos at P,T
   !!---- Call this Function directly when the calling routine  knows that the direction is a principal axis
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Volume_Axis(P, T, Cell_Eos, Ieos) Result(L)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      integer,            intent(in)  :: ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                   :: L !returned length or volume

      !---- Local Variables ----!

      !> init
      l=10.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            L=get_volume(p,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            L=get_volume(p,t,cell_eos%eos(1))

         case(3)
            L=get_volume_third(p,T,cell_eos,ieos)

         case(4)     ! only in mono  ...the d-sapcing of the unique axis
            L=get_volume(p,t,cell_eos%eos(cell_eos%unique))
      end select

   End Function Get_Volume_Axis

   !!----
   !!---- FUNCTION GET_VOLUME_CELL
   !!----
   !!---- Returns the value of volume or length of any axis in unit cell in
   !!---- cell_eos at P,T
   !!---- Call this Function when the calling routine does not know if the direction
   !!---- is a principal axis or not
   !!---- If a principal direction is requested, only axis%ieos is required
   !!---- axis%v and axis%atype only used if axis%ieos=-2
   !!----
   !!---- Date:  09/09/2020
   !!
   Module Function Get_Volume_Cell(P, T, Cell_Eos, Axis) Result(L)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: L !returned length or volume

      !---- Local Variables ----!

      !> init
      l=10.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            L=get_volume_axis(p,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            L=get_volume_general(p,T,cell_eos,axis)
      end select

   End Function Get_Volume_Cell

   !!--++
   !!--++ FUNCTION GET_VOLUME_GENERAL
   !!--++
   !!--++ Returns the value of volume or length of any axis in unit cell in cell_eos at P,T
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Volume_General(P, T, Cell_Eos, Axis) Result(L)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: L !returned length

      !---- Local Variables ----!
      type(cell_g_type) :: cell

      !> get the unit cell, metric tensors
      cell= get_params_cell(P,T,cell_eos)

      !> calculate the distance
      select case(U_case(axis%atype))
         case('U')
            L= dot_product(axis%v,matmul(cell%gd,axis%v))
            L=sqrt(abs(L))

         case('H')
            L= 1.0_cp/dot_product(axis%v,matmul(cell%gr,axis%v))
            L=sqrt(abs(L))

         case default
            L=10.0_cp
      end select

   End Function Get_Volume_General

   !!--++
   !!--++ FUNCTION GET_VOLUME_K
   !!--++
   !!--++ Returns the value of Volume for a given K  at T without using pressure
   !!--++ This has limited precision when Kp is small, so do not use except to
   !!--++ obtain approximate V (eg for limits to eos)
   !!--++
   !!--++ Date: 06/03/2019
   !!
   Module Function Get_Volume_K(K, T, EoS) Result(V)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: K       ! Bulk modulus
      real(kind=cp),  intent(in) :: T       ! Temperature
      type(Eos_Type), intent(in) :: EoS     ! Eos Parameter
      real(kind=cp)              :: V

      !---- Local Variables ----!
      integer                            :: ic
      real(kind=cp)                      :: vprev,Kprev,kc,vnew,delv,delvprev,Vstep

      !> Init
      v=0.0_cp

      Vprev=EoS%params(1)             !This is Vo
      Kprev=K_cal(Vprev,T,EoS)        !This is Ko

      !> Initial search
      Vstep=0.001_cp*EoS%params(1)            !If K is smaller than Ko, must go up in volume
      if (K > Kprev) Vstep=-1.0_cp*Vstep

      do
         Vprev=Vprev+Vstep
         kc=K_cal(Vprev,T,EoS)
         if (K < Kprev)then                   !going to volumes > 1.0
            if (Kc < K)exit
            if (Vprev > 2.0_cp*EoS%params(1) )then
               V=2.0_cp*EoS%params(1)            !stop infinite looping
               return
            end if

         else
            if (Kc > K)exit
            if (Vprev < 0.001_cp*EoS%params(1) )then
               V=0.001_cp*EoS%params(1)      !stop infinite looping
               return
            end if
        end if
      end do

      !> set-up for Newton-raphson
      ic=0
      Kprev=Kc
      V=Vprev-Vstep

    !     write(unit=6,fmt='(a,f10.3,a,f10.3)') 'Got to NR, Kprev = ',Kprev,'      V = ',V
    !     write(unit=6,fmt='(a,f10.3,a,f10.3)') 'Got to NR, Vprev = ',Vprev,'  Vstep = ',Vstep

      do     ! does a newton-raphson search
         ic=ic+1
         if (ic > 10) exit

         call clear_error()
         !  write(unit=6,fmt='(a,f10.3,a,f10.3)') 'In NR, T =  ',T,'      V = ',V
         kc=K_cal(V,T,EoS)

         !   write(unit=6,fmt='(a,f10.3)') 'In  NR, Kc = ',Kc
         if (abs(kc-k) < 0.001*k) exit                                 !new limit May 2019: was 0.001 now 0.001*K
         if (ic > 1)then                                              !introduced if(ic > 1) May 2019: otherwise delVprev is not initialised and delV becomes nan
            delV= (k-kc)*(V-Vprev)/(kc-Kprev)
            !   write(unit=6,fmt='(a,f10.3)') 'In  NR, delv= ',delv
            if (abs(delV) > abs(delVprev))delV=sign(delVprev,delV)       !prevents step getting bigger

         else
            delV=Vstep
         end if

         Vnew= V + delV
         if (Vnew < 0._cp) Vnew=0.99*V          ! stops V going negative
         Kprev=Kc
         Vprev=V
         delVprev=delV
         V=vnew
      end do

   End Function Get_Volume_K

   !!----
   !!---- FUNCTION GET_VOLUME_S
   !!----
   !!---- Returns the value of Volume obtained from Strain (S)
   !!----
   !!---- Date: 15/02/2013
   !!
   Module Function Get_Volume_S(S, T, Eos) Result(V)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: S    ! Strain
      real(kind=cp),  intent(in) :: T    ! Temperature
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: V

      !---- Local Variables ----!
      real(kind=cp)                      :: v0
      real(kind=cp), dimension(N_EOSPAR) :: ev

      !> Init
      v=0.0_cp

      !> Local Eos Copy
      ev= EoS_to_Vec(EoS)

      !> Allow for thermal: s=function of V(P,T)/V(P=0,T)
      select case (EoS%itherm)
         case (0)
            v0=ev(1)

         case (1:n_therm_models)
            v0=get_volume(0.0_cp,t,EoS)
            if (EoS%linear) v0=v0**3.0_cp
      end select

      select case (EoS%imodel)
         case (1,5,7) ! Murnaghan and Tait, no strain defined
            v=0.0_cp

         case (2) ! Birch-Murnaghan
            V=v0*(1.0_cp+2.0_cp*s)**(-1.5_cp)

         case (3) ! Vinet
            V=v0*(1.0_cp-s)**3.0_cp

         case (4) ! Natural Strain
            V=v0*exp(-3.0_cp*s)
      end select

      !> Linear case
      if (EoS%linear) v=v**(1.0_cp/3.0_cp)

   End Function Get_Volume_S

   !!--++
   !!--++ FUNCTION GET_VOLUME_THIRD
   !!--++
   !!--++ Returns the value of volume or length of a principal axis ieos in unit
   !!--++ cell in cell_eos at P,T when it can be calculated from others
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Volume_Third(P, T, Cell_Eos, Ieos) Result(L)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: cell_eos
      integer,            intent(in) :: ieos     ! the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                  :: l        !returned length or volume

      !---- Local Variables ----!
      integer       :: i
      real(kind=cp) :: vfactor

      !> init
      l=10.0_cp

      !> safety check: if mono or triclinic, should only be called if angle poly used
      if (U_case(cell_eos%system(1:4)) == 'TRIC' .or. U_case(cell_eos%system(1:3)) == 'MONO')then
         if (cell_eos%eosang%iangle == 0) then
            call set_error(1,'Get_Volume_Third called for mono or triclinic, without angle poly set')
         end if
      end if

      !> Factor for unit cell volume V = a.b.c.vfactor
      vfactor=1.0
      if (U_case(cell_eos%system(1:4)) == 'TRIG' .or. U_case(cell_eos%system(1:3)) == 'HEX ') &
         vfactor=sqrt(3.0_cp)/2.0_cp
      call clear_error()

      select case(U_case(cell_eos%system(1:4)))
         case('ORTH','MONO','TRIC')
            vfactor=Get_Angle_Volfactor(P,T,cell_eos)

            select case(ieos)
               case(0)
                  l=Get_Volume(P,T,cell_eos%eos(1))*Get_Volume(P,T,cell_eos%eos(2))* &
                    Get_Volume(P,T,cell_eos%eos(3))*vfactor

               case default
                  l=Get_Volume(P,T,cell_eos%eos(0))/vfactor
                  do i=1,3
                     if (i == ieos)cycle
                     l=l/Get_Volume(P,T,cell_eos%eos(i))
                  end do
            end select

         case('TRIG','HEXA','TETR')
            select case(ieos)
               case(0)     ! calc volumes from a and c
                  l=Get_Volume(P,T,cell_eos%eos(1))**2.0_cp*Get_Volume(P,T,cell_eos%eos(3))*vfactor

               case(1)     ! a from V and c
                  l=sqrt(Get_Volume(P,T,cell_eos%eos(0))/vfactor/Get_Volume(P,T,cell_eos%eos(3)))

               case(3)     ! c from a and V
                  l=Get_Volume(P,T,cell_eos%eos(0))/vfactor/Get_Volume(P,T,cell_eos%eos(1))**2.0_cp
            end select

         case('CUBI','ISOT')
            select case(ieos)
               case(0)     ! calc volume from a
                  l=Get_Volume(P,T,cell_eos%eos(1))**3.0_cp

               case(1:3)     ! a,b, or c from V
                  l=Get_Volume(P,T,cell_eos%eos(0))**(1.0_cp/3.0_cp)
            end select
      end select

   End Function Get_Volume_Third

End SubModule Eos_Get_Volume