!!----
!!----
!!----
SubModule (CFML_EoS) Eos_Get_Temperature
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_TEMPERATURE
   !!----
   !!---- Returns Temperature at given P,V
   !!----
   !!---- Date: 01/04/2014
   !!
   Module Function Get_Temperature(P, V, Eos) Result(T)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P    ! Pressure
      real(kind=cp),  intent(in) :: V    ! Volume
      type(Eos_Type), intent(in) :: EoS  ! Eos Parameter
      real(kind=cp)              :: t

      !---- Local Variables ----!
      integer                           :: nstep
      real(kind=cp)                     :: pa,va,ta
      real(kind=cp)                     :: step,dp1,dp2

      !> Init
      t=EoS%tref
      pa=p               ! local copy p
      va=v

      !> Check
      if (EoS%itherm == 0) return          ! no calcs possible without thermal model

      !> If PTV table, go directly
      if (EoS%imodel == -1) then
         t=get_props_ptvtable(p,0.0,v,EoS,'T')     ! get_props_ptvtable works in length if linear
         return
      end if

      !> First estimate at P=0
      t=Get_Temperature_P0(va,EoS)
      if (EoS%imodel ==0) return
      if (err_cfml%flag) return

      !> Use iterative solution, by minimising p-pcalc
      !> Init this part
      ta=t
      dp1=p-get_pressure(va,ta,EoS)
      nstep=0
      step=100.0_cp  ! For positive thermal expansion, should increase T from P=0

      !> Do iteration
      do
         !> Trap infinite loops
         if (nstep > 10000)then
            call set_error(1,' ')
            write(err_cfml%msg,'("No convergence in get_temperature after ",i5," steps, dp= ",f10.4)')nstep,dp1
            exit
         end if

         !> Increment Temperature
         ta=ta+step
         dp2=p-get_pressure(va,ta,EoS)
         nstep=nstep+1

         !> test for sufficient convergence
         if (abs(step) < 1) exit          ! Go to 1 K precision

         !> not converged, so adjust step size
         if (dp1*dp2 < 0.0_cp) then
            !> overshot ptr:reverse step direction and make size smaller
            step=-0.5_cp*step
         else
            if (abs(dp2) > abs(dp1))then
               step=-1.0*step      ! wrong direction: reverse
            end if                 !(correct direction uses same step again)
         end if

         dp1=dp2        ! update delta-p values and go back for next cycle
      end do

      t=ta              ! success

   End Function Get_Temperature

   !!----
   !!---- FUNCTION GET_TEMPERATURE_P0
   !!----
   !!---- Returns Temperature at P=0 for given V0T
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Function Get_Temperature_P0(V, EoS, Tmin, Tmax) Result(Tk)
      !---- Arguments ----!
      real(kind=cp),           intent(in) :: V       ! Volume at temperature T or Pth (Pthermal case)
      type(Eos_Type),          intent(in) :: EoS     ! Eos Parameter
      real(kind=cp), optional, intent(in) :: Tmin    ! Range for solution in T
      real(kind=cp), optional, intent(in) :: Tmax
      real(kind=cp)                       :: tk

      !---- Local Variables ----!
      real(kind=cp)                      :: tref
      real(kind=cp)                      :: v00,v0T
      real(kind=cp)                      :: a,b,c,d,t1,t2,t3,x1,x2,x3
      real(kind=cp)                      :: a1,a2,a3,q,r,s1,s2,dd,th
      real(kind=cp)                      :: y,p
      real(kind=cp)                      :: kp,th_e,eps0
      real(kind=cp)                      :: Tkmin,Tkmax
      real(kind=cp), dimension(N_EOSPAR) :: ev

      !> Init
      Tk=EoS%tref

      !> Local copy EoS to handle linear or volume
      ev= EoS_to_Vec(EoS)

      !> all equations written for volume
      v00=ev(1)
      if (EoS%linear) then
         v0T=V**3.0_cp
      else
         v0T=V
      end if

      !> Optional arguments
      TKmin=0.0_cp
      if (present(Tmin)) TKmin=tmin

      TKmax=1.0e4
      if (present(Tmax)) TKmax=tmax

      !> Check
      Tref=EoS%tref
      if (v00 <= tiny(0.0)) return

      !> Init local variables
      t1=0.0
      t2=0.0
      t3=0.0

      select case (EoS%itherm)
         case (1) ! Berman
            ! modified RJA 31.03.2014 to fix case params(11)=0.
            if (abs(ev(11)) < tiny(0.0_cp)) then
               if (ev(10) > tiny(0.0_cp)) tk=Tref+(v0T/v00 -1.0_cp)/ev(10)
            else
               a= 0.5*ev(11)
               b= ev(10) - ev(11)*tref
               c= 1.0 - ev(10)*tref + 0.5*ev(11)*tref*tref - (v0t/v00)

               d= b*b - 4.0*a*c
               if (d >= 0.0) then
                  t1=(-b+sqrt(d))/(2.0*a)
                  t2=(-b-sqrt(d))/(2.0*a)
                  if (t1 > 0.0) then
                     if (t1 >=TKmin .and. t1 <=TKmax)then
                        tk=t1
                     else
                        call set_error(1,'No valid solution for temperature')
                     end if

                  elseif (t2 > 0.0) then
                     if (t2 >=TKmin .and. t2 <=TKmax) then
                        tk=t2
                     else
                        call set_error(1,'No valid solution for temperature')
                     end if
                  else
                     call set_error(1,'No valid solution for temperature')
                  end if
               end if
            end if

         case (2) ! Fei
            ! modified RJA 31.03.2014 to fix case params(11)=0.
            if (abs(ev(11)) < tiny(0.0_cp)) then
               if (ev(10) > tiny(0.0_cp))tk=Tref+log(v0T/v00)/ev(10)
            else
               a=0.5*ev(11)
               b=ev(10)
               c=-b*tref -a*tref*tref + ev(12)/tref -log(v0t/v00)
               d=-ev(12)

               a1=b/a
               a2=c/a
               a3=d/a
               q=(3.0*a2 - a1*a1)/9.0
               r=(9.0*a1*a2-27.0*a3-2.0*a1*a1*a1)/54.0

               dd=q**3+r**2
               if (dd >= 0.0) then
                  s1=(r + sqrt(q**3+r**2))**(1.0/3.0)
                  s2=(r - sqrt(q**3+r**2))**(1.0/3.0)

                  t1=s1+s2-a1/3.0
                  if (abs(s1-s2) <= tiny(0.0)) t2=-0.5*(s1+s2)-a1/3.0

                  if (t1 > 0.0) then
                     if (t1 >=TKmin .and. t1 <=TKmax)then
                        tk=t1
                     else
                        call set_error(1,'No valid solution for temperature')
                     end if

                  elseif (t2 > 0.0) then
                     if (t2 >=TKmin .and. t2 <=TKmax)then
                        tk=t2
                     else
                        call set_error(1,'No valid solution for temperature')
                     end if
                  else
                     call set_error(1,'No valid solution for temperature')
                  end if

               else
                  th=acos(r/sqrt(-q**3))
                  t1=2.0*sqrt(-q)*cos(th/3.0)-a1/3.0
                  t2=2.0*sqrt(-q)*cos((th+2.0*pi)/3.0) -a1/3.0
                  t3=2.0*sqrt(-q)*cos((th+4.0*pi)/3.0) -a1/3.0
                  if (t1 > 0.0) then
                     if (t1 >=TKmin .and. t1 <=TKmax) tk=t1

                  elseif (t2 > 0.0) then
                     if (t2 >=TKmin .and. t2 <=TKmax) tk=t2

                  elseif (t3 > 0.0) then
                     if (t3 >=TKmin .and. t3 <=TKmax) tk=t3

                  else
                     call set_error(1,'No valid solution for temperature')
                  end if
               end if
            end if

         case (3) ! HP
            ! modified RJA 31.03.2014 to fix coding errors
            a=ev(10)
            b=-2.0*(10.0*ev(10) + ev(11))
            c=1.0 - ev(10)*tref + 2.0*(10.0*ev(10) + ev(11))*sqrt(tref) - (V0t/V00)

            d=b*b - 4.0*a*c
            if (d >= 0.0) then
               x1=(-b+sqrt(d))/(2.0*a)
               x2=(-b-sqrt(d))/(2.0*a)
               if (x1 > 0.0) then
                  t1=x1*x1
                  if (t1 >=TKmin .and. t1 <=TKmax) tk=t1
               else if (x2 > 0.0) then
                  t2=x2*x2
                  if (t2 >=TKmin .and. t2 <=TKmax) tk=t2
               end if
            else
               call set_error(1,'No valid solution for temperature')
            end if

         case (4) ! Kroll
            !>>>>> kp=ev(3) : version before 11/11/2016
            if (EoS%icross == 2)then
               kp=ev(8)                    !uses Anderson delta
            else
               kp=ev(3)
            end if

            th_e=ev(11)
            a=th_e/Tref
            b=-1.0/(kp*(kp+2.0))
            eps0=(a**2)*exp(a)/(exp(a)-1.0)**2

            x1=(-b*(1.0+kp))*eps0/(ev(10)*th_e)
            x2=1.0 - (((v0t/v00)+kp)/(1.0+kp))**(1.0/b)
            x3=1.0/(exp(a)-1)

            c=1.0/(x1*x2+x3)
            if (c <= -1.0_cp) then
               call set_error(1,'No valid solution for temperature')
            else
               d=log(1.0 +c)
               t1=th_e/d
               if (t1 >=TKmin .and. t1 <=TKmax) then
                  tk=t1
               else
                  call set_error(1,'No valid solution for temperature')
               end if
            end if

         case (5) ! Salje
            x1=0.0
            x2=0.0
            x3=0.0

            y=ev(10)*ev(11)
            p=v00**(1.0/3.0) -y

            a=y**3
            b=3.0 * y*y*p
            c=3.0 * y * p*p
            d=p**3 - v0t

            a1=b/a
            a2=c/a
            a3=d/a
            q=(3.0*a2 - a1*a1)/9.0
            r=(9.0*a1*a2-27.0*a3-2.0*a1*a1*a1)/54.0

            dd=q**3+r**2
            if (dd >= 0.0) then
               s1=(r + sqrt(q**3+r**2))**(1.0/3.0)
               s2=(r - sqrt(q**3+r**2))**(1.0/3.0)

               x1=s1+s2-a1/3.0
               if (abs(s1-s2) <= tiny(0.0)) then
                  x2=-0.5*(s1+s2)-a1/3.0
               end if
            else
               th=acos(r/sqrt(-q**3))
               x1=2.0*sqrt(-q)*cos(th/3.0)-a1/3.0
               x2=2.0*sqrt(-q)*cos((th+2.0*pi)/3.0) -a1/3.0
               x3=2.0*sqrt(-q)*cos((th+4.0*pi)/3.0) -a1/3.0
            end if
            if (abs(x1) >= 1.0) t1=2.0*ev(11)/log((1.0+x1)/(x1-1.0))
            if (abs(x2) >= 1.0) t2=2.0*ev(11)/log((1.0+x2)/(x2-1.0))
            if (abs(x3) >= 1.0) t3=2.0*ev(11)/log((1.0+x3)/(x3-1.0))

            if (t1 > 0.0) then
               if (t1 >=TKmin .and. t1 <=TKmax) tk=t1
            end if
            if (t2 > 0.0 .and. tk < 0.0) then
               if (t2 >=TKmin .and. t2 <=TKmax) tk=t2
            end if
            if (t3 > 0.0 .and. tk < 0.0) then
               if (t3 >=TKmin .and. t3 <=TKmax) tk=t3
            end if

         case(6)
            th_e=ev(11)
            a=th_e/Tref
            eps0=(a**2)*exp(a)/(exp(a)-1.0)**2

            x1=eps0/(ev(10)*ev(2)*ev(11))
            x2=1.0/(exp(a)-1.0)

            c=1.0/(v0t*x1+x2)
            d=log(1.0 + c)

            t1=th_e/d
            if (t1 >=TKmin .and. t1 <=TKmax) tk=t1

         case(7,8)  !For MGD Pthermal, no real meaningful value of T to return, so set as Tref
            tk=EoS%tref

      end select

   End Function Get_Temperature_P0

   !!--++
   !!--++ FUNCTION GET_DMDT_AXIS
   !!--++
   !!--++ Returns the value of temperature derivative of the modulus of principal axis (ieos) in unit
   !!--++ cell in cell_eos at P,T
   !!--++
   !!--++ Call this Function directly when the calling routine  knows that the direction is a principal axis
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_DmDt_Axis(P, T, Cell_eos, Ieos) Result(dMdT)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: P,T
      type(eos_cell_type),intent(in)  :: cell_eos
      integer,            intent(in)  :: ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                   :: dMdT

      !---- Local Variables ----!

      !> init
      dmdt=0._cp

      select case(cell_eos%loaded(ieos))
         case(1)
            dmdt=dKdT_Cal(p,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            dmdt=dKdT_Cal(p,t,cell_eos%eos(1))

         case(3)
            dmdt=get_dmdt_third(p,T,cell_eos,ieos)

         case(4)
            dmdt=dKdt_cal(p,t,cell_eos%eos(cell_eos%unique))
      end select

   End Function Get_DmDt_Axis

   !!--++
   !!--++ FUNCTION GET_DMDT_CELL
   !!--++
   !!--++ Returns the value of temperature derivative of modulus of any axis in unit cell in cell_eos at P,T
   !!--++ Call this Function when the calling routine does not know if the direction is a principal axis or not
   !!--++ If a principal direction is requested, only axis%ieos is required
   !!--++ axis%v and axis%atype only used if axis%ieos=-2
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_DmDt_Cell(P, T, Cell_eos, Axis) Result(dMdT)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: dMdT

      !---- Local Variables ----!

      !> init
      dmdt=0.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            dmdt=get_dmdt_axis(p,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            dmdt=get_dmdt_general(p,T,cell_eos,axis)

      end select

   End Function Get_DmDt_Cell

   !!--++
   !!--++ FUNCTION GET_DMDT_GENERAL
   !!--++
   !!--++ Returns the value of temperature derivative of modulus of any axis in unit
   !!--++ cell in cell_eos at P,T
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_DmDt_General(P, T, Cell_eos, Axis) Result(dMdT)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: dMdT

      !---- Local Variables ----!
      real(kind=cp)   :: tstep,tcal
      integer         :: i

      !> for spline
      integer,parameter :: nstep=21   !must be odd
      integer           :: imid
      real(kind=cp),dimension(nstep):: x,y,dy !,d2y

      tstep=20.
      tcal=t-int(nstep/2)*tstep
      do i=1,nstep
         x(i)=tcal
         y(i)=get_mod_general(P,Tcal,cell_eos,axis)
         tcal=tcal+tstep
      end do

      !call Second_Derivative(x, y, nstep, d2y)
      dy= First_Derivative(x, y, nstep)

      imid=int(nstep/2) + 1
      dMdT=dy(imid)

   End Function Get_DmDt_General

   !!--++
   !!--++ FUNCTION GET_DMDT_THIRD
   !!--++
   !!--++ Returns the value of temperature derivative of modulus of a principal axis ieos in unit
   !!--++ cell in cell_eos at P,T when it can be calculated from others
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_DmDt_Third(P, T, Cell_eos, Ieos) Result(modp)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: cell_eos
      integer,            intent(in) :: ieos     ! the modulus of the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                  :: modp

      !---- Local Variables ----!
      integer        :: i
      real(kind=cp)  :: Kp,M1p,M2p,M3p,Mangp,vf

      !> init
      modp=0.0_cp

      !> safety check: if mono or triclinic, should only be called if angle poly used
      if (U_case(cell_eos%system(1:4)) == 'TRIC' .or. U_case(cell_eos%system(1:3)) == 'MONO')then
         if (cell_eos%eosang%iangle == 0) then
            call set_error(1,'Get_DmDt_Third called for mono or triclinic, without angle poly set')
         end if
      end if

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC','MONO','ORTH')
            if (U_case(cell_eos%system(1:4)) == 'ORTHO')then
               Mangp=0._cp
            else
               vf=Get_Angle_Volfactor(P,T,cell_eos)
               Mangp=(Get_Angle_Volfactor_Deriv(P,T,cell_eos,'P') * &
                      Get_Angle_Volfactor_Deriv(P,T,cell_eos,'T')/vf - &
                      Get_Angle_Volfactor_Deriv2(P,T,cell_eos,'PT'))/vf
            end if

            select case(ieos)
               case(0)
                  M1p= dKdT_cal(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  M2p= dKdT_cal(P,T,cell_eos%eos(2))/Get_K(P,T,cell_eos%eos(2))**2.0_cp
                  M3p= dKdT_cal(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,0)**2.0_cp*(M1p+M2p+M3p-Mangp)

               case default
                  modp= dKdT_cal(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  do i=1,3
                     if (i == ieos)cycle
                     modp = modp - dKdT_cal(P,T,cell_eos%eos(i))/Get_K(P,T,cell_eos%eos(i))**2.0_cp
                  end do
                  modp=modp+Mangp
                  modp = get_mod_third(P,T,cell_eos,ieos)**2.0_cp * modp
            end select

         case('TRIG','HEXA','TETR')
            select case(ieos)
               case(0)     ! calc V from a and c
                  M1p= dKdT_cal(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  M3p= dKdT_cal(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,0)**2.0_cp*(2.0*M1p+M3p)

               case(1)     ! a from V and c
                  Kp=  dKdT_cal(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  M3p= dKdT_cal(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,1)**2.0_cp*(Kp-M3p)/2.0

               case(3)     ! c from a and V
                  Kp= dKdT_cal(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  M1p= dKdT_cal(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,3)**2.0_cp*(Kp-2.0*M1p)
            end select

         case('CUBI','ISOT')
            select case(ieos)
               case(0)     ! calc volume from a
                  modp=dKdT_cal(P,T,cell_eos%eos(1))/3.0_cp

               case(1,2,3)     ! a,b, or c from V
                  modp=dKdT_cal(P,T,cell_eos%eos(0))*3.0_cp
            end select
      end select

   End Function Get_DmDt_Third

End SubModule Eos_Get_Temperature