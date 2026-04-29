!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Angle
   implicit none

   Contains

   !!----
   !!---- GET_ANGLE_DERIV
   !!----
   !!----
   !!---- Date: 04/02/2021
   !!---- Revision: OK
   !!
   Module Function Get_Angle_Deriv(P, T, Cell_Eos, Ia, RealAng, Dx) Result(Da)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos     ! The cell eos
      integer,            intent(in)  :: ia           ! The angle number
      logical,            intent(in)  :: realang      !.true. for real angle, .false. for recip
      character(len=1),   intent(in)  :: dx           ! either T or P
      real(kind=cp)                   :: da           ! the resulting angle derivative w.r.t. variable dx

      !---- Local Variables ----!

      !> init
      da=0.0_cp

      !> check that ia is valid and calculations required because da /= 0
      select case(U_case(cell_eos%system(1:4)))
         case('MONO')
            if (ia /=cell_eos%unique)return

         case('TRIC')
            if (ia < 1 .and. ia > 3)return

         case default
            return
      end select

      !> calculate: outer loop over angles
      if (realang .and. cell_eos%eosang%iangle > 0) then
         !> polynomial model for angles
         da=get_angle_poly_deriv(p,t,cell_eos%eosang,ia,dx)
      else
         !all other cases
         da=get_angle_eos_deriv(p,t,cell_eos,ia,realang,dx)
      end if

   End Function Get_Angle_Deriv

   !!--++
   !!--++ GET_ANGLE_EOS_DERIV
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_Eos_Deriv(Pin, Tin, Cell_EoS, Ia, RealAng, Xl) Result(D)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: pin,tin
      type(eos_cell_type),intent(in)  :: cell_eos
      integer,            intent(in)  :: ia       ! The angle number
      logical,            intent(in)  :: realang  !.true. for real angle, .false. for recip
      character(len=1),   intent(in)  :: xl       ! either T or P
      real(kind=cp)                   :: d        !the derivative d(angle_i)/dx in radians per x

      !---- Local Variables ----!
      integer           :: i
      real(kind=cp)     :: p,t

      !> for spline
      integer,parameter              :: nstep=11   !must be odd
      integer                        :: imid
      real(kind=cp),dimension(nstep) :: x,y,dy !,d2y
      type(cell_g_type)              :: c

      !>init
      d=0.0_cp
      imid=int(nstep/2) + 1

      do i=1,nstep
          if (U_case(xl) == 'P')then
             T=Tin
             P=Pin+(i-imid)*0.1_cp       !step P in 0.1
             x(i)=P

          else
             T=tin+(i-imid)*10.0_cp
             P=Pin
             x(i)=T
          end if

          c= get_params_cell(P,T,cell_eos)
          if (realang)then
             y(i)=c%ang(ia)
          else
             y(i)=c%rang(ia)
          end if
      end do

      !d2y= Second_Derivative(x, y, nstep)
      dy= First_Derivative(x, y, nstep)

      d=dy(imid)*TO_RAD

   End Function Get_Angle_Eos_Deriv

   !!--++
   !!--++ GET_ANGLE_POLY
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_Poly(P, T, EoS, Ia) Result(Ang)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: p, t
      type(eos_type),intent(in) :: eos
      integer,       intent(in) :: ia            ! The angle number
      real(kind=cp)             :: ang

      !---- Local Variables ----!
      integer           :: i
      real(kind=cp)     :: dt

      if (eos%angpoly(ia,0,1) < tiny(0.0_cp))then
         ang=90.0_cp
         return
      else
         ang=eos%angpoly(ia,0,1)
      end if

      dt=t-eos%tref
      do i=1,N_ANGPOLY
         ang=ang+eos%angpoly(ia,1,i)*p**i
         ang=ang+eos%angpoly(ia,2,i)*dt**i
      end do

      ang=ang+eos%angpoly(ia,3,1)*p*dt
      ang=ang+eos%angpoly(ia,3,2)*p*p*dt
      ang=ang+eos%angpoly(ia,3,3)*p*dt*dt

      if (ang < 0.0_cp .or. ang > 180.0_cp) then
         call set_error(1,'Angle polynomial predicted cell angle <0 or >180: reset to 90deg')
         ang=90._cp
      end if

   End Function Get_Angle_Poly

   !!--++
   !!--++ GET_ANGLE_POLY_DERIV
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_Poly_Deriv(P, T, EoS, Ia, X) Result(D)
      !---- Arguments ----!
      real(kind=cp),   intent(in) :: p,t
      type(eos_type),  intent(in) :: eos
      integer,         intent(in) :: ia ! The angle number
      character(len=1),intent(in) :: x  ! either T or P
      real(kind=cp)     :: d  !the derivative d(angle_i)/dx in radians per x

      !---- Local Variables ----!
      integer           :: i
      real(kind=cp)     :: dt !T-Tref

      !>init
      d=0._cp
      dt=t-eos%tref

      select case(U_case(x))
         case('P')
            do i=1,N_ANGPOLY
               d=d+eos%angpoly(ia,1,i)*i*p**(i-1)
            end do
            d=d+eos%angpoly(ia,3,1)*dt
            d=d+eos%angpoly(ia,3,2)*2.0_cp*p*dt
            d=d+eos%angpoly(ia,3,3)*dt*dt

         case('T')
            do i=1,N_ANGPOLY
               d=d+eos%angpoly(ia,2,i)*i*dt**(i-1)
            end do
            d=d+eos%angpoly(ia,3,1)*p
            d=d+eos%angpoly(ia,3,2)*p*p
            d=d+eos%angpoly(ia,3,3)*2.0_cp*p*dt

      end select
      d=d*TO_RAD

   End Function Get_Angle_Poly_Deriv

   !!--++
   !!--++ GET_ANGLE_VOLFACTOR
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_VolFactor(P, T, E) Result(vf)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: e
      real(kind=cp)                  :: vf

      !---- Local Variables ----!
      real(kind=cp)   :: aprod, cosang
      integer         :: i

      select case(U_case(e%system(1:4)))
         case default
            vf=1.0_cp

         case('MONO')
            i=2     !default b-unique
            if (e%unique > 0 .and. e%unique < 4)i=e%unique
            vf=sind(get_angle_poly(P,T,e%eosang,i))

         case('TRIC')
            aprod=2._cp
            vf=1.0_cp
            do i=1,3
               cosang=cosd(get_angle_poly(P,T,e%eosang,i) )
               vf=vf-cosang**2._cp
               aprod=aprod*cosang
            end do
            vf=vf+aprod
            if (vf > tiny(0.0_cp))then
               vf=sqrt(vf)
            else
               vf=1.0_cp
            end if
      end select

   End Function Get_Angle_VolFactor

   !!--++
   !!--++ GET_ANGLE_VOLFACTOR_DERIV
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_VolFactor_Deriv(P, T, E, X) Result(D)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: e
      character(len=1),   intent(in) :: x        !P or T
      real(kind=cp)                  :: d !returned volume  factor derivative

      !---- Local Variables ----!
      real(kind=cp)                 ::  pi,ti,del
      real(kind=cp),dimension(-2:2) :: a
      integer                       :: ia,j


      select case(U_case(e%system(1:4)))
         case default
            d=0.0_cp

         case('MONO')    !calculates cos(beta). d(beta)/dx
            ia=2     !default b-unique
            if (e%unique > 0 .and. e%unique < 4)ia=e%unique

            if (U_case(x) =='P')then
               Ti=T
               del=0.1
               do j=-2,2,1
                  pi=p+real(j)*del
                  a(j)=get_angle_poly(Pi,Ti,e%eosang,ia)
               end do

            else
               Pi=P
               del=10._cp
               do j=-2,2,1
                  Ti=T+real(j)*del
                  a(j)=get_angle_poly(Pi,Ti,e%eosang,ia)
               end do
            end if

            d=(a(-2)+8.0_cp*(a(1)-a(-1))-a(2))/(12.0_cp*del)     ! Derivative to second order approximation
            d=cosd(get_angle_poly(Pi,Ti,e%eosang,ia))*d*to_rad

         case('TRIC')
            !> calculate Volfactor as function of x, then direct deriv
            if (U_case(x) =='P')then
               Ti=T
               del=0.1
               do j=-2,2,1
                  pi=p+real(j)*del
                  a(j)=get_angle_volfactor(Pi,Ti,e)
               end do

            else
               Pi=P
               del=10._cp
               do j=-2,2,1
                  Ti=T+real(j)*del
                  a(j)=get_angle_volfactor(Pi,Ti,e)
               end do
            end if

            d=(a(-2)+8.0_cp*(a(1)-a(-1))-a(2))/(12.0_cp*del)     ! Derivative to second order approximation

      end select

   End Function Get_Angle_VolFactor_Deriv

   !!--++
   !!--++ GET_ANGLE_VOLFACTOR_DERIV2
   !!--++
   !!--++ Date: 04/02/2021
   !!--++ Revision: OK
   !!
   Module Function Get_Angle_Volfactor_Deriv2(P, T, E, X) Result(D)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: e
      character(len=*),   intent(in) :: x        !P or T or PT
      real(kind=cp)                  :: d !returned volume  factor 2nd derivative

      !---- Local Variables ----!
      real(kind=cp)                 :: pii,ti,del
      real(kind=cp),dimension(-2:2) :: a
      integer                       :: j

      !> Init
      d=0.0_cp
      if (index(U_case(e%system),'TRIC') == 0 .and. index(U_case(e%system),'MONO') == 0) return

      !> calculate Volfactor as function of x, then direct 2nd deriv
      if (index(U_case(x),'P') > 0 .and. index(U_case(x),'T') > 0)then
         !> cross derivative
         del=40._cp
         do j=-2,2,1
            ti=t+real(j)*del
            a(j)=get_angle_volfactor_deriv(P,Ti,e,'P')
         end do

      else if(index(U_case(x),'P') > 0 .and. index(U_case(x),'T') == 0)then
         Ti=T
         del=0.4
         do j=-2,2,1
            pii=p+real(j)*del
            a(j)=get_angle_volfactor_deriv(Pii,Ti,e,'P')
         end do

      else
         Pii=P
         del=40._cp
         do j=-2,2,1
            Ti=T+real(j)*del
            a(j)=get_angle_volfactor_deriv(Pii,Ti,e,'T')
         end do
      end if

      d=(a(-2)+8.0_cp*(a(1)-a(-1))-a(2))/(12.0_cp*del)     ! Derivative to second order approximation

   End Function Get_Angle_VolFactor_Deriv2

End SubModule EoS_Get_Angle