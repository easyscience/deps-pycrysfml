!!----
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Spher_Harm
 implicit none
 Contains

    !!----
    !!---- CUBIC_HARM_ANG
    !!----
    !!----    Calculation of the cubic harmonics given in Table 3 of reference
    !!----    M.Kara & K. Kurki-Suonio, Acta Cryt. A37, 201 (1981).
    !!----    Only up to tenth order.
    !!----
    !!---- 09/04/2019
    !!
    Elemental Module Function Cubic_Harm_Ang(L,M,Theta,Phi) Result(Klm)
       !---- Arguments ----!
       integer,      intent (in) :: l          !
       integer,      intent (in) :: m          !
       real(kind=cp),intent (in) :: theta      !
       real(kind=cp),intent (in) :: phi        !
       real(kind=cp)             :: klm        ! Output value

       select case (l)
          case(0)   ! 0 1
             klm=real_spher_harm_ang(0,0,1,theta,phi)

          case(3)   ! 3 1
             klm=real_spher_harm_ang(3,2,-1,theta,phi)

          case(4)   ! 4 1
             klm=0.5*sqrt(7.0/3.0)*real_spher_harm_ang(4,0,1,theta,phi)
             klm=klm+0.5*sqrt(5.0/3.0)*real_spher_harm_ang(4,4,1,theta,phi)

          case(6)
             if (m == 1) then   ! 6 1
                klm=0.5*sqrt(0.5)*real_spher_harm_ang(6,0,1,theta,phi)
                klm=klm-0.5*sqrt(7.0/2.0)*real_spher_harm_ang(6,4,1,theta,phi)
             else               ! 6 2
                klm=0.25*sqrt(11.0)*real_spher_harm_ang(6,2,1,theta,phi)
                klm=klm-0.25*sqrt(5.0)*real_spher_harm_ang(6,6,1,theta,phi)
             end if

          case(7)   ! 7 1
             klm=0.5*sqrt(13.0/6.0)*real_spher_harm_ang(7,2,-1,theta,phi)
             klm=klm+0.5*sqrt(11.0/6.0)*real_spher_harm_ang(7,6,-1,theta,phi)

          case(8)   ! 8 1
             klm=0.125*sqrt(33.0)*real_spher_harm_ang(8,0,1,theta,phi)
             klm=klm+0.25*sqrt(7.0/3.0)*real_spher_harm_ang(8,4,1,theta,phi)
             klm=klm+0.125*sqrt(65.0/3.0)*real_spher_harm_ang(8,8,1,theta,phi)

          case(9)
             if (m == 1) then   ! 9 1
                klm=0.25*sqrt(3.0)*real_spher_harm_ang(9,2,-1,theta,phi)
                klm=klm-0.25*sqrt(13.0)*real_spher_harm_ang(9,6,-1,theta,phi)
             else               ! 9 2
                klm=0.5*sqrt(17.0/6.0)*real_spher_harm_ang(9,4,-1,theta,phi)
                klm=klm-0.5*sqrt(7.0/6.0)*real_spher_harm_ang(9,8,-1,theta,phi)
             end if

          case(10)
             if (m == 1) then   ! 10 1
                klm=0.125*sqrt(65.0/6.0)*real_spher_harm_ang(10,0,1,theta,phi)
                klm=klm-0.25*sqrt(11.0/2.0)*real_spher_harm_ang(10,4,1,theta,phi)
                klm=klm-0.125*sqrt(187.0/6.0)*real_spher_harm_ang(10,8,1,theta,phi)
             else               ! 10 2
                klm=0.125*sqrt(247.0/6.0)*real_spher_harm_ang(10,2,1,theta,phi)
                klm=klm+(1.0/16.0)*sqrt(19.0/3.0)*real_spher_harm_ang(10,6,1,theta,phi)
                klm=klm-(1.0/16.0)*sqrt(85.0)*real_spher_harm_ang(10,10,1,theta,phi)
             end if

          case default
             klm=0.0_cp
       end select

    End Function Cubic_Harm_Ang

    !!----
    !!---- CUBIC_HARM_UCVEC
    !!--<<
    !!----    Calculation of the cubic harmonics given in Table 3 of reference
    !!----    M.Kara & K. Kurki-Suonio, Acta Cryt. A37, 201 (1981).
    !!----    Only up to tenth order. A control of errors is included.
    !!----    For "m3m" symmetry, calculations include up to L=20 M=2 using the
    !!----    coefficients from F.M. Mueller and M.G. Priestley, Phys Rev 148, 638 (1966)
    !!-->>
    !!----
    !!---- 09/04/2019
    !!
    Pure Module Function Cubic_Harm_Ucvec(L,M,U) Result(Klm)
       !---- Arguments ----!
       integer,                    intent (in) :: l      !
       integer,                    intent (in) :: m      !
       real(kind=cp),dimension(3), intent (in) :: u      !
       real(kind=cp)                           :: Klm    ! Output value

       select case (l)
          case(0)   ! 0 1
             klm=real_spher_harm_ucvec(0,0,1,u)

          case(3)   ! 3 1
             klm=real_spher_harm_ucvec(3,2,-1,u)

          case(4)   ! 4 1
             klm=0.5*sqrt(7.0/3.0)*real_spher_harm_ucvec(4,0,1,u)
             klm=klm+0.5*sqrt(5.0/3.0)*real_spher_harm_ucvec(4,4,1,u)

          case(6)
             if (m == 1) then   ! 6 1
                klm=0.5*sqrt(0.5)*real_spher_harm_ucvec(6,0,1,u)
                klm=klm-0.5*sqrt(7.0/2.0)*real_spher_harm_ucvec(6,4,1,u)
             else               ! 6 2
                klm=0.25*sqrt(11.0)*real_spher_harm_ucvec(6,2,1,u)
                klm=klm-0.25*sqrt(5.0)*real_spher_harm_ucvec(6,6,1,u)
             end if

          case(7)   ! 7 1
             klm=0.5*sqrt(13.0/6.0)*real_spher_harm_ucvec(7,2,-1,u)
             klm=klm+0.5*sqrt(11.0/6.0)*real_spher_harm_ucvec(7,6,-1,u)

          case(8)   ! 8 1
             klm=0.125*sqrt(33.0)*real_spher_harm_ucvec(8,0,1,u)
             klm=klm+0.25*sqrt(7.0/3.0)*real_spher_harm_ucvec(8,4,1,u)
             klm=klm+0.125*sqrt(65.0/3.0)*real_spher_harm_ucvec(8,8,1,u)

          case(9)
             if (m == 1) then   ! 9 1
                klm=0.25*sqrt(3.0)*real_spher_harm_ucvec(9,2,-1,u)
                klm=klm-0.25*sqrt(13.0)*real_spher_harm_ucvec(9,6,-1,u)
             else               ! 9 2
                klm=0.5*sqrt(17.0/6.0)*real_spher_harm_ucvec(9,4,-1,u)
                klm=klm-0.5*sqrt(7.0/6.0)*real_spher_harm_ucvec(9,8,-1,u)
             end if

          case(10)
             if (m == 1) then   ! 10 1
                klm=0.125*sqrt(65.0/6.0)*real_spher_harm_ucvec(10,0,1,u)
                klm=klm-0.25*sqrt(11.0/2.0)*real_spher_harm_ucvec(10,4,1,u)
                klm=klm-0.125*sqrt(187.0/6.0)*real_spher_harm_ucvec(10,8,1,u)
             else               ! 10 2
                klm=0.125*sqrt(247.0/6.0)*real_spher_harm_ucvec(10,2,1,u)
                klm=klm+(1.0/16.0)*sqrt(19.0/3.0)*real_spher_harm_ucvec(10,6,1,u)
                klm=klm-(1.0/16.0)*sqrt(85.0)*real_spher_harm_ucvec(10,10,1,u)
             end if

          case(12)
             ! from here only the cubic harmonics for m3m symmetry are calculated
             ! coefficients from F.M. Mueller and M.G. Priestley, Phys Rev 148, 638 (1966)
             if (m == 1) then   ! 12 1
                klm=    0.69550265*real_spher_harm_ucvec(12,0,1,u)
                klm=klm+0.31412555*real_spher_harm_ucvec(12,4,1,u)
                klm=klm+0.34844954*real_spher_harm_ucvec(12,8,1,u)
                klm=klm+0.54422797*real_spher_harm_ucvec(12,12,1,u)
             else               ! 12 2
                klm=    0.55897937*real_spher_harm_ucvec(12,4,1,u)
                klm=klm-0.80626751*real_spher_harm_ucvec(12,8,1,u)
                klm=klm+0.19358400*real_spher_harm_ucvec(12,12,1,u)
             end if

          case(14)
             ! 12 1
             klm=    0.44009645*real_spher_harm_ucvec(14,0,1,u)
             klm=klm-0.45768183*real_spher_harm_ucvec(14,4,1,u)
             klm=klm-0.49113230*real_spher_harm_ucvec(14,8,1,u)
             klm=klm-0.59634848*real_spher_harm_ucvec(14,12,1,u)

          case(16)
             if (m == 1) then   ! 16 1
                klm=    0.68136168*real_spher_harm_ucvec(16,0,1,u)
                klm=klm+0.27586801*real_spher_harm_ucvec(16,4,1,u)
                klm=klm+0.29048987*real_spher_harm_ucvec(16,8,1,u)
                klm=klm+0.32756975*real_spher_harm_ucvec(16,12,1,u)
                klm=klm+0.51764542*real_spher_harm_ucvec(16,16,1,u)
             else               ! 16 2
                klm=    0.63704821*real_spher_harm_ucvec(16,4,1,u)
                klm=klm-0.32999033*real_spher_harm_ucvec(16,8,1,u)
                klm=klm-0.64798073*real_spher_harm_ucvec(16,12,1,u)
                klm=klm+0.25572816*real_spher_harm_ucvec(16,16,1,u)
             end if

          case(18)
             if (m == 1) then   ! 18 1
                klm=    0.45791513*real_spher_harm_ucvec(18,0,1,u)
                klm=klm-0.38645598*real_spher_harm_ucvec(18,4,1,u)
                klm=klm-0.40209462*real_spher_harm_ucvec(18,8,1,u)
                klm=klm-0.43746593*real_spher_harm_ucvec(18,12,1,u)
                klm=klm-0.53657149*real_spher_harm_ucvec(18,16,1,u)
             else               ! 18 2
                klm=    0.14872751*real_spher_harm_ucvec(18,4,1,u)
                klm=klm-0.63774601*real_spher_harm_ucvec(18,8,1,u)
                klm=klm+0.72334167*real_spher_harm_ucvec(18,12,1,u)
                klm=klm-0.21894515*real_spher_harm_ucvec(18,16,1,u)
             end if

          case(20)
             if (m == 1) then   ! 20 1
                klm=    0.67141495*real_spher_harm_ucvec(20,0,1,u)
                klm=klm+0.24982619*real_spher_harm_ucvec(20,4,1,u)
                klm=klm+0.25782846*real_spher_harm_ucvec(20,8,1,u)
                klm=klm+0.27469333*real_spher_harm_ucvec(20,12,1,u)
                klm=klm+0.31248919*real_spher_harm_ucvec(20,16,1,u)
                klm=klm+0.49719956*real_spher_harm_ucvec(20,20,1,u)
             else               ! 20 2
                klm=    0.66299538*real_spher_harm_ucvec(20,4,1,u)
                klm=klm-0.11295259*real_spher_harm_ucvec(20,8,1,u)
                klm=klm-0.42738441*real_spher_harm_ucvec(20,12,1,u)
                klm=klm-0.52810433*real_spher_harm_ucvec(20,16,1,u)
                klm=klm+0.29347435*real_spher_harm_ucvec(20,20,1,u)
             end if

          case default
             klm=0.0_cp
       end select

    End Function Cubic_Harm_Ucvec

    !!----
    !!---- INTEGRAL_SLATER_BESSEL
    !!--<<
    !!----    Returns the integral:
    !!----    Int[0,inf]{r**(n+2)*exp(-chi*r)*j_l(s*r)} dr
    !!----    where j_l is the spherical Bessel function of order l
    !!----    only -1 <= n  and 0 <= l <= n+1
    !!-->>
    !!----
    !!---- 09/04/2019
    !!
    Elemental Module Function Integral_Slater_Bessel(N,L,Z,S) Result(V)
       !---- arguments ----!
       integer,       intent(in) :: n
       integer,       intent(in) :: l
       real(kind=cp), intent(in) :: z
       real(kind=cp), intent(in) :: s
       real(kind=cp)             :: v

       !---- Local Variables ----!
       real(kind=cp)  :: isb0, tmp
       integer        :: fact, i

       v =0.0_cp
       if (n >= -1 .and. l<=n+1 .and. l>=0) then
          if (l == 0) then
             fact=1
          else
             fact= 1
             do i=1,l
                fact=fact*i
             end do
          end if
          if (l == n+1) then
             v = (2.0_cp*s/(s*s+z*z))**l*fact/(s*s+z*z)
             return
          end if
          if (l == n) then
             v = real(2*l+2,kind=cp)*z* (2.0_cp*s/(s*s+z*z))**l*fact/(s*s+z*z)/(s*s+z*z)
             return
          end if

          !---- We arrive here iif l>=n+1
          !---- We calculate the case n=l-1
          isb0 = (2.0_cp*s/(s*s+z*z))**l*fact/(s*s+z*z)

          !---- We calculate the case n=l
          v = real(2*l+2,kind=cp)*z* (2.0_cp*s/(s*s+z*z))**l*fact/(s*s+z*z)/(s*s+z*z)

          !---- And apply the recursivity up to n
          do i=l+1,n
             tmp= v
             v = (2.0_cp*(i+1)*z*v - &
                                  real(i+l+1,kind=cp)*real(i-l,kind=cp)*ISB0)/(s*s+z*z)
             isb0= tmp
          end do

          return
       end if

    End Function Integral_Slater_Bessel

    !!----
    !!---- REAL_SPHER_HARM_ANG
    !!----
    !!----    Real Spherical Harmonics: M.Kara & K. Kurki-Suonio, Acta Cryt. A37, 201 (1981)
    !!----    Input spherical coordinates Theta & Phi in degrees
    !!----
    !!---- 09/04/2019
    !!
    Elemental Module Function Real_Spher_Harm_Ang(l,m,p,theta,phi) result(ylmp)
       !---- Arguments ----!
       integer,      intent (in) :: l         ! Index l >= 0
       integer,      intent (in) :: m         ! Index m <= l
       integer,      intent (in) :: p         ! +1: cosinus, -1: sinus
       real(kind=cp),intent (in) :: theta     ! Spherical coordinate in degree
       real(kind=cp),intent (in) :: phi       ! Spherical coordinate in degree
       real(kind=cp)             :: ylmp

       !---- Local Variables ----!
       real(kind=dp)             :: norm
       real(kind=cp)             :: pphi,x
       integer                   :: i

       x=cos(theta*to_rad)
       if (p > 0) then
          pphi = cos(m*phi*to_rad)
       else
          pphi = sin(m*phi*to_rad)
       end if
       ylmp=poly_legendre(l,m,x)*pphi
       norm=real(2*l+1,kind=cp)

       do i=l-m+1,l+m
          norm=norm/real(i,kind=dp)
       end do
       if (m == 0) then
          norm=sqrt(norm/(4.0_dp*pi))
       else
          norm=sqrt(norm/(2.0_dp*pi))
       end if
       ylmp=norm*ylmp

    End Function Real_Spher_Harm_Ang

    !!----
    !!---- REAL_SPHER_HARM_UCVEC
    !!--<<
    !!----    Real Spherical Harmonics: M.Kara & K. Kurki-Suonio, Acta Cryt. A37, 201 (1981)
    !!----    Input U: unit vector in cartesian coordinates. If the provided vector is not
    !!----    normalized to unit, this version calculates ylmp along the unit vector defined by
    !!----    the given vector that is not modified on output
    !!-->>
    !!---- 09/04/2019
    !!
    Pure Module Function Real_Spher_Harm_Ucvec(l,m,p,u) result(ylmp)
       !---- Arguments ----!
       integer,                    intent (in) :: l,m,p
       real(kind=cp),dimension(3), intent (in) :: u
       real(kind=cp)                           :: ylmp

       !---- Local Variables ----!
       real(kind=dp)             :: norm
       real(kind=dp),parameter   :: sqrt_pi=1.7724538509055160272981674833411 !sqrt(pi)
       real(kind=cp)             :: pphi,x,ss
       integer                   :: i
       real(kind=cp),dimension(3):: v

       if(l == m .and. m == 0) then !for trivial zero-order give the value and return
         ylmp=0.5_dp/sqrt_pi
         return
       end if
       v=u
       ss=dot_product(v,v)
       if (abs(ss-1.0_cp) > 2.0_cp*epss) then  !Test the provided unit vector
          if(ss > epss) then
            v=v/sqrt(ss)             !Normalize the input vector
          else
            ylmp=0.0_cp
            return
          end if
       end if
       x=v(3)                    !costheta
       if (abs(x-1.0_cp) < epss .or. abs(x+1.0_cp) < epss) then
          pphi=0.0
       else
          pphi=atan2(v(2),v(1))  !This is the good function to obtain the true spherical angles
       end if
       if (p > 0) then
          pphi = cos(m*pphi)     !cos(m*phi)
       else
          pphi = sin(m*pphi)     !sin(m*phi)
       end if
       ylmp=poly_legendre(l,m,x)*pphi
       norm=real(2*l+1,kind=dp)

       do i=l-m+1,l+m  !this is not executed when l=m=0 !!!
          norm=norm/real(i,kind=dp)
       end do
       if (m == 0) then
          norm=sqrt(norm)/(2.0_dp*sqrt_pi)
       else
          norm=sqrt(norm/2.0_dp)/sqrt_pi
       end if
       ylmp=norm*ylmp

    End Function Real_Spher_Harm_Ucvec

    !!----
    !!---- REAL_SPHER_HARMCHARGE_UCVEC
    !!--<<
    !!----    This function difers from the previous one (real spherical harmonics) only in the
    !!----    normalization constants, so that: Dlmp= Clmp Ylmp
    !!----    The constants Clmp are selected so that Int[abs(Dlmp) dOmega] = 2 -d(l,0)
    !!----    They have been calculated using Gaussian/Legendre quadrature.
    !!-->>
    !!---- 09/04/2019
    !!
    Pure Module Function Real_Spher_HarmCharge_Ucvec(L,M,P,U) Result(Dlmp)
       !---- Arguments ----!
       integer,                    intent (in) :: l,m,p
       real(kind=cp),dimension(3), intent (in) :: u        ! unit vector in cartesian coordinates
       real(kind=cp)                           :: Dlmp

       !---- Local Variables ----!
       integer                   :: i
       real(kind=dp),dimension(0:10,0:10,2) :: Clmp

       if (l > 10 .or. m > 10 .or. m > l) then
          Dlmp=0.0_cp
          return
       end if

       !> Assign values to the normalization constants
       Clmp = 0.0
       Clmp(  0,  0,  1)=     0.28209481_dp
       Clmp(  1,  1,  2)=     0.65147001_dp
       Clmp(  1,  0,  1)=     0.65147001_dp
       Clmp(  1,  1,  1)=     0.65146995_dp
       Clmp(  2,  2,  2)=     0.68646848_dp
       Clmp(  2,  1,  2)=     0.68646836_dp
       Clmp(  2,  0,  1)=     0.65552908_dp
       Clmp(  2,  1,  1)=     0.68646836_dp
       Clmp(  2,  2,  1)=     0.68646848_dp
       Clmp(  3,  3,  2)=     0.71929127_dp
       Clmp(  3,  2,  2)=     0.69189525_dp
       Clmp(  3,  1,  2)=     0.70087820_dp
       Clmp(  3,  0,  1)=     0.65613419_dp
       Clmp(  3,  1,  1)=     0.70087814_dp
       Clmp(  3,  2,  1)=     0.69189525_dp
       Clmp(  3,  3,  1)=     0.71929145_dp
       Clmp(  4,  4,  2)=     0.74899846_dp
       Clmp(  4,  3,  2)=     0.70616245_dp
       Clmp(  4,  2,  2)=     0.69879586_dp
       Clmp(  4,  1,  2)=     0.70847464_dp
       Clmp(  4,  0,  1)=     0.65620995_dp
       Clmp(  4,  1,  1)=     0.70847464_dp
       Clmp(  4,  2,  1)=     0.69879586_dp
       Clmp(  4,  3,  1)=     0.70616263_dp
       Clmp(  4,  4,  1)=     0.74899834_dp
       Clmp(  5,  5,  2)=     0.77591366_dp
       Clmp(  5,  4,  2)=     0.72266090_dp
       Clmp(  5,  3,  2)=     0.70547515_dp
       Clmp(  5,  2,  2)=     0.70407319_dp
       Clmp(  5,  1,  2)=     0.71306634_dp
       Clmp(  5,  0,  1)=     0.65617186_dp
       Clmp(  5,  1,  1)=     0.71306634_dp
       Clmp(  5,  2,  1)=     0.70407319_dp
       Clmp(  5,  3,  1)=     0.70547533_dp
       Clmp(  5,  4,  1)=     0.72266084_dp
       Clmp(  5,  5,  1)=     0.77591383_dp
       Clmp(  6,  6,  2)=     0.80047959_dp
       Clmp(  6,  5,  2)=     0.73945135_dp
       Clmp(  6,  4,  2)=     0.71554828_dp
       Clmp(  6,  3,  2)=     0.70703226_dp
       Clmp(  6,  2,  2)=     0.70801049_dp
       Clmp(  6,  1,  2)=     0.71609598_dp
       Clmp(  6,  0,  1)=     0.65610999_dp
       Clmp(  6,  1,  1)=     0.71609592_dp
       Clmp(  6,  2,  1)=     0.70801049_dp
       Clmp(  6,  3,  1)=     0.70703244_dp
       Clmp(  6,  4,  1)=     0.71554822_dp
       Clmp(  6,  5,  1)=     0.73945147_dp
       Clmp(  6,  6,  1)=     0.80047959_dp
       Clmp(  7,  7,  2)=     0.82308108_dp
       Clmp(  7,  6,  2)=     0.75586903_dp
       Clmp(  7,  5,  2)=     0.72696197_dp
       Clmp(  7,  4,  2)=     0.71344930_dp
       Clmp(  7,  3,  2)=     0.70896560_dp
       Clmp(  7,  2,  2)=     0.71099448_dp
       Clmp(  7,  1,  2)=     0.71822095_dp
       Clmp(  7,  0,  1)=     0.65604913_dp
       Clmp(  7,  1,  1)=     0.71822089_dp
       Clmp(  7,  2,  1)=     0.71099448_dp
       Clmp(  7,  3,  1)=     0.70896578_dp
       Clmp(  7,  4,  1)=     0.71344924_dp
       Clmp(  7,  5,  1)=     0.72696209_dp
       Clmp(  7,  6,  1)=     0.75586903_dp
       Clmp(  7,  7,  1)=     0.82308108_dp
       Clmp(  8,  8,  2)=     0.84402764_dp
       Clmp(  8,  7,  2)=     0.77168250_dp
       Clmp(  8,  6,  2)=     0.73882592_dp
       Clmp(  8,  5,  2)=     0.72154719_dp
       Clmp(  8,  4,  2)=     0.71311980_dp
       Clmp(  8,  3,  2)=     0.71081281_dp
       Clmp(  8,  2,  2)=     0.71331161_dp
       Clmp(  8,  1,  2)=     0.71977973_dp
       Clmp(  8,  0,  1)=     0.65599412_dp
       Clmp(  8,  1,  1)=     0.71977967_dp
       Clmp(  8,  2,  1)=     0.71331161_dp
       Clmp(  8,  3,  1)=     0.71081293_dp
       Clmp(  8,  4,  1)=     0.71311975_dp
       Clmp(  8,  5,  1)=     0.72154731_dp
       Clmp(  8,  6,  1)=     0.73882592_dp
       Clmp(  8,  7,  1)=     0.77168250_dp
       Clmp(  8,  8,  1)=     0.84402764_dp
       Clmp(  9,  9,  2)=     0.86356676_dp
       Clmp(  9,  8,  2)=     0.78682709_dp
       Clmp(  9,  7,  2)=     0.75072247_dp
       Clmp(  9,  6,  2)=     0.73046678_dp
       Clmp(  9,  5,  2)=     0.71902418_dp
       Clmp(  9,  4,  2)=     0.71349597_dp
       Clmp(  9,  3,  2)=     0.71247119_dp
       Clmp(  9,  2,  2)=     0.71514851_dp
       Clmp(  9,  1,  2)=     0.72096473_dp
       Clmp(  9,  0,  1)=     0.65595061_dp
       Clmp(  9,  1,  1)=     0.72096467_dp
       Clmp(  9,  2,  1)=     0.71514851_dp
       Clmp(  9,  3,  1)=     0.71247137_dp
       Clmp(  9,  4,  1)=     0.71349585_dp
       Clmp(  9,  5,  1)=     0.71902430_dp
       Clmp(  9,  6,  1)=     0.73046678_dp
       Clmp(  9,  7,  1)=     0.75072247_dp
       Clmp(  9,  8,  1)=     0.78682709_dp
       Clmp(  9,  9,  1)=     0.86356682_dp
       Clmp( 10, 10,  2)=     0.88188988_dp
       Clmp( 10,  9,  2)=     0.80130792_dp
       Clmp( 10,  8,  2)=     0.76244813_dp
       Clmp( 10,  7,  2)=     0.73974365_dp
       Clmp( 10,  6,  2)=     0.72590035_dp
       Clmp( 10,  5,  2)=     0.71787411_dp
       Clmp( 10,  4,  2)=     0.71415150_dp
       Clmp( 10,  3,  2)=     0.71390396_dp
       Clmp( 10,  2,  2)=     0.71663243_dp
       Clmp( 10,  1,  2)=     0.72189075_dp
       Clmp( 10,  0,  1)=     0.65590489_dp
       Clmp( 10,  1,  1)=     0.72189069_dp
       Clmp( 10,  2,  1)=     0.71663243_dp
       Clmp( 10,  3,  1)=     0.71390414_dp
       Clmp( 10,  4,  1)=     0.71415144_dp
       Clmp( 10,  5,  1)=     0.71787423_dp
       Clmp( 10,  6,  1)=     0.72590035_dp
       Clmp( 10,  7,  1)=     0.73974365_dp
       Clmp( 10,  8,  1)=     0.76244813_dp
       Clmp( 10,  9,  1)=     0.80130792_dp
       Clmp( 10, 10,  1)=     0.88188988_dp
       i=p
       if( i < 1) i = 2

       Dlmp=Clmp(l,m,i)*Real_Spher_Harm_Ucvec(L,M,P,U)

    End Function Real_Spher_HarmCharge_Ucvec

    !!----
    !!---- PIKOUT_LJ_CUBIC
    !!--<<
    !!----    Picking out rules for indices of cubic harmonics for the 5 cubic groups
    !!----    Only up to tenth order Given in Table 4 of reference M.Kara &
    !!----    K. Kurki-Suonio, Acta Cryt. A37, 201 (1981)
    !!-->>
    !!---- 09/04/2019
    !!
    Pure Module Subroutine Pikout_Lj_Cubic(Group,Lj,Ncoef)
       !---- Arguments ----!
       character(len=*),         intent(in)  :: group
       integer, dimension(2,11), intent(out) :: lj
       integer,                  intent(out) :: ncoef

       !> Init
       lj=0
       ncoef=0

       Select case (group)
          Case("23")
             ncoef=11
             lj(1,1) =0
             lj(2,1) =1
             lj(1,2) =3
             lj(2,2) =1
             lj(1,3) =4
             lj(2,3) =1
             lj(1,4) =6
             lj(2,4) =1
             lj(1,5) =6
             lj(2,5) =2
             lj(1,6) =7
             lj(2,6) =1
             lj(1,7) =8
             lj(2,7) =1
             lj(1,8) =9
             lj(2,8) =1
             lj(1,9) =9
             lj(2,9) =2
             lj(1,10)=10
             lj(2,10)=1
             lj(1,11)=10
             lj(2,11)=2

          Case("m3","m-3")
             ncoef=7
             lj(1,1) =0
             lj(2,1) =1
             lj(1,2) =4
             lj(2,2) =1
             lj(1,3) =6
             lj(2,3) =1
             lj(1,4) =6
             lj(2,4) =2
             lj(1,5) =8
             lj(2,5) =1
             lj(1,6) =10
             lj(2,6) =1
             lj(1,7) =10
             lj(2,7) =2

          Case("432")
             ncoef=6
             lj(1,1) =0
             lj(2,1) =1
             lj(1,2) =4
             lj(2,2) =1
             lj(1,3) =6
             lj(2,3) =1
             lj(1,4) =8
             lj(2,4) =1
             lj(1,5) =9
             lj(2,5) =2
             lj(1,6) =10
             lj(2,6) =1

          Case("-43m")
             ncoef=8
             lj(1,1) =0
             lj(2,1) =1
             lj(1,2) =3
             lj(2,2) =1
             lj(1,3) =4
             lj(2,3) =1
             lj(1,4) =6
             lj(2,4) =1
             lj(1,5) =7
             lj(2,5) =1
             lj(1,6) =8
             lj(2,6) =1
             lj(1,7) =9
             lj(2,7) =1
             lj(1,8) =10
             lj(2,8) =1

          case("m3m","m-3m")
             ncoef=5
             lj(1,1) =0
             lj(2,1) =1
             lj(1,2) =4
             lj(2,2) =1
             lj(1,3) =6
             lj(2,3) =1
             lj(1,4) =8
             lj(2,4) =1
             lj(1,5) =10
             lj(2,5) =1
       end select

    End Subroutine Pikout_Lj_Cubic

 End Submodule Maths_Spher_Harm




