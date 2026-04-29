!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Tof_Jorg_Vondreele
  implicit none
   Contains
   !!----
   !!---- TOF_JORGENSEN_VONDREELE
   !!----
   !!----    Calculate de Profile of TOF according to Jorgensen_Vondreele
   !!--..    Authors:J. Rodriguez-Carvajal and Laurent C Chapon
   !!----
   !!---- 21/04/2019
   !!
   Module Subroutine Tof_Jorgensen_Vondreele(Dt,Alfa,Beta,gamm,Eta,Tof_Peak,Deriv)
      !---- Arguments ----!
      real(kind=cp),             intent( in) :: dt       ! dt = TOF(channel i) -TOF(Bragg position)
      real(kind=cp),             intent( in) :: alfa     !  alfa  : units microsecs-1
      real(kind=cp),             intent( in) :: beta     !  beta  : units microsecs-1
      real(kind=cp),             intent( in) :: gamm     !  gamm  : units microsecs
      real(kind=cp),             intent( in) :: eta      !  eta   : mixing coefficient calculated using TCH
      real(kind=cp),             intent(out) :: tof_peak
      type(Deriv_TOF_Type), optional, intent(out) :: deriv    ! present if derivatives are to be calculated

      !---- local variables ----!
      complex(kind=dp):: z1,z2,fz1,fz2
      real(kind=dp)   :: u,v,expu,expv,a, b, erfy, erfz, y,z
      real(kind=dp)   :: norm, deno,denoinv,ca,cb,a2,b2,d3, sigma
      real(kind=dp)   :: omeg, omel, afpbet,oml_a,oml_b,one_e, &
                         erfyp,erfzp,domg_t,domg_a,domg_b,domg_g,exper1,exper2,  &
                         doml_t,doml_a,doml_b,doml_g,omega,omegb
      integer         :: i, udiv, vdiv

      sigma=gamm*gamm*inv_8ln2
      u=0.5*alfa*(alfa*sigma+2.0*dt)
      v=0.5*beta*(beta*sigma-2.0*dt)
      !> To avoid pathological behaviour EXP(U) is calculated as
      !> {EXP(U/N)}^N using mutiplicative loops
      udiv=max(1,nint(exp_m*u))
      vdiv=max(1,nint(exp_m*v))
      u=u/udiv
      v=v/vdiv
      afpbet=alfa+beta
      norm=0.5*alfa*beta/afpbet
      deno=SQRT(2.0_dp*sigma)
      denoinv=1.0_dp/deno
      y=(alfa*sigma+dt)*denoinv
      z=(beta*sigma-dt)*denoinv

      expu=exp(u)
      erfy=erfc(y)              !error function of y
      omega=expu*erfy
      do i=1,udiv-1
         omega=omega*expu
      end do
      expv=exp(v)
      erfz=erfc(z)             !error function of z
      omegb=expv*erfz
      do i=1,vdiv-1
         omegb=omegb*expv
      end do

      omeg=norm*(omega+omegb)    !Gaussian contribution

      if (lorcomp) then
         z1=CMPLX( alfa*dt,0.5_dp*alfa*gamm,kind=dp)
         z2=CMPLX(-beta*dt,0.5_dp*beta*gamm,kind=dp)
         fz1=expi_e1(z1)                  ! exp(p).E1(p)
         fz2=expi_e1(z2)                  ! exp(q).E1(q)
         oml_a=-AIMAG(fz1)*two_over_pi    ! OmL,alfa
         oml_b=-AIMAG(fz2)*two_over_pi    ! OmL,beta
         omel= norm*(oml_a+oml_b)         ! OmL = OmL,alfa + OmL,beta (Lorentzian contribution)
         one_e=1.0-eta
         tof_peak = one_e * omeg + eta * omel
      else
         tof_peak = omeg
      end if

      ! Derivatives
      if (.not. present(deriv)) return

      ! Derivatives of Omega(Gaussian)
      a2=alfa*alfa
      b2=beta*beta
      d3= 2.0_dp*(denoinv*denoinv*denoinv)
      ca=d3*(alfa*sigma-dt)
      cb=d3*(beta*sigma+dt)

      if (omeg <= 1.e-30) then
         domg_t= 0.0           ! DOmG/Ddt
         domg_a= 0.0           ! DOmG/Dalfa
         domg_b= 0.0           ! DOmG/Dbeta
         domg_g= 0.0           ! DOmG/Dgamm
      else
         erfyp=erfc_deriv(real(y,kind=cp))         ! erfcc'(y)
         erfzp=erfc_deriv(real(z,kind=cp))         ! erfcc'(z)
         a=expu*erfyp
         b=expv*erfzp
         do i=1,udiv-1
            a=a*expu              !a=exp(u)*erfcc'(y)
         end do
         do i=1,vdiv-1
            b=b*expv              !b=exp(v)*erfcc'(z)
         end do
         domg_t = norm*(alfa*omega-beta*omegb+(a-b)*denoinv)
         domg_a = norm*(2.0_dp*omeg/a2+deno*(y*omega+0.5_dp*a))
         domg_b = norm*(2.0_dp*omeg/b2+deno*(z*omegb+0.5_dp*b))

         !>Multiply by Dsigma/Dgamm=gamm/4ln2
         domg_g = inv_8ln2 * norm * gamm * (a2*omega+b2*omegb+a*ca+b*cb)
      end if

      if (lorcomp) then

         ! Derivatives of Omega(Lorentzian)
         exper1=-REAL(fz1)*two_over_pi
         exper2=-REAL(fz2)*two_over_pi
         doml_t= norm*(alfa* oml_a - beta* oml_b)                               ! DOmL/Ddt
         doml_a= norm*( 2.0_dp * omel/a2 + dt* oml_a + 0.5_dp * gamm * exper1) ! DOmL/Dalfa
         doml_b= norm*( 2.0_dp * omel/b2 - dt* oml_b + 0.5_dp * gamm * exper2) ! DOmL/Dbeta
         doml_g= 0.5 * norm * ( alfa*exper1 + beta*exper2 )                     ! DOmL/Dgamm

         ! Total derivatives
         deriv%dt    = one_e * domg_t + eta * doml_t
         deriv%alfa  = one_e * domg_a + eta * doml_a
         deriv%beta  = one_e * domg_b + eta * doml_b
         deriv%gamm = one_e * domg_g + eta * doml_g
         deriv%eta   = omel-omeg
      else
         deriv%dt    = domg_t
         deriv%alfa  = domg_a
         deriv%beta  = domg_b
         deriv%gamm = domg_g
         deriv%eta   = 0.0
      end if
      deriv%sigma = deriv%gamm/(2.0_dp*inv_8ln2*gamm)

   End Subroutine Tof_Jorgensen_Vondreele

End SubModule Profile_Tof_Jorg_Vondreele