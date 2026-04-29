!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Tof_Jorgensen
  implicit none
   Contains
   !!----
   !!---- TOF_JORGENSEN
   !!----
   !!----    Calculate de Profile of TOF according to Jorgensen
   !!--..    Authors:J. Rodriguez-Carvajal and Laurent C Chapon
   !!----
   !!---- 21/04/2019
   !!
   Module Subroutine Tof_Jorgensen(Dt,Alfa,Beta,Sigma,Tof_Peak,Deriv)
      !---- Arguments ----!
      real(kind=cp),                  intent( in)  :: dt       !  dt = TOF(channel i) -TOF(Bragg position): units microsecs
      real(kind=cp),                  intent( in)  :: alfa     !  alfa  : units microsecs-1
      real(kind=cp),                  intent( in)  :: beta     !  beta  : units microsecs-1
      real(kind=cp),                  intent( in)  :: sigma    !  sigma : units microsecs**2
      real(kind=cp),                  intent(out)  :: tof_peak
      type(Deriv_TOF_Type), optional, intent(out)  :: deriv    ! present if derivatives are to be calculated

      !---- Local Variables ----!
      integer        :: i, udiv, vdiv
      real(kind=dp)  :: u,v,y,z,expu,expv,erfy,erfz,a,b,omega,omegb
      real(kind=dp)  :: norm, deno, denoinv,ca,cb,a2,b2,d3,omeg
      real(kind=dp)  :: afpbet,erfyp,erfzp

      u=0.5*alfa*(alfa*sigma+2.0*dt)
      v=0.5*beta*(beta*sigma-2.0*dt)

      !> To avoid pathological behaviour EXP(U) is calculated as
      !> {EXP(U/N)}^N using mutiplicative loops
      udiv=max(1,nint(exp_m*u))   !udiv=max(1,int(2.0*u))
      vdiv=max(1,nint(exp_m*v))   !vdiv=max(1,int(2.0*v))
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
      omeg=norm*(omega+omegb)
      tof_peak=omeg

      ! Derivatives
      if(.not. present(deriv)) return

      ! Derivatives of Omega(Gaussian)
      if (omeg <= 1.0e-30) then
         deriv%dt   = 0.0          ! DOmeG/Ddt
         deriv%alfa = 0.0          ! DOmeG/Dalfa
         deriv%beta = 0.0          ! DOmeG/Dbeta
         deriv%sigma= 0.0          ! DOmeG/Dsigma
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
         a2=alfa*alfa
         b2=beta*beta
         d3= 2.0_dp*(denoinv*denoinv*denoinv)
         ca=d3*(alfa*sigma-dt)
         cb=d3*(beta*sigma+dt)
         deriv%dt    = norm*(alfa*omega-beta*omegb+(a-b)*denoinv)
         deriv%alfa  = norm*(2.0_dp*omeg/a2+deno*(y*omega+0.5_dp*a))
         deriv%beta  = norm*(2.0_dp*omeg/b2+deno*(z*omegb+0.5_dp*b))
         deriv%sigma = 0.5_dp*norm*(a2*omega+b2*omegb+a*ca+b*cb)
         deriv%gamm = 2.0_dp*deriv%sigma*sqrt(sigma*inv_8ln2)
      end if

   End Subroutine Tof_Jorgensen
End SubModule Profile_Tof_Jorgensen