!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Tof_Carpenter
  implicit none
   Contains
   !!----
   !!---- TOF_CARPENTER
   !!----
   !!----    Calculate de Profile of TOF according to Carpenter
   !!--..    Author:Laurent C Chapon
   !!----
   !!---- 21/04/2019
   !!
   Module Subroutine Tof_Carpenter(Dt,D,Alfa,Beta,gamm,Eta,Kappa,Tof_Theta,Tof_Peak,Deriv)
      !---- Arguments ----!
      real(kind=cp),             intent( in) :: dt        ! dt = TOF(channel i) -TOF(Bragg position)
      real(kind=cp),             intent( in) :: d         ! d-spacing of the peak in A
      real(kind=cp),             intent( in) :: alfa      !  alfa  : units microsecs-1
      real(kind=cp),             intent( in) :: beta      !  beta  : units microsecs-1
      real(kind=cp),             intent( in) :: gamm      !  gamm  : units microsecs
      real(kind=cp),             intent( in) :: eta       !  eta   : mixing coefficient calculated using TCH
      real(kind=cp),             intent( in) :: kappa     ! Mixing coeficient of the Ikeda-Carpenter function
      real(kind=cp),             intent( in) :: tof_theta ! This is the value of 2sin(theta)
      real(kind=cp),             intent(out) :: tof_peak
      type(Deriv_TOF_Type), optional, intent(out) :: deriv     ! present if derivatives are to be calculated

      !---- local variables ----!
      integer          :: i,udiv,vdiv,sdiv,rdiv
      complex(kind=dp) :: zu,zv,zs,zr,fzu,fzv,fzs,fzr
      real(kind=dp)    :: lambda,sigma,R,Nu,Nv,Ns,Nr,deno,denoinv,ki,alfa_min,alfa_plu, &
                          xik,yik,zik,norm,g_u,g_v,g_s,g_r,y_u,y_v,y_s,y_r, yy,yy2,&
                          expu,expv,exps,expr,erfu,erfv,erfs,erfr,omg, &
                          oml_u,oml_v,oml_s,oml_r,one_e,oml, &
                          omg_u,omg_v,omg_s,omg_r,erfpu,erfpv,erfps,erfpr,a_u,a_v,a_s,a_r, &
                          domg_t, domg_a, domg_b, domg_g, domg_k, doml_t, doml_a, &
                          doml_b, doml_g, doml_k, dnuda,dnudb,dnvda,dnvdb,dnsda,dnsdb,dnrda,dnrdb, &
                          expru,exprv,exprs,exprr,im_const,re_const

      !> Definitions of all parameters for the Gaussian part :
      !> Define sigma with respect to gamm (sigma is the variance of sigma)
      sigma=gamm*gamm*INV_8LN2
      lambda=d*tof_theta
      R=exp(-81.799_dp/(kappa*lambda*lambda))
      deno=SQRT(2.0_dp*sigma)
      denoinv=1.0_dp/deno
      ki=0.05_dp
      alfa_min=alfa*(1.0_dp-ki)
      alfa_plu=alfa*(1.0_dp+ki)
      xik=alfa_min-beta
      yik=alfa-beta
      zik=alfa_plu-beta
      norm=0.25_dp*alfa*(1-ki*ki)/ki/ki
      Nu=1.0_dp-(R*alfa_min/xik)
      Nv=1.0_dp-(R*alfa_plu/zik)
      Ns=-2.0_dp*(1-R*alfa/yik)
      Nr=2.0_dp*R*alfa*alfa*beta*ki*ki/xik/yik/zik
      y_u=(alfa_min*sigma-dt)*denoinv
      y_v=(alfa_plu*sigma-dt)*denoinv
      y_s=(alfa*sigma-dt)*denoinv
      y_r=(beta*sigma-dt)*denoinv
      g_u=0.5_dp*alfa_min*(alfa_min*sigma-2.0*dt)
      g_v=0.5_dp*alfa_plu*(alfa_plu*sigma-2.0*dt)
      g_s=0.5_dp*alfa*(alfa*sigma-2.0*dt)
      g_r=0.5_dp*beta*(beta*sigma-2.0*dt)
      udiv=max(1,nint(exp_m*g_u))
      vdiv=max(1,nint(exp_m*g_v))
      sdiv=max(1,nint(exp_m*g_s))
      rdiv=max(1,nint(exp_m*g_r))
      g_u=g_u/udiv
      g_v=g_v/vdiv
      g_s=g_s/sdiv
      g_r=g_r/rdiv
      !> End of definitions

      expu=exp(g_u)
      expv=exp(g_v)
      exps=exp(g_s)
      expr=exp(g_r)
      erfu=erfc(y_u)
      erfv=erfc(y_v)
      erfs=erfc(y_s)
      erfr=erfc(y_r)

      if (y_u < 27.0_dp) then      ! LCC , november 2003: Modifications to the original code
         omg_u=expu*erfu      ! to solve underflow problems occuring
         do i=1, udiv-1       ! when erfc(y_*) becomes too small and exp(g_*) too large,
            omg_u=omg_u*expu  ! --> Use an approximation at third order to erfc.
         end do
      else
         yy=y_u*y_u
         yy2=2.0_dp*yy
         omg_u=exp(g_u*udiv-yy)/(sqrt(2.0_dp/TWO_OVER_PI)*y_u)* &
         (1.0_dp-1.0_dp/yy2+3.0_dp/(yy2*yy2)+15.0_dp/(yy2*yy2*yy2))
      end if

      if (y_v < 27.0_dp) then
         omg_v=expv*erfv
         do i=1, vdiv-1
            omg_v=omg_v*expv
         end do
      else
         yy=y_v*y_v
         yy2=2.0_dp*yy
         omg_v=exp(g_v*vdiv-yy)/(sqrt(2.0_dp/TWO_OVER_PI)*y_v)* &
         (1.0_dp-1.0_dp/yy2+3.0_dp/yy2*yy2+15.0_dp/(yy2*yy2*yy2))
      end if

      if (y_s < 27.0_dp) then
         omg_s=exps*erfs
         do i=1, sdiv-1
            omg_s=omg_s*exps
         end do
      else
         yy=y_s*y_s
         yy2=2.0_dp*yy
         omg_s=exp(g_s*sdiv-yy)/(sqrt(2.0_dp/two_over_pi)*y_s)* &
              (1.0_dp-1.0_dp/yy2+3.0_dp/yy2*yy2+15.0_dp/(yy2*yy2*yy2))
      end if

      if (y_r < 27.0_dp) then
         omg_r=expr*erfr
         do i=1, rdiv-1
            omg_r=omg_r*expr
         end do
      else
         yy=y_r*y_r
         yy2=2.0_dp*yy
         omg_r=exp(g_r*rdiv-yy)/(sqrt(2.0_dp/two_over_pi)*y_r)* &
              (1.0_dp-1.0_dp/yy2+3.0_dp/yy2*yy2+15.0_dp/(yy2*yy2*yy2))
      end if

      omg=(Nu*omg_u+Nv*omg_v+Ns*omg_s+Nr*omg_r) ! End of gaussian part of the function

      if (lorcomp) then
         zs=CMPLX(-alfa*dt,0.5_dp*alfa*gamm,kind=dp)
         zu=(1.0_dp-ki)*zs
         zv=(1.0_dp+ki)*zs
         zr=CMPLX(-beta*dt,0.5_dp*beta*gamm,kind=dp)
         fzu=expi_e1(zu)
         fzv=expi_e1(zv)
         fzs=expi_e1(zs)
         fzr=expi_e1(zr)
         oml_u=-AIMAG(fzu)*TWO_OVER_PI
         oml_v=-AIMAG(fzv)*TWO_OVER_PI
         oml_s=-AIMAG(fzs)*TWO_OVER_PI
         oml_r=-AIMAG(fzr)*TWO_OVER_PI
         oml=Nu*oml_u+Nv*oml_v+Ns*oml_s+Nr*oml_r   ! End of Lorentzian part of the function
         one_e=1.0-eta
         tof_peak=norm*(one_e*omg+eta*oml)         ! Total function
      else
         tof_peak=norm*omg
      end if

      !> Derivatives

      if(.not. present(deriv)) return

      ! Derivatives of Omega(Gaussian)

      if(omg <= 1.0E-35) then
         domg_t= 0.0           ! DOmG/Ddt
         domg_a= 0.0           ! DOmG/Dalfa
         domg_b= 0.0           ! DOmG/Dbeta
         domg_g= 0.0           ! DOmG/Dgamm
         domg_k= 0.0           ! DOmG/Dkappa
      else
         dnuda=R*beta/xik/xik*(1-ki)  ! Partial derivatives of Nu,Nv,Ns and Nr /dalpha, dbeta
         dnudb=-R*alfa_min/xik/xik
         dnvda=R*beta/zik/zik*(1+ki)
         dnvdb=-R*alfa_plu/zik/zik
         dnsda=-2*R*beta/yik/yik
         dnsdb=2*R*alfa/yik/yik
         dnrda=-2*R*alfa*beta*ki*ki*(alfa**3-3.0*alfa*beta**2-alfa**3*ki**2+2.0*beta**3)/xik**2/yik**2/zik**2
         dnrdb=-dnrda*alfa/beta
         erfpu=erfc_deriv(real(y_u,kind=cp))                         ! d(erfc(y_u))/dy_u
         erfpv=erfc_deriv(real(y_v,kind=cp))                         ! d(erfc(y_v))/dy_v
         erfps=erfc_deriv(real(y_s,kind=cp))                         ! d(erfc(y_s))/dy_s
         erfpr=erfc_deriv(real(y_r,kind=cp))                         ! d(erfc(y_r))/dy_r
         a_u=expu*erfpu
         a_v=expv*erfpv
         a_s=exps*erfps
         a_r=expr*erfpr
         do i=1, udiv-1
           a_u=a_u*expu
         end do
         do i=1, vdiv-1
           a_v=a_v*expv
         end do
         do i=1, sdiv-1
           a_s=a_s*exps
         end do
         do i=1, rdiv-1
           a_r=a_r*expr
         end do
         domg_t=-Nu*alfa_min*omg_u-Nv*alfa_plu*omg_v-Ns*alfa*omg_s-Nr*beta*omg_r -  &
                 (Nu*a_u+Nv*a_v+Ns*a_s+Nr*a_r)*denoinv
         domg_a= dnuda*omg_u+dnvda*omg_v+dnsda*omg_s+dnrda*omg_r + &
                 deno*(Nu*(1-ki)*(y_u*omg_u+0.5_dp*a_u)+Nv*(1+ki)*(y_v*omg_v+0.5_dp*a_v)+Ns*(y_s*omg_s+0.5_dp*a_s))
         domg_b= dnudb*omg_u+dnvdb*omg_v+dnsdb*omg_s+dnrdb*omg_r+deno*Nr*(y_r*omg_r+0.5_dp*a_r)
         domg_g= inv_8ln2*gamm*(Nu*alfa_min**2*omg_u+Nv*alfa_plu**2*omg_v+Ns*alfa**2*omg_s+Nr*beta**2*omg_r) + &
                 2.0_dp*inv_8ln2*gamm*denoinv**3*(Nu*(alfa_min*sigma+dt)*a_u+Nv*(alfa_plu*sigma+dt)*a_v+          &
                 Ns*(alfa*sigma+dt)*a_s+Nr*(beta*sigma+dt)*a_r)
         domg_k= 81.799_dp/kappa**2/lambda**2*R*(-alfa_min/xik*omg_u-alfa_plu/zik*omg_v+2.0_dp*alfa/yik*omg_s+     &
                 2.0_dp*alfa**2*beta*ki**2/xik/yik/zik*omg_r)
      end if

      if (lorcomp) then

         !> Derivatives of Omega(Lorentzian)
         im_const=AIMAG(alfa/zs)
         re_const=REAL(alfa/zs)
         expru=REAL(fzu)
         exprv=REAL(fzv)
         exprs=REAL(fzs)
         exprr=REAL(fzr)
         doml_t=-Nu*alfa_min*oml_u-Nv*alfa_plu*oml_v-Ns*alfa*oml_s-Nr*beta*oml_r-two_over_pi*im_const*(Nu+Nv+Ns+Nr)
         doml_a=dnuda*oml_u+dnvda*oml_v+dnsda*oml_s+dnrda*oml_r+Nu*(1.0_dp-ki)*(-dt*oml_u-0.5_dp*two_over_pi*gamm*expru)+ &
                Nv*(1.0+ki)*(-dt*oml_v-0.5*two_over_pi*gamm*exprv)+Ns*(-dt*oml_s-0.5_dp*two_over_pi*gamm*exprs)
         doml_b=dnudb*oml_u+dnvdb*oml_v+dnsdb*oml_s+dnrdb*oml_r+Nr*(-dt*oml_r-0.5_dp*two_over_pi*gamm*exprr)
         doml_g=0.5_dp*two_over_pi*(-Nu*alfa_plu*expru-Nv*alfa_min*exprv-Ns*alfa*exprs-Nr*beta*exprr+(Nu+Nv+Ns+Nr)*re_const)
         doml_k=81.799_dp/kappa**2/lambda**2*R*(-alfa_min/xik*oml_u-alfa_plu/zik*oml_v+2.0_dp*alfa/yik*oml_s+  &
                2.0_dp*alfa**2*beta*ki**2/xik/yik/zik*oml_r)

         !> Total derivatives

         deriv%dt    = norm*(one_e*domg_t+eta*doml_t)
         deriv%alfa  = tof_peak/alfa+norm*(one_e*domg_a+eta*doml_a)
         deriv%beta  = norm*(one_e*domg_b+eta*doml_b)
         deriv%gamm = norm*(one_e*domg_g+eta*doml_g)
         deriv%kappa = norm*(one_e*domg_k+eta*doml_k)
         deriv%eta   = norm*(oml-omg)

      else

         deriv%dt    = norm*domg_t
         deriv%alfa  = tof_peak/alfa+norm*domg_a
         deriv%beta  = norm*domg_b
         deriv%gamm = norm*domg_g
         deriv%kappa = norm*domg_k
         deriv%eta   = 0.0
      end if
      deriv%sigma = deriv%gamm/(2.0_dp*inv_8ln2*gamm)

   End Subroutine Tof_Carpenter

End SubModule Profile_Tof_Carpenter