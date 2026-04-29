!!----
!!----
!!----
!!----
SubModule (CFML_ExtinCorr) Ext_FlippingRatios
  implicit none
   Contains

   !!----
   !!---- CORRECT_FLIPPINGRATIOS
   !!----
   !!---- Extiction correction for flipping ratios (neutrons)
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Correct_FlippingRatios(iext,Lambda,q,extc,ssnn,hkl,AN,BN,AM,BM,yp,ym,ypm,dyp,dym,dypm,dymag)
      !---- Arguments ----!
      integer,                      intent(in)  :: iext
      real(kind=cp),                intent(in)  :: lambda, q !Q=sin^2(alpha), alpha:angle between z and Q
      real(kind=cp), dimension(6),  intent(in)  :: extc
      real(kind=cp),                intent(in)  :: ssnn
      real(kind=cp), dimension(3),  intent(in)  :: hkl
      real(kind=cp),                intent(in)  :: AN, BN, AM, BM    ! real and imaginary parts of
                                                                     ! nuclear and magnetic structure factor
      real(kind=cp),                intent(out) :: yp, ym, ypm       ! Extinction corrections to I++
                                                                     ! I-- and I+- (=I-+)
      real(kind=cp), dimension(:), optional, intent(out) :: dyp, dym, dypm    ! Derivatives of the extinction correction
      real(kind=cp), dimension(:), optional, intent(out) :: dymag   !! stores the derivatives of
                                                                    !! yp, ym, and ypm w.r.t. AM and BM
                                                                    !! dymag(1)= dyp/dAM
                                                                    !! dymag(2)= dyp/dBM
                                                                    !! dymag(3)= dym/dAM etc...

      !---- Local Variables ----!
      real(kind=cp), parameter    :: epsil=0.001, cst=0.001
      real(kind=cp)               :: sin2t, a, b, c, r, g, dydr, dydg, f2
      real(kind=cp)               :: h2, k2, l2, hk, hl, kl,s2
      real(kind=cp), dimension(4) :: cext
      logical                     :: derivatives

      !> We initialize the three extinction param. to 1 and derivatives to zero
      yp=    1.0_cp
      ym=    1.0_cp
      ypm=   1.0_cp
      dyp=   0.0_cp
      dym=   0.0_cp
      dypm=  0.0_cp
      dymag= 0.0_cp
      derivatives=present(dyp) .and. present(dym) .and. present(dypm) .and. present(dymag)
      if (nint(sum(abs(hkl(:)))) == 0 ) return

      !>  Extinction corrections
      a = lambda**2

      !> s2=sin^2(theta)
      s2=ssnn*a

      !> a=Lambda^3
      a=a*lambda

      !> sin(2theta)
      sin2t=2.0_cp*sqrt(s2*(1.0_cp-s2))

      Select Case (iext)
         Case (1)   !Shelx-like extinction correction
            ! yp: f2=AN*AN+BN*BN+ 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN**2+BN**2+ Q*(2.0*(AN*AM+BN*BM)+(AM**2+BM**2))
            b = cst*f2*a/sin2t
            c = 1.0_cp + b * extc(1)

            !          if (c > 0) &
            yp=1.0/SQRT(c)
            if (derivatives) then
               dyp(1)= -0.5_cp*c**(-1.5)*b
               if (abs(f2) > epsil) then
                  dymag(1)= (yp**3-yp)*Q*(AN+AM)/f2
                  dymag(2)= (yp**3-yp)*Q*(BN+BM)/f2
               end if
            end if

            ! ym: f2=AN*AN+BN*BN- 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN**2+BN**2-Q*(2.0*(AN*AM+BN*BM)-(AM**2+BM**2))
            b = cst*f2*a/sin2t
            c = 1.0_cp + b * extc(1)

            !          if (c > 0) &
            ym=1.0_cp/SQRT(c)
            if (derivatives) then
               dym(1)= -0.5*c**(-1.5)*b
               if (abs(f2) > epsil) then
                  dymag(3)= (ym**3-ym)*Q*(AM-AN)/f2
                  dymag(4)= (ym**3-ym)*Q*(BM-BN)/f2
               end if
            end if

            ! ypm: f2= (1.0-q)*q*(AM**2+BM**2)
            f2= (1.0_cp-Q)*Q*(AM**2+BM**2)
            b = cst*f2*a/sin2t
            c = 1.0_cp + b * extc(1)

            !
            ypm=1.0_cp/SQRT(c)
            if (derivatives) then
               dypm(1)= -0.5*c**(-1.5)*b
               if (abs(f2) > epsil) then
                  dymag(5)= (yp**3-yp)*(1.0-Q)*Q*AM/f2
                  dymag(6)= (yp**3-yp)*(1.0-Q)*Q*BM/f2
               end if
            end if

         Case(2,3)  !Gaussian/Lorentzian Becker-Coppens extinction correction

            cext(1)=extc(3)  !Tbar*1000*Lambda^3/(V^2 sin2t)
            cext(2)=lambda/sin2t
            cext(3)=extc(4)  ! A(theta) Should be pre-calculated calling the appropriate function and stored in extc(4)
            cext(4)=extc(5)  ! B(theta)  idem
            r = extc(1)  !Extinction parameters
            g = extc(2)

            ! yp: f2= AN*AN+BN*BN+ 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN*AN+BN*BN+ 2.0*q*(AN*AM+BN*BM)+ q*(AM*AM+BM*BM)
            if (derivatives) then
               call Becker_Coppens(iext,f2,cext,r,g,yp,dydr,dydg)
               dyp(1)= dydr   !Derivative of Y w.r.t. r: Dy/Dr
               dyp(2)= dydg   !Derivative of Y w.r.t. g: Dy/Dg
            else
               Call Becker_Coppens(iext,f2,cext,r,g,yp)
            end if

            ! ym: f2= AN*AN+BN*BN- 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN*AN+BN*BN- 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            if (derivatives) then
               call Becker_Coppens (iext,f2,cext,r,g,ym,dydr,dydg)
               dym(1)= dydr   !Derivative of Y w.r.t. r: Dy/Dr
               dym(2)= dydg   !Derivative of Y w.r.t. g: Dy/Dg
            else
               Call Becker_Coppens(iext,f2,cext,r,g,ym)
            end if

            ! ypm: f2= (1.0-q)*q*(AM**2+BM**2)
            f2= (1.0-q)*q*(AM**2+BM**2)
            if (derivatives) then
               call Becker_Coppens(iext,f2,cext,r,g,ypm,dydr,dydg)
               dypm(1)= dydr   !Derivative of Y w.r.t. r: Dy/Dr
               dypm(2)= dydg   !Derivative of Y w.r.t. g: Dy/Dg
            else
               Call Becker_Coppens(iext,f2,cext,r,g,ypm)
            end if

         Case (4)   !Anisotropic Shelx-like extinction correction

            a = lambda*lambda*lambda
            sin2t=SIN(2.0*ASIN(SQRT(ssnn)*lambda))
            h2=hkl(1)**2
            k2=hkl(2)**2
            l2=hkl(3)**2
            hk=hkl(1)*hkl(2)
            hl=hkl(1)*hkl(3)
            kl=hkl(2)*hkl(3)
            r=extc(1)*h2+extc(2)*k2+ extc(3)*l2+ extc(4)*hk+extc(5)*hl+ extc(6)*kl

            ! yp: f2= AN*AN+BN*BN+ 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN*AN+BN*BN+ 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            b = 0.001*f2*a/sin2t    !b = 0.000001*ff(nn,n_pat)*a/sin2t
            b = b * 0.25 /ssnn
            c = 1.0 + b * r
            yp=1.0/SQRT(c)
            if (derivatives) then
               g=-0.5*c**(-1.5)*b !component of derivative
               dyp(:)= g*f2/yp *(/h2,k2,l2,hk,hl,kl/)
               if (f2 >= 0.00001) then
                  dymag(1:2)= -yp**3* b*r/f2*(/AM +AN*q +q*AM, BM +BN*q +q*BM/)
               end if
            end if

            ! ym: f2= AN*AN+BN*BN- 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            f2= AN*AN+BN*BN- 2.0*q*(AN*AM+BN*BM)+q*(AM*AM+BM*BM)
            b = 0.001*f2*a/sin2t    !b = 0.000001*ff(nn,n_pat)*a/sin2t
            b = b * 0.25 /ssnn
            c = 1.0 + b * r
            ym=1.0/SQRT(c)
            if (derivatives) then
               g=-0.5*c**(-1.5)*b !component of derivative
               dym(:)= g*f2/ym *(/h2,k2,l2,hk,hl,kl/)
               if (f2 >= 0.00001) then
                  dymag(3:4)= -ym**3* b*r/f2*(/AM-AN*q + q*AM, BM-BN*q +q*BM/)
               end if
            end if

            ! ypm: f2= (1.0-q)*q*(AM**2+BM**2)
            f2= (1.0-q)*q*(AM**2+BM**2)
            b = 0.001*f2*a/sin2t    !b = 0.000001*ff(nn,n_pat)*a/sin2t
            b = b * 0.25 /ssnn
            c = 1.0 + b * r
            ypm=1.0/SQRT(c)
            if (derivatives) then
               g=-0.5*c**(-1.5)*b !component of derivative
               dypm(:)= g*f2/ypm *(/h2,k2,l2,hk,hl,kl/)
               if (f2 >= 0.00001) then
                  dymag(3:4)= -ypm**3* b*r/f2*(/(1.0 -q) * q * AM, (1.0 -q) * q * BM /)
               end if
            end if

      End Select

   End Subroutine Correct_FlippingRatios

End SubModule Ext_FlippingRatios