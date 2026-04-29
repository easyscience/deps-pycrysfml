!!----
!!----
!!----
!!----
SubModule (CFML_ExtinCorr) Ext_BeckerCoppens
  implicit none
   Contains

   !!----
   !!---- BECKER_COPPENS
   !!----
   !!----   The secondary extinction correction is calculated according to the equations developped
   !!----   by Becker&Coppens in Acta Cryst A30, 129 (1974).
   !!----   The fortran code (name of variables) is inspired from the CCSL extinction calculations.
   !!----   The coefficients of the Becker-Coppens correction for secondary extinctions are the
   !!----   same as those used in CCSL (P.J. Brown and J.C. Matthewman):
   !!----   The following coefficients are provided externally to the subroutine
   !!----   using the functions of this module A(Theta) and B(Theta) defined above
   !!----
   !!----    cext(1) = !Tbar*1000*Lambda^3/(V^2 sin2t) (CW)
   !!----            = !Tbar*1000*Lambda^4/(V^2 sint^2) (TOF)
   !!----    cext(2) = Lambda/sin2t (CW) (Lambda/sint)**2 (TOF)
   !!----    cext(3) = A(Theta)
   !!----    cext(4) = B(Theta)
   !!----
   !!----    The free parameters of the model are "r" and "g"
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Becker_Coppens(iext,f2,cext,r,g,ys,dydr,dydg)
      !---- Arguments ----!
      integer,                     intent (in) :: iext       !2: Gaussian, 3: Lorentzian
      real(kind=cp),               intent (in) :: f2         !Square of structure factor
      real(kind=cp), dimension(4), intent (in) :: cext       !coefficients for Becker-Coppens correction
      real(kind=cp),               intent (in) :: r,g        !Radius of blocks, width of mosaic distribution
      real(kind=cp),               intent(out) :: ys         !Extinction correction factor: Icorr = I.ys
      real(kind=cp), optional,     intent(out) :: dydr       !Derivative of "ys" w.r.t "r"
      real(kind=cp), optional,     intent(out) :: dydg       !Derivative of "ys" w.r.t "g"

      !---- Local variables ----!
      real(kind=cp) :: a, b, c, d, h, factor, x, x2, xx, c4, yy, der

      a = 1.5*r/cext(2)     ! r = domr in Jane's notation
                            ! a = 3/2 * r * sin2t/L = <alpha>
                            ! <alpha>= l* sin2t/L =>   l = 3/2 * r
      b = cext(1)*f2/1.5    ! b = 2/3*1000*L^3/(sin2t* V^2)*F^2
      c = 1.5*g             ! g = amosc in Jane's notation
      h = 2.0*g*g

      if (iext == 2) then          !Gaussian
         d=1.0/sqrt(1.0+ a*a /h)
      else if(iext == 3) then     !Lorentzian
         d=1.0/(1.0+a/c)
      end if

      x=b*a*d
      x2=2.0*x
      xx=x*x
      c4=1.0+cext(4)*x
      yy=1.0/(1.0+x2+cext(3)*xx/c4)
      ys=sqrt(yy)
      if (iext == 2) then          !Gaussian
         factor=a*d/h
      else if(iext == 3) then     !Lorentzian
         factor=1.0/c
      end if
      if (present(dydr) .and. present(dydg)) then
         der=-yy*ys*(1.0+cext(3)*(1.0-0.5*xx*cext(4)/c4)/c4)
         dydr = der * x*(1.0-a*d*factor)/r
         dydg = der * 1.5*factor*x*d*a/c
      end if

   End Subroutine Becker_Coppens

End SubModule Ext_BeckerCoppens