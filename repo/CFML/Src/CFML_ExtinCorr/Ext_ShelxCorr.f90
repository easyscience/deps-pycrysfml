!!----
!!----
!!----
!!----
SubModule (CFML_ExtinCorr) Ext_ShelxCorr
  implicit none
   Contains
   !!----
   !!---- SHELX_EXTINCTION
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine SHELX_Extinction(job,iext,Lambda,ssnn,hkl,f2,extc,ys,der,derf2)
      !---- Arguments ----!
      integer,                    intent (in) :: job     !> =0,2 for x-rays, =1,3 for neutrons
      integer,                    intent (in) :: iext    !> Extinction model (1: isotropic, 4:anisotropic)
      real(kind=cp),              intent (in) :: Lambda  !> Wavelength
      real(kind=cp),              intent (in) :: ssnn    !> (SinTheta/Lambda)^2
      real(kind=cp), dimension(3),intent (in) :: hkl     !> Components of the scattering vectors
      real(kind=cp),              intent (in) :: f2      !> Square of the structure factor
      real(kind=cp), dimension(6),intent (in) :: extc    !> Extinction coefficients
      real(kind=cp),              intent(out) :: ys      !> Extinction correction factor: Icorr = I.ys
      real(kind=cp), dimension(6),optional,intent(out) :: der   !> Derivatives of ys w.r.t. extinction coefficients
      real(kind=cp),              optional,intent(out) :: derf2 ! Derivative of ys w.r.t. F2 (useful of SF normalization)
                                                                 ! If derf2 is present, der should also be present
      !---- Local variables ----!
      real(kind=cp)               :: a,b,c, g, r, sin2t, coefa, qq
      real(kind=cp), dimension(6) :: hkl_q
      character(len=10)           :: chkl

      !> Init
      call clear_error()

      coefa=0.001_cp
      if (job == 0 .or. job == 2 ) coefa=0.000001_cp
      if (present(der)) der=0.0_cp
      if (present(derf2)) derf2=0.0_cp

      ys=1.0_cp
      a = lambda*lambda*lambda
      qq=SQRT(ssnn)*lambda
      if (abs(qq) > 1.0_cp) then
         err_cfml%ierr=1
         write(unit=chkl,fmt="(3f8.2)") hkl
         err_cfml%msg="WARNING!: Extinction correction fixed to 1.0 for for reflection: "//trim(chkl)//&
                      "  Check cell parameters !!!"
         return
      else
         sin2t=sin(2.0_cp*asin(qq))
      end if

      Select Case(iext)
         Case(1)
            b = coefa*f2*a/sin2t
            c = 1.0_cp + b * extc(1)
            ys=1.0_cp/SQRT(c)
            if (present(der)) der(1)=-0.5_cp*c**(-1.5_cp)*b

         Case (4)   !Anisotropic Shelx-like extinction correction
            b = coefa*f2*a/sin2t
            b = b * 0.25_cp /ssnn
            hkl_q=(/ hkl(1)*hkl(1),hkl(2)*hkl(2),hkl(3)*hkl(3),hkl(1)*hkl(2), &
                     hkl(1)*hkl(3),hkl(2)*hkl(3) /)
            r=dot_product(extc,hkl_q)
            if(r < 0.0_cp) then
              c = 1.0_cp
            else
              c = 1.0_cp + b * r
            end if
            ys=1.0_cp/SQRT(c)
            if (present(der)) then
               g=-0.5_cp*c**(-1.5_cp)*b !component of derivative
               der= g * hkl_q
            end if
      End Select
      if (present(derf2)) derf2=g*(c-1.0_cp)/max(1.0e-6_cp,f2)

   End Subroutine SHELX_Extinction

End SubModule  Ext_ShelxCorr