SubModule (CFML_Profiles) Profile_TCHpVoigt
  implicit none
   Contains

    !!----
    !!---- FUNCTION TCH_PVOIGT
    !!----
    !!----
    !!---- Update: April - 2009
    !!
    Pure Module Function TCH_pVoigt(X,Par) Result (Pv_Val)
       !---- Arguments ----!
       real(kind=cp),              intent(in) :: x
       real(kind=cp), dimension(:),intent(in) :: par
       real(kind=cp)                          :: pv_val

       !--- Local variables ---!
       real(kind=cp)            :: Hg,Hl,eta,H,x2,ag,bg,al,bl,lor,gauss,r
       real(kind=cp), parameter :: o1= 2.69269, o2=2.42843, o3=4.47163, o4= 0.07842
       real(kind=cp), parameter :: e1= 1.36603, e2=0.47719, e3=0.11116

       Hg=par(1)
       Hl=par(2)

       !> Calculate H and eta from Tomson-Cox-Hasting formula
       H=hg**5+o1*hg**4*hl+o2*hg**3*hl**2+o3*hg**2*hl**3+o4*hg*hl**4+hl**5
       H=abs(H)**0.2_cp
       r = hl/H                       !HL/FWHM
       eta = max( 1.0e-06_cp, r*(e1 -(e2 + e3*r)*r) )  !eta
       x2=x*x
       ag= 0.93943727869965133377234032841018_cp/H
       bg= 2.7725887222397812376689284858327_cp/(H*H)
       al= 0.63661977236758134307553505349006_cp/H
       bl= 4.0_cp/(H*H)
       gauss = ag* exp(-bg*x2)
       lor   = al/(1.0_cp+bl*x2)
       pv_val = eta*lor + (1.0_cp - eta)*gauss

    End Function TCH_pVoigt

    !!----
    !!---- SUBROUTINE TCH_PVOIGT_DER
    !!----
    !!----  Pv_Val is the value of the function for the argument x
    !!----  Par=[HG,HL]  and Dpar(1:3)=[derx,derHG,derHL]
    !!----
    !!---- Update: April - 2009
    !!
    Pure Module Subroutine TCH_pVoigt_Der(X,Par,Pv_Val,dPar)
       !---- Arguments ----!
       real(kind=cp),                       intent(in) :: x
       real(kind=cp), dimension(:),         intent(in) :: par
       real(kind=cp),                       intent(out):: pv_val
       real(kind=cp), optional,dimension(:),intent(out):: dpar

       !--- Local variables ---!
       real(kind=cp), parameter :: o1= 2.69269_cp, o2=2.42843_cp, o3=4.47163_cp, o4= 0.07842_cp
       real(kind=cp), parameter :: e1= 1.36603_cp, e2=0.47719_cp, e3=0.11116_cp
       real(kind=cp) :: Hg,Hl,eta,H,x2,ag,bg,al,bl,lor,gauss, invH,invH2,r
       real(kind=cp) :: derH,derEta,derx,dlorH,dgaussH,lorp,gaussp, &
                        dhdhg,dhdhl,deta,detag,detal,derHg,derHl

       Hg=par(1)
       Hl=par(2)
       !Calculate H and eta from Tomson-Cox-Hasting formula
       H=hg**5+o1*hg**4*hl+o2*hg**3*hl**2+o3*hg**2*hl**3+o4*hg*hl**4+hl**5
       H=abs(H)**0.2_cp
       r = hl/H                       !HL/FWHM
       eta = max( 1.0e-06_cp, r*(e1 -(e2 + e3*r)*r) )  !eta
       x2=x*x
       invH=1.0_cp/H
       invH2=invH*invH
       ag= 0.93943727869965133377234032841018_cp*invH
       bg= 2.7725887222397812376689284858327_cp*invH2
       al= 0.63661977236758134307553505349006_cp*invH
       bl= 4.0_cp*invH2
       gauss = ag* exp(-bg*x2)
       lor   = al/(1.0_cp+bl*x2)
       pv_val = eta*lor + (1.0_cp - eta)*gauss

       if(present(dpar)) then
          dhdhg = 0.2_cp/H**4*(5.0_cp*hg**4+ 4.0_cp*o1* hg**3*hl+  &
                  3.0_cp*o2*hg*hg*hl*hl + 2.0_cp*o3*hg*hl**3 + o4*hl**4)
          dhdhl = 0.2_cp/H**4*(o1*hg**4+ 2.0_cp*o2*hg**3*hl+  &
                  3.0_cp*o3*hg*hg*hl*hl + 4.0_cp*o4*hg*hl**3 + 5.0_cp*hl**4)
           deta = e1-(2.0_cp*e2 - 3.0_cp*e3*r)*r  !derivative of ETA w.r.t. r
          detag = -r*deta*dhdhg*invH
          detal = (1.0_cp-r*dhdhl)*deta*invH

          derEta= lor-gauss  !Eta
          lorp = -2.0_cp *lor*lor*bl*x/al  !x
          gaussp = -2.0_cp * gauss * bg * x  !x
          derx=eta*lorp+(1.0-eta)*gaussp  !x

          dlorH= (2.0_cp*bl*lor*x2/al -1.0_cp)*invH*lor
          dgaussH= (2.0_cp*bg*x2-1.0_cp)*invH*gauss
          derH=eta*dlorH + (1.0_cp-eta) * dgaussH

          derHG= derH * dhdhg + derEta * detag  !Chain rule
          derHL= derH * dhdhl + derEta * detal
          dpar(1:3)=[derx,derHG,derHL]
       end if

    End Subroutine TCH_pVoigt_Der

    Pure Module Subroutine Get_HG_HL(fwhm,eta,HG,HL)
      real(kind=cp), intent(in)  :: fwhm, eta
      real(kind=cp), intent(out) :: HG,HL
      real(kind=cp) :: eta2,eta3

      eta2=eta*eta; eta3=eta2*eta
      HL=fwhm*(0.72928_cp * eta + 0.19289_cp * eta2 + 0.07783_cp * eta3)
      HG=fwhm*sqrt(1.0_cp - 0.74417_cp * eta - 0.24781_cp * eta2 - 0.00810_cp * eta3)

    End Subroutine Get_HG_HL

    Pure Module Subroutine Get_FWHM_Eta(HG,HL,fwhm,eta)
      real(kind=cp), intent(in)  :: Hg
      real(kind=cp), intent(in)  :: Hl
      real(kind=cp), intent(out) :: fwhm
      real(kind=cp), intent(out) :: eta

      ! Local Variables ----!
      real(kind=cp), parameter :: o1 = 2.69269, o2 = 2.42843, o3 = 4.47163, o4 = 0.07842
      real(kind=cp), parameter :: e1 = 1.36603, e2 = 0.47719, e3 = 0.11116
      real(kind=cp)            :: ctl, tlr

      ! There is no exception handling because it is supposed to be
      ! perfomed before calling TCH
      ctl = Hg**5.0+o1*Hg**4.0*Hl+o2*Hg**3.0*Hl**2.0+o3*Hg**2.0*Hl**3.0+  &
            o4*Hg*Hl**4.0+Hl**5.0
      fwhm = ctl**0.2
      tlr = Hl/fwhm
      eta = max(1.0e-06,e1*tlr-e2*tlr*tlr+e3*tlr**3.0)

    End Subroutine Get_FWHM_Eta

    !!----
    !!----  Pure Module Subroutine Keijser(h,eta,hg,hl)
    !!----
    !!----  real(kind=cp), intent( in) :: h,eta
    !!----  real(kind=cp), intent(out) :: hg,hl
    !!----
    !!---- Update: May - 2025 (From FullProf)
    !!
    !>  Calculation of the HG and HL from H and eta
    !>  Uses the formula of de Keijser (1982). This formula assumes that we are working
    !>  with peaks intensities. There is no relation to normalized Pseudo Voigt Function
    !>  However, as the form-factor parameter is the ratio of FWHM and Beta, this should
    !>  be independent of the particular formulation of the pV-function, provided the
    !>  calculation of beta is correct. Here we assume a NORMALIZED pV-function
    !>
    !> Normalized Lorentzian: L(x) = aL/(1+bL x^2)    aL=2/(pi.HL), bL=4/(H^2)
    !>                               beta_L=pi HL/2
    !> Normalized Gaussian  : G(x) = aG exp(-bG x^2), aG= 2 sqrt(Ln2/pi)/HG, bG=4Ln2/(H^2)
    !>                               beta_G= sqrt(pi/Ln2) HG/2
    !>
    !>        beta_pV= 0.5*H/( eta/pi+(1.0-eta)/sqrpil)    form-factor: phi = H/beta

    Pure Module Subroutine Keijser(h,eta,hg,hl)
       real(kind=cp), intent( in) :: h,eta
       real(kind=cp), intent(out) :: hg,hl

       real(kind=cp), parameter   :: twoopi = 0.636619772367581343_cp
       real(kind=cp), parameter   :: pi    =3.1415926536_cp, &    !, ln2=0.69314718055
                                     sqrpil=2.1289340388_cp, &    ! sqrt(pi/Ln2)
                                     a0 = 2.0207_cp , a1= -0.4803_cp,  a2= -1.7756_cp,   &
                                     b0 = 0.6420_cp, b12= 1.4187_cp, b1=-2.2043_cp, b2=1.8706_cp
       real(kind=cp)  :: beta,phi,beta_L,beta_G, phmtp

        phi= 2.0 * (eta/pi+(1.0-eta)/sqrpil)
        beta = H/phi
        phmtp=phi - twoopi
        if(phmtp < 0.0) then
          beta_L=0.0
          beta_G=beta
        else
          beta_L = beta * (a0 + (a1 + a2 * phi) * phi)
          beta_G = beta * (b0 + b12 * sqrt(phmtp) + (b1 + b2 * phi) * phi)
        end if
        hg = beta_G * 2.0/sqrpil
        hl = beta_L * twoopi

    End Subroutine Keijser

End SubModule Profile_TCHpVoigt
