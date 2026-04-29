!!----
!!----
!!----
!!---- 21/04/19
!!
SubModule (CFML_Profiles) Profile_PseudoVoigt
  implicit none
   Contains
    !!----
    !!---- FUNCTION PSEUDOVOIGT
    !!----
    !!----  Par=[H,eta]
    !!---- Update: October - 2005
    !!
    Pure Module Function Pseudovoigt(X,Par) Result (Pv_Val)
       !---- Arguments ----!
       real(kind=cp),              intent(in) :: x !x=T-T0
       real(kind=cp), dimension(:),intent(in) :: par
       real(kind=cp)                          :: pv_val

       !--- Local variables ---!
       real(kind=cp) :: eta,H,x2,ag,bg,al,bl,lor,gauss

       H=par(1)
       eta=par(2)
       x2=x*x
       ag= 0.93943727869965133377234032841018/H
       bg= 2.7725887222397812376689284858327/(H*H)
       al= 0.63661977236758134307553505349006/H
       bl= 4.0/(H*H)
       gauss = ag* exp(-bg*x2)
       lor   = al/(1.0+bl*x2)
       pv_val = eta*lor + (1.0 - eta)*gauss

    End Function Pseudovoigt

    !!----
    !!---- SUBROUTINE PSEUDOVOIGT_DER
    !!----
    !!---- Compact calculation of the pseudo-Voigt function and drivatives.
    !!---- Pv_Val is the value of the function for the argument x=T-T0
    !!----  Par=[H,eta]  and Dpar(1:3)=[derx,derH,derEta], derx is the derivative w.r.t. T0
    !!----
    !!---- Update: October - 2021
    !!
    Pure Module Subroutine Pseudovoigt_Der(X,Par,Pv_Val,Dpar)
       !---- Arguments ----!
       real(kind=cp),                       intent(in) :: x  ! x=T-T0
       real(kind=cp), dimension(:),         intent(in) :: par
       real(kind=cp),                       intent(out):: pv_val
       real(kind=cp), optional,dimension(:),intent(out):: dpar

       !--- Local variables ---!
       real(kind=cp) :: eta,H,x2,ag,bg,al,bl,lor,gauss, invH,invH2
       real(kind=cp) :: derH,derEta,derx,dlorH,dgaussH,lorp,gaussp

       H=par(1)
       eta=par(2)
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

       if (present(dpar)) then
          derEta= lor-gauss                  !Eta
          lorp =  2.0_cp *lor*lor*bl*x/al    !dL/dT0 = -L'(x)  !The sign is changed because it is d/dT0 and not d/dx
          gaussp =  2.0_cp * gauss * bg * x  !dG/dT0 = -G'(x)
          derx=eta*lorp+(1.0_cp-eta)*gaussp  !dPV/dT0
          dlorH= (2.0_cp*bl*lor*x2/al -1.0_cp)*invH*lor
          dgaussH= (2.0_cp*bg*x2-1.0_cp)*invH*gauss
          derH=eta*dlorH + (1.0_cp-eta) * dgaussH
          dpar(1:3)=[derx,derH,derEta]
       end if

    End Subroutine Pseudovoigt_Der


    !!----
    !!---- CALC_PSEUDO_VOIGT
    !!----
    !!----    Return the calculated ordinates Y(:) corresponding to a normalized Pseudo-Voigt function,
    !!----    characterized by: Twoth0,Eta,Fwhm,asym1,asym1, for points X(:).
    !!----
    !!----    This subroutine is useful for calculating the contribution of a single peak.
    !!----    It calls prof_val without using derivatives
    !!----
    !!---- 21/04/2019
    !!
    Module Subroutine Calc_Pseudo_Voigt(x,y,Twoth0,Eta,Fwhm,asym1,asym2)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in)   :: x
       real(kind=cp), dimension(:), intent(out)  :: y
       real(kind=cp),               intent(in)   :: Twoth0
       real(kind=cp),               intent(in)   :: eta
       real(kind=cp),               intent(in)   :: Fwhm
       real(kind=cp),               intent(in)   :: asym1        ! s_l source width/detector distance or D_L+S_L if  use_hps is true
       real(kind=cp),               intent(in)   :: asym2        ! d_l detector width/detector distance or D_L-S_L if  use_hps is true

       !--- Local variables ---!
       real(kind=cp)   :: dprdt,dprdg,dprde, dprds, dprdd
       Logical         :: use_asym, use_hps
       integer         :: npoints,i

       npoints=size(x)
       use_asym=.true.
       if(abs(asym1)+abs(asym2) < 0.00001) use_asym=.false.
       use_hps=.false.
       if (abs(asym2) < 0.00001) use_hps=.true.
       do i=1,npoints
          call Prof_Val( eta, fwhm, asym1, asym2, x(i), twoth0, dprdt, dprdg,  &
                         dprde , dprds , dprdd , y(i), use_asym, use_hps)
       end do

    End Subroutine Calc_Pseudo_Voigt

    !!--++
    !!--++ PSVOIGTIAN
    !!--++
    !!--++    (Private)
    !!--++    Returns value of Pseudo Voigt
    !!--++
    !!--++ Update: October - 2005
    !!
    Module Subroutine PsVoigtian(Twoth , Twoth0 , Eta , Gamm, Dprdt , Dprdg , Dprde, PsVoigt )
       !---- Arguments ----!
       real(kind=cp), intent(in)  :: twoth     ! point at which to evaluate the profile
       real(kind=cp), intent(in)  :: twoth0    ! two theta value for peak
       real(kind=cp), intent(in)  :: eta       ! mixing coefficient between Gaussian and Lorentzian
       real(kind=cp), intent(in)  :: gamm      ! FWHM
       real(kind=cp), intent(out) :: dprdt     ! derivative of profile wrt TwoTH0
       real(kind=cp), intent(out) :: dprdg     ! derivative of profile wrt gamm
       real(kind=cp), intent(out) :: dprde     ! derivative of profile wrt Eta
       real(kind=cp), intent(out) :: psvoigt

       !---- Local Variables ----!
       real(kind=cp) :: Gauss           ! Gaussian part
       real(kind=cp) :: Lorentz         ! Lorentzian part
       real(kind=cp) :: dgdt , dgdg , dldt , dldg


       call Prof_Gaussian(twoth , twoth0 , gamm , dgdt , dgdg ,Gauss)
       call Prof_Lorentzian(twoth , twoth0 , gamm , dldt , dldg, Lorentz)
       psvoigt = eta * Lorentz + (1.0_cp - eta) * Gauss
       dprdt = eta * dldt + (1.0_cp - eta) * dgdt
       dprdg = eta * dldg + (1.0_cp - eta) * dgdg
       dprde = Lorentz - Gauss

    End Subroutine PsVoigtian

    !!----
    !!---- FUNCTION SPLIT_PSEUDOVOIGT
    !!----
    !!---- Par=[H1,H2,eta1,eta2]
    !!----
    !!---- Update: October - 2005
    !!
    Pure Module Function Split_Pseudovoigt(X,Par) Result (Pv_Val)
       !---- Arguments ----!
       real(kind=cp),              intent(in) :: x
       real(kind=cp), dimension(:),intent(in) :: par
       real(kind=cp)                          :: pv_val

       !--- Local variables ---!
       real(kind=cp) :: eta1,eta2,eta,H1,H2,hsq,Norm,x2,bg,bl,lor,gauss

       H1=par(1)
       H2=par(2)
       eta1=par(3)
       eta2=par(4)
       Norm= 0.25*H1*(eta1*1.0126586+2.128934)+ &
             0.25*H2*(eta2*1.0126586+2.128934)
       if (x < 0.0) then
          eta=eta1
          hsq=h1*h1
       else
          eta=eta2
          hsq=h2*h2
       end if
       x2=x*x
       bg= 2.7725887222397812376689284858327/hsq
       bl= 4.0/hsq
       gauss =  exp(-bg*x2)
       lor   = 1.0/(1.0+bl*x2)
       pv_val = (eta*lor + (1.0 - eta)*gauss)/Norm

    End Function Split_Pseudovoigt

    !!----
    !!---- SUBROUTINE SPLIT_PSEUDOVOIGT_DER
    !!----
    !!---- Par=[H1,H2,eta1,eta2]
    !!---- DPar=[derx,derH1,derH2,derEta1,derEta2]
    !!---- Update: October - 2021
    !!
    Pure Module Subroutine Split_Pseudovoigt_Der(X,Par,Pv_Val,Dpar)
       !---- Arguments ----!
       real(kind=cp),                       intent(in) :: x  !x=T-T0, derx is derivative w.r.t. T0
       real(kind=cp), dimension(:),         intent(in) :: par
       real(kind=cp),                       intent(out):: pv_val
       real(kind=cp), optional,dimension(:),intent(out):: dpar

       !--- Local variables ---!
       real(kind=cp) :: eta1,eta2,eta,H1,H2,hsq,Norm,x2,bg,bl,lor,gauss,  &
                        derH1,derH2,derEta1,derEta2,derx,dlorH,dgaussH,   &
                        lorp,gaussp, invH, Numer

       H1=par(1)
       H2=par(2)
       eta1=par(3)
       eta2=par(4)
       Norm= 0.25_cp*H1*(eta1*1.0126586_cp+2.128934_cp)+ &
             0.25_cp*H2*(eta2*1.0126586_cp+2.128934_cp)
       if (x < 0.0) then
          eta=eta1
          hsq=h1*h1
          invH=1.0_cp/h1
       else
          eta=eta2
          hsq=h2*h2
          invH=1.0_cp/h2
       end if
       x2=x*x
       bg= 2.7725887222397812376689284858327_cp/hsq
       bl= 4.0_cp/hsq
       gauss =  exp(-bg*x2)
       lor   = 1.0_cp/(1.0_cp+bl*x2)
       pv_val = (eta*lor + (1.0_cp - eta)*gauss)/Norm

       if (present(dpar)) then

            lorp = 2.0_cp *lor*lor*bl*x     !x=T-T0 derivative w.r.t. T0 (change of sign)
          gaussp = 2.0_cp * gauss * bg * x  !x
          derx=(eta*lorp+(1.0_cp-eta)*gaussp)/Norm  !x
          !
          dlorH= 2.0_cp*bl*x2*invH*lor*lor
          dgaussH= 2.0_cp*bg*x2*invH*gauss
          Numer  = eta*dlorH + (1.0_cp-eta) * dgaussH

          if (x < 0.0_cp) then

             ! dNormEi= 0.25*Hi*1.0126586 = Hi*0.25316465
             derEta1= (lor-gauss- pv_val*H1*0.25316465_cp)/Norm   !Eta
             derEta2= -pv_val*h2*0.25316465_cp/Norm

             ! pv_val = (eta*lor + (1.0 - eta)*gauss)/Norm
             ! derH1 =  (eta*dLorH1+(1-eta)*dgaussH1)*Norm - pv_val*Norm*dNormH1/Norm**2
             ! derH1 =  (Numer - pv_val*dNormH1)/Norm
             ! dNormH= 0.25*(eta*1.0126586+2.128934)
             ! derH2 = pv_val*Norm (-1/Norm**2)  * dNormH2 = - pv_val*0.25*(eta2*1.0126586+2.128934)/Norm

             derH1  = (numer - pv_val* 0.25_cp*(eta*1.0126586_cp+2.128934_cp))/Norm
             derH2  = - pv_val*0.25_cp*(eta2*1.0126586_cp+2.128934_cp)/Norm

          else

             ! dNormEi= 0.25*Hi*1.0126586 = Hi*0.25316465
             derEta2= (lor-gauss- pv_val*H2*0.25316465_cp)/Norm   !Eta
             derEta1= -pv_val*h1*0.25316465_cp/Norm

             ! pv_val = (eta*lor + (1.0 - eta)*gauss)/Norm
             ! derH2 =  (eta*dLorH2+(1-eta)*dgaussH2)*Norm - pv_val*Norm*dNormH2/Norm**2
             ! derH2 =  (Numer - pv_val*dNormH1)/Norm
             ! dNormH= 0.25*(eta*1.0126586+2.128934)
             ! derH1 = pv_val*Norm (-1/Norm**2)  * dNormH1 = - pv_val*0.25*(eta1*1.0126586+2.128934)/Norm

             derH2  = (numer - pv_val* 0.25_cp*(eta*1.0126586_cp+2.128934_cp))/Norm
             derH1  = - pv_val*0.25_cp*(eta1*1.0126586_cp+2.128934_cp)/Norm

          end if

          dpar(1:5)=[derx,derH1,derH2,derEta1,derEta2]

       end if

    End Subroutine Split_Pseudovoigt_Der


 End SubModule Profile_PseudoVoigt
