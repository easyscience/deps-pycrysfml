SubModule(CFML_Powder) Pow_Lorentz_Absorption
  !>
  !> Derived from FullProf Modules (Lorentz, polarization and absorption corrections)
  !>
  implicit none

  contains

   Module Function Powder_Lorentz_IntegInt_CW(job,cmono,rkks,muR,sinth2,costh) result(plor)     !loren <- FullProf
      Character(len=*), intent(in)  :: job        !>  X or N for x-rays and neutrons
      real(kind=cp),    intent(in)  :: cmono      !>  Cos(2theta_monok)^2
      real(kind=cp),    intent(in)  :: rkks       !>  Polarization factor for synchrotron
      real(kind=cp),    intent(in)  :: muR        !>  Absorption coefficient x Radius
      real(kind=cp),    intent(in)  :: sinth2     !>  sintheta**2
      real(kind=cp),    intent(in)  :: costh      !>  costheta
      real(kind=cp)                 :: plor
      !--- L o c a l   V a r i a b l e s
      real(kind=cp) :: cos2th_sqr
      !-----------------------------------------------
      !   sinth2= sintheta**2
      !    costh= costheta           PLOR = L p

      !  First Lorentz factor

      plor=EXP((-(1.7133-0.0368*sinth2)*muR)+(0.0927+0.375*sinth2)*muR*muR)
      plor=plor/(2.0*sinth2*costh)

!  Apply polarization correction for X-rays

      if(job == 'X') then
        cos2th_sqr=(1.0-2.0*sinth2)*(1.0-2.0*sinth2)        !(cos2theta)**2
        if (rkks > 0.0) then
          plor = plor * (1.0-rkks+rkks*cos2th_sqr)
        else
          plor = plor * (1.0+cos2th_sqr*cmono)
        end if
      end if
   End Function Powder_Lorentz_IntegInt_CW

   Module Function Lorentz_abs_CW(sinth,costh,postt,tmv,radia,ilor,cabs,cthm,twoTh0,alpsd,rkk,ip,rmua) result(plor)
      !-----------------------------------------------
     !   D u m m y   A r g u m e n t s
     !-----------------------------------------------
     real(kind=cp),           intent(in)  :: sinth        !> Sin(theta)
     real(kind=cp),           intent(in)  :: costh        !> Cos(theta)
     real(kind=cp),           intent(in)  :: postt        !> 2Theta
     real(kind=cp),           intent(in)  :: tmv          !> Global Absorption coefficient (mu.Radius or mu.Thickness)
     Character(len=*),        intent(in)  :: radia        !> Radiation type X or N
     Character(len=*),        intent(in)  :: ilor         !> Lorentz-factor type (diffraction geometry)
     Character(len=*),        intent(in)  :: cabs         !> Absorption correction type (HEWAT or LOBANOV) for DBS/SYN geometry
     real(kind=cp), optional, intent(in)  :: cthm         !> cos^2(2Theta_monok)
     real(kind=cp), optional, intent(in)  :: twoTh0       !> Only for Bragg-Brentano, initial 2Theta for which the intercepted beam is completely inside the sample surface
     real(kind=cp), optional, intent(in)  :: alpsd        !> Incident angle for fixed sample position
     real(kind=cp), optional, intent(in)  :: rkk          !> Polarization correction for synchrotron
     integer ,      optional, intent(in)  :: ip           !> ip: number of the phase (only for Thin Films multiphase)
     real(kind=cp), optional, intent(in)  :: rmua(:)      !> Series of mu.T attached to phases (multi phase thin-films)
     real(kind=cp)                        :: plor         !> Product of Lorentz, polarization and absorption
     !-----------------------------------------------
     !   L o c a l   V a r i a b l e s
     !-----------------------------------------------
     integer       :: i
     real(kind=cp) :: ssent0, xabs, psd, sclow, sic, x1,cthm1, x2, sint2
     real(kind=cp) :: k0=1.697653,k1,k2,k3,k4,k5,k6,k7
     !-----------------------------------------------
     character(len=:), allocatable :: ILO
     !
     ! SINTH= sintheta
     !  costh= costheta           PLOR = Lorentz . polarization . Absorption
     ! POSTT= 2theta/TOF
     !

     xabs = 1.0
     psd = 1.0
     sclow = 1.0
     sint2=sinth*sinth
     if(present(twoTh0) .and. radia == "X") then
       ssent0 = sind(twoTh0*0.5)
       if (sinth <= ssent0) sclow = sinth/ssent0
     end if

     ILO=trim(u_case(ilor))

     !Absorption correction
     if (tmv >= 0.000001 .and. ILO /= "FILMS") then

       Select Case (ILO)

         Case('DBS','SYN') !For debye-scherrer geometry (neutrons cylindrical sample can and synchrotron capillary)

           Select Case (trim(u_case(cabs)))

             Case('HEWAT')  !if(iabscor(n_pat) == 2) then
               xabs=exp((-(1.7133-0.0368*sint2)*tmv)+(0.0927+0.375*sint2)*tmv*tmv)
             Case('LOBANOV') !else if(iabscor(n_pat) == 4) then
                ! N.N Lobanov and L. Alte da Veiga correction (6th EPDIC, Abstract P12-16, Aug.22-25, 1998)
                if(tmv <= 3.0) then
                  k1=(25.99978-0.01911*sinth**0.25)*exp(-0.024514*sinth)+0.109561*sqrt(sinth)-26.0456
                  k2=-0.02489-0.39499*sinth+1.219077*sinth**1.5-1.31268*sint2+0.871081*sinth**2.5-0.2327*sinth**3
                  k3=0.003045+0.018167*sinth-0.03305*sint2
                  xabs=(((k3*tmv+k2)*tmv+k1)*tmv+k0)*tmv
                  xabs=exp(-xabs)
                else ! if(tmv > 3.0) then
                  k4=0.01433902+(0.11075504+(-0.0877629+(0.1002088-0.0336778*sinth)*sinth)*sinth)*sinth
                  k5=(0.013869-0.01249*sinth)*exp(3.27094*sinth)
                  k5=k5+(0.337894+13.77317*sinth)/(1.0+11.53544*sinth)**1.555039
                  k6=1.163198-0.13576*sqrt(sinth)+1.933433/(1.0+23.12967*sinth)**1.686715
                  k7=0.00044365-0.004259/(1.0+0.41051*sinth)**148.4202
                  xabs=k7+(k4-k7)/(1.0+k5*(tmv-3.0))**k6
                end if
             Case Default
                xabs=1.0_cp
           End Select

         Case('BB') !FP ilor=0  Bragg-Brentano (no absorption correction)

           xabs = 1.0_cp

         Case('TBG') !FP ilor=2 !Transmission bisecting geometry

           xabs = exp((-tmv/costh))/costh

         Case('TFX','PSD') !FP ilor=4 !Transmission fixed geometry
           !
           ! For phi/=0
           !  A=XABS= {exp(-mt sec(th+phi)) - exp(-mt sec(th-phi))}/ m{1 - sec(th+phi)/sec(th-phi)}
           !
           ! For phi=0
           !  A=XABS= tsec(th) exp(-mt sec(th))
           ! phi is the angle of the (hkl) planes with the normal to the incident surface
           !
           ! If the incident beam is fixed with respect to the surface. Incidence angle: alpsd the
           ! formula is modified in the following sense:
           !
           !  A=XABS= {exp(-mt csec(2th+alpsd)) - exp(-mt csec(alpsd))}/ m{1 - sin(alpsd)/sin(2th+alpsd)}
           !
           if(present(alpsd)) then
              k1=sind(alpsd)
              k2=sind(postt+alpsd)
              xabs = exp(-tmv/k2)-exp(-tmv/k1)
              xabs=xabs/(1.0 - k1/k2)
           else
              xabs=1.0_cp
           end if
       End Select

     end if  !tmv (global absorption)

     Select Case (ILO)

       Case('PSD','PSD-SYN')  !Position sensitive detector ilor(n_pat) == 1

         if(present(alpsd)) then
            sic = postt - alpsd
            psd = 1.0_cp + sind(alpsd)/sind(sic)
         else
            psd = 1.0_cp
         end if

       Case('FILMS','VSL-FILMS')  !(ilor(n_pat) == 5 .or. ilor(n_pat)==6)
         !
         !  Thin films corrections (ILOR = 5) Bragg-Brentano X-rays
         !
         xabs=1.0
         if(present(rmua) .and. present(ip)) then
           do i=1,ip-1
             xabs=xabs*exp(-2.0*rmua(i)/sinth)
           end do
           if(rmua(ip) > 0.00001) then
              xabs=0.5*xabs*(1.0-exp(-2.0*rmua(ip)/sinth))/rmua(ip)
           end if
         end if
     End Select

     !
     !  psd: correction factor for flat plate psd geometry, synchrotron and
     !  transmission geometry
     !
     !  First Lorentz factor * absorption
     !
     Select Case(ILO)
       Case('FX','TG','FILMS','BB','SYN')  !Ilor=4,2,5,0
          plor = 1.0/(2.0*sint2*costh)*xabs
       Case('NC')  !Diffraction pattern already corrected from LP
          plor = xabs
       Case Default
          !  plor = 1.0/(2.0*sint2*costh*psd)*xabs
          plor = 1.0/(2.0*sint2*costh*psd)*xabs
     End Select

     ! Old ilor(n_pat) == 6
     If(ILO == 'VSL' .or. ILO == 'VSL-FILMS' .and. present(twoTh0) .and. radia == "X") then !Automatic(Variable) slit as in Philips X'Pert (Bragg-Brentano) ilor(n_pat) == 6
        ! corr= R sin(a/2)/L (1/sin(Th+a/2) + 1/sin(th-a/2)) with
        ! L = 2RSin(Th).Tan(a/2)/(sin^2(th)-Cos^2(Th)tan^2(a/2)
         k1= 0.5*twoTh0
         k2= tand(k1)
         x1= (1.0/sind(0.5*postt+k1)+1.0/sind(0.5*postt-k1))*sind(k1)
         x2= 2.0*sinth*k2/(sint2-costh*costh*k2*k2) ! = L/R
         plor=plor*x2/x1
     End If
     !
     !  Apply polarization correction
     !
     if (radia == "X" .and. ILO /= 'NC') then

        x1 = (1.0 - 2.0*sint2)*(1.0 - 2.0*sint2)        !(cos2theta)**2
        Select Case(ILO)

          Case('BB','PSD','VSL')
            if(present(cthm)) then
               plor = plor*(1.0 + x1*cthm)*sclow
            else
               plor = plor*sclow
            end if

          Case('TBG','TFX','PSD-SYN') !transmission
            if(present(rkk) .and. present(cthm))  then
               cthm1 = sqrt(cthm)     !cos2thetamonok
               x2 = (1.0 + x1*cthm)/(1.0 + cthm)
               x1 = (1.0 + x1*cthm1)/(1.0 + cthm1)
               plor = plor*(rkk*x2 + (1.0 - rkk)*x1)*sclow
            end if
          Case('SYN','DBS') !(ilor(n_pat) == 3 .or. ilor(n_pat) == -2)

           if(present(rkk)) plor = plor*(1.0 - rkk + rkk*x1)

        End Select

     end if
     !
     !  End of Absorption, lorentz and polarization corrections
     !
   End Function Lorentz_abs_CW

End SubModule Pow_Lorentz_Absorption
