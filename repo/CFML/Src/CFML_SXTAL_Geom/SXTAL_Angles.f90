!!----
!!----
!!----
 SubModule (CFML_SXTAL_Geom) SXTAL_Angles

  implicit none

  Contains

    !!---- Elemental Module Function chkin180(angle) result(angle_in)
    !!----   real(kind=cp), intent(in) :: angle
    !!----   real(kind=cp)             :: angle_in
    !!----
    !!----  Elemental function (can be used with arrays) putting the
    !!----  input angle (in degrees) into the interval (-180,180)
    !!----
    !!----  Created: March 2013 (JRC)
    !!----
    Elemental Module Function chkin180(angle) result(angle_in)
      real(kind=cp), intent(in) :: angle
      real(kind=cp)             :: angle_in

      angle_in=angle
      do
        if(angle_in > 180.0_cp) then
          angle_in=angle_in-360.0_cp
        else if(angle_in < -180.0_cp) then
          angle_in=angle_in+360.0_cp
        else
          exit
        end if
      end do

    End Function chkin180

    !!----
    !!---- Module Subroutine Angs_4c_bisecting(wave,z1,tth,om,ch,ph)
    !!----    real(kind=cp), Intent(In)                :: wave
    !!----    real(kind=cp), Intent(In), Dimension(3)  :: z1
    !!----    real(kind=cp), Intent(Out)               :: tth,om,ch,ph
    !!----
    !!----    Calculate 2-THETA, OMEGA (=THETA), CHI, PHI to put the
    !!----    vector Z1 in the bisecting diffraction condition. The
    !!----    reciprocal vector Z1 is given in cartesian components
    !!----    with respect to the laboratory system. This geometry
    !!----    corresponds to the bisecting PSI=0 (igeom=1).
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine Angs_4c_bisecting(wave,z1,tth,om,ch,ph)
       !---- Arguments ----!
       real(kind=cp), Intent(In)                :: wave
       real(kind=cp), Intent(In), Dimension(3)  :: z1
       real(kind=cp), Intent(Out)               :: tth,om,ch,ph

       !---- Local Variables ----!
       real(kind=cp) :: th, ds

       call clear_error()
       Call Equatorial_Chi_Phi(z1,ch,ph) !Eq. 38 of Busing & Levy
       Call Get_dspacing_theta(wave,z1,ds,th)
       If(Err_CFML%Ierr == 0) Then
          om=th             !Bisecting position
          tth=th*2.0
       Else
          tth=0.0  !Case of error (ierr=1, null z1; ierr=2 outside resolution sphere)
          om=0.0
          ch=0.0
          ph=0.0
       End If

    End Subroutine Angs_4c_bisecting

    !!----
    !!---- Module Subroutine calang(h,tteta,om,ch,ph,wav,ubm,geom)
    !!----    real(kind=cp),Dimension(3),             Intent(In) :: h
    !!----    real(kind=cp),                          Intent(Out):: tteta,om,ch,ph
    !!----    real(kind=cp),                optional, intent(in) :: wav
    !!----    real(kind=cp), dimension(3,3),optional, intent(in) :: ubm
    !!----    integer,                      optional, intent(in) :: geom
    !!----
    !!--<<    Original comments:
    !!----    SUBROUTINE *** CALANG ***
    !!----    VERSION DU 6/10/1976
    !!----    CALCULE LES ANGLES 2THETA OMEGA CHI PHI  :
    !!----           IPARA=1  GEOMETRIE BISECTING (PSI=0)
    !!----           IPARA=2  GEOMETRIE BISECTING - HICHI  (D9, displex,cryostat)
    !!----           IPARA=3  GEOMETRIE NORMAL BEAM
    !!----           IPARA=4  GEOMETRIE PARALLELE (PSI=90)  (D15,D16)
    !!----      POUR REFLECTION H,K,L ET MATRICE UB DONNEES
    !!----
    !!----    This subroutine is a more general variant of Angs_4c_bisecting.
    !!----    If the optional arguments are given, the corresponding values
    !!-->>    are adopted instead of those of the current instrument.
    !!----
    !!---- Update: July 2008, June 2020
    !!
    Module Subroutine calang(h,tteta,om,ch,ph,wav,ubm,geom)
       !---- Arguments ----!
       real(kind=cp),Dimension(3),             Intent(In) :: h
       real(kind=cp),                          Intent(Out):: tteta,om,ch,ph
       real(kind=cp),                optional, intent(in) :: wav
       real(kind=cp), dimension(3,3),optional, intent(in) :: ubm
       integer,                      optional, intent(in) :: geom

       !--- Local Variables ---!
       real(kind=cp)                 :: ttmax,ttmin,sint,wave,chmax,diff,ds,theta
       real(kind=cp), Dimension(3)   :: z1
       real(kind=cp), Dimension(3,3) :: ub
       Integer                       :: igeom

       !---- Set local variables with the current instrument
       if (present(wav)) then
         wave = wav
       else
         wave = Current_Orient%wave
       end if
       if (present(geom)) then
         igeom = geom
       else
         igeom = Current_Instrm%igeom
       end if
       if(igeom == 2) then  !high-chi
          chmax= 200.0
       else
          chmax= 170.0
       end if
       if(Current_Instrm%Ang_Limits(3,2) < chmax) chmax= Current_Instrm%Ang_Limits(3,2)

       ttmin= 0.0
       ttmax= 180.0
       if (present(ubm)) then
          ub   = ubm
       else
          ub   = Current_Orient%ub
       end if
       if(Current_Instrm%Ang_Limits(1,1) > ttmin) ttmin=Current_Instrm%Ang_Limits(1,1)  !    1       2      3     4   ...
       if(Current_Instrm%Ang_Limits(1,2) < ttmax) ttmax=Current_Instrm%Ang_Limits(1,2)  ! 2theta   Omega   Chi   Phi  ...
       call clear_error()

       z1=Matmul(ub,h)
       sint=0.5*wave*Sqrt(Dot_Product(z1,z1))
       If (abs(sint) > 1.0) Then
          Err_CFML%ierr=1
          Return
       End If
       theta =asind(sint)
       om=theta       !Theta = omega in bisecting geometry
       tteta=2.0*theta

       If (tteta < ttmin .or. tteta > ttmax) Then
          Err_CFML%ierr=1
          Err_CFML%Msg="Calculated 2Theta out of limits @ calang"
          Return
       End If

       ds = Sqrt(z1(1)*z1(1)+z1(2)*z1(2))
       Select Case(igeom)
          Case(1,2)    !1:Bisecting Geometry  (PSI=0), 2:Bisecting Geometry - High CHI
             ph = atan2d(z1(2),z1(1))  !Eqs 38 BL
             ch = atan2d (z1(3),ds)
             If(igeom == 2) Then
               ch=180.0-ch  !Eqs 40 BL
               ph=180.0+ph
             End If

             If(ph >  180.0) ph=ph-360.0
             If(ph < -180.0) ph=ph+360.0
             If(ch >  180.0) ch=ch-360.0
             If(ch < -180.0) ch=ch+360.0
             If (chmax > 180.0) Then
                diff=chmax-180.0
                If (ch < -180.0 + diff .AND. ch >= -180.0)  ch=ch+360.0
             End If

          Case(3)    !3:  Geometry NORMAL BEAM
             ch=-90.0
             ph=-atan2d(z1(1),z1(2))
             om=theta+90.0-atan2d(z1(3),ds)

          Case(4)    !4:  Geometry parallel (PSI=90) ..D15-D16  (PSI=90)
             ch=+90.0
             ph=atan2d(z1(1),-z1(2))     ! Eqs 41 BL (with different definition of omega
             om=theta+atan2d(-ds,z1(3))  ! Omega=Theta+atan2d(-ds,z1(3))

       End Select

    End Subroutine calang

    !!----
    !!---- Module Function Calc_Psi(vhkl,vlab1,om,ch,ph,ub) Result(psi)
    !!----    real(kind=cp), Intent(In),Dimension(3)   :: vhkl,vlab1
    !!----    real(kind=cp), Intent(In)                :: om,ch,ph
    !!----    real(kind=cp), Intent(In),Dimension(3,3) :: ub
    !!----    real(kind=cp)                            :: psi
    !!----
    !!--<<    Calculate PSI for the reflection VHKL positioned at OM, CH and PH.
    !!----    The value of PSI is taken to be zero when (the values of OM, CH, PH
    !!----    are such that) the reflection H0 lies in the plane of VHKL and VZ,
    !!----    on the same side of VHKL as VZ.
    !!----    The reference vectors H0 and VZ are defined in subroutine REFVEC.
    !!----    there, the vector VZ is the z-axis of the fixed laboratory system
    !!----    (Busing and Levy Convention, Y along beam, X in positive 2-THETA
    !!----    direction). H0 is (0,0,1) for all VHKL except when VHKL is parallel
    !!----    to (0,0,1), in which case (0,1,0) is chosen.
    !!----    Construct two orthonormal-vector-triplets, TS in the PHI-axis
    !!----    system with normalised VLAB1, (VLAB1 x TS2) x VLAB1, VLAB1 x TS2 as
    !!----    columns; and TZ in the fixed-lab system with normalised [R].VLAB1,
    !!----    ([R].VLAB1 x TZ2) x [R].VLAB1, [R].VLAB1 x TZ2 as columns.
    !!----    Then the diffraction condition can be expressed as:
    !!----    [TZ]=[OM].[CH].[PH].[TS].[PSI]-1  (all are orthonormal matrices)
    !!----    hence [PSI]=[TZ]-1.[R].[TS] where [R]=[OM].[CH].[PH]
    !!----                                             R.F.D.Stansfield May-83
    !!-->>    PSI matrix and equations corrected 1/9/83 - results not changed
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function Calc_Psi(vhkl,vlab1,om,ch,ph,ub) Result(psi)
       !---- Arguments ----!
       real(kind=cp), Intent(In),Dimension(3)   :: vhkl,vlab1
       real(kind=cp), Intent(In)                :: om,ch,ph
       real(kind=cp), Intent(In),Dimension(3,3) :: ub
       real(kind=cp)                            :: psi

       !--- Local Variables ---!
       real(kind=cp),Dimension(3)   ::  z1,z4,vs,vz
       real(kind=cp),Dimension(3,3) :: dum1,dum2,dum3,ts,tz

       call clear_error()
       z1=vlab1
       z4=z4frz1(z1,om,ch,ph)
       Call refvec(vhkl,ub,vs,vz)
       Call triple(z1,vs,ts)
       Call triple(z4,vz,tz)
       If(Err_CFML%Ierr /= 0) Return
       dum2= Phi_mat(ph)
       dum1= Matmul(dum2,ts)
       dum3= Chi_mat(ch)
       dum2= Matmul(dum3,dum1)
       dum1= Phi_mat(om)
       dum3= Matmul(dum1,dum2)
       dum2= Transpose(tz)
       dum1= Matmul(dum2,dum3)
       psi=atan2d(-dum1(2,3),dum1(2,2))
    End Function Calc_Psi

    !!----
    !!---- Module Subroutine dspace(wave,vhkl,cell,ds,th)
    !!----    real(kind=cp), Intent(In)               :: wave
    !!----    real(kind=cp), Intent(In),Dimension(3)  :: vhkl
    !!----    real(kind=cp), Intent(In),Dimension(6)  :: cell
    !!----    real(kind=cp), Intent(Out)              :: ds,th
    !!----
    !!----    Calculate d-spacing and theta from cell parameters
    !!----    and wavelength assume triclinic symmetry. The reflection
    !!----    vector vhkl is provided in reciprocal lattice components
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine dspace(wave,vhkl,cell,ds,th)
       !---- Arguments ----!
       real(kind=cp), Intent(In)               :: wave
       real(kind=cp), Intent(In),Dimension(3)  :: vhkl
       real(kind=cp), Intent(In),Dimension(6)  :: cell
       real(kind=cp), Intent(Out)              :: ds,th

       !--- Local Variables ---!
       real(kind=cp) :: a,b,c,al,be,ga,d,e,f,g, sint

       call clear_error()
       a=vhkl(1)/cell(1)
       b=vhkl(2)/cell(2)
       c=vhkl(3)/cell(3)
       al=cosd(cell(4))
       be=cosd(cell(5))
       ga=cosd(cell(6))
       d=a*(1.0-al*al) + b*(al*be-ga) + c*(ga*al-be)
       e=b*(1.0-be*be) + c*(be*ga-al) + a*(al*be-ga)
       f=c*(1.0-ga*ga) + a*(ga*al-be) + b*(be*ga-al)
       g=1.0 - al*al - be*be - ga*ga + 2.0*al*be*ga
       If (g /= 0.0) Then
          ds=(a*d + b*e + c*f)/g
          If (ds > 0.0) Then
             ds=1.0/Sqrt(ds)
             sint=wave/(2.0*ds)
             If (Abs(sint) <= 1.0)Then
                th=asind(sint)
             Else
                Err_CFML%Ierr=2
                th=0.0
             End If
          Else
             Err_CFML%Ierr=1
             th=0.0
          End If
       Else
          Err_CFML%Ierr=-1
          ds=0.0
          th=0.0
       End If
    End Subroutine dspace

    !!----
    !!---- Module Subroutine Equatorial_Chi_Phi(z1,ch,ph)
    !!----    real(kind=cp), Intent(In), Dimension(3)  :: z1
    !!----    real(kind=cp), Intent(Out)               :: ch,ph
    !!----
    !!----    Calculate CHI, PHI to put the vector Z1 in the equatorial plane
    !!----    The reciprocal vector Z1 is given in cartesian components with
    !!----    respect to the laboratory system (Eq. 38 of Busing & Levy)
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Subroutine Equatorial_Chi_Phi(z1,ch,ph)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)  :: z1
       real(kind=cp), Intent(Out)               :: ch,ph

       !---- Local Variables ----!
       real(kind=cp) :: d

       If (z1(1) /= 0.0_cp .or. z1(2) /= 0.0_cp)  Then
          ph=atan2d(z1(2),z1(1))
          d=Sqrt(z1(1)*z1(1)+z1(2)*z1(2))
          ch=atan2d(z1(3),d)
       Else
          ph=0.0_cp
          ch=90.0_cp
          If(z1(3) < 0.0) ch=-ch
       End If

    End Subroutine Equatorial_Chi_Phi

    !!----
    !!---- Module Subroutine fixdnu(wave,z1,ch,ph,ga,om,nu)
    !!----    real(kind=cp), Intent(In)               :: wave
    !!----    real(kind=cp), Intent(In), Dimension(3) :: z1
    !!----    real(kind=cp), Intent(In)               :: nu
    !!----    real(kind=cp), Intent(Out)              :: ch,ph,ga,om
    !!----
    !!----    Calculate a setting CH,PH,GA,OM to put the diffracted beam at NU.
    !!----    PH puts the diffraction vector Z1 into the CHI circle (as for
    !!----    bisecting geometry), CH brings the vector to the appropriate NU
    !!----    and OM then positions the beam at GA.
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Subroutine fixdnu(wave,z1,nu,ch,ph,ga,om)
       !---- Arguments ----!
       real(kind=cp), Intent(In)               :: wave
       real(kind=cp), Intent(In), Dimension(3) :: z1
       real(kind=cp), Intent(In)               :: nu
       real(kind=cp), Intent(Out)              :: ch,ph,ga,om

       !--- Local Variables ---!
       real(kind=cp), Dimension(3) ::  z4
       real(kind=cp) :: ch1,ch2,twoth,theta, cosga

       call clear_error()
       !---- Get CH1 and PH as for bisecting geometry
       Call Angs_4c_bisecting(wave,z1,twoth,theta,ch1,ph)
       If (Err_CFML%Ierr == 0) Then
          If (Abs(cosd(nu)) <= 0.0001) Then
             !---- One unique vector can diffract at ABS(NU)=90. (for which THETA=45)
             If (.Not. (theta > 44.99 .and. theta < 45.01) )  Then
                Err_CFML%ierr=-1
             Else
                ga=90.0
                om=90.0
                ch2=Sign(1.0_cp,nu)*45.0_cp
                ch=ch1-ch2
                !---- Convert to -180.0 <= CH <= +180.0
                ch=ch-360.0_cp*Int((Sign(180.0_cp,ch)+ch)/360.0_cp)
             End If
          Else
             cosga=cosd(twoth)/cosd(nu)  !General expression coming from the definition of the angles
             If (Abs(cosga) > 1.0_cp) Then
                Err_CFML%Ierr=-2
                Err_CFML%Msg = " Error: abs(cos(gamma)) > 1 @ fixdnu"
             Else
                ga=acosd(cosga)
                !- --- Diffraction vector in lab system
                z4 = z4frgn(wave,ga,nu)
                om=atan2d(-z4(2),z4(1))
                !---- CH2 is the angle between the scattering vector and horizontal plane
                ch2=asind(z4(3)*wave/(2.0*sind(theta)))
                ch=ch1-ch2
                !---- Convert to -180.0<= CH <=+180.0
                ch=ch-360.0_cp*Int((Sign(180.0_cp,ch)+ch)/360.0_cp)
             End if
          End If
       End If

       If (Err_CFML%Ierr /= 0) Then
          ch=0.0_cp
          ph=0.0_cp
          ga=0.0_cp
          om=0.0_cp
       End If
    End Subroutine fixdnu

    !!----
    !!---- Module Subroutine Get_Angs_NB(wave,z1,ga,om,nu)
    !!----    real(kind=cp), Intent(In)                      :: wave
    !!----    real(kind=cp), Intent(In),Dimension(3)         :: z1
    !!----    real(kind=cp), Intent(Out)                     :: ga,om,nu
    !!----
    !!----    Calculate normal-beam angles GAMMA, OMEGA, NU to put the
    !!----    vector Z1 into the diffracting condition.
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine Get_Angs_NB(wave,z1,ga,om,nu)
       !---- Arguments ----!
       real(kind=cp), Intent(In)                      :: wave
       real(kind=cp), Intent(In),Dimension(3)         :: z1
       real(kind=cp), Intent(Out)                     :: ga,om,nu

       !--- Local Variables ---!
       real(kind=cp), Dimension(3)   ::  znew
       real(kind=cp), Dimension(3,3) ::  dum
       real(kind=cp)                 :: theta,d,a,sint,b

       call clear_error()
       Call Get_dspacing_theta(wave,z1,d,theta)
       ga=0.0_cp
       om=0.0_cp
       nu=0.0_cp
       If (Err_CFML%Ierr == 0) Then
          a=Sqrt(z1(1)*z1(1)+z1(2)*z1(2))
          If (a <= 1.0e-10_cp) Then
             !---- Anything on the omega axis is blind
             Err_CFML%ierr=-1
          Else
             sint=sind(theta)
             b=2.0_cp*sint*sint/(wave*a)
             If (b > 1.0_cp) Then
                Err_CFML%ierr=-2
             Else
                a=-atan2d(z1(2),-z1(1))
                b=-asind(b)
                om=a+b
                dum= Phi_mat(om)
                znew=Matmul(dum,z1)
                If (znew(1) <= 0.0_cp) Then
                   om=om-2.0_cp*atan2d(-znew(1),-znew(2))
                End If
                om=om-360.0_cp*Int((Sign(180.0_cp,om)+om)/360.0_cp)
                nu=asind(wave*z1(3))
                ga=acosd(cosd(2.0_cp*theta)/cosd(nu))
             End If
          End If
       End If
    End Subroutine Get_Angs_NB

    !!----
    !!---- Module Subroutine Get_dspacing_theta(wave,z1,ds,th)
    !!----    real(kind=cp), Intent(In)                 :: wave
    !!----    real(kind=cp), Intent(In), Dimension(3)   :: z1
    !!----    real(kind=cp), Intent(Out)                :: ds,th
    !!----
    !!----    Calculate D-spacing (real space) and THETA from the length of Z1
    !!----    The reciprocal vector Z1 is given in cartesian components with
    !!----    respect to the laboratory system. If Err_CFML%Ierr=1 the calculated d-spacing
    !!----    is is fixed to 0.0 as well as theta. This error condition appears when
    !!----    the length of the reciprocal vector z1 is lower or equal to 0.0001
    !!----    If Err_CFML%Ierr=2 the reflection is outside the resolution sphere.
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine Get_dspacing_theta(wave,z1,ds,th)
       !---- Arguments ----!
       real(kind=cp), Intent(In)                 :: wave
       real(kind=cp), Intent(In), Dimension(3)   :: z1
       real(kind=cp), Intent(Out)                :: ds,th

       !---- Local Variables ---!
       real(kind=cp) :: dstar, sint

       call clear_error()
       dstar=Sqrt(Dot_Product(z1,z1))
       If (dstar > 0.00001_cp) Then
          ds=1.0/dstar
          sint=wave*dstar/2.0_cp
          If (Abs(sint) <= 1.0_cp) Then
             th=asind(sint)
          Else
             Err_CFML%Ierr=2
             Err_CFML%Msg=" Reflection outside the resolution sphere"
             th=0.0_cp
          End If
       Else
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Null input vector z1 @ Get_dspacing_theta"
          ds=0.0_cp
          th=0.0_cp
       End If
    End Subroutine Get_dspacing_theta

    !!----
    !!---- Module Subroutine Get_GaOmNu_frChiPhi(wave,z1,ch,ph,ga,om,nu)
    !!----    real(kind=cp), Intent(In)                :: wave
    !!----    real(kind=cp), Intent(In), Dimension(3)  :: z1
    !!----    real(kind=cp), Intent(In)                :: ch,ph
    !!----    real(kind=cp), Intent(Out)               :: ga,om,nu
    !!----
    !!----    Given CHI & PHI, calculate normal-beam angles GAMMA, OMEGA, NU
    !!----    to put the vector Z1 into the diffraction condition.
    !!----    converts VLAB2=[CHI0].[PHI0].VLAB1 and relies on subroutine
    !!----    Get_Angs_NB to do the rest!
    !!----    The reciprocal vector Z1 is given in Cartesian components with
    !!----    respect to the laboratory system.
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Subroutine Get_GaOmNu_frChiPhi(wave,z1,ch,ph,ga,om,nu)
       !---- Arguments ----!
       real(kind=cp), Intent(In)                :: wave
       real(kind=cp), Intent(In), Dimension(3)  :: z1
       real(kind=cp), Intent(In)                :: ch,ph
       real(kind=cp), Intent(Out)               :: ga,om,nu

       !--- Local Variables ---!
       real(kind=cp),Dimension(3) :: z3

       call clear_error()
       z3=z3frz1(z1,ch,ph)
       Call Get_Angs_NB(wave,z3,ga,om,nu)
    End Subroutine Get_GaOmNu_frChiPhi

    !!----
    !!---- Module Subroutine Get_WaveGaNu_frZ4(z4,wave,ga,nu)
    !!----    real(kind=cp),    Intent(In), Dimension(3)  :: z4
    !!----    real(kind=cp),    Intent(Out)               :: wave,ga,nu
    !!----
    !!----    Calculate GA, NU and wavelength for diffraction
    !!----    vector in laboratory system
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Subroutine Get_WaveGaNu_frZ4(z4,wave,ga,nu)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)  :: z4
       real(kind=cp), Intent(Out)               :: wave,ga,nu

       !--- Local Variables ---!
       real(kind=cp) :: a,b,sinnu, cosnu,cosga, singa

       call clear_error()
       a=-2.0_cp*z4(2)
       b=Dot_Product(z4,z4)
       If (.Not. (a <= 0.0_cp .or. b <= 0.0_cp) ) Then
          wave=a/b
          sinnu=wave*z4(3)
          If (Abs(sinnu) > 1.0_cp) Then
             Err_CFML%Ierr=1
             Err_CFML%Msg=" Error: abs(sin(nu)) > 1  @ Get_WaveGaNu_frZ4"
             nu=0.0_cp
             ga=0.0_cp
          Else If(Abs(sinnu) < 1.0_cp) Then
             nu=asind(sinnu)
             cosnu=Sqrt(1.0_cp - sinnu*sinnu)
             singa=wave*z4(1)/cosnu
             If (Abs(singa) > 1.0_cp) Then
                Err_CFML%Ierr=1
                Err_CFML%Msg=" Error: abs(sin(gamma)) > 1  @ Get_WaveGaNu_frZ4"
                nu=0.0_cp
                ga=0.0_cp
                Return
             End If
             cosga=(wave*z4(2)+1.0_cp)/cosnu
             ga=atan2d(singa,cosga)
          Else
             nu=90.0_cp
             ga=0.0_cp
          End If
       Else
          wave=0.0_cp
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Error: bad z4 input > 1  @ Get_WaveGaNu_frZ4"
          nu=0.0_cp
          ga=0.0_cp
       End If

    End Subroutine Get_WaveGaNu_frZ4

    !!----
    !!---- Module Subroutine Normal_Beam_Angles(wav,ub,h,sig,anbcal,ier,zer,nusign)
    !!----    real(kind=cp),                 Intent(In)         :: wav    ! Wavelength
    !!----    real(kind=cp), dimension(3,3), Intent(In)         :: ub     ! [UB] matrix or Chi.Phi.[UB] matrix
    !!----    real(kind=cp), dimension(3),   Intent(In)         :: h      ! Miller indices of reflection h
    !!----    Integer,                       Intent(In Out)     :: sig    ! -1 for negative gammas
    !!----    real(kind=cp), dimension(:),   Intent(Out)        :: anbcal ! Normal beam angles
    !!----    Integer,                       Intent(In Out)     :: ier    ! Zero correction on input, Error flag on output
    !!----    real(kind=cp), dimension(3),   Intent(In),optional:: zer    ! zero corrections of NB angles in degrees
    !!----    integer,                       Intent(In),optional:: nusign ! Sign of the nu-angle (=-1 if nu is positive when elevation towards z < 0)
    !!----
    !!--<<    Based on the subroutine CANNB from  A.FILHOL 01-Avr-1981 & 08-Aou-1984
    !!----    Calculation of the normal-beam diffraction angles for reflection h()
    !!----    from the [UB] matrix of the crystal.
    !!----    On input:
    !!----    sig      : 1/-1 defines the sign for "GAMMA"
    !!----    If zer is provided, the calculated angles are corrected for zero shifts
    !!----
    !!----    On output:
    !!----      Angles(calculated) = Angles(theoretical) + zero shifts
    !!----      ANBCAL(1:4) -> GAMMA, OMEGA(NB), NU, THETA   (in degrees)
    !!----     -> Error Flag
    !!----        ier  : 0   all angles are calculable
    !!----             : 1   reflection hors sphere d'Ewald
    !!----             : 2   reflection dans zone aveugle  ==> angle "NU"
    !!----             : 3   reflection dans zone aveugle  ==> angle "GAMMA"
    !!-->>             : 4   H,K,L tous les trois nuls
    !!----
    !!---- Created: April 2008, Updated: April 2013 (JRC)
    !!
    Module Subroutine Normal_Beam_Angles(wav,ub,h,sig,anbcal,ier,zer,nusign)
       !---- Arguments ----!
       real(kind=cp),                 Intent(In)          :: wav
       real(kind=cp), dimension(3,3), Intent(In)          :: ub
       real(kind=cp), dimension(3),   Intent(In)          :: h
       Integer,                       Intent(In Out)      :: sig
       real(kind=cp), dimension(:),   Intent(Out)         :: anbcal
       Integer,                       Intent(Out)         :: ier
       real(kind=cp), dimension(3),   Intent(In),optional :: zer
       integer,                       Intent(In),optional :: nusign

       !---- Local Variables ----!
       real(kind=cp), Dimension(3) ::  z1,zo1 !Scattering vectors in Lab-system
       real(kind=cp)               ::  ds,dpl, thc, sthc,snu,cnu,cga,sga, &
                                       oma,omb,tom1,tom2,ome,wav2,sn

       ier=0
       anbcal(:)=0.0_cp
       sn=1.0_cp
       if(present(nusign)) sn=real(nusign)

       wav2=wav*wav
       If(sig == 0) sig=1

       !---- Calculation of angle THETA and tests, Ewald sphere limits , etc.

       z1=matmul(ub,h)                ! h in laboratory system
       ds=sqrt(dot_product(z1,z1))    ! |h|=dstar
       If (ds < 0.000001_cp) Then
         ier=4
         Return
       End If
       zo1=z1/ds                      ! unit vector along z1
       dpl= z1(1)*z1(1) + z1(2)*z1(2) ! Projection of z1 on equatorial plane

       !---- test limit Ewald sphere
       sthc = ds*wav*0.5_cp
       If(Abs(sthc) > 1.0_cp) Then
         ier=1
         Return
       End If

       !-- Bragg angle THETA
       thc = Asind(sthc)
       thc = real(sig)*thc

       !-- NU   (Elevation angle of scattered beam)
       snu=sn*wav*z1(3)         !Sin(nu) sn=1 for nu > 0 when elevation w.r.t. positive z-axis
       If (Abs(snu) > 1.0_cp  .or. dpl < 0.00001_cp) Then
          ier=2
          Return
       End If
       cnu=Sqrt(1.0_cp-snu*snu)   !Cos(nu)

       !-- GAM    (Projection of 2THETA on the equatorial plane)
       cga=(1.0_cp+cnu*cnu)-wav2*dpl
       if (cnu < 0.00001_cp) then
         cga = 2.0
       else
         cga=cga/(2.0_cp*cnu)
       end if
       If (Abs(cga) > 1.0_cp) Then
          ier=3
          Return
       End If
       sga=sig*Sqrt(1.0_cp-cga*cga)

       !-- OME(NB)   (Rotation angle of the sample)
       oma=sig*cnu*sga
       omb=1.0_cp-cnu*cga
       tom1=omb*z1(1)+oma*z1(2)
       tom2=oma*z1(1)-omb*z1(2)
       ome=Atan2d(sig*tom1,tom2)

       !- Final angle values
       anbcal(1)=sig*Acosd(cga) !Gamma of reflection
       anbcal(2)=ome            !Omega
       anbcal(3)=Asind(snu)     !Nu
       anbcal(4)=thc            !Theta
       if (present(zer)) then
          anbcal(1:3) = anbcal(1:3) + zer(1:3)
       end if
    End Subroutine Normal_Beam_Angles

    !!----
    !!---- Module Function s4cnb(angl_4C) Result(angl_NB)
    !!----    real(kind=cp), dimension(4), Intent(In)     :: angl_4C
    !!----    real(kind=cp), dimension(3), Intent(Out)    :: angl_NB
    !!----
    !!--<<    Subroutine *** S4CNB ***   A.Filhol & M.Thomas
    !!----    Conversion of diffraction angles from the geometry
    !!----    4-CERCLES === TO ==> NORMAL-BEAM
    !!----        "ANGL_4C()"                       "ANGL_NB()"
    !!----   (/ 2Theta, Omega, Chi, Phi /)   (/ Gamma, Omega_nb, nu /)
    !!----    Error messages:
    !!----      IERR=0  O.K.
    !!----      IERR=1  calculation of nu impossible
    !!----      IERR=2  calculation of gamma impossible
    !!-->>      IERR=3  calculation of phi impossible
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function s4cnb(angl_4C) Result(angl_NB)
       !---- Arguments ----!
       real(kind=cp), dimension(4), Intent(In) :: angl_4C
       real(kind=cp), dimension(3)             :: angl_NB

       !--- Local variables ---!
       real(kind=cp) :: si, sint, sint2, sinnu, phi

       si=1.0_cp
       call clear_error()

       !angl_4C=(/ 2Theta, Omega, Chi, Phi /)
       !angl_NB=(/ Gamma, Omega_nb, nu /)

       !.....NU NB
       sint=Sind(angl_4C(1)/2.0_cp)
       sint2=sint*sint
       angl_NB(3)=2.0_cp*Sind(angl_4C(3)) * sint
       If (Abs(angl_NB(3)) > 1.0_cp ) Then
          Err_CFML%Ierr=1
          return
       End If
       angl_NB(3)= Asind(angl_NB(3))
       angl_NB(3)= Abs(angl_NB(3)) *Sign(si,angl_4C(3))
       sinnu = Sind(angl_NB(3))

       !.....GAMMA NB
       angl_NB(1)=Cosd(angl_4C(1))/Cosd(angl_NB(3))
       If (Abs(angl_NB(1)) > 1.0_cp ) Then
          Err_CFML%Ierr=2
          Return
       End If
       angl_NB(1)=Acosd(angl_NB(1))
       angl_NB(1)=angl_NB(1)*Sign(si,angl_4C(1))

       !.....OMEGA NB
       phi=2.0_cp*sint2/Sqrt(4.0_cp*sint2-sinnu**2)
       If (Abs(phi) > 1.0_cp ) Then
          Err_CFML%Ierr=3
          Return
       End If
       phi=Acosd(phi)-90.0_cp
       phi=phi*Sign(si,angl_4C(1))
       angl_NB(2)=angl_4C(4)-phi
    End Function s4cnb

    !!----
    !!---- Module Function snb4c(angl_NB) Result(angl_4C)
    !!----    real(kind=cp), dimension(3), Intent(In )  :: angl_NB
    !!----    real(kind=cp), dimension(4)               :: angl_4C
    !!--<<
    !!----    Subroutine ***SNB4C ***      A.Filhol & M.Thomas (I.L.L.)
    !!----   Conversion of diffraction angles from the geometry
    !!----   NORMAL-BEAM  === TO ==>  4-CERCLES
    !!-->>      "ANGL_NB()"                         "ANGL_4C()"
    !!----    (/ Gamma, Omega_nb, nu /)  (/ 2Theta, Omega, Chi, Phi /)
    !!----
    !!---- Update: April 2008
    !!
    Module Function snb4c(angl_NB) Result(angl_4C)
       !---- Arguments ----!
       real(kind=cp), dimension(3), Intent(In )  :: angl_NB
       real(kind=cp), dimension(4)               :: angl_4C

       !--- Local variables ---!
       real(kind=cp) :: si, sint, sint2, sinnu, phi

       si=1.0_cp
       !.....2THETA 4C
       angl_4C(1)=Acosd(Cosd(angl_NB(1))*Cosd(angl_NB(3)))
       angl_4C(1)=angl_4C(1)*Sign(si,angl_NB(1))

       !.....OMEGA 4C
       angl_4C(2)=angl_4C(1)/2.0_cp

       !.....CHI 4C
       sint=Sind(angl_4C(2))
       sint2=sint*sint
       sinnu=Sind(angl_NB(3))
       angl_4C(3)=Asind(sinnu/(2.0_cp* sint))

       ! ***** On D15, both NU & 2-THETA might be positive or negative
       angl_4C(3)= Abs(angl_4C(3)) *Sign(si,angl_NB(3))

       !.....PHI 4C
       phi=2.0_cp*sint2/Sqrt(4.0_cp*sint2-sinnu**2)
       phi=Acosd(phi)-90.0_cp
       phi=phi*Sign(si,angl_NB(1))
       angl_4C(4)=angl_NB(2)+phi

    End Function snb4c

 End SubModule SXTAL_Angles