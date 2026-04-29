 SubModule (CFML_SXTAL_Geom) SXTAL_Matx_Zvect

  implicit none

  Contains

    !!----
    !!---- Module Function Chi_mat(chi) Result(dum)
    !!----    real(kind=cp), Intent(In)      :: chi
    !!----    real(kind=cp), Dimension(3,3)  :: dum
    !!----
    !!----    Calculate the Busing and Levy conventional rotation matrix for CHI (eq. 9)
    !!----    The CHI angle must be provided in degrees.
    !!----
    !!---- Update: April 2008
    !!
    Module Function Chi_mat(chi) Result(dum)
       !---- Arguments ----!
       real(kind=cp), Intent(In)      :: chi
       real(kind=cp), Dimension(3,3)  :: dum

       dum=0.0_cp
       dum(1,1)=cosd(chi)
       dum(1,3)=sind(chi)
       dum(2,2)=1.0_cp
       dum(3,1)=-dum(1,3)
       dum(3,3)= dum(1,1)
    End Function Chi_mat

    !!----
    !!---- Module Function Phi_mat(phi) Result(dum)
    !!----    real(kind=cp), Intent(In)      :: phi
    !!----    real(kind=cp), Dimension(3,3)  :: dum
    !!----
    !!----    Calculate the Busing and Levy conventional rotation matrix for PHI
    !!----    or OMEGA [eq. 8 & 10]. The PHI/OMEGA angle must be provided in degrees.
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function Phi_mat(phi) Result(dum)
       !---- Arguments ----!
       real(kind=cp), Intent(In)    :: phi
       real(kind=cp), Dimension(3,3):: dum

       dum=0.0_cp
       dum(1,1)= cosd(phi)
       dum(1,2)= sind(phi)
       dum(2,1)=-dum(1,2)
       dum(2,2)= dum(1,1)
       dum(3,3)= 1.0_cp
    End Function Phi_mat

    !!----
    !!---- Module Function  Psi_mat(psi) Result(dum)
    !!----    real(kind=cp), Intent(In)      :: psi
    !!----    real(kind=cp), Dimension(3,3)  :: dum
    !!----
    !!----    Calculate the Busing and Levy  [eq. 46] conventional rotation matrix for PSI
    !!----    The PSI angle must be provided in degrees.
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function Psi_mat(psi) Result(dum)
       !---- Arguments ----!
       real(kind=cp), Intent(In)     :: psi
       real(kind=cp), Dimension(3,3) :: dum

       dum=0.0_cp
       dum(1,1)=1.0_cp
       dum(2,2)= cosd(psi)
       dum(2,3)=-sind(psi)
       dum(3,2)=-dum(2,3)
       dum(3,3)= dum(2,2)
    End Function Psi_mat

    !!----
    !!---- Module Function Get_z1_D9angls(wave,ttheta,om,ch,ph) Result(z1)
    !!----    real(kind=cp), intent (in)  :: wave,ttheta,om,ch,ph
    !!----    real(kind=cp), dimension(3) :: z1
    !!----
    !!----  !This function should be checked ... no reference to pixels are inside
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function Get_z1_D9angls(wave,ttheta,om,ch,ph) Result(z1)
       !---- Arguments ----!
       real(kind=cp), intent (in)  :: wave,ttheta,om,ch,ph
       real(kind=cp), dimension(3) :: z1

       !--- Local Variables ---!
       real(kind=cp) :: th,dsp,ome,cosom,coski,cosfi,sinom,sinfi

       th=ttheta*0.5_cp
       dsp= 2.0_cp*sind(th)/wave
       ome=om-th
       cosom=cosd(ome)
       sinom=sind(ome)
       coski=cosd(ch)
       sinfi=sind(ph)
       cosfi=cosd(ph)
       z1(1)=cosom*coski*cosfi-sinom*sinfi
       z1(2)=cosom*coski*sinfi+sinom*cosfi
       z1(3)=cosom*sind(ch)
       z1=z1*dsp
    End Function Get_z1_D9angls

    !!----
    !!---- Module Function z1frfc(wave,tth,om,ch,ph,z1)
    !!----    real(kind=cp), Intent(In)   :: wave,tth,om,ch,ph
    !!----    real(kind=cp), Dimension(3) :: z1
    !!----
    !!----    Z1 from Four Circle diffractometer angles
    !!----    Calculate diffraction vector Z1 from TTH, OM, CH, PH (Need not
    !!----    be bisecting, but Z4 is assumed to be in the equatorial plane)
    !!----
    !!---- Update: July 2008, June 2020
    !!
    Module Function z1frfc(wave,tth,om,ch,ph) Result(z1)
       !---- Argument ----!
       real(kind=cp), Intent(In)   :: wave,tth,om,ch,ph
       real(kind=cp), Dimension(3) :: z1

       !--- Local Variables ---!
       real(kind=cp),Dimension(3) ::  z4

       z4(1)=sind(tth)/wave         ! ( 2.0*sind(tth)*cosd(tth))/wave
       z4(2)=(cosd(tth)-1.0)/wave   ! (-2.0*sind(tth)*sind(tth))/wave
       z4(3)=0.0                    ! 0.0
       z1=z1frz4(z4,om,ch,ph)
    End Function z1frfc

    !!----
    !!---- Module Function  z1frmd(wave,ch,ph,ga,om,nu) Result(z1)
    !!----    real(kind=cp), Intent(In)    :: wave,ch,ph,ga,om,nu
    !!----    real(kind=cp), Dimension(3)  :: z1
    !!----
    !!----    Z1 from Four Circle + PSD multi-detector diffractometer angles
    !!----    Calculate diffraction vector Z1 from CH, PH, GA, OM, NU
    !!----    for a multi-detector. The angles Chi,Phi,Gamma,Omega and Nu for
    !!----    the equatorial plane are Chi,Phi,2Theta and Omega (Nu=0)
    !!----
    !!---- Update: April 2008
    !!
    Module Function  z1frmd(wave,ch,ph,ga,om,nu) Result(z1)
       !---- Argument ----!
       real(kind=cp), Intent(In)    :: wave,ch,ph,ga,om,nu
       real(kind=cp), Dimension(3)  :: z1

       !--- Local Variables ---!
       real(kind=cp), Dimension(3)   ::  z3
       z3=z1frnb(wave,ga,om,nu)
       z1=z1frz3(z3,ch,ph)
    End Function z1frmd

    !!----
    !!---- Module Function  z1frnb(wave,ga,om,nu,z1)
    !!----    real(kind=cp), Intent(In)                 :: wave,ga,om,nu
    !!----    real(kind=cp), Intent(Out), Dimension(3)  :: z1
    !!----
    !!----    Z1 from Normal Beam diffractometer angles
    !!----    Calculates diffraction vector Z1 from GA, OM, NU, assuming CH=PH=0
    !!----    This is is the normal beam geometry for a Lifting arm detector or
    !!----    for a PSD with a single Omega axis for the sample.
    !!----
    !!---- Update: April 2008
    !!
    Module Function z1frnb(wave,ga,om,nu) Result(z1)
       !---- Arguments ----!
       real(kind=cp), Intent(In)     :: wave,ga,om,nu
       real(kind=cp),  Dimension(3)  :: z1

       !--- Local Variables ---!
       real(kind=cp), Dimension(3,3) ::  omg
       real(kind=cp), Dimension(3)   ::  z4

       z4 = z4frgn(wave,ga,nu)
       omg= Phi_mat(om)
       z1=Matmul(Transpose(omg),z4)
    End Function z1frnb

    !!----
    !!---- Module Function z1frz2(z2,ph,z1)
    !!----    real(kind=cp), Intent(In ), Dimension(3)  :: z2
    !!----    real(kind=cp), Intent(In )                :: ph
    !!----    real(kind=cp), Intent(Out), Dimension(3)  :: z1
    !!----
    !!----    Calculates Z1 = [PHI]T.Z2
    !!----
    !!---- Update: April 2008
    !!
    Module Function z1frz2(z2,ph) Result(z1)
       !---- Arguments ----!
       real(kind=cp), Intent(In ), Dimension(3)  :: z2
       real(kind=cp), Intent(In )                :: ph
       real(kind=cp),              Dimension(3)  :: z1

       !--- Local Variables ---!
       real(kind=cp),Dimension(3,3)  :: phim

       phim= Phi_mat(ph)
       z1=Matmul(Transpose(phim),z2)
    End Function z1frz2

    !!----
    !!----   Module Function z1frz3(z3,ch,ph) Result(z1)
    !!----      real(kind=cp), Intent(In), Dimension(3)   :: z3
    !!----      real(kind=cp), Intent(In)                 :: ch,ph
    !!----      real(kind=cp), Intent(Out), Dimension(3)  :: z1
    !!----
    !!----    Calculates Z1 = [PHI]T.[CHI]T.Z3
    !!----
    !!---- Update: April 2008
    !!
    Module Function z1frz3(z3,ch,ph) Result(z1)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)   :: z3
       real(kind=cp), Intent(In)                 :: ch,ph
       real(kind=cp),             Dimension(3)   :: z1

       !--- Local Variables ---!
       real(kind=cp),Dimension(3)    :: z2
       real(kind=cp),Dimension(3,3)  :: chim

       chim = Chi_mat(ch)
       z2=Matmul(Transpose(chim),z3)
       z1=z1frz2(z2,ph)
    End Function z1frz3

    !!----
    !!---- Module Function z1frz4(z4,om,ch,ph) Result(z1)
    !!----    real(kind=cp), Intent(In), Dimension(3)   :: z4
    !!----    real(kind=cp), Intent(In)                 :: om,ch,ph
    !!----    real(kind=cp),             Dimension(3)   :: z1
    !!----
    !!----    Calculates Z1 = [PHI]T.[CHI]T.[OM]T.Z4
    !!----
    !!---- Update: April 2008
    !!
    Module Function z1frz4(z4,om,ch,ph) Result(z1)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)   :: z4
       real(kind=cp), Intent(In)                 :: om,ch,ph
       real(kind=cp),             Dimension(3)   :: z1

       !--- Local Variables ---!
       real(kind=cp),Dimension(3)    :: z3
       real(kind=cp),Dimension(3,3)  :: phim

       phim=Phi_mat(om)
       z3=Matmul(Transpose(phim),z4)
       z1=z1frz3(z3,ch,ph)
    End Function z1frz4

    !!----
    !!---- Module Function z2frz1(z1,ph) Result(z2)
    !!----    real(kind=cp), Intent(In),Dimension(3)    :: z1
    !!----    real(kind=cp), Intent(In)                 :: ph
    !!----    real(kind=cp),            Dimension(3)    :: z2
    !!----
    !!----    Calculates Z2 = [PHI].Z1
    !!----    The reciprocal vector Z1 is given in cartesian components with
    !!----    respect to the laboratory system.
    !!----
    !!---- Update: April 2008
    !!
    Module Function z2frz1(z1,ph) Result(z2)
       !---- Arguments ----!
       real(kind=cp), Intent(In),Dimension(3)    :: z1
       real(kind=cp), Intent(In)                 :: ph
       real(kind=cp),            Dimension(3)    :: z2

       z2=Matmul(Phi_mat(ph),z1)
    End Function z2frz1

    !!----
    !!----  Module Function z3frz1(z1,ch,ph)  Result(z3)
    !!----    real(kind=cp), Intent(In), Dimension(3)    :: z1
    !!----    real(kind=cp), Intent(In)                  :: ch,ph
    !!----    real(kind=cp),             Dimension(3)    :: z3
    !!----
    !!----    Calculates Z3 = [CHI].Z2 = [CHI].[PHI].Z1
    !!----    The reciprocal vector Z1 is given in cartesian components with
    !!----    respect to the laboratory system.
    !!----
    !!---- Update: April 2008
    !!
    Module Function z3frz1(z1,ch,ph)  Result(z3)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)    :: z1
       real(kind=cp), Intent(In)                  :: ch,ph
       real(kind=cp),             Dimension(3)    :: z3

       !--- Local Variables ---!
       real(kind=cp), Dimension(3)   ::  z2

       z2=z2frz1(z1,ph)
       z3=Matmul(Chi_mat(ch),z2)
    End Function z3frz1

    !!----
    !!---- Module Function z4frgn(wave,ga,nu) Result(z4)
    !!----    real(kind=cp), Intent(In)    :: wave
    !!----    real(kind=cp), Intent(In)    :: ga,nu
    !!----    real(kind=cp), Dimension(3)  :: z4
    !!----
    !!----    Calculates diffraction vector of a reflection in the
    !!----    laboratory system from the angles Gamma and NU.
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function z4frgn(wave,ga,nu) Result(z4)
       !---- Arguments ----!
       real(kind=cp), Intent(In)    :: wave
       real(kind=cp), Intent(In)    :: ga,nu
       real(kind=cp), Dimension(3)  :: z4

       z4(1)=( sind(ga)*cosd(nu)     )/wave
       z4(2)=( cosd(ga)*cosd(nu)-1.0 )/wave  ! without 1 => Cartesian system on the
       z4(3)=( sind(nu)              )/wave  ! centre of Ewald sphere
    End Function z4frgn

    !!----
    !!---- Module Function z4frz1(z1,om,ch,ph) Result(z4)
    !!----    real(kind=cp), Intent(In), Dimension(3)    :: z1
    !!----    real(kind=cp), Intent(In)                  :: om,ch,ph
    !!----    real(kind=cp),             Dimension(3)    :: z4
    !!----
    !!----    Calculates Z4 = [OM].[CHI].[PHI].Z1
    !!----
    !!---- Update: April 2008
    !!
    Module Function z4frz1(z1,om,ch,ph) Result(z4)
       !---- Arguments ----!
       real(kind=cp), Intent(In), Dimension(3)    :: z1
       real(kind=cp), Intent(In)                  :: om,ch,ph
       real(kind=cp),             Dimension(3)    :: z4

       !--- Local Variables ---!
       real(kind=cp), Dimension(3)  ::  z3
       real(kind=cp), Dimension(3,3)::  omg

       z3= z3frz1(z1,ch,ph)
       omg = Phi_mat(om)
       z4=Matmul(omg,z3)
    End Function z4frz1

 End SubModule SXTAL_Matx_Zvect
