!!----
!!----
!!----
SubModule (CFML_SXTAL_Geom) SXTAL_FlatCone

  implicit none

  Contains

    !!----
    !!---- Module Subroutine Calc_Om_Chi_Phi(vhkl,vlab1,psi,ub,om,ch,ph)
    !!----    real(kind=cp), Intent(In),Dimension(3)     :: vhkl,vlab1
    !!----    real(kind=cp), Intent(In)                  :: psi
    !!----    real(kind=cp), Intent(In),Dimension(3,3)   :: ub
    !!----    real(kind=cp), Intent(In Out)              :: om,ch,ph
    !!----
    !!----    Calculate OM, CH, PH for diffraction vector at azimuthal angle PSI
    !!----    from the diffraction condition expressed as :
    !!----    [TZ]=[OM].[CH].[PH].[TS].[PSI]-1 if [R]=[OM].[CH].[PH]
    !!----    then [R]'=[TZ].[PSI].[TS]-1
    !!----    The om,ch,ph angles are provided, on input, to calculate the components
    !!----    of the vector vlab1 in the Theta-system for Psi=0
    !!----    Used only in the Flat_Cone_vertDet subroutine.
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine Calc_Om_Chi_Phi(vhkl,vlab1,psi,ub,om,ch,ph)
       !---- Arguments ----!
       real(kind=cp), Intent(In),Dimension(3)     :: vhkl,vlab1
       real(kind=cp), Intent(In)                  :: psi
       real(kind=cp), Intent(In),Dimension(3,3)   :: ub
       real(kind=cp), Intent(In Out)              :: om,ch,ph

       !--- Local Variables ---!
       real(kind=cp),Dimension(3)   :: z1,z4,vs,vz
       real(kind=cp),Dimension(3,3) :: r,dum1,dum2,ts,tz

       call clear_error()
       z1=vlab1
       z4= z4frz1(z1,om,ch,ph)
       Call refvec(vhkl,ub,vs,vz)
       Call triple(z1,vs,ts)
       Call triple(z4,vz,tz)
       If(Err_CFML%Ierr /= 0) Return
       r=Transpose(ts)
       dum2= Psi_mat(psi)
       dum1=Matmul(dum2,r)
       r=Matmul(tz,dum1)

       !---- Solve R for new Eulerian components
       If (Abs(r(3,3)) < 1.0) Then

          !---- We choose 0.0<= CH <=180.0 but remember that an equivalent
          !---- solution is CH'=-CH, PH'=180.0+PH, OM'=180.0+OM
          om=atan2d(-r(2,3),r(1,3))
          ch=atan2d( Sqrt(r(1,3)*r(1,3)+r(2,3)*r(2,3)), r(3,3) )
          ph=atan2d( -r(3,2), -r(3,1) )
       Else
          !---- The singular case of CH=0; OM and PH no longer independent
          !---- Choose OM=90.0+THE angle that bisects projected INC. and DIF. beams
          z4= z4frz1(vlab1,om,ch,ph)
          om=90.0+atan2d(-z4(2),z4(1))
          If(r(3,3) > 0.0) ch=0.0
          If(r(3,3) < 0.0) ch=180.0
          ph=Sign(1.0_cp,r(3,3))*(atan2d(r(1,2),r(2,2))-om)

          !---- Convert -180.0 <= PH <= +180.0
          ph=ph-360.0_cp*Int((Sign(180.0_cp,ph)+ph)/360.0_cp)
       End If

    End Subroutine Calc_Om_Chi_Phi

    !!----
    !!---- Module Subroutine Flat_Cone_vertDet(wave,z1,ub,vrho,rho,ch,ph,ga,om,nu)
    !!----    real(kind=cp), Intent(In)                   :: wave
    !!----    real(kind=cp), Intent(In),  Dimension(3)    :: z1
    !!----    real(kind=cp), Intent(In),Dimension(3,3)    :: ub
    !!----    real(kind=cp), Intent(In Out), Dimension(3) :: vrho
    !!----    real(kind=cp), Intent(Out)                  :: rho,ch,ph,ga,om,nu
    !!----
    !!----    Calculate RHO (=PSI) about given rotation vector VRHO (and the
    !!----    corresponding angles OM, CH, PH) to put the vector Z1 into the
    !!----    flat-cone diffracting position. Calculate resulting GAMMA and NU.
    !!----
    !!--<<    V = [UB].H = [UB].[G].D where D is a real-space vector.
    !!-->>        [G]-1 = [B]T.[B] = [UB]T.[UB] hence V = [[UB]T]-1.D
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine Flat_Cone_vertDet(wave,z1,ub,vrho,rho,ch,ph,ga,om,nu)
       !---- Arguments ----!
       real(kind=cp), Intent(In)                      :: wave
       real(kind=cp), Intent(In),     Dimension(3)    :: z1
       real(kind=cp), Intent(In),   Dimension(3,3)    :: ub
       real(kind=cp), Intent(In Out), Dimension(3)    :: vrho
       real(kind=cp), Intent(Out)                     :: rho,ch,ph,ga,om,nu

       !---- Local Variables ----!
       real(kind=cp), Dimension(3)   ::  z3, z4, v1
       real(kind=cp), Dimension(3,3) ::  dum1
       real(kind=cp)                 ::  pstar, theta, duf, sinmu, cosnu, psi1,csi1,csi2

       call clear_error()
       dum1=Transpose(ub)
       dum1=invert(dum1)
       v1=Matmul(dum1,vrho)  ! Components of vrho in reciprocal lattice
       Call normal(v1)
       If (err_CFML%Ierr == 0) Then
          !---- Pstar is the reciprocal spacing of this layer
          pstar= Dot_Product(z1,v1)
          If (pstar < 0.0_cp) Then
             v1=-v1
             vrho=-vrho
             pstar=-pstar
          End If

          !---- Get GA (=MU) for this flat-cone, then NU.
          !---- Choose 0.0_cp <= GA <=180.0_cp and 0.0_cp <= NU <=90.0_cp  (GA,NU = 180.0_cp+GA,180.0_cp-NU)
          Call Get_dspacing_theta(wave,z1,duf,theta)
          If (Err_CFML%ierr == 0) Then
             sinmu=wave*pstar
             If (sinmu > 1.0_cp) Then
                Err_CFML%ierr=-1
             Else
                ga=asind(sinmu)
                !---- TH > 45.0_cp is equivalent to PSTAR > (SQRT(2.0_cp))/WAVE
                If (Abs(theta) > 45.0_cp) ga=180.0_cp-ga
                !---- TH = 45.0 is equivalent to MU=90.0, and NU is indeterminate.
                If (sinmu >= 1.0_cp) Then
                   nu=0.0_cp
                Else
                   cosnu=cosd(2.0_cp*theta)/cosd(ga)
                   If (Abs(cosnu) > 1.0_cp) Then
                      Err_CFML%ierr=-2
                   Else
                      nu=acosd(cosnu)
                   End If
                End If

                If (err_CFML%Ierr == 0) Then
                   !---- Get CH,PH to put rec.-lat.-plane-normal V1 in equatorial plane
                   !---- OM=GA puts V1 in flat-cone setting
                   Call Equatorial_Chi_Phi(v1,ch,ph)
                   om=ga
                   !---- Get RHO, the PSI angle about V1 to make Z1 diffract!
                   !---- The present OM,CH,PH define PSI1 about V1.
                   psi1 = Calc_Psi(vrho,v1,om,ch,ph,ub)
                   If (err_CFML%Ierr == 0) Then
                      !---- The angle (-CSI1+CSI2), about V1 (coincident with the x-omega axis),
                      !---- puts the present Z3 into the diffracting position.
                      z3  = z3frz1(z1,ch,ph)
                      csi1= atan2d(z3(3),z3(2))
                      z4  = z4frgn(wave,ga,nu)
                      z3  = z1frz2(z4,om)
                      csi2= atan2d(z3(3),z3(2))
                      rho = psi1-csi1+csi2
                      rho = rho-360.0_cp*Int((Sign(180.0_cp,rho)+rho)/360.0_cp)
                      Call Calc_Om_Chi_Phi(vrho,v1,rho,ub,om,ch,ph)
                      If (err_CFML%Ierr == 0) Return
                   End If
                End If
             End If
          End If
       End If
       If (err_CFML%Ierr /= 0) Then
          rho=0.0_cp
          ch=0.0_cp
          ph=0.0_cp
          ga=0.0_cp
          om=0.0_cp
          nu=0.0_cp
       End If

    End Subroutine Flat_Cone_vertDet

    !!---- Module Subroutine Get_FlatCone_Angles_D10(z,mu_fc,psi1,psi2,n_ang,limits,psi,omega,chi,phi,inlim,Rot_base)
    !!----   real(kind=cp), dimension(3),             intent(in)  :: z
    !!----   real(kind=cp),                           intent(in)  :: mu_fc,psi1,psi2
    !!----   integer,                                 intent(in)  :: n_ang
    !!----   real(kind=cp), dimension(:,:),           intent(in)  :: limits
    !!----   real(kind=cp), dimension(:),             intent(out) :: psi,omega,chi,phi
    !!----   logical,       dimension(:),             intent(out) :: inlim
    !!----   real(kind=cp), dimension(3,3), optional, intent(out) :: Rot_base
    !!----
    !!----  Subroutine for getting all accessible angles for making a flat-cone data collection with
    !!----  a geometry like D10
    !!----
    !!----
    !!----
    !!----  Created: March 2013 (JRC), Updated June 2020
    !!----
    Module Subroutine Get_FlatCone_Angles_D10(z,mu_fc,psi1,psi2,n_ang,limits,psi,omega,chi,phi,inlim,Rot_base)
      real(kind=cp), dimension(3),             intent(in)  :: z
      real(kind=cp),                           intent(in)  :: mu_fc,psi1,psi2
      integer,                                 intent(in)  :: n_ang
      real(kind=cp), dimension(:,:),           intent(in)  :: limits
      real(kind=cp), dimension(:),             intent(out) :: psi,omega,chi,phi
      logical,       dimension(:),             intent(out) :: inlim
      real(kind=cp), dimension(3,3), optional, intent(out) :: Rot_base

      !--- Local variables ---!
      real(kind=cp),dimension(3)    :: dL,angl
      real(kind=cp),dimension(3,3)  :: Rot,R_psi,Rt
      real(kind=cp)                 :: ps,om,ch,ph,oms,chs,phs,step
      integer                       :: i
      logical                       :: ok

      !Initializing all output arrays
      inlim=.false.; psi=0.0_cp; omega=0.0_cp; chi=0.0_cp; phi=0.0_cp

      step=(psi2-psi1)/real(n_ang-1,kind=cp)
      dL=(/0.0_cp, sind(mu_fc), cosd(mu_fc)/)    !unitary vector normal to detector plane in L-system for D10 z-down
      call Get_Matrix_moving_v_to_u(z,dL,Rt)     !One of the infinite matrices

      !Saving the base rotation matrix corresponding to psi=0.0 if needed
      if(present(Rot_base))   Rot_base=Rt

      ok=.false.
      do i=1,n_ang
        ps=psi1+real(i-1,kind=cp)*step
        R_psi= Rot_Gibbs_Matrix(dL,ps)
        Rot=Matmul(R_psi,Rt)   !Full rotation matrix

        !Get the Euler angles corresponding to matrix Rot
        Call Get_OmegaChiPhi(Rot,om,ch,ph,"D")
        if(ERR_CFML%Ierr /= 0) then
            write(*,"(a)") trim(ERR_CFML%Msg)
        else
            angl=[om,ch,ph]
            if(in_limits(angl,limits,3)) then
              omega(i)=om; chi(i)=ch; phi(i)=ph; psi(i)=ps
              inlim(i)=.true.
              cycle
            end if
            oms=om+180.0_cp
            chs=-ch
            phs=ph+180_cp
            angl=Chkin180([oms,chs,phs])
            if(in_limits(angl,limits,3)) then
              omega(i)=oms; chi(i)=chs; phi(i)=phs; psi(i)=ps
              inlim(i)=.true.
              cycle
            end if
            omega(i)=om; chi(i)=ch; phi(i)=ph
        end if
      end do

    End Subroutine Get_FlatCone_Angles_D10

End SubModule SXTAL_FlatCone