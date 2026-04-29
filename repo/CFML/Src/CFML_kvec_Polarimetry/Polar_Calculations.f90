SubModule (CFML_kvec_Polarimetry) Polar_Calculations
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Calc_Polar(frame,wave,Cell, UB, Pin, NSF, Mag_dom, Mh, Pf,ok,mess,B_Q)
    !!----    real(kind=cp),                intent(in) :: wave    ! In -> Cell variable
    !!----    character(len=3),             intent(in) :: frame   ! In -> Frame used for polarisation
    !!----    type (Cell_G_Type),           intent(in) :: Cell    ! In -> Busing-Levy UB-matrix
    !!----    Real(kind=cp), dimension(3,3),intent(in) :: UB      ! In -> Nuclear Structure Factor
    !!----    Real(kind=cp), dimension(3),  intent(in) :: Pin     ! In -> Incident polarisation (Cartesian or spherical)
    !!----    complex(kind=cp),             intent(in) :: NSF     ! In -> Nuclear Structure Factor
    !!----    type(Magnetic_Domain_type),   intent(in) :: Mag_Dom ! In -> Magnetic domains information
    !!----    type(MagHD_Type),             intent(in) :: Mh      ! In -> Contains Magnetic structure factor, MiV, domain info, ...
    !!----    Real(kind=cp), dimension(3),  intent(out):: Pf      ! Out ->Final polarisation in frame given by "frame"
    !!----    logical,                      intent(out):: ok      ! Out -> .true. if everything gone ok
    !!----    Character (len=*),            intent(out):: mess    ! Out ->Error message
    !!----    character(len=*), optional,   intent(in) :: B_Q     ! Original Blume equations are used Q=Q_BM
    !!----
    !!----
    !!----    We use here the crystallographic convention for the scattering vector: Q=kf-ki
    !!----    So that the equations written below have the sign of the terms containing explicitly
    !!----    the imaginary unit opposite to the terms of the original equations.
    !!----    This soubroutine calculates the final polarization in two reference frames: BL and BM
    !!----    BL-> Busing-Levy and BM-> Blume-Maleyev. The incident polarisation can be provided
    !!----    in Cartesian components or giving the module and two angles of rotation (Theta,Chi)
    !!----    corresponding to nutator and precesion coils in MAD, by default they are in Cartesian
    !!----    when frame="BL" or frame="BM". The Theta rotation corresponds to a clockwise
    !!----    rotation around the "yBL-axis" and the Chi rotation to a clockwise rotation
    !!----    around the "xBL-axis). The product of the two matrices R_chi.R_theta applied to (0,0,pol)
    !!----    provides the incident polarisation in the BL system.
    !!----    For providing "angular coordinates" frame="BLS", frame="BMS" or
    !!----    frame="MAD" (BL for Pi and scattered beam for Pf)
    !!----
    !!----    Calling N the nuclear structure factor (complex scalar) and M the magnetic
    !!----    interaction vector (complex vector), the B-M equations are:
    !!----
    !!----    I = N.N* + M.M* + (N.M* + N*.M)Pi - i(M* x M)Pi
    !!----    Pf.I = N.N* Pi - M.M* Pi + (Pi.M*) M + (Pi.M) M* -i(N*.M - N.M*)xPi +
    !!----           + N.M* + N*.M + i(M* x M)
    !!----
    !!----    Defining the Chiral vector as T=i(M* x M) and the nuclear-magnetic
    !!----    interference vector as W= 2 N.M* = Wr + i Wi, calling Inuc=N.N* and
    !!----    Imag=M.M*, the equations are written in the following form:
    !!----
    !!----    I = Inuc + Imag + Wr.Pi - T.Pi
    !!----    Pf.I = (Inuc-Imag) Pi + (Pi.M*) M + (Pi.M) M* - Wi x Pi + Wr + T
    !!----
    !!----    The Busing-Levy UB-matrix is provided as an input argument.
    !!----    Alternative to the use of partial functions
    !!----
    !!---- Created: June - 2012 JRC
    !!
    Module Subroutine Calc_Polar(frame,wave,Cell,UB, Pin, NSF, Mag_dom, Mh, Pf,ok,mess,B_Q)
       !---- Arguments ----!
       character(len=*),             intent(in)    :: frame
       real(kind=cp),                intent(in)    :: wave
       type (Cell_G_Type),           intent(in)    :: Cell
       Real(kind=cp), dimension(3,3),intent(in)    :: UB
       Real(kind=cp), dimension(3),  intent(in)    :: Pin
       complex(kind=cp),             intent(in)    :: NSF
       type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
       type(MagHD_Type),             intent(in out):: Mh
       Real(kind=cp), dimension(3),  intent(   out):: Pf
       logical,                      intent(   out):: ok
       Character(len=*),             intent(   out):: mess
       Character(len=*), optional,   intent(in)    :: B_Q
!
!       !---- Local variables ----!
       real(kind=cp)                  :: s                 ! sign for T and Wi terms
       real(kind=cp)                  :: I_inv,Inuc,Imag   ! Inverse of elastic cross section
       real(kind=cp)                  :: gamma,omega,nu    ! Normal beam angles
       real(kind=cp)                  :: pol,ptheta,pchi   ! Module of the incident pol. and angles of nutator + rotation
       complex(kind=cp), dimension (3):: MiV, W            ! MiV for one domain and in polarisation frame,Nuclear-Magnetic interference
       integer                        :: nd,ich,nch
       Real(kind=cp), dimension(3)    :: z1,z4, Pic,T,Wr,Wi
       Real(kind=cp), dimension(3,3)  :: Um,Rot,Rot_omega,ubinv,r_pth,r_chi,BL2BM
       Real(kind=cp)                  :: suma

       ok=.true.
       nch=1
       if(Mag_Dom%chir) nch=2
       suma=0.0
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           suma=suma+dot_product(Mh%MiVC(:,ich,nd),Mh%MiVC(:,ich,nd))
         end do
       end do
       if(abs(NSF) < 0.0001 .and. suma < 0.0001) then
         mess="Error: the provided reflection is Nuclear and Magnetically forbidden!"
         ok=.false.
         return
       end if
       s=1.0_cp
       if(present(B_Q)) s=-1.0_cp
       z1=Matmul(UB,Mh%h) !Cartesian coordinates of reflection hkl when all motors are at zero
       ubinv=Inverse_Matrix(UB)
       call Get_Angs_NB(wave,z1,gamma,omega,nu)  !Getting normal beam angles from wavelength and z1
       if(err_CFML%ierr /= 0) then
         mess="Error calculating the normal beam angles"
         ok=.false.
         return
       else
         if(abs(nu) > 1.0) then !The UB matrix does not correspond to horizontal
           ok=.false.           !plane scattering geometry => gamma=2theta
           mess="The nu-angle is incompatible with horizontal-plane scattering geometry"
           return
         end if
       end if
       Rot_omega= Phi_mat(omega)      !Rotation of the omega motor to put reflection in diffraction position
                                      !This is counter-clockwise (c s 0|-s c 0|0 0 1)
       z4=matmul(Rot_omega,z1)        !Crystallographic scattering vector in Lab-system when
                                      !the reflection is in diffraction position.
       Rot=Matmul(UB,Matmul(Cell%GD,Cell%Orth_Cr_cel)) !Conversion to BL (in diffraction position) from Crystal Cartesian

       Rot=Matmul(Rot_omega,Rot) !Rotation matrix putting the MiVC in the BL frame

       if(frame(1:2) == "BM" .or. frame(1:3) == "MAD")  then  !Crystallographic Blume-Maleyev frame
         z4 = z4 / sqrt(dot_product(z4,z4)) !Unit vector along Q in BL system
         Um(1,:) = 0.0
         Um(2,:) = Cross_Product((/0.0_cp,0.0_cp,1.0_cp/),z4)
         Um(3,:) = (/0.0_cp,0.0_cp,1.0_cp/)
         Rot=Matmul(Um,Rot)
       else if(frame(1:2) /= "BL" .and. frame(1:3) /= "MAD") then
         ok=.false.
         mess="Undefined reference frame"
         return
       end if
       !Put now the incident polarisation in Cartesian coordinates if angular
       !data have been provided.
       if(frame == "BLS" .or. frame == "BMS" .or. frame == "MAD") then
         pol=Pin(1); ptheta=Pin(2); pchi=Pin(3)
         R_pth=chi_mat(ptheta) !This is clockwise (c 0 s|0 1 0|-s 0 c)
         R_chi=psi_mat(pchi)   !This is counter-clockwise (1 0 0|0 c -s|0 s c)
         R_Chi=transpose(R_Chi)     !Put chi clockwise
         Pic=matmul(R_chi,matmul(R_pth,(/0.0_cp,0.0_cp,pol/))) !BL-system incident polarisation
         if(frame == "BMS" .or. frame == "MAD") then     !Transform to BM system
           BL2BM=Phi_mat(gamma*0.5+90.0) !Active
           Pic=matmul(transpose(BL2BM),Pic)      !Incident polarisation in BM system
         end if
       else
         Pic=Pin
       end if
       ! Loop over domains
       Pf=0.0
       Inuc=real(Conjg(NSF)*NSF)
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           MiV=Matmul(Rot,Mh%MiVC(:,ich,nd))  !Convert the MiVC to the frame BM or BL
           Imag=dot_Product(MiV,MiV)          ! dot_product => MiV*.MiV
           T=-s*aimag(Cross_Product(Conjg(MiV),MiV)) !Chiral Vector
           W=2.0*NSF*Conjg(MiV)       !Nuclear-Magnetic Interaction vector
           Wr=real(W); Wi=s*aimag(W)  !Real and Imaginary parts
           I_inv=1.0/(Inuc+Imag+dot_product(Wr,Pic)-dot_product(T,Pic))
           Pf=Pf+ I_inv * Mag_Dom%Pop(ich,nd)*( (Inuc-Imag)*Pic +                  &
              dot_product(Pic,MiV)*Conjg(MiV) + dot_product(Pic,Conjg(MiV))* MiV + &
              T + Wr - Cross_Product(Wi,Pic) )
         end do !loop over chiral domains
       end do !loop over S-domains

       !At the end of the calculation Pf is in Cartesian components with respect to
       !BM or BL system depending if the user wants to get the angles of the scattered
       !beam nutator and precesion we have to pass first to the Cartesian coordinates
       !in the scattered beam system. Otherwise the calculated Pf is returned as is.
       if(frame == "BLS") then   !put polarisation in scattered beam system
         Rot= Phi_mat(gamma)
         Pf = matmul(transpose(Rot) ,Pf)
       else if(frame == "BMS" .or. frame == "MAD" ) then
         Rot= Phi_mat(gamma)
         Pf = matmul(transpose(Rot),matmul(BL2BM, Pf) )
       else
         return
       end if
       !Calculate now the angles pchi and ptheta to bring back the polarisation
       !to the z-direction
       pol=sqrt(dot_product(Pf,Pf)) !magnitude of scattered polarisation
       if(abs(pf(3)) < 1.0e-5) then
         pchi=90.0
       else
         pchi=atan2d(pf(2),pf(3))
       end if
       ptheta=atan2d(pf(1),pf(2)*sind(pchi)+pf(3)*cosd(pchi))
       pf=(/pol,ptheta,pchi/)

    End Subroutine Calc_Polar

    !!---- Module Subroutine Get_Pol_Tensor_Pc(frame,wave,Cell,UB,Pin, NSF, Mag_dom, Mh, Pol_tens, Pc,B_Q)
    !!----    character(len=3),             intent(in)    :: frame   ! In -> Frame used for polarisation
    !!----    real(kind=cp),                intent(in)    :: wave
    !!----    type (Cell_G_Type),           intent(in)    :: Cell
    !!----    Real(kind=cp), dimension(3,3),intent(in)    :: UB
    !!----    Real(kind=cp), dimension(3),  intent(in)    :: Pin
    !!----    complex(kind=cp),             intent(in)    :: NSF
    !!----    type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
    !!----    type(MagHD_Type),             intent(in)    :: Mh
    !!----    real(kind=cp), dimension(3,3),intent(out)   :: Pol_tens
    !!----    real(kind=cp), dimension(3),  intent(out)   :: Pc
    !!----    character(len=*), optional,   intent(in)    :: B_Q  !Original Blume equations are used Q=Q_BM
    !!----
    !!----    This subroutine provides the tensor [P] and vector Pc
    !!----    in the crystallographic Blume-Maleyev frame. The input polarisation
    !!----    may be provided either in BM Cartesian coordinates or using the module
    !!----    and two angles of rotation (Theta,Chi)corresponding to nutator
    !!----    and precesion coils in MAD
    !!----    The tensorial form (for whatever reference frame) can be written as:
    !!----    Pf = [P] Pi + Pc => Pc= (Wr + T)/I
    !!----      [P] =  ([D] + [S] + [A])/I
    !!----      [D] =  (Inuc- Imag*) E
    !!----      [S] =  [M o M* + M* o M]  -> o indicates tensorial product
    !!----      [A] =  [+i(N.M* - N*.M)]cross = [-Wi]cross
    !!----
    Module Subroutine Get_Pol_Tensor_Pc(frame,wave,Cell,UB,Pin, NSF, Mag_dom, Mh, Pol_tens, Pc,ok,mess,B_Q)
       character(len=*),             intent(in)    :: frame
       real(kind=cp),                intent(in)    :: wave
       type (Cell_G_Type),           intent(in)    :: Cell
       Real(kind=cp), dimension(3,3),intent(in)    :: UB
       Real(kind=cp), dimension(3),  intent(in)    :: Pin
       complex(kind=cp),             intent(in)    :: NSF
       type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
       type(MagHD_Type),             intent(in)    :: Mh
       real(kind=cp), dimension(3,3),intent(out)   :: Pol_tens
       real(kind=cp), dimension(3),  intent(out)   :: Pc
       logical,                      intent(   out):: ok
       Character(len=*),             intent(   out):: mess
       Character(len=*), optional,   intent(in)    :: B_Q  !Original Blume equations are used Q=Q_BM
       !--- Local variables
       real(kind=cp)                  :: s                 ! sign for T and Wi terms
       real(kind=cp)                  :: I_inv,Inuc,Imag   ! inverse of elastic cross section
       real(kind=cp)                  :: gamma,omega,nu    ! Normal beam angles
       real(kind=cp)                  :: pol,ptheta,pchi   ! Incident polarisation
       complex(kind=cp), dimension (3):: MiV, W     !MiV for one domain and in polarisation frame
       integer                        :: nd,ich,nch
       Real(kind=cp), dimension(3)    :: z1,z4, Pic,T,Wr,Wi
       Real(kind=cp), dimension(3,3)  :: Um,Rot,Rot_omega,DD,AA,SS,r_pth,r_chi,BL2BM
       Real(kind=cp), dimension(3,3), parameter  :: Identity= reshape ( (/1.0,0.0,0.0, &
                                                                          0.0,1.0,0.0, &
                                                                          0.0,0.0,1.0/),(/3,3/) )
       ok=.true.
       s=1.0_cp
       if(present(B_Q)) s=-1.0_cp
       z1=Matmul(UB,Mh%h)
       call Get_Angs_NB(wave,z1,gamma,omega,nu)
       if(err_CFML%ierr /= 0) then
         mess="Error calculating the normal beam angles"
         ok=.false.
         return
       else
         if(abs(nu) > 1.0) then !The UB matrix does not correspond to horizontal
           ok=.false.           !plane scattering geometry
           mess="The nu-angle is incompatible with horizontal-plane scattering geometry"
         end if
       end if
       Rot_omega=Phi_mat(omega)  !Rotation of the omega motor to put reflection in diffraction position
       z4=matmul(Rot_omega,z1) !Crystallographic scattering vector in Lab-system when
                               !the reflection is in diffraction position.
       Rot=Matmul(UB,Matmul(Cell%GD,Cell%Orth_Cr_cel)) !Conversion to BL (in diffraction position) from Crystal Cartesian
       Rot=Matmul(Rot_omega,Rot) !Rotation matrix putting the MiVC in the BL frame
       z4 = z4 / sqrt(dot_product(z4,z4))
       Um(1,:) = 0.0_cp
       Um(2,:) = Cross_Product((/0.0_cp,0.0_cp,1.0_cp/),z4)
       Um(3,:) = (/0.0_cp,0.0_cp,1.0_cp/)
       Rot=Matmul(Um,Rot)
       !Put now the incident polarisation in Cartesian coordinates w.r.t. BM frame
       !if angular data have been provided.
       if(frame == "BLS" .or. frame == "BMS" .or. frame == "MAD") then
         pol=Pin(1); ptheta=Pin(2); pchi=Pin(3)
         R_pth=chi_mat(ptheta) !This is clockwise (c 0 s|0 1 0|-s 0 c)
         R_chi=psi_mat(pchi)   !This is counter-clockwise (1 0 0|0 c -s|0 s c)
         R_Chi=transpose(R_Chi)     !Put chi clockwise
         Pic=matmul(R_chi,matmul(R_pth,(/0.0_cp,0.0_cp,pol/))) !BL-system incident polarisation
         BL2BM=Phi_mat(gamma*0.5_cp+90.0_cp) !Active
         Pic=matmul(transpose(BL2BM),Pic)   !Incident polarisation in BM system
       else
         Pic=Pin
       end if
       nch=1
       if(Mag_Dom%chir) nch=2
       ! Loop over domains
       Pol_tens = 0.0_cp
       Pc       = 0.0_cp
       Inuc=real(Conjg(NSF)*NSF)
       do nd=1,Mag_Dom%nd
         do ich=1,nch
          MiV=Matmul(Rot,Mh%MiVC(:,ich,nd)) !Convert the MiVC to the frame BM
          Imag=dot_Product(MiV,MiV)
          T=-s*aimag(Cross_Product(Conjg(MiV),MiV)) !Chiral Vector
          W=2.0_cp*NSF*Conjg(MiV)     !Nuclear-Magnetic Interaction vector
          Wr=real(W); Wi=s*aimag(W)  !Real and Imaginary parts
          I_inv=1.0_cp/(Inuc+Imag+dot_product(Wr,Pic)-dot_product(T,Pic))
          DD=(Inuc-Imag)*Identity
          SS=real(Tensor_Product(MiV,Conjg(MiV))+Tensor_Product(Conjg(MiV),MiV))
          AA=-Mat_Cross(Wi)
          Pol_tens=Pol_tens+ I_inv*Mag_Dom%Pop(ich,nd)*( DD + SS + AA )
          Pc=Pc+ I_inv*Mag_Dom%Pop(ich,nd)*(T+Wr)
         end do !loop over chiral domains
       end do !loop over S-domains

    End Subroutine Get_Pol_Tensor_Pc

End SubModule Polar_Calculations