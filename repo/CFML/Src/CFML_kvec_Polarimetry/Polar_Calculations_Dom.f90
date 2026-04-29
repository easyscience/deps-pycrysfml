SubModule (CFML_kvec_Polarimetry) Polar_Calculations_Dom
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Calc_Polar_Dom(Cell, H, SPV, Pin, NSF, Mag_dom, Mh, Polari,ok,mess,B_Q)
    !!----    Type (Cell_G_Type),          intent(in)    :: Cell  !  In -> Cell variable
    !!----    real(kind=cp), dimension (3),intent(in)    :: H     !  In -> Scattering vector in hkl
    !!----    real(kind=cp), dimension(3), intent(in)    :: SPV   !  In -> Second Scattering plane vector in hkl
    !!----    real(kind=cp), intent( in)                 :: Pin   !  In -> magnitude of initial polarisation
    !!----    complex(kind=cp), intent( in)              :: NSF   !  In -> Nuclear Scattering Factor
    !!----    Type(Magnetic_Domain_type),  intent(in)    :: Mag_Dom
    !!----    Type(MagHD_Type),            intent(in out):: Mh
    !!----    Type (Polar_calc_type),      intent( out)  :: Polari !  Out ->type with all information about polarisation in
    !!----                                                                 one point hkl
    !!----    Logical,                     intent(out)   :: ok
    !!----    character(len=*),            intent(out)   :: mess
    !!----    character(len=*), optional,  intent(in)    :: B_Q  !Original Blume equations are used Q=Q_BM
    !!----
    !!----    Calculates Polarization matrix for domain case
    !!----
    !!---- Created: March - 2009 OZ, Updated: June-2012 (JRC)
    !!
    Module Subroutine Calc_Polar_Dom(Cell, H, SPV, Pin, NSF, Mag_dom, Mh, Polari,ok,mess,B_Q)
       ! ---- Arguments ---- !
       type (Cell_G_Type),          intent(in)       :: Cell
       real(kind=cp), dimension (3),intent(in)       :: H
       real(kind=cp), dimension(3), intent(in)       :: SPV
       real(kind=cp),               intent(in)       :: Pin
       complex(kind=cp),            intent(in)       :: NSF
       type(Magnetic_Domain_type),  intent(in)       :: Mag_Dom
       type(MagHD_Type),            intent(in out)   :: Mh
       type(Polar_calc_type),       intent(out)      :: Polari
       logical,                     intent(out)      :: ok
       character(len=*),            intent(out)      :: mess
       character(len=*), optional,  intent(in)       :: B_Q

       !---- Local variables ----!
       real(kind=cp), dimension (3)   :: sigma        ! elastic cross for different incident polarisation directions
       real(kind=cp)                  :: nc,my,mz,rnmy,rnmz,inmy,inmz,tc,mmc,a,suma !the different contribution to cross-section
       complex(kind=cp), dimension (3):: MiV, MiV_PF       !MiV for one domain and in polarisation frame
       integer                        :: nd,ich,nch


       A = tpi**3/Cell%Vol
       ok=.true.
       !First store given info in Polari
       Polari%H = H
       Polari%SPV = SPV
       Polari%Cell = Cell
       Polari%P = Pin
       Polari%NSF = NSF

       Polari%Pij(:,:) = 0.0_cp

       nch=1
       if(Mag_Dom%chir) nch=2
       ! Loop over domains
       suma=0.0
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           suma=suma+dot_product(Mh%MiVC(:,ich,nd),Mh%MiVC(:,ich,nd))      !Use the Cartesian components before calling Magn_Inter_Vec_PF
         end do
       end do
       if(abs(NSF) < 0.0001 .and. suma < 0.0001) then
         mess="Error: the provided reflection is Nuclear and Magnetically forbidden!"
         ok=.false.
         return
       end if
       !Calculate the rest and also store it in Polari
       ! Loop over domains
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           MiV=Mh%MiVC(:,ich,nd)      !Use the Cartesian components before calling Magn_Inter_Vec_PF
           !magnetic interaction in polarisation frame
           MiV_PF = Magn_Inter_Vec_PF(MiV,H,SPV, Cell)
           Polari%MiV(:,ich,nd) = MiV_PF

           !the different contributions to the scattering cross-section
           nc = nuc_contr(NSF)
           Polari%NC = A * nc

           my = mag_y(MiV_PF)
           Polari%MY(ich,nd) = A * my

           mz = mag_z(MiV_PF)
           Polari%MZ(ich,nd) = A * mz

           rnmy = real_nm_y(NSF, MiV_PF)
           Polari%RY(ich,nd) = A * rnmy

           rnmz = real_nm_z(NSF, MiV_PF)
           Polari%RZ(ich,nd) = A * rnmz


           if(present(B_Q)) then
             inmy = im_nm_y(NSF, MiV_PF,"B_Q")
             inmz = im_nm_z(NSF, MiV_PF,"B_Q")
             tc   = tchiral(MiV_PF,"B_Q")
           else
             inmy = im_nm_y(NSF, MiV_PF)
             inmz = im_nm_z(NSF, MiV_PF)
             tc   = tchiral(MiV_PF)
           end if

           Polari%IY(ich,nd) = A * inmy
           Polari%IZ(ich,nd) = A * inmz
           Polari%TC(ich,nd) = tc

           mmc = mm(MiV_PF)
           Polari%MM(ich,nd) = A * mmc

           !scattering cross-section for the different initial polarisation vectors
           sigma = (/ nc + my + mz - Pin * tc, nc + my + mz + Pin * rnmy, nc + my + mz + Pin * rnmz /)
           Polari%CS(:,ich,nd) = sigma

           !summing of the polarisation matrix
           Polari%Pij(1,1) = Polari%Pij(1,1) + Mag_Dom%Pop(ich,nd)*((nc - my - mz)* Pin + tc)/sigma(1)
           Polari%Pij(1,2) = Polari%Pij(1,2) + Mag_Dom%Pop(ich,nd)*(inmz * Pin + tc)/sigma(2)
           Polari%Pij(1,3) = Polari%Pij(1,3) + Mag_Dom%Pop(ich,nd)*(-inmy * Pin + tc)/sigma(3)
           Polari%Pij(2,1) = Polari%Pij(2,1) + Mag_Dom%Pop(ich,nd)*(-inmz * Pin + rnmy)/sigma(1)
           Polari%Pij(2,2) = Polari%Pij(2,2) + Mag_Dom%Pop(ich,nd)*((nc + my - mz) * Pin + rnmy)/sigma(2)
           Polari%Pij(2,3) = Polari%Pij(2,3) + Mag_Dom%Pop(ich,nd)*(mmc * Pin + rnmy)/sigma(3)
           Polari%Pij(3,1) = Polari%Pij(3,1) + Mag_Dom%Pop(ich,nd)*(inmy * Pin + rnmz)/sigma(1)
           Polari%Pij(3,2) = Polari%Pij(3,2) + Mag_Dom%Pop(ich,nd)*(mmc * Pin + rnmz)/sigma(2)
           Polari%Pij(3,3) = Polari%Pij(3,3) + Mag_Dom%Pop(ich,nd)*((nc - my + mz) * Pin + rnmz)/sigma(3)

         end do !loop over chiral domains
       end do !loop over S-domains

    End Subroutine Calc_Polar_Dom

    !!----
    !!----    Module Subroutine Calc_Polar_Dom_Efficiency(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Polari)
    !!----      type (Cell_G_Type),          intent(in)     :: Cell
    !!----      REAL(kind=cp), DIMENSION (3),intent(in)     :: H
    !!----      Real(kind=cp), dimension(3), intent(in)     :: SPV
    !!----      Real(kind=cp),               intent(in)     :: Pin
    !!----      complex(kind=cp),            intent(in)     :: NSF
    !!----      type(Magnetic_Domain_type),  intent(in)     :: Mag_Dom
    !!----      type(MagHD_Type),            intent(in out) :: Mh
    !!----      type(Polar_calc_type),       intent(out)    :: Polari
    !!----
    !!----   Calculates Polarization matrix from polarised cross-sections, accounts for partial polarization
    !!----
    !!----   Created: November - 2011 OZ
    !!
    Module Subroutine Calc_Polar_Dom_Efficiency(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Polari)

      Type (Cell_G_Type),          intent(in)    :: Cell
      Real(kind=cp), dimension (3),intent(in)    :: H
      Real(kind=cp), dimension(3), intent(in)    :: SPV
      Real(kind=cp),               intent(in)    :: Pin
      complex(kind=cp),            intent(in)    :: NSF
      Type(Magnetic_Domain_type),  intent(in)    :: Mag_Dom
      Type(MagHD_Type),            intent(in out):: Mh
      Type(Polar_calc_type),       intent(out)   :: Polari

      !!---- Local variables ----!
      integer                           :: nd,ich,nch,i,j
      complex(kind=cp), dimension(2,2)  :: ScatAmp
      complex(kind=cp), dimension(2,2)  :: Spin_Px,Spin_Py,Spin_Pz
      Real(kind=cp)                     :: coef, Pinm, Pf !, A
      complex(kind=cp), dimension(3)    :: MiV, MiV_PF  !MiV for one domain and in polarisation frame
      complex(kind=cp), dimension(3,3,4):: sVs          ! 1dim in x,y,z 2dim out x,y,z 3dim sign 1++ 2+- 3-+ 4--
      Real(kind=cp), dimension(3,3,4)   :: CrSec
      Real(kind=cp), dimension(3,3)     :: Ipp,Ipm,Imp,Imm
      real(kind=cp), parameter          :: eps=0.00001_cp

       ! Shortcut for TASP as two benders have same efficiency
       Pf=Pin
       ! Neutron spin states
       coef=1.0_cp/sqrt(2.0_cp)

       Spin_Px(1,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 1.0_cp, 0.0_cp)/) !up
       Spin_Px(2,:)=(/coef*(1.0_cp, 0.0_cp),coef*(-1.0_cp, 0.0_cp)/) !down

       Spin_Py(1,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 0.0_cp, 1.0_cp)/) !up
       Spin_Py(2,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 0.0_cp,-1.0_cp)/) !down

       Spin_Pz(1,:)=(/(1.0_cp, 0.0_cp),( 0.0_cp, 0.0_cp)/) !up
       Spin_Pz(2,:)=(/(0.0_cp, 0.0_cp),(-1.0_cp, 0.0_cp)/) !down

       !A = tpi**3/Cell%CellVol   !not used here
       !First store given info in Polari
       Polari%H = H
       Polari%SPV = SPV
       Polari%Cell = Cell
       Polari%P = Pin
       Polari%NSF = NSF
       Polari%Pij(:,:) = 0.0_cp
       Ipp = 0.0_cp
       Ipm = 0.0_cp
       Imp = 0.0_cp
       Imm = 0.0_cp

       nch=1
       if(Mag_Dom%chir) nch=2
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           MiV=Mh%MiVC(:,ich,nd) !use Cartesian components
           ! MiV=Mh%MiV(:,ich,nd) !this does not use Cartesian components as was in the original sulbroutine
           ! Magnetic Interaction Vector in polarisation frame
           MiV_PF = Magn_Inter_Vec_PF(MiV,H,SPV, Cell)
           ! Partial Scattering Amplitudes
           ! U++ U-+
           ! U+- U--
           ScatAmp(1,1)=NSF+MiV_PF(3)
           ScatAmp(1,2)=MiV_PF(1)-(0.0_cp,1.0_cp)*MiV_PF(2)
           ScatAmp(2,1)=MiV_PF(1)+(0.0_cp,1.0_cp)*MiV_PF(2)
           ScatAmp(2,2)=NSF-MiV_PF(3)
           ! Matrix elements between two spin states <spin|Potential|spin>
           do i=1,2
             sVs(1,1,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs xx,-xx
             sVs(2,1,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs yx,-yx
             sVs(3,1,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs zx,xzx

             sVs(1,2,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs xy,-xy
             sVs(2,2,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs yy,-yy
             sVs(3,2,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs zy,-zy

             sVs(1,3,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs xz,-xz
             sVs(2,3,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs yz,-yz
             sVs(3,3,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs zz,-zz
           end do

           do i=1,2
             sVs(1,1,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs x-x,-x-x
             sVs(2,1,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs y-x,-y-x
             sVs(3,1,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs z-x,-z-x

             sVs(1,2,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs x-y,-x-y
             sVs(2,2,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs y-y,-y-y
             sVs(3,2,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs z-y,-z-y

             sVs(1,3,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs x-z,-x-z
             sVs(2,3,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs y-z,-y-z
             sVs(3,3,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs z-z,-z-z
           end do

           ! Scattering Cross-Sections and Polarization Matrices
           ! 1st direction is scattered, 2nd - incoming
           CrSec=CONJG(sVs)*sVs

           if(Pin > 0.0_cp) then
             do i=1,3
               do j=1,3
                Ipp(i,j) = Ipp(i,j) + Mag_Dom%Pop(ich,nd)*                      &
                         ( CrSec(i,j,1)*Pin*Pf + CrSec(i,j,2)*Pin*(1.0_cp-Pf) + &
                           CrSec(i,j,3)*(1.0_cp-Pin)*Pf + CrSec(i,j,4)*(1.0_cp-Pin)*(1.0_cp-Pf) )
                Imp(i,j) = Imp(i,j) + Mag_Dom%Pop(ich,nd)*                      &
                         ( CrSec(i,j,2)*Pin*Pf + CrSec(i,j,1)*Pin*(1.0_cp-Pf) + &
                           CrSec(i,j,4)*(1.0_cp-Pin)*Pf + CrSec(i,j,3)*(1.0_cp-Pin)*(1.0_cp-Pf) )
               end do
             end do
           end if

           if(Pin < 0.0_cp) then
             Pinm=-Pin
             Pf=Pinm
             do i=1,3
               do j=1,3
                Imm(i,j) = Imm(i,j) + Mag_Dom%Pop(ich,nd)* &
                         ( CrSec(i,j,4)*Pinm*Pf + CrSec(i,j,3)*Pinm*(1.0_cp-Pf) + &
                           CrSec(i,j,2)*(1.0_cp-Pinm)*Pf + CrSec(i,j,1)*(1.0_cp-Pinm)*(1.0_cp-Pf) )
                Ipm(i,j) = Ipm(i,j) + Mag_Dom%Pop(ich,nd)* &
                         ( CrSec(i,j,3)*Pinm*Pf + CrSec(i,j,4)*Pinm*(1.0_cp-Pf) + &
                           CrSec(i,j,1)*(1.0_cp-Pinm)*Pf + CrSec(i,j,2)*(1.0_cp-Pinm)*(1.0_cp-Pf) )
               end do
             end do
           end if

         end do !loop over chiral domains
       end do !loop over S-domains

       if(Pin > 0.0_cp) then
         do i=1,3
           do j=1,3
             if(Ipp(i,j)+Imp(i,j) <= eps) then
               Polari%Pij(i,j) = 0.0_cp
             else
               Polari%Pij(i,j)=(Ipp(i,j)-Imp(i,j))/(Ipp(i,j)+Imp(i,j))
             end if
           end do
         end do
       end if

       if(Pin < 0.0_cp) then
         do i=1,3
           do j=1,3
             if(Imm(i,j)+Ipm(i,j) <= eps) then
              Polari%Pij(i,j) = 0.0_cp
             else
              Polari%Pij(i,j)=-(Imm(i,j)-Ipm(i,j))/(Imm(i,j)+Ipm(i,j))
             end if
           end do
         end do
       end if

    End Subroutine Calc_Polar_Dom_Efficiency

    !!----
    !!---- Module Subroutine Calc_Polar_CrSec(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Ipp,Ipm,Imp,Imm)
    !!----
    !!----  Type (Cell_G_Type),           intent(in)    :: Cell
    !!----  Real(kind=cp), dimension (3), intent(in)    :: H
    !!----  Real(kind=cp), dimension(3),  intent(in)    :: SPV
    !!----  Real(kind=cp),                intent(in)    :: Pin
    !!----  complex(kind=cp),             intent(in)    :: NSF
    !!----  Type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
    !!----  Type(MagHD_Type),             intent(in out):: Mh
    !!----  Real(kind=cp), dimension(3,3),intent(out)   :: Ipp,Ipm,Imp,Imm
    !!----
    !!---- Calculates polarised cross-sections, accounts for partial polarization
    !!---- is useful for MultiSourceData refinement
    !!----
    Module Subroutine Calc_Polar_CrSec(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Ipp,Ipm,Imp,Imm)

      Type (Cell_G_Type),           intent(in)     :: Cell
      Real(Kind=Cp), dimension (3), intent(in)     :: H
      Real(kind=cp), dimension(3),  intent(in)     :: SPV
      Real(kind=cp),                intent(in)     :: Pin
      complex(kind=cp),             intent(in)     :: NSF
      Type(Magnetic_Domain_type),   intent(in)     :: Mag_Dom
      Type(MagHD_Type),             intent(in out) :: Mh
      Real(kind=cp), dimension(3,3),intent(out)    :: Ipp,Ipm,Imp,Imm

    !!---- Local variables ----!
      integer                           :: nd,ich,nch,i,j
      complex(kind=cp), dimension(2,2)  :: ScatAmp
      complex(kind=cp), dimension(2,2)  :: Spin_Px,Spin_Py,Spin_Pz
      Real(kind=cp)                     :: coef, Pinm, Pf !, A
      complex(kind=cp), dimension(3)    :: MiV, MiV_PF   !MiV for one domain and in polarisation frame
      complex(kind=cp), dimension(3,3,4):: sVs ! 1dim in x,y,z 2dim out x,y,z 3dim sign 1++ 2+- 3-+ 4--
      Real(kind=cp), dimension(3,3,4)   :: CrSec

      ! shortcut for TASP as two benders have same efficiency
      Pf=Pin
      ! Neutron spin states
      coef=1.0_cp/sqrt(2.0_cp)

      Spin_Px(1,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 1.0_cp, 0.0_cp)/) !up
      Spin_Px(2,:)=(/coef*(1.0_cp, 0.0_cp),coef*(-1.0_cp, 0.0_cp)/) !down

      Spin_Py(1,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 0.0_cp, 1.0_cp)/) !up
      Spin_Py(2,:)=(/coef*(1.0_cp, 0.0_cp),coef*( 0.0_cp,-1.0_cp)/) !down

      Spin_Pz(1,:)=(/(1.0_cp, 0.0_cp),( 0.0_cp, 0.0_cp)/) !up
      Spin_Pz(2,:)=(/(0.0_cp, 0.0_cp),(-1.0_cp, 0.0_cp)/) !down

      !A = tpi**3/Cell%CellVol   !not used here

      Ipp = 0.0_cp
      Ipm = 0.0_cp
      Imp = 0.0_cp
      Imm = 0.0_cp

      nch=1
      if(Mag_Dom%chir) nch=2
      do nd=1,Mag_Dom%nd
        do ich=1,nch
          MiV=Mh%MiVC(:,ich,nd) !MiV must be provided in Cartesian Crystallographic Frame
          ! Magnetic Interaction Vector in polarisation frame
          MiV_PF = Magn_Inter_Vec_PF(MiV,H,SPV, Cell)
          ! Partial Scattering Amplitudes
          ! U++ U-+
          ! U+- U--
          ScatAmp(1,1)=NSF+MiV_PF(3)
          ScatAmp(1,2)=MiV_PF(1)-(0.0_cp,1.0_cp)*MiV_PF(2)
          ScatAmp(2,1)=MiV_PF(1)+(0.0_cp,1.0_cp)*MiV_PF(2)
          ScatAmp(2,2)=NSF-MiV_PF(3)
          ! Matrix elements between two spin states <spin|Potential|spin>
          do i=1,2
            sVs(1,1,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs xx,-xx
            sVs(2,1,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs yx,-yx
            sVs(3,1,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Px(1,:))) ! sVs zx,xzx

            sVs(1,2,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs xy,-xy
            sVs(2,2,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs yy,-yy
            sVs(3,2,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Py(1,:))) ! sVs zy,-zy

            sVs(1,3,i)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs xz,-xz
            sVs(2,3,i)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs yz,-yz
            sVs(3,3,i)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Pz(1,:))) ! sVs zz,-zz
          end do

          do i=1,2
            sVs(1,1,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs x-x,-x-x
            sVs(2,1,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs y-x,-y-x
            sVs(3,1,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Px(2,:))) ! sVs z-x,-z-x

            sVs(1,2,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs x-y,-x-y
            sVs(2,2,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs y-y,-y-y
            sVs(3,2,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Py(2,:))) ! sVs z-y,-z-y

            sVs(1,3,i+2)=dot_product(Spin_Px(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs x-z,-x-z
            sVs(2,3,i+2)=dot_product(Spin_Py(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs y-z,-y-z
            sVs(3,3,i+2)=dot_product(Spin_Pz(i,:),matmul(ScatAmp,Spin_Pz(2,:))) ! sVs z-z,-z-z
          end do

         ! Scattering Cross-Sections and Polarization Matrices
         ! 1st direction is scattered, 2nd - incoming
         CrSec=CONJG(sVs)*sVs

         if(Pin > 0.0_cp) then
           do i=1,3
             do j=1,3
              Ipp(i,j) = Ipp(i,j) + Mag_Dom%Pop(ich,nd)* &
                       ( CrSec(i,j,1)*Pin*Pf + CrSec(i,j,2)*Pin*(1.0_cp-Pf) + &
                         CrSec(i,j,3)*(1.0_cp-Pin)*Pf + CrSec(i,j,4)*(1.0_cp-Pin)*(1.0_cp-Pf) )
              Imp(i,j) = Imp(i,j) + Mag_Dom%Pop(ich,nd)* &
                       ( CrSec(i,j,2)*Pin*Pf + CrSec(i,j,1)*Pin*(1.0_cp-Pf) + &
                         CrSec(i,j,4)*(1.0_cp-Pin)*Pf + CrSec(i,j,3)*(1.0_cp-Pin)*(1.0_cp-Pf) )
             end do
           end do
         end if

         if(Pin < 0.0_cp) then
           Pinm=-Pin
           Pf=Pinm
           do i=1,3
             do j=1,3
              Imm(i,j) = Imm(i,j) + Mag_Dom%Pop(ich,nd)*                        &
                       ( CrSec(i,j,4)*Pinm*Pf + CrSec(i,j,3)*Pinm*(1.0_cp-Pf) + &
                         CrSec(i,j,2)*(1.0_cp-Pinm)*Pf + CrSec(i,j,1)*(1.0_cp-Pinm)*(1.0_cp-Pf) )
              Ipm(i,j) = Ipm(i,j) + Mag_Dom%Pop(ich,nd)*                        &
                       ( CrSec(i,j,3)*Pinm*Pf + CrSec(i,j,4)*Pinm*(1.0_cp-Pf) + &
                         CrSec(i,j,1)*(1.0_cp-Pinm)*Pf + CrSec(i,j,2)*(1.0_cp-Pinm)*(1.0_cp-Pf) )
             end do
           end do
         end if

        end do !loop over chiral domains
       end do !loop over S-domains

    End Subroutine Calc_Polar_CrSec

End SubModule Polar_Calculations_Dom