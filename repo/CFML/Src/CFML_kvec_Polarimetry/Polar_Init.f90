SubModule (CFML_kvec_Polarimetry) Polar_Init
   implicit none
   Contains
    !!----
    !!---- Subroutine Set_Polar_Info(Cell, H, Spv, Pin, Nsf, MiV, Polari,B_Q)
    !!----    Type (Cell_G_Type),             intent(in)  :: Cell     !  In -> Cell variable
    !!----    real(kind=cp), DIMENSION (3),   intent(in)  :: H        !  In -> Scattering vector in hkl
    !!----    real(kind=cp), dimension(3),    intent(in)  :: SPV      !  In -> Second Scattering plane vector in hkl
    !!----    real(kind=cp),                  intent(in)  :: Pin      !  In -> magnitude of initial polarisation
    !!----    complex(kind=cp),               intent(in)  :: NSF      !  In -> Nuclear Scattering Factor
    !!----    complex(kind=cp), dimension(3), intent(in)  :: MiV      !  In -> Magnetic interaction vector
    !!----    Type (Polar_Info_type),         intent(out) :: Polari   !  Out ->type with all information about polarisation in
    !!----                                                                     one point hkl
    !!----    character(len=*), optional,     intent(in)  :: B_Q  !Original Blume equations are used Q=Q_BM
    !!----
    !!----    Initializes the polarisation info type
    !!----
    !!---- Updated: April - 2008, June-2012 (JRC)
    !!
    Module Subroutine Set_Polar_Info(Cell, H, Spv, Pin, Nsf, MiV, Polari,B_Q)
       !---- Arguments ----!
       Type (Cell_G_Type),             intent(in)  :: Cell
       real(kind=cp), DIMENSION (3),   intent(in)  :: H
       real(kind=cp), dimension(3),    intent(in)  :: SPV
       real(kind=cp),                  intent(in)  :: Pin
       complex(kind=cp),               intent(in)  :: NSF
       complex(kind=cp), dimension(3), intent(in)  :: MiV
       Type (Polar_Info_type),         intent(out) :: Polari
       character(len=*), optional,     intent(in)  :: B_Q

       !---- Local variables ----!
       real(kind=cp), DIMENSION (3)     :: sigma        ! elastic cross for different inicdent polarisation directions
       real(kind=cp)                    :: nc, my, mz, rnmy, rnmz, inmy, inmz, tc, mmc, A !the different contribution to cross-section
       complex(kind=cp), DIMENSION (3)  :: MiV_PF       !MiV in polarisation frame


       A = tpi**3/Cell%Vol

       !First store given info in Polari
       Polari%H = H
       Polari%SPV = SPV
       Polari%Cell = Cell
       Polari%P = Pin
       Polari%NSF = NSF
       if(abs(dot_product(MiV,MiV)) < 0.0001 .and. abs(NSF) < 0.0001) then
         Polari%Pij=0.0
         return
       end if
       !Calculate the rest and also store it in Polari

       !magnetic interaction in polarisation frame
       MiV_PF = Magn_Inter_Vec_PF(MiV,H,SPV, Cell) !It is assumed that MiV is provided in Cartesian components
       Polari%MiV = MiV_PF
       !the different contributions to the scattering cross-section
       nc = nuc_contr(NSF)
       Polari%NC = A * nc

       my = mag_y(MiV_PF)
       Polari%MY = A * my

       mz = mag_z(MiV_PF)
       Polari%MZ = A * mz

       rnmy = real_nm_y(NSF, MiV_PF)
       Polari%RY = A * rnmy

       rnmz = real_nm_z(NSF, MiV_PF)
       Polari%RZ = A * rnmz

       if(present(B_Q)) then
         inmy = im_nm_y(NSF, MiV_PF,"B_Q")
         inmz = im_nm_z(NSF, MiV_PF,"B_Q")
         tc = tchiral(MiV_PF,"B_Q")
       else
         inmy = im_nm_y(NSF, MiV_PF)
         inmz = im_nm_z(NSF, MiV_PF)
         tc = tchiral(MiV_PF)
       end if

       Polari%IY = A * inmy
       Polari%IZ = A * inmz
       Polari%TC = tc

       mmc = mm(MiV_PF)
       Polari%MM = A * mmc

       !scattering cross-section for the different initial polarisation vectors
       sigma = (/ nc + my + mz - Pin * tc, nc + my + mz + Pin * rnmy, nc + my + mz + Pin * rnmz /)
       Polari%CS = sigma
       write(*,*) sigma
       if(dot_product(sigma,sigma) > 0.001) then
         !the polar matrix
         Polari%Pij(1,1) = ((nc - my - mz)* Pin + tc)/sigma(1)
         Polari%Pij(1,2) = (inmz * Pin + tc)/sigma(2)
         Polari%Pij(1,3) = (-inmy * Pin + tc)/sigma(3)
         Polari%Pij(2,1) = (-inmz * Pin + rnmy)/sigma(1)
         Polari%Pij(2,2) = ((nc + my - mz) * Pin + rnmy)/sigma(2)
         Polari%Pij(2,3) = (mmc * Pin + rnmy)/sigma(3)
         Polari%Pij(3,1) = (inmy * Pin + rnmz)/sigma(1)
         Polari%Pij(3,2) = (mmc * Pin + rnmz)/sigma(2)
         Polari%Pij(3,3) = ((nc - my + mz) * Pin + rnmz)/sigma(3)
       else
         Polari%Pij=0.0
       end if
       return
    End Subroutine Set_Polar_Info
End SubModule Polar_Init