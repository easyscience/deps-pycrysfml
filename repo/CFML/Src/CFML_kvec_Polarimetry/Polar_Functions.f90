SubModule (CFML_kvec_Polarimetry) Polar_Functions
   implicit none
   Contains
    !-------------------!
    !---- Functions ----!
    !-------------------!

    !!--++
    !!--++ Function Im_Nm_Y(Nsf, MiV_Pf) Result(I_Nm_Y,B_Q)
    !!--++    complex(kind=cp),               intent(in) :: NSF      !  In  -> Nuclear Structure Factor
    !!--++    complex(kind=cp), dimension(3), intent(in) :: MiV_PF   !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    character(len=*),optional,      intent(in) :: B_Q      !  In  -> The calculation is done with original Blume equation Q_B= -Q_cryst
    !!--++    real(kind=cp)                              :: I_NM_Y   !  Out -> Imaginary part of nuclear-magnetic interference contribution along Y
    !!--++
    !!--++    (Private)
    !!--++    Calculates the imaginary part of the nuclear-magnetic interference contribution along Y
    !!--++    to scattering in the polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Updated: April - 2005, June-2012 (JRC)
    !!
    Module Function Im_Nm_Y(Nsf, MiV_Pf,B_Q) Result(I_Nm_Y)
       !---- Argument ----!
       complex(kind=cp),               intent( in)  :: NSF
       complex(kind=cp), dimension(3), intent( in)  :: MiV_PF
       character(len=*),optional,      intent(in)   :: B_Q
       real(kind=cp)                                :: I_Nm_Y
       !---- Local variables ----!
       real(kind=cp) :: s

       s=-1.0
       if(present(B_Q)) s=1.0
       I_NM_Y = s*Aimag(NSF * Conjg(MiV_PF(2)) - Conjg(NSF) * MiV_PF(2))

    End Function  Im_Nm_Y

    !!--++
    !!--++ Real Function Im_Nm_Z(Nsf, MiV_Pf,B_Q) Result(I_Nm_Z)
    !!--++    complex(kind=cp),              intent(in) :: NSF     !  In -> Nuclear Structure Factor
    !!--++    complex(kind=cp), dimension(3),intent(in) :: MiV_PF  !  In -> Magnetic Interaction Vector in polarisation frame
    !!--++    character(len=*),optional,     intent(in) :: B_Q     !  In  -> The calculation is done with original Blume equation Q_B= -Q_cryst
    !!--++    real(kind=cp)                             :: I_NM_Z  !  Out-> Imaginary part of nuclear magnetic interference contribution along Z
    !!--++
    !!--++    (Private)
    !!--++    Calculates the imaginary part of the nuclear magnetic interference contribution along Z
    !!--++    to scattering in the polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Updated: April - 2005, June-2012 (JRC)
    !!
    Module Function Im_Nm_Z(Nsf, MiV_Pf,B_Q) Result(I_Nm_Z)
       !---- Argument ----!
       complex(kind=cp),              intent(in) :: NSF
       complex(kind=cp), dimension(3),intent(in) :: MiV_PF
       character(len=*),optional,     intent(in) :: B_Q
       real(kind=cp)                             :: I_NM_Z
       !---- Local variables ----!
       real(kind=cp) :: s

       s=-1.0
       if(present(B_Q)) s=1.0

       I_NM_Z = s*Aimag(NSF * Conjg(MiV_PF(3)) - Conjg(NSF) * MiV_PF(3))

    End Function  Im_Nm_Z

    !!--++
    !!--++ Function Mag_Y(MiV_Pf) Result(My)
    !!--++    complex(kind=cp), dimension(3), intent( in) :: MiV_PF !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                               :: MY     !  Out -> Magnetic contribution along Y
    !!--++
    !!--++    (Private)
    !!--++    Calculates the magnetic contribution along Y to scattering in the polarisation
    !!--++    coordinate frame according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Mag_Y(MiV_Pf) Result(My)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in)  :: MiV_PF
       real(kind=cp)                                :: MY

       MY = MiV_PF(2) * Conjg(MiV_PF(2))

    End Function  Mag_Y

    !!--++
    !!--++ Module Function Mag_Z(MiV_Pf) Result(Mz)
    !!--++    complex(kind=cp), dimension(3), intent( in):: MiV_PF  !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                     :: MZ      !  Out -> Magnetic contribution along Z
    !!--++
    !!--++    (Private)
    !!--++    Calculates the magnetic contribution along Z to scattering in the polarisation
    !!--++    coordinate frame according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Mag_Z(MiV_Pf) Result(Mz)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: MZ

       MZ = MiV_PF(3) * Conjg(MiV_PF(3))

    End Function  Mag_Z

    !!--++
    !!--++ Module Function Magn_Inter_Vec_Pf(MiV,H,Spv, Cell) Result(MiV_Pf)
    !!--++    complex(kind=cp),dimension(3), intent( in) :: MiV            !  In -> Magnetic Interaction Vector in Crystal Cartesian Coordinates
    !!--++    real(kind=cp),   dimension(3), intent( in) :: H              !  In -> Scattering Vector in hkl
    !!--++    real(kind=cp),   dimension(3), intent( in) :: SPV            !  In -> Second Scattering plane vector in hkl
    !!--++    Type (Cell_G_Type),            intent(in)  :: Cell           !  In -> Cell variable which holds transformation matrices
    !!--++    complex(kind=cp), dimension(3)             :: MiV_PF         !  Out -> Magnetic Interaction Vector in polarisation coordinate frame
    !!--++
    !!--++    (Private)
    !!--++    Calculates the magnetic interaction vector in the polarisation coordinate frame according to the Blume equations
    !!--++    and therefore depending on the scattering vector.
    !!--++
    !!--++    Polarisation coordinate frame according to Blume
    !!--++    X  || scattering vector Q     (where Q is the scattering Vector in cartesian real space coordinates, it will be calculated from H and matrices in Cell)
    !!--++    Y _|_ scattering vector Q in scattering plane
    !!--++    Z _|_ scattering vector Q out of scattering plane (ATTENTION: This choice is not non-ambiguous, there are always two possible choices
    !!--++                                                       for a right handed coordinate frame which will fullfil this condition!!!)
    !!--++
    !!--++                           Y
    !!--++                          /|\
    !!--++                           |
    !!--++                   Q       | Z
    !!--++            ____________ _\o_____\ X
    !!--++            \             /      /
    !!--++             \           /
    !!--++              \         /
    !!--++               \       /
    !!--++                \     /  K_f
    !!--++             K_i \   /
    !!--++                 _\|/_
    !!--++
    !!--++    Therefore the right handed coordinate frame will be explicitly chosen like this:
    !!--++    X := Q/|Q|               where Q is the scattering Vector in cartesian real space coordinates
    !!--++    Z := (Q x SV)/|(Q x SV)| where SV is a second vector in the scattering plane in cartesian real space coordinates
    !!--++    Y := (Z x X)
    !!--++
    !!--++    ATTENTION: Be aware that the choice of SV with respect to Q will decide which of the two possible right handed coordinates fullfilling
    !!--++               the conditions above will be used!!!
    !!--++
    !!--++
    !!--++ Updated: April - 2005, June-2012 (JRC)
    !!
    Module Function Magn_Inter_Vec_Pf(MiVC,H,Spv, Cell) Result(MiV_Pf)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent(in) :: MiVC   !Must be provided in Crystallographic
       real(kind=cp),    dimension(3), intent(in) :: H      !Cartesian frame
       real(kind=cp),    dimension(3), intent(in) :: SPV
       Type (Cell_G_Type),             intent(in) :: Cell
       complex(kind=cp), dimension(3)             :: MiV_PF

       !---- Local variables ----!
       real(kind=cp), dimension (3)            :: QSV,Q,SV,X,Y,Z
       real(kind=cp), dimension (3,3)          :: M

       Q = Cart_Vector("R",H,Cell)             !Here Q is the crystallographic Q=-Q(Blume)
       SV = Cart_Vector("R",SPV,Cell)

       X = Q / sqrt(dot_product(Q,Q))          !Crystallographic Blume-frame
       QSV= Cross_Product(Q,SV)                !components w.r.t. Cartesian
       Z = QSV / sqrt(dot_product(QSV,QSV))    !default (set in Cell) frame
       Y = Cross_Product(Z,X)


       M(1,:) = 0.0    ! X-Component of Magnetic Interaction Vector is always equal to ZERO because
                       ! of MRI = Q x (M(Q) x Q); where M(Q) is the Fourier Transform of Magnetic Density of the sample
       M(2,:) = Y
       M(3,:) = Z


       MiV_PF = Matmul(M, MiVC) !conversion of MiVC to Blume frame
    End Function  Magn_Inter_Vec_Pf

    !!--++
    !!--++ Module Function Mm(Nsf, MiV_Pf) Result(Mmc)
    !!--++    complex(kind=cp), dimension(3), intent( in) :: MiV_PF !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                               :: MMC    !  Out -> magnetic-magnetic interference term
    !!--++
    !!--++    (Private)
    !!--++    Calculates the magnetic-magnetic interference contribution to scattering in the
    !!--++    polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Update April - 2005
    !!
    Module Function Mm(MiV_PF) Result(Mmc)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: MMC

       MMC = Real(MiV_PF(2) * Conjg(MiV_PF(3)) + Conjg(MiV_PF(2)) * MiV_PF(3))

    End Function  Mm

    !!--++
    !!--++ Module Function Nuc_Contr(Nsf, MiV_Pf) Result(Nsc)
    !!--++    complex(kind=cp),               intent( in):: NSF     !  In  -> Nuclear Structure Factor
    !!--++    complex(kind=cp), dimension(3), intent( in):: MiV_PF  !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                              :: NSC     !  Out -> nuclear scattering contribution
    !!--++
    !!--++    (Private)
    !!--++    Calculates the nuclear contribution to scattering in the polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Nuc_Contr(Nsf) Result(Nsc)
       !---- Argument ----!
       complex(kind=cp), intent( in)   :: NSF
       real(kind=cp)                   :: NSC

       NSC = NSF * Conjg(NSF)

    End Function  Nuc_Contr

    !!--++
    !!--++ Module Function Real_Nm_Y(Nsf, MiV_Pf) Result(R_Nm_Y)
    !!--++    complex(kind=cp),               intent(in)  :: NSF     !  In  -> Nuclear Structure Factor
    !!--++    complex(kind=cp), dimension(3), intent(in)  :: MiV_PF  !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                               :: R_NM_Y  !  Out -> real part of nuclear magnetic interference contribution along Y
    !!--++
    !!--++    (Private)
    !!--++    Calculates the real part of the nuclear-magnetic interference contribution along Y to scattering in the polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Real_Nm_Y(Nsf, MiV_Pf) Result(R_Nm_Y)
       !---- Argument ----!
       complex(kind=cp),               intent(in)  :: NSF
       complex(kind=cp), dimension(3), intent(in)  :: MiV_PF
       real(kind=cp)                               :: R_Nm_Y

       R_Nm_Y = Real(NSF * Conjg(MiV_PF(2)) + Conjg(NSF) * MiV_PF(2))

    End Function  Real_Nm_Y

    !!--++
    !!--++ Module Function Real_Nm_Z(Nsf, MiV_Pf) Result(R_Nm_Z)
    !!--++    complex(kind=cp), intent( in)               :: NSF            !  In  -> Nuclear Structure Factor
    !!--++    complex(kind=cp), dimension(3), intent( in) :: MiV_PF         !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    real(kind=cp)                               :: R_NM_Z         !  Out -> nuclear real part of magnetic interference contribution along Z
    !!--++
    !!--++    (Private)
    !!--++    Calculates the real part of the nuclear-magnetic interference contribution along Z
    !!--++    to scattering in the polarisation coordinate frame according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Real_Nm_Z(Nsf, MiV_Pf) Result(R_Nm_Z)
       !---- Argument ----!
       complex(kind=cp),               intent( in) :: NSF
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: R_Nm_Z

       R_Nm_Z = Real(NSF * Conjg(MiV_PF(3)) + Conjg(NSF) * MiV_PF(3))

    End Function  Real_Nm_Z

    !!--++
    !!--++ Module Function Tchiral(MiV_Pf,B_Q) Result(Tc)
    !!--++    complex(kind=cp), dimension(3), intent( in):: MiV_PF !  In  -> Magnetic Interaction Vector in polarisation frame
    !!--++    character(len=*),optional,      intent(in) :: B_Q    !  In  -> The calculation is done with original Blume equation Q_B= -Q_cryst
    !!--++    real(kind=cp)                              :: TC     !  Out -> chiral contribution
    !!--++
    !!--++    (Private)
    !!--++    Calculates the chiral contribution to scattering in the polarisation coordinate frame
    !!--++    according to the Blume equations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Tchiral(MiV_Pf,B_Q) Result(Tc)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent(in) :: MiV_PF
       character(len=*),optional,      intent(in) :: B_Q
       real(kind=cp)                              :: TC
       !---- Local variables ----!
       real(kind=cp) :: s

       s=-1.0
       if(present(B_Q)) s=1.0

       TC = -s * Aimag(MiV_PF(2) * Conjg(MiV_PF(3)) - Conjg(MiV_PF(2)) * MiV_PF(3))

    End Function  Tchiral

End SubModule Polar_Functions