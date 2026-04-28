 Module CFML_Powder
  !> This module containts submodules with subroutines and functions imported from FullProf and adapted
  !> to CrysFML. Not everything is implemented (or tested!) yet.
    Use CFML_GlobalDeps,   only: CP, PI, TPI, Clear_Error, set_error, Err_CFML, CFML_debug
    use CFML_Strings,      only: l_case, u_case, File_Type, Reading_File, Get_Num, Get_Words
    use CFML_Profiles,     only: Keijser              !Subroutine Keijser(h,eta,hg,hl), not yet used
    Use CFML_Metrics,      only: Cell_G_Type, Cart_Vector
    Use CFML_Reflections,  only: Refl_type, RefP_type
    Use CFML_gSpaceGroups, only: SPG_Type,rot_mat_type
    Use CFML_Maths,        only: locate
    use CFML_DiffPatt,     only: Pattern_Type, Interval_Type, Bck_Type, Excl_reg_type
    use CFML_ioForm,       only: Phase_Type

    implicit none

    private
    public :: date
    public :: Read_IRF, Init_IRF, Write_IRF, Read_Patt_IRF, & ! Instrumental resolution functions (Submodule > Pow_IRF)
              Corr_Int
    public :: Calc_Anisotropic_Strain, Calc_Anisotropic_Size  ! Microstructure (Submodule > Pow_Microstructure)
                                                              ! Still to implement: Calc_Anisotropic_Shift
    public :: Powder_Lorentz_IntegInt_CW, Lorentz_abs_CW      ! Lorentz factors and absorption corrections (Submodule > Pow_Lorentz_Absorption)
    public :: Preferred_orientation

    Type, public :: IRF_Type
       Character(Len=:), allocatable :: Nam            !> Title
       Character(Len=:), allocatable :: geom           !> Bragg-Brentano, Debye-Scherrer, Flat-Plate-PSD, etc...
       Character(Len=:), allocatable :: file_Nam       !> Name of the input file
       Character(Len=:), allocatable :: JOBT           !> Radiation XRAY, NEUTRONS
       Character(Len=:), allocatable :: irftype        !> type of IRF file
       real(Kind=cp)                 :: tmin,step,tmax !> range of scattering variable
       integer                       :: Num_Patt       !> Number of the corresponding pattern
       integer                       :: N_points       !> If /=0 a list for CW 2theta,HG,HL, ...for tof, sigma, gamma, alpha, beta and shifts are provided
       integer                       :: N_items        !> Number of columns when N_points/=0
       real(Kind=cp), dimension(:),allocatable :: shift, corr_int
    End Type IRF_Type

    Type, Extends(IRF_Type), public  :: IRF_CW_Type
       real(Kind=cp), dimension(2)   :: lambda      !> Wavelengths
       real(Kind=cp)                 :: ratio       !> Lambda(2)/Lambda(1) intensity ratio
       real(Kind=cp)                 :: rkk         !> Polarization factor (synchrotron))
       real(Kind=cp)                 :: alpsd       !> Incident angle for fixed flat-plate
       real(Kind=cp)                 :: cthm        !> Cos(2th_Monok)^2
       real(Kind=cp), dimension(2)   :: U_i,V_i,W_i !> Gaussian FWHM parameters for Lambda(1,2)
       real(Kind=cp), dimension(2)   :: X_i,Y_i,Z_i !> Lorentzian FWHM parametersa for Lambda(1,2)
       real(Kind=cp)                 :: sl_i,dl_i   !> Asymmetry parameters
       !> List of items for interpolation (to be allocated witn N_points)
       real(Kind=cp), dimension(:),allocatable :: ttheta  !> 2Theta in degrees
       real(Kind=cp), dimension(:),allocatable :: HG, HL  !> Gaussiand and Lorentzian FWHM of the Voigt profile function
    End Type IRF_CW_Type

    Type, Extends(IRF_Type), public :: IRF_TOF_Type
       real(Kind=cp) :: TOFTET_i      !> 2Theta of the detector bank in TOF
       real(Kind=cp) :: zero_i        !> Epithermal zero
       real(Kind=cp) :: zerot_i       !> Thermal zero
       real(Kind=cp) :: DTT1_i        !> TOF =Zeroe + DTT1*d + DTT2 *d^2 (for NPRO=10, dtt2=0)
       real(Kind=cp) :: DTT2_i        !>              "
       real(Kind=cp) :: DTT_1overd_i  !> TOF =Zeroe + DTT1*d + DTT2 *d^2 + DTT_1overd/d" (for NPRO=14)
       real(Kind=cp) :: DTT1t_i       !> TOF =Zerot + DTT1t*d - DTT2t/d  (only for NPRO=10)
       real(Kind=cp) :: DTT2t_i       !>

       real(Kind=cp) :: alfa0_i       !> Instrumental constants for
       real(Kind=cp) :: alfa1_i       !> Time of Flight diffractometers
       real(Kind=cp) :: alfaq_i       !>
       real(Kind=cp) :: beta0_i       !>
       real(Kind=cp) :: betaq_i       !> beta=bet0+beta1/d^4+ betaq/d^2
       real(Kind=cp) :: beta1_i       !> All these parameters are read from
       real(Kind=cp) :: alfa0t_i      !> the instrumental resolution function
       real(Kind=cp) :: alfa1t_i      !> file characteristics of the instrument.
       real(Kind=cp) :: beta0t_i      !>
       real(Kind=cp) :: beta1t_i      !> They are additive with respect to refined
       real(Kind=cp) :: xcross_i      !> parameters. The sum is performed in the
       real(Kind=cp) :: wcross_i      !> subroutine calling the calculation of TOF profile

       real(Kind=cp) :: sig0_i        !> Gaussian width
       real(Kind=cp) :: sig1_i        !>
       real(Kind=cp) :: sig2_i        !>
       real(Kind=cp) :: sigq_i        !>
       real(Kind=cp) :: gamma0_i      !> Lorentzian width
       real(Kind=cp) :: gamma1_i      !>
       real(Kind=cp) :: gamma2_i      !>
      ! List of items for interpolation (to be allocated witn N_points)
       real(Kind=cp), dimension(:),allocatable :: tof              !> TOF in micro-seconds
       real(Kind=cp), dimension(:),allocatable :: sigma, gamma     !> Gaussian and Lorentzian widths
       real(Kind=cp), dimension(:),allocatable :: alpha, beta      !> Rising and decay parameters
    End Type IRF_TOF_Type

    !> Global types and varibales to be shared by programs aiming
    !> to treat powder diffraction data
    integer,public :: N_phases   !> Number of phases (dimension of Ph)
    integer,public :: N_patterns !> Number of patterns (dimension of Pat)
    integer,public :: N_CW_patt  !> Number of CW patterns (dimension of cw_irf)
    integer,public :: N_TOF_patt !> Number of TOF patterns (dimension of tof_irf)

    type(Phase_Type),   dimension(:), allocatable, public :: Ph        !> Phases
    type(Pattern_Type), dimension(:), allocatable, public :: Pat       !> Patterns
    type(IRF_TOF_Type), dimension(:), allocatable, public :: tof_irf   !> IRF Patterns
    type(IRF_CW_Type),  dimension(:), allocatable, public :: cw_irf    !> IRF Patterns
    type(RefP_type),    dimension(:), allocatable, public :: RL        !> To be allocated as (N_phases)
    type(Bck_type),     dimension(:), allocatable, public :: Bck       !> To be allocated as (N_patterns)
    type(Excl_reg_type),dimension(:), allocatable, public :: Excl      !> To be allocated as (N_patterns)
    logical,            dimension(:), allocatable, public :: gen_pat   !true if the pattern has been generated (N_patterns)

    ! Global indicators of the goodness of fit
    real(kind=cp),      dimension(:), allocatable, public :: Chi_sqr   !> Chi-square for each pattern (N_patterns)
    real(kind=cp),      dimension(:), allocatable, public :: R_patt    !> R-pattern           (N_patterns)
    real(kind=cp),      dimension(:), allocatable, public :: R_wpatt   !> R-weighted-pattern  (N_patterns)
    real(kind=cp),      dimension(:), allocatable, public :: R_exp     !> R-expected          (N_patterns)
    real(kind=cp),    dimension(:,:), allocatable, public :: R_Bragg   !> Bragg R-factors     (N_phases,N_patterns)
    real(kind=cp),    dimension(:,:), allocatable, public :: R_Mag     !> Magnetic R-factors  (N_phases,N_patterns)
    real(kind=cp),    dimension(:,:), allocatable, public :: R_Fact    !> R-factors for single crystals R_fact=Sum{Fobs-Fcalc}/Sum{Fobs}  (N_phases,N_patterns)
    Character(len=15),dimension(:,:), allocatable, public :: Calc_Typ  !> Calculation type  (N_phases,N_patterns)
                                                                       !> Nuclear/X-rays/Electron-Diff/Magnetic-k-type/
                                                                       !> Magnetic-MSG/Magnetic-MSSG/Nuc-XRA-Elect/
                                                                       !> Symmetry-Modes/Mag-Only-MSG/Mag-Only-MSSG

    Character(len=70),    save,public :: DAT_TIM                      !> Date and time for the job
    Integer, dimension(8),save,public :: NUM_datim                    !> Numerical values for date and time


    Interface

      Module Subroutine Corr_Int(npcorr,corrv,postt,xcorr,corrp)
      !>
      !> Subroutine for correcting intensities at position postt
      !>
        integer,                              intent(in)  :: npcorr !> Number of points in the correction array, or polynomial order
        real(kind=cp), dimension(:),          intent(in)  :: corrv  !> Value array, or polynomial coefficients
        real(kind=cp),                        intent(in)  :: postt  !> Position to correct for intensity
        real(kind=cp),                        intent(out) :: xcorr  !> Correction factor to apply to the intensity
        real(kind=cp), dimension(:),optional, intent(in)  :: corrp  !> Position array for linear interpolation
      End Subroutine Corr_Int

      Module Subroutine Init_IRF(irf,np)
      !> Initializes the object irf
        class(IRF_type),   intent(in out) :: irf  !> IRF object that can be of types IRF_CW_Type or IRF_TOF_Type
        integer, optional, intent(in)     :: np   !> Number of points when the irf object is defined by allocating
                                                  !> the array components of the object (if np = 0, they are not allocated)
      End Subroutine Init_IRF

      Module Subroutine Read_Patt_IRF(ffile,N_ini,N_end,mode,IRF,ipat)
      !> Intermediate subroutine for reading an IRF file
      !> The input CFL file is passed as ffile of type file_type.
      !> the output is the object IRF of type IRF_type.
      !>
         Type(file_type),              intent(in)   :: ffile
         integer,                      intent(in)   :: n_ini
         integer,                      intent(in)   :: n_end
         character(len=*),             intent(in)   :: mode
         class(IRF_Type), allocatable, intent(out)  :: IRF
         integer, optional,            intent(in)   :: ipat
      End Subroutine Read_Patt_IRF

      Module Subroutine Read_IRF(filename, IRF, mode, ipat)
      !> In this subroutine, the instrumental parameters of a Constant-Wavelength
      !> diffractometer are read to construct the full resolution function of the instrument.
      !>
      !> The instrumental peak shape is considered as Voigtian, thus the FWHM
      !> of the Gaussian and Lorentzian components vs. scattering angle have
      !> to be given as a table for interpolation or as the coefficients of
      !> the following functions:
      !>
      !>  pV_IRF (old IRESO=-1)
      !>    H**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i   ETA= X_i + Y_i * (2theta/100.0) +ZINS * (2theta/100.0)^2
      !>    HG and HL are calculated for each reflection inside the program handling
      !>    the Bragg reflections
      !>
      !>  TCH_pV_IRF1 (old IRESO=1)
      !>    HG**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i
      !>    HL    = X_i*tan(theta)+Y_i/cos(theta)+Z_i
      !>
      !>  TCH_pV_IRF2 (old IRESO=2)
      !>    HG**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i
      !>    HL    = (X_i*(2theta/100.0)+Y_i)*(2theta/100.0)+Z_i
      !>
      !>  TCH_pV_IRF3 (old IRESO=3)
      !>    HG**2 = (U_i*(2theta/100.0)+V_i)*(2theta/100.0)+W_i
      !>    HL    = (X_i*(2theta/100.0)+Y_i)*(2theta/100.0)+Z_i
      !>
      !>  (old IRESO=5)   T.O.F. instrumetal resolution function for NPROF=9 or NPROF=13
      !>
      !>     The following additive parameters are read
      !>
      !> D2TOF    dtt1_i   dtt2_i   dtt_1overd_i  zero
      !> TWOTH    toftet
      !> SIGMA      sig2_i    sig1_i    sig0_i  sigQ_i
      !> GAMMA    gamma2_i  gamma1_i  gamma0_i
      !> ALFBE    alpha0_i   beta0_i  alpha1_i   beta1_i  alphaQ_i  betaQ_i
      !>
      !> (not implemented yet!) (old IRESO=6)   T.O.F. instrumetal resolution function for NPROF=10 (Jason Hodges)
      !>
         character(len=*),             intent(in)  :: filename !> Complete name of the .irf file
         class(IRF_Type), allocatable, intent(out) :: IRF      !> Object containing the information about the instrumental resolution
         character(len=*),             intent(in)  :: mode     !> CW of TF, needed for allocating the proper type of IRF
         integer,            optional, intent(in)  :: ipat     !> Number of the pattern for which is IRF applies
      End Subroutine Read_IRF

      Module Subroutine Write_IRF(IRF,iprint)
      !> Writes the information contained in IRF to the logical unit iprint (if absent prints to the screen)
         class(IRF_Type),   intent(in) :: IRF     !> Object containing the information about the instrumental resolution
         integer, optional, intent(in) :: iprint  !> Logical unit for writing the information on IRF
      End Subroutine Write_IRF


      Module Subroutine Calc_Anisotropic_Strain(str_model,Laue,mode,par,hkl,sq,dst2,dtt,pst,cell,der)
      !>  Subroutine for calculating the broadening due to anisotropic strains
      !>  Some constants used in the calculations are provided below
      !>     PRMUL = 1.0E-08 * 8Ln2 * (180/PI)**2   For CW patterns
      !>     PRMUL = 1.0E-08 * 8Ln2 * 0.25 * dtt1 * dtt1    For TOF patterns
      !>
      !>     DST: contribution to the broadening of reflection HKL due
      !>          to the particular model of strain
      !>     Der(j): Derivative of DST2=DST*DST with respect to the strain
      !>             parameter E
      !>     SQ: 1/d^2
      !>
      !>     As strain produces a tan(theta) dependence of the broadening
      !>     of Bragg reflections, the effect of the strain has been included
      !>     as the following formula for Gaussian and Lorentian broadening:
      !>
      !>            HG = sqrt {(U + (1-n)^2  DST2) tan^2 (theta) + V tan(theta) + W}
      !>            HL = ( X + n DST ) tan(theta)
      !>
      !>     this implies a gaussian distribution of strains
      !>
      !>       Fwhm(strain) = 2sqrt(2Ln2) Sigma(SQ)/SQ  * tan(theta) = DST*tan(theta)
      !>
      !>       Var(SQ)=Sigma(SQ)**2= Sum(i,j){Cov(ai,aj)(D(SQ)/Dai)(D(SQ)/Daj)}
      !>
        character(len=*),                      intent(in)   :: str_model  !Model for strains: QUARTIC_FORM, UNIAXIAL_STRAIN, SPHERICAL_HARMONICS
        character(len=*),                      intent(in)   :: Laue !> Laue class
        character(len=*),                      intent(in)   :: mode !> CW or TF (Constant Wavelength or Time-of-Flight)
        real(kind=cp), dimension(:),           intent(in)   :: par  !> Values of parameters
        real(kind=cp), dimension(3),           intent(in)   :: hkl  !> (h,k,l)
        real(kind=cp),                         intent(in)   :: sq   !> 1/d^2
        real(kind=cp),                         intent(out)  :: dst2 !> Contribution to broadening due to strain effects
        real(kind=cp),                optional,intent(in)   :: dtt  !> dtt1 for TOF case
        real(kind=cp), dimension(4),  optional,intent(in)   :: pst  !> [first three components is the axis for uniaxial strain, 4 component is the modulus]
        class(cell_G_type),           optional,intent(in)   :: cell !> cell (Needed for spherical harmonics and uniaxial strains)
        real(kind=cp), dimension(:),  optional,intent(out)  :: der  !> Derivatives of dst2 w.r.t. the provided parameters
      End Subroutine Calc_Anisotropic_Strain

      Module Subroutine Calc_Anisotropic_Size(size_model,Laue,mode,par,hkl,sq,wav,dsiz,psz,nv,cell,der)
      !>   Subroutine for calculating broadening due to anisotropic crystallite sizes
      !>
      !>       DSIZ: Contribution to the broadening of reflection HKL due
      !>             to the particular model of particle size
      !>       Der(j): Derivative of DSIZ with respect to the size parameter Zj
      !>       SQ: 1/d2
      !>
      !>       In constant wavelength powder diffraction size effects produce a
      !>       1/cos(theta) dependence of the broadening of Bragg reflections.
      !>       The effect of the size broadening has been included as contributing
      !>       to the parameter Y(VLOR) of the Thompson-Cox-Hasting formula for
      !>       Lorentzian broadening
      !>
      !>
      !>              Hlorentz = X*tan(theta)+{Y+DSIZ)}/cos(theta)
      !>              DISZ= lambda*0.001*360/pi^2*SZ
      !>
      !>      For T.O.F. powder diffraction the Lorentzian size broadening contributes as:
      !>
      !>              gamma = (gam-2 + DSIZ) d^2 + (gam-1+ DSTR) d + gam-0
      !>
      !>                           gamma(size) =  DSIZ d^2
      !>
      !>              DSIZ = Dtt1 * 2/pi * 0.001 * SZ
      !>
      !>       SZ is the size function  SZ=1000/D=f(h,k,l,Zj), where f(nh,nk,nl,Za)=f(h,k,l,Zj)
      !>
      !>    The term 2/pi is introduced to take into account the integral breadth for interpreting
      !>    the crystallite size as a volume average.
      !>   -------------------------------------------------------------------------------------
      !>     ISTR=0 + ISI=1 -----> Platelet-like particles
      !>      "     + ISI=-1-----> Needle-like particles
      !>      "     + ISI=2 -----> --- (00L)
      !>      "     + ISI=3 -----> --- (0K0)
      !>      "     + ISI=4 -----> --- (H00)
      !>      "     + ISI=5 -----> --- (HK0)
      !>      "     + ISI=6 -----> --- (H0L)
      !>      "     + ISI=7 -----> --- (0KL)
      !>      "     + ISI=8 -----> correlation length for satellites
      !>      "     + ISI=9 -----> H=2n+1 and K=2m+1
      !>      "     + ISI=10-----> H0L (H+L=2n)
      !>      "     + ISI=11-----> HKL (except HHL)
      !>      "     + ISI=12-----> HKL (h=2n+1 )
      !>      "     + ISI=13-----> HKL (h=2n   ) and (k=2n+1)
      !>      "     + ISI=14-----> HKL (h=2n+1 or k=2m+1 and l=2n+1)
      !>
      !>
      !>      "     +++ Options 15-22 implemented by Robert J. Papoular and JRC +++
      !>      "     +    Ref: M. Jarvinen, J. Appl. Cryst. 26 (1993),p.525                                                          +
      !>                 ( see Fig.1 and Tables 2 & 4 in particular )
      !>
      !>                   Last revision 28 December 1998.
      !>      "     + ISI=15-----> Monoclinic with b-axis up to 4th-order Ylm's:
      !>                           Y00,Y20,Y22+,Y22-,Y40,Y42+,Y42-,Y44+,Y44-
      !>                           Spacegroups 3-15
      !>                           Laue class 2/m
      !>
      !>
      !>      "     + ISI=16-----> Trigonal with hexagonal setting, unique axis c
      !>
      !>                           Spacegroups 149-167
      !>                           Laue class -3 m
      !>
      !>                           Ylm's up to 6th order:
      !>                           Y00,Y20,Y40,Y43-,Y60,Y63-,Y66+
      !>
      !>      "     + ISI=17-----> Cubic - x,y,z along a,b and c.
      !>
      !>                           Beware ! For spacegroups 207-230,
      !>                                    coeffficient of K62 is strictly NIL.
      !>                           Laue class = m -3 m
      !>                           No restriction for spacegroups 195-206
      !>                           Laue class = m -3
      !>
      !>                           Cubic harmonics Klm's up to 8th order:
      !>                           K00,K41,K61,K62,K81
      !>
      !>      "     + ISI=18-----> Orthorhombic - Spacegroups 16-74
      !>                                          Laue class mmm
      !>
      !>                           Ylm's up to 4th order:
      !>                           Y00,Y20,Y22+,Y40,Y42+,Y44+
      !>
      !>      "     + ISI=19-----> Hexagonal - Spacegroups 168-194
      !>
      !>                           Beware ! For spacegroups 177-194,
      !>                                    coefficient of Y66- is strictly NIL.
      !>
      !>                                   Laue class = 6/mmmm
      !>
      !>                                   No restriction for spacegroups 168-176
      !>                                   Laue class = 6/m
      !>
      !>                           Spherical harmonics Ylm's up to 6th order:
      !>                           Y00,Y20,Y40,Y60,Y66+,Y66-
      !>   !
      !>      "     + ISI=20-----> Trigonal with hexagonal setting, unique axis c
      !>
      !>                           Spacegroups 143-148
      !>                           Laue class -3
      !>
      !>                           Ylm's up to 4th order:
      !>                           Y00,Y20,Y40,Y43-,Y43+
      !>
      !>      "     + ISI=21-----> Tetragonal - Spacegroups 75-142
      !>
      !>                           Beware ! For spacegroups 89-142
      !>                               coefficients of Y44- & Y64- are strictly NIL
      !>
      !>                           Laue class = 4/mmm
      !>
      !>                           No restriction for spacegroups 75-88
      !>                           Laue class = 4/m
      !>
      !>                           Ylm's up to 6th order:
      !>                           Y00,Y20,Y40,Y44+,Y44-,Y60,Y64+,Y64-
      !>
      !>      "     + ISI=22-----> Triclinic
      !>
      !>                           Spacegroups 1-2
      !>                           Laue class -1
      !>
      !>                           Ylm's up to 2th order:
      !>                           Y00,Y20,Y21+,Y21-,Y22+,Y22-
      !>
      !> -------------------------------------------------------------------
        character(len=*),                      intent(in)   :: size_model  !Model for size: !Model for size: "QUADRATIC_FORM", "NEEDLE", "PLATELET", "SPHERICAL_HARMONICS", "HKL RULE", "HKL_CONDIT"
        character(len=*),                      intent(in)   :: Laue !> Laue class
        character(len=*),                      intent(in)   :: mode !> CW or TF (Constant Wavelength or Time-of-Flight)
        real(kind=cp), dimension(:),           intent(in)   :: par  !> Values of parameters
        real(kind=cp), dimension(3),           intent(in)   :: hkl  !> (h,k,l)
        real(kind=cp),                         intent(in)   :: sq   !> 1/d^2
        real(kind=cp),                         intent(in)   :: wav  !> Lambda for CW or dtt1 for TOF case
        real(kind=cp),                         intent(out)  :: dsiz !> Contribution to broadening due to size effects
        real(kind=cp), dimension(4),  optional,intent(in)   :: psz  !> [first three components is the axis for uniaxial size (Platelet/Needle), 4 component is the modulus squared]
        integer      , dimension(:,:),optional,intent(in)   :: nv   !> (5,:) Sets of 5 integers for the size model HKL_CONDIT: n1 H + n2 K + n3 L = n4 n + n5
        class(cell_G_type),           optional,intent(in)   :: cell !> cell (Needed for spherical harmonics and uniaxial size)
        real(kind=cp), dimension(:),  optional,intent(out)  :: der  !> Derivatives of dsiz w.r.t. the provided parameters
      End Subroutine Calc_Anisotropic_Size

      Module Function Lorentz_abs_CW(sinth,costh,postt,tmv,radia,ilor,cabs,cthm,twoTh0,alpsd,rkk,ip,rmua) result(plor)
      !> Function to calculate the product of the Lorentz, polarization and absorption factors
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
      End Function Lorentz_abs_CW

      Module Function Powder_Lorentz_IntegInt_CW(job,cmono,rkks,muR,sinth2,costh) result(plor)     !loren <- FullProf
         Character(len=*), intent(in)  :: job        !>  X or N for x-rays and neutrons
         real(kind=cp),    intent(in)  :: cmono      !>  Cos(2theta_monok)^2
         real(kind=cp),    intent(in)  :: rkks       !>  Polarization factor for synchrotron
         real(kind=cp),    intent(in)  :: muR        !>  Absorption coefficient x Radius
         real(kind=cp),    intent(in)  :: sinth2     !>  sintheta**2
         real(kind=cp),    intent(in)  :: costh      !>  costheta
         real(kind=cp)                 :: plor
      End Function Powder_Lorentz_IntegInt_CW

      Module Subroutine Preferred_orientation(model,n_pref,axes_pref,gr,mRot,par,Rfl,pref_corr,lambda,norstep,der)
      !> Multi-Axial March-Dollase Model for Preferred Orientation
        character(len=*),              intent(in) :: model      !> MAX_MD, MAXHP_MD
        integer,                       intent(in) :: n_pref     !> Number of preferred orientation axes for multiaxial March-Dollase model
        real(kind=cp), dimension(:,:), intent(in) :: axes_pref  !> (4,n_pref) Axes for preferred orientation, forth componet is the modulus squared
        real(kind=cp), dimension(3,3), intent(in) :: gr         !> Reciprocal metric tensor
        type(rot_mat_type),            intent(in) :: mRot       !> Rotational part of symmetry operators: mRot%rot(:,:,i=1,mRot%numops)
        real(kind=cp), dimension(:,:), intent(in) :: par        !> (2,n_pref) Preferred orientation parameters (value and fraction)
        Class(Refl_Type),              intent(in) :: Rfl        !> Bragg reflection
        real(kind=cp),                 intent(out):: pref_corr  !> Preferred orientation correction factor for reflection Rfl
        real(kind=cp),       optional, intent(in) :: lambda     !> Needed for MAXHP_MD model to calculate sinth
        integer,             optional, intent(in) :: norstep    !> Number of steps for integration in model MAXHP_MD
        real(kind=cp), dimension(:,:), optional, intent(out) :: der !> derivatives w.r.t. free parameters
      End Subroutine Preferred_orientation

    End Interface

    Contains

      Subroutine date()
        character (len=10):: dat,tim
        call date_and_time(date=dat,time=tim,values= NUM_datim)
        write(unit=DAT_TIM,fmt="(4a)")                              &
          "    Date: ",dat(7:8)//"/"//dat(5:6)//"/"//dat(1:4),      &
            "  Time: ",tim(1:2)//":"//tim(3:4)//":"//tim(5:10)
      End Subroutine date

 End Module CFML_Powder