!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_DiffPatt
!!----   INFO: Diffraction Patterns Information
!!----
!!
 Module CFML_DiffPatt
    !---- Use Modules ----!
    Use CFML_GlobalDeps, only : cp, ops_sep, err_cfml, clear_error, eps, CFML_debug
    Use CFML_Maths,      only : spline_d2y, spline_interpol, locate, second_derivative, Linear_Interpol
    use CFML_Strings,    only : u_case, get_words, get_num, Get_NumStd
    use CFML_Profiles,   only : Pseudovoigt_Der, Split_Pseudovoigt_Der
    implicit none

    private

    !---- List of public functions ----!
    public ::   FWHM_peak

    !---- List of public subroutines ----!
    public ::  Add_Patterns,Allocate_Pattern, Deallocate_Pattern,     &
               Calc_Background, Del_NoisyPoints, Read_Pattern,        &
               Read_Background_File, Write_Pattern_FreeFormat,        &
               Write_Pattern, Write_Pattern_INSTRM5, Write_Pattern_XYSig, &
               Calc_BackGround_Chebychev, Calc_BackGround_split_pVoigt, &
               Calc_BackGround_pVoigt,Calc_Rfactors

    public ::  Load_Pattern, Calc_Chi2


    !---- Definitions ----!

    !!----
    !!---- TYPE :: DIFFPAT_TYPE
    !!----
    Type, public :: DiffPat_Type
       character(len=180)                           :: Title  = " "        ! Identification/ Title
       character(len=20)                            :: KindRad= " "        ! Type of Radiation
       character(len=20)                            :: ScatVar= " "        ! 2Theta, TOF, Q, s, d-spacing, SinTL/L,...
       real(kind=cp)                                :: xmin   = 0.0_cp     ! Maximum and Minimum values for X and Y
       real(kind=cp)                                :: xmax   = 0.0_cp
       real(kind=cp)                                :: ymin   = 0.0_cp
       real(kind=cp)                                :: ymax   = 0.0_cp
       real(kind=cp)                                :: step   = 0.0_cp
       integer                                      :: NPts   = 0          ! Number of Points
       logical                                      :: SigVar = .false.     ! .True. for sigma values / .False. for variance
       real(kind=cp), dimension(5)                  :: Wave   = 0.0_cp     ! Wave1, Wave2, ratio, zero / Dtt1, Dtt2, Dtt_1overD, zero...
       real(kind=cp), allocatable, dimension (:)    :: x
       real(kind=cp), allocatable, dimension (:)    :: y
       real(kind=cp), allocatable, dimension (:)    :: sigma
    End Type DiffPat_Type

    !!----
    !!---- TYPE :: DiffPat_E_Type
    !!----
    Type, public, extends (DiffPat_Type) ::  DiffPat_E_Type
       character(len=30)           :: Instr   =" "                    ! Instrument name
       character(len=80)           :: Filename=" "                    ! Filename
       character(len=512)          :: FilePath=" "                    ! Path, info, etc.
       real(kind=cp)               :: Scal    =1.0_cp                 ! Scale factor
       real(kind=cp)               :: Monitor =0.0_cp
       real(kind=cp)               :: Norm_Mon=0.0_cp
       real(kind=cp)               :: Col_Time=0.0_cp
       real(kind=cp)               :: Tsample =298.0_cp               ! Sample temperature
       real(kind=cp)               :: Tset    =0.0_cp                 ! Wished temperature
       logical                     :: CT_Step =.false.                ! Constant step
       logical                     :: al_x    =.false.                ! Flags for variable allocations
       logical                     :: al_y    =.false.                !
       logical                     :: al_sigma=.false.                !
       logical                     :: al_ycalc=.false.                !
       logical                     :: al_bgr  =.false.                !
       logical                     :: al_istat=.false.                !
       real(kind=cp), allocatable, dimension (:) :: ycalc             ! Caculated intensity
       real(kind=cp), allocatable, dimension (:) :: bgr               ! Background
       integer,       allocatable, dimension (:) :: istat             ! Information about point "i"
       integer,       allocatable, dimension (:) :: ND                ! Number of Detectors contributing to point "i"
    End Type DiffPat_E_Type

    !!----
    !!---- TYPE :: DIFFPAT_G_TYPE
    !!----
    Type, public, extends (DiffPat_E_Type) :: DiffPat_G_Type
       character(len=40)                           :: Legend_X=" "     !x-axis legend, eg. "Lambda (Angstroms)"
       character(len=40)                           :: Legend_Y=" "     !y-axis legend, eg. "Intensity (arb. units)"
       logical                                     :: gy      =.false. ! Flags for Graphics
       logical                                     :: gycalc  =.false.
       logical                                     :: gsigma  =.false.
       logical                                     :: gbgr    =.false.
    End Type DiffPat_G_Type

    Type, public :: DiffPatt_Conditions_Type
       integer            :: job
       real(kind=cp)      :: scale_factor = 1.0
       character(len=140) :: title
    End Type DiffPatt_Conditions_Type

    !!----
    !!---- TYPE :: PowPatt_CW_Conditions_Type
    !!----
    Type, public, extends(DiffPatt_Conditions_Type) :: PowPatt_CW_Conditions_Type
        character(len=20)                        :: profile              ! 'TCH_pVoigt' | pVoigt | split_pVoigt
        real(kind=cp)                            :: tthmin               ! degrees
        real(kind=cp)                            :: step                 ! degrees
        real(kind=cp)                            :: tthmax               ! degrees
        real(kind=cp)                            :: wdt                  ! Number of FWHM for calculating the contribution of a peak
        logical                                  :: is_asym  = .false.   ! True if asymmetry is used
        logical                                  :: is_tth   = .false.   ! True if the user provides two theta axis
        logical                                  :: twowaves = .false.   ! True if the there are two distinct wavelengths
        real(kind=cp), dimension(:), allocatable :: tth                  ! degrees
        !Variables
        real(kind=cp)                            :: zero                 ! degrees
        real(kind=cp)                            :: sycos                ! Systematic cosine dependence (FullProf)
        real(kind=cp)                            :: sysin                ! Systematic sine dependence (FullProf)
        real(kind=cp), dimension(2)              :: lambda               ! Angstroms (k-alpha Doublet)
        real(kind=cp)                            :: ratio                ! Intensity_lambda(1)/Intensity_lambda(2)
        real(kind=cp)                            :: u                    ! Parameter for gaussian broadening  (strain-like)
        real(kind=cp)                            :: v                    ! Parameter for gaussian broadening
        real(kind=cp)                            :: w                    ! Parameter for gaussian broadening
        real(kind=cp)                            :: x                    ! Parameter for lorentzian broadening (strain-like)
        real(kind=cp)                            :: y                    ! Parameter for lorentzian broadening (size-like)
        real(kind=cp)                            :: asym1 = 0.0          ! Asymmetry parameter (S_L in pcr)
        real(kind=cp)                            :: asym2 = 0.0          ! Asymmetry parameter (D_L in pcr)
        real(kind=cp), dimension(5)              :: prf_par              ! Additional profile parameters
        real(kind=cp), dimension(18)             :: multip
        integer,       dimension(18)             :: Lprof                ! Order: zero,sycos,sysin,lamb1,lamb2,ratio,u,v,w,x,y,asym1,asym2,prf_par
    End Type PowPatt_CW_Conditions_Type

    !!----
    !!---- TYPE :: PowPatt_TOF_Conditions_Type
    !!----
    Type, public, extends(DiffPatt_Conditions_Type) :: PowPatt_TOF_Conditions_Type
        character(len=20)                        :: profile              ! 'Conv_b2b_EXP*pVoigt' | Conv_IkCarp*pVoigt | split_pVoigt | TCH_pVoigt
        real(kind=cp)                            :: tof_min          ! 10^-6 s
        real(kind=cp)                            :: step             ! 10^-6 s
        real(kind=cp)                            :: tof_max          ! 10^-6 s
        real(kind=cp)                            :: wdt              ! Number of FWHM for calculating the contribution of a peak
        real(kind=cp)                            :: bank_angle       ! degrees
        real(kind=cp)                            :: d_min            ! angstroms
        real(kind=cp)                            :: d_max            ! angstroms
        logical                                  :: is_tof = .false. ! True if the user provides tof axis
        real(kind=cp), dimension(:), allocatable :: tof              ! 10^-6 s
        ! Variables
        ! TOF = Zero + dtt1 * d + dtt2 * d^2 + dtt_1overD/d
        real(kind=cp)                            :: zero             ! 10^-6 s
        real(kind=cp)                            :: dtt1             ! 10^-6 s / angstrom
        real(kind=cp)                            :: dtt2             ! 10^-6 s / angstrom^2
        real(kind=cp)                            :: dtt_1overD       ! 10^-6 s * angstrom
        ! Sigma^2 =  (sig-2 + GSIZ ) * d^4 + (sig-1+DST ) * d^2 + sig-0 + sig_Q * d
        real(kind=cp)                            :: sigma2
        real(kind=cp)                            :: sigma1
        real(kind=cp)                            :: sigma0
        real(kind=cp)                            :: sigmaQ
        ! gamma   =  (gam-2 + DSIZ ) * d^2 + (gam-1+LSTR) * d  + gam-0
        real(kind=cp)                            :: gamma2
        real(kind=cp)                            :: gamma1
        real(kind=cp)                            :: gamma0
        ! alpha=alpha0 + alpha1/d+ alphaQ/sqrt(d)
        real(kind=cp)                            :: alpha0
        real(kind=cp)                            :: alpha1
        real(kind=cp)                            :: alphaQ
        ! beta=beta0 + beta1/d^4  + betaQ/d^2
        real(kind=cp)                            :: beta0
        real(kind=cp)                            :: beta1
        real(kind=cp)                            :: betaQ
        real(kind=cp), dimension(5)              :: prf_par ! Additional profile parameters
        real(kind=cp), dimension(22)             :: multip  ! Order: zero,dtt1,dtt2,dtt_1overD,sigma2,sigma1,sigma0,
        integer,       dimension(22)             :: Lprof   ! gamma2,gamma1,gamma0,alpha0,alpha1,alphaQ,beta0,beta1,betaq,prf_par
    End Type PowPatt_TOF_Conditions_Type

    Type, public :: Bck_type
      integer                                   :: ipatt      !> Number of the pattern to which this background applies
      character(len=:),             allocatable :: Funct_typ  !> Polynomial, Chebychev, Linear_Interpol, Spline_interpol
      integer                                   :: Num_Fpar   !> Number of parameters defining the function
      real(kind=cp), dimension(:),  allocatable :: Fpar       !> Parameters of the function
      real(kind=cp), dimension(:),  allocatable :: xFpar      !> Positions for interpolations (not used in polynomial functions)
      character(len=:),             allocatable :: Peak_typ   !> Type of peaks: pseudo-Voigt or split-pseudo-Voigt
      integer                                   :: Num_peaks  !> Number of peaks
      real(kind=cp), dimension(:,:),allocatable :: Ppar       !> Peak Parameters (4/6, Num_peaks)
                                                              !>           1         2          3         4           5          6
                                                              !> Order: position, intensity, FWHM_left, Eta_left, FWHM_right, Eta_right
                                                              !> If Peak_typ=="pseudo-Voigt" -> Ppar(4,Num_peaks)
                                                              !> If Peak_typ=="split-pseudo-Voigt" -> Ppar(6,Num_peaks)
    End Type Bck_type

   !!----
   !!---- TYPE :: INTERVAL_TYPE
   !!--..
   !!
   Type, public :: Interval_Type
      real(kind=cp) :: Mina=0.0_cp  !low limit
      real(kind=cp) :: Maxb=0.0_cp  !high limit
   End Type Interval_Type

    Type, public :: Excl_reg_type
       integer :: num_excl
       type(Interval_Type), dimension(:), allocatable :: exc
    End Type Excl_reg_type

    !!----
    !!---- TYPE :: PATTERN_TYPE
    !!--..
    !!----
    !!---- Update: January - 2025
    Type, public :: Pattern_Type
       character(len=:), allocatable                :: Filename  !> Name of the file (including path) containing the data Pdat
       character(len=:), allocatable                :: name      !> Pattern name
       character(len=2)                             :: mode      !> 'CW': constant-wavelength | 'TF': time of flight
       character(len=1)                             :: radiation !> 'N' : Neutron | 'X' : X-ray | 'E' : Electron
       character(len=1)                             :: sample    !> 'P' : powder  | 'X' : single-crystal'
       character(len=:), allocatable                :: patt_type !> radiation sample [mode]
       integer                                      :: irf       !> Nuber of the IRF file that has been read to be used with this pattern
       class(DiffPatt_Conditions_Type), allocatable :: cond      !> Pattern conditions
       class(DiffPat_E_Type), allocatable           :: PDat      !> Pattern data
    end type Pattern_Type




    !---- Interfaces - Overload ----!
    Interface Read_Pattern
       Module procedure Read_Pattern_Mult
       Module procedure Read_Pattern_One
    End Interface

    Interface

       Pure Module Function is_in_excluded_region(x,Excl) result(isin)
         real(kind=cp),       intent(in) :: x
         type(Excl_reg_type), intent(in) :: Excl
         logical                         :: isin
       End Function is_in_excluded_region

       Module Function Calc_Chi2(Pat,Excl,nfree) result(Chi2)
          class(DiffPat_E_Type), intent(in) :: Pat     !> Diffraction pattern
          type(Excl_reg_type),   intent(in) :: Excl    !> Excluded region
          integer,               intent(in) :: nfree
          real(kind=cp)                     :: Chi2
       End Function Calc_Chi2

       Module Function Load_Pattern(filename, mode, sig, header) Result(Pat)
         !> Read a diffraction pattern from a file.
         character(len=*),                    intent (in)      :: filename !> Full path of the file
         character(len=*), target, optional,  intent (in)      :: mode     !> Mode: CIF | DMC | D1A | D1AOLD | D1B | D2B | D2BOLD | D20 | GSAS | GSASTOF | G41 | G42 | HRPT | PANALYTICAL | NLS |  SOCABIM | TIMEVARIABLE | XYSIGMA | 3T2
         logical,          target, optional,  intent (in)      :: sig      !> If sig is False, pat%sigma will be the standard deviation. Otherwise, it will be the variance.
         character(len=*), target, optional,  intent (out)     :: header   !> Header
         type(DiffPat_E_Type)                                  :: pat      !> Diffraction pattern
       End Function Load_Pattern

       Module Subroutine Add_Patterns(Patterns, N, Active, Pat, step_int, VNorm)
          !---- Arguments ----!
          class(DiffPat_Type), dimension(:), intent(in)  :: Patterns
          integer,                           intent(in)  :: N
          logical,             dimension(:), intent(in)  :: Active
          class(DiffPat_Type),               intent(out) :: Pat
          real(kind=cp), optional,           intent(in)  :: step_int
          real(kind=cp), optional,           intent(in)  :: VNorm
       End Subroutine Add_Patterns

       Module Subroutine Calc_BackGround(Pat, Ncyc, Np, Xmin, Xmax)
          !> Calculate a background using an iterative process according
          !> to Bruckner, S. (2000). J. Appl. Cryst., 33, 977-979.
          !> Np is the extension of Np points at Left and Right. Normally it could be
          !> around 10-40 points.
          !>
          !> 30/04/2019
          class(DiffPat_E_Type),     intent(in out) :: Pat   !> Diffraction pattern
          integer,                   intent(in)     :: NCyc  !> Number of cycles
          integer,                   intent(in)     :: Np    !> Number of extension points to the right and left (usually around 10-40)
          real(kind=cp), optional,   intent(in)     :: Xmin  !> Minimum value of x
          real(kind=cp), optional,   intent(in)     :: Xmax  !> Maximum value of x
       End Subroutine Calc_BackGround

       Module Subroutine Calc_BackGround_Chebychev(Pat, Nd, Par_bck, Xmin, Xmax,der)
          !> Calculate a background using a Chebychev polynomial of degree Nd
          !> The applied formula is:
          !> Np is the extension of Np points at Left and Right. Normally it could be
          !> around 10-40 points.
          !>
          !> JRC 14/06/2025
          class(DiffPat_E_Type),                 intent(in out) :: Pat     !> Diffraction pattern
          integer,                               intent(in)     :: Nd      !> Degree of polynomial
          real(kind=cp), dimension(:),           intent(in)     :: Par_bck !> Parameter vector of Chebychev coefficients
          real(kind=cp), optional,               intent(in)     :: Xmin    !> Minimum value of x
          real(kind=cp), optional,               intent(in)     :: Xmax    !> Maximum value of x
          real(kind=cp), optional, dimension(:), intent(out)    :: der     !> Derivatives w.r.t. coefficients
       End Subroutine Calc_BackGround_Chebychev

       Module Subroutine Calc_BackGround_split_pVoigt(Pat, Np, Par, deriv)
          class(DiffPat_E_Type),                     intent(in out) :: Pat     !> Diffraction pattern
          integer,                                   intent(in)     :: Np      !> Number of peaks
          real(kind=cp), dimension(:,:),             intent(in)     :: Par     !> Parameters (1:6,Np)
          real(kind=cp), optional, dimension(:,:,:), intent(out)    :: deriv   !> Derivatives w.r.t. parameters for each point (1:6,Np,npts)
       End Subroutine Calc_BackGround_split_pVoigt

       Module Subroutine Calc_BackGround_pVoigt(Pat, Np, Par, deriv)
          class(DiffPat_E_Type),                     intent(in out) :: Pat     !> Diffraction pattern
          integer,                                   intent(in)     :: Np      !> Number of peaks
          real(kind=cp), dimension(:,:),             intent(in)     :: Par     !> Parameters (1:4,Np)
          real(kind=cp), optional, dimension(:,:,:), intent(out)    :: deriv   !> Derivatives w.r.t. parameters (1:4,Np,npts)
       End Subroutine Calc_BackGround_pVoigt

       Module Subroutine Calc_Rfactors(Pat,Excl,nfree,Chi2,Rexp,Rwpatt,Rpatt,Gof,sigvar)
          class(DiffPat_E_Type), intent(in)  :: Pat     !> Diffraction pattern
          type(Excl_reg_type),   intent(in)  :: Excl    !> Excluded region
          integer,               intent(in)  :: nfree   !> Number of degrees of freedom
          real(kind=cp),         intent(out) :: Chi2    !> Chi-square
          real(kind=cp),         intent(out) :: Rexp    !> Expected weigthed R-factor
          real(kind=cp),         intent(out) :: Rwpatt  !> Weigthed R-factor
          real(kind=cp),         intent(out) :: Rpatt   !> R-factor
          real(kind=cp),         intent(out) :: Gof     !> "Goodness-of-fit"
          logical,               intent(in)  :: sigvar  !> If .false. sigma component is the variance
       End Subroutine Calc_Rfactors

       Module Function FWHM_Peak(Pat, Xi, Yi, Ybi, RLim) Result(v)
          !---- Arguments ----!
          class(DiffPat_Type),       intent(in) :: Pat
          real(kind=cp),             intent(in) :: Xi
          real(kind=cp),             intent(in) :: Yi
          real(kind=cp),             intent(in) :: Ybi
          real(kind=cp),optional,    intent(in) :: RLim
          real(kind=cp)                         :: V
       End Function FWHM_Peak

       Module Subroutine Del_NoisyPoints(Pat, NoisyP, FileInfo)
          !---- Arguments ----!
          class(DiffPat_Type),  intent(in out) :: Pat
          integer,              intent(out)    :: NoisyP
          logical, optional,    intent(in)     :: FileInfo
       End Subroutine Del_NoisyPoints

       Module Subroutine Read_Background_File(Bck_File, Bck_Mode, Pat)
          !---- Arguments ----!
          character(len=*),         intent(in   )    :: bck_file
          character(len=*),         intent(in   )    :: bck_mode
          class(DiffPat_E_Type),  intent(in out)     :: Pat
       End Subroutine Read_Background_File

       Module Subroutine Read_Pattern_CIF(Filename,Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_Type), intent(out) :: Pat
       End Subroutine Read_Pattern_CIF

       Module Subroutine Read_Pattern_DMC(Filename,Pat)
          !---- Arguments ----!
          character (len=*),   intent(in)  :: Filename
          class(DiffPat_type), intent(out) :: pat
       End Subroutine Read_Pattern_DMC

       Module Subroutine Read_Pattern_D1A_D2B(Filename,Pat)
          !---- Arguments ----!
          character(len=*),        intent(in)  :: Filename
          class(DiffPat_E_Type),   intent(out) :: Pat
       End Subroutine Read_Pattern_D1A_D2B

       Module Subroutine Read_Pattern_D1A_D2B_OLD(Filename,Pat)
          !---- Arguments ----!
          character(len=*),        intent(in)  :: Filename
          class(DiffPat_E_Type),   intent(out) :: Pat
       End Subroutine Read_Pattern_D1A_D2B_OLD

       Module Subroutine Read_Pattern_D1B_D20(Filename,Pat)
          !---- Arguments ----!
          character(len=*),        intent(in)  :: Filename
          class(DiffPat_E_Type),   intent(out) :: Pat
       End Subroutine Read_Pattern_D1B_D20

       Module Subroutine Read_Pattern_Free(Filename,Pat,ext)
          !---- Arguments ----!
          character(len=*),           intent(in)  :: Filename
          class(DiffPat_Type),        intent(out) :: Pat
          character(len=*), optional, intent(in)  :: ext
       End Subroutine Read_Pattern_Free

       Module Subroutine Read_Pattern_Gsas(Filename, Pat, mode)
          !---- Arguments ----!
          character(len=*),           intent(in)  :: Filename
          class(DiffPat_Type),        intent(out) :: Pat
          character(len=*), optional, intent(in)  :: Mode
       End Subroutine Read_Pattern_Gsas

       Module Subroutine Read_Pattern_G41(Filename,Pat)
          !---- Arguments ----!
          character(len=*),        intent(in)  :: Filename
          class(DiffPat_E_Type),   intent(out) :: Pat
       End Subroutine Read_Pattern_G41

       Module Subroutine Read_Pattern_Isis_m(Filename, VPat, NPat)
          !---- Arguments ----!
          Character(len=*),                   intent(in)  :: Filename
          class(DiffPat_Type), dimension(:),  intent(out) :: VPat
          integer,                            intent(out) :: Npat
       End Subroutine Read_Pattern_Isis_m

       Module Subroutine Read_Pattern_Mult(filename, Patts, NPats, mode)
          !---- Arguments ----!
          character(len=*),                   intent (in)      :: Filename
          class(DiffPat_Type), dimension(:),  intent (out)     :: Patts
          integer,                            intent (in out)  :: NPats
          character(len=*), optional,         intent (in)      :: Mode
       End Subroutine Read_Pattern_Mult

       Module Subroutine Read_Pattern_NLS(Filename,Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_Type), intent(out) :: Pat
       End Subroutine Read_Pattern_NLS

       Module Subroutine Read_Pattern_One(Filename, Pat, Mode, Sig, Header)
          !> Read a pattern from a file.
          character(len=*),            intent (in)      :: Filename !> Full path of the file
          class(DiffPat_Type),         intent (in out)  :: Pat      !> Diffraction pattern
          character(len=*), optional,  intent (in)      :: mode     !> Mode: CIF | DMC | D1A | D1AOLD | D1B | D2B | D2BOLD | D20 | GSAS | GSASTOF | G41 | G42 | HRPT | PANALYTICAL | NLS |  SOCABIM | TIMEVARIABLE | XYSIGMA | 3T2
          logical,          optional,  intent (in)      :: sig      !> If sig is False, pat%sigma will be the standard deviation. Otherwise, it will be the variance.
          character(len=*), optional,  intent (out)     :: header   !> Header
       End Subroutine Read_Pattern_One

       Module Subroutine Read_Pattern_Panalytical_CSV(Filename,Pat)
          !---- Arguments ----!
          character (len=*),   intent(in)  :: Filename
          class(DiffPat_type), intent(out) :: pat
       End Subroutine Read_Pattern_Panalytical_CSV

       Module Subroutine Read_Pattern_Panalytical_JCP(Filename, Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_type), intent(out) :: pat
       End Subroutine Read_Pattern_Panalytical_JCP

       Module Subroutine Read_Pattern_Panalytical_UDF(Filename, Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_type), intent(out) :: pat
       End Subroutine Read_Pattern_Panalytical_UDF

       Module Subroutine Read_Pattern_Panalytical_XRDML(Filename,Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_type), intent(out) :: pat
       End Subroutine Read_Pattern_Panalytical_XRDML

       Module Subroutine Read_Pattern_Socabim(Filename,Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_Type), intent(out) :: Pat
       End Subroutine Read_Pattern_Socabim

       Module Subroutine Read_Pattern_TimeVar(Filename, Pat)
          !---- Arguments ----!
          character(len=*),    intent(in)  :: Filename
          class(DiffPat_Type), intent(out) :: Pat
       End Subroutine Read_Pattern_TimeVar

       Module Subroutine Read_Pattern_XYSigma(Filename, Pat, PDF, Header)
          !---- Arguments ----!
          character(len=*),           intent(in)  :: Filename
          class(DiffPat_Type),        intent(out) :: Pat
          logical,          optional, intent(in)  :: PDF
          character(len=*), optional, intent (out):: Header
       End Subroutine Read_Pattern_XYSigma

       Module Subroutine Set_Background_Inter(Pat, Bcky, Bckx, N)
          !---- Arguments ----!
          class(DiffPat_E_Type),         intent(in out) :: Pat
          real (kind=cp), dimension(:),  intent(in out) :: bcky
          real (kind=cp), dimension(:),  intent(in out) :: bckx
          integer,                       intent(in    ) :: n
       End Subroutine Set_Background_Inter

       Module Subroutine Set_Background_Poly(Pat, Bkpos, Bckx, N)
          !---- Arguments ----!
          class(DiffPat_E_Type),         intent(in out) :: Pat
          real (kind=cp),                intent(in    ) :: bkpos
          real (kind=cp), dimension(:),  intent(in    ) :: bckx
          integer,                       intent(in    ) :: n
       End Subroutine Set_Background_Poly

       Module Subroutine Write_Pattern(Filename, Pat, Mode, excl, xmin, xmax)
          !---- Arguments ----!
          character(len=*),               intent(in)    :: filename
          class(DiffPat_Type),            intent(inout) :: Pat
          character(len=*),               intent(in)    :: Mode
          logical, dimension(:),optional, intent(in)    :: excl
          real(kind=cp),        optional, intent(in)    :: xmin
          real(kind=cp),        optional, intent(in)    :: xmax
       End Subroutine Write_Pattern

       Module Subroutine Write_Pattern_FreeFormat(Filename,Pat,excl,xmin,xmax)
          !---- Arguments ----!
          character (len=*),               intent(in)     :: Filename
          class(DiffPat_Type),             intent(in out) :: Pat
          logical, dimension(:),optional,  intent(in)     :: excl
          real(kind=cp),        optional,  intent(in)     :: xmin
          real(kind=cp),        optional,  intent(in)     :: xmax
       End Subroutine Write_Pattern_FreeFormat

       Module Subroutine Write_Pattern_INSTRM5(Filename,Pat,excl,xmin,xmax,var)
          !---- Arguments ----!
          character(len=*),               intent(in)     :: Filename
          class(DiffPat_Type),            intent(in out) :: Pat
          logical, dimension(:),optional, intent(in)     :: excl
          real(kind=cp),        optional, intent(in)     :: xmin
          real(kind=cp),        optional, intent(in)     :: xmax
          character(len=*),    optional,  intent(in)     :: var
       End Subroutine Write_Pattern_INSTRM5

       Module Subroutine Write_Pattern_XYSig(Filename,Pat,excl,xmin,xmax,calc)
          !---- Arguments ----!
          character(len=*),               intent(in) :: filename     ! Path+Filename
          class(DiffPat_Type),            intent(in) :: Pat          ! Pattern object
          logical, dimension(:),optional, intent(in) :: excl         ! Exclusion zones
          real(kind=cp),        optional, intent(in) :: xmin         ! Limits
          real(kind=cp),        optional, intent(in) :: xmax
          logical,              optional, intent(in) :: calc         ! if calc=.true. write ycalc istead of y
       End Subroutine Write_Pattern_XYSig

    End Interface

 Contains

    !!----
    !!---- ALLOCATE_PATTERN
    !!----
    !!----    Allocate the Part of Diffractions Patterns
    !!----
    !!---- 30/04/2019
    !!
    Subroutine Allocate_Pattern(Pat,Npts)
       !---- Arguments ----!
       class(DiffPat_Type), intent (in out) :: Pat      ! Pattern object
       integer, optional,   intent (in)     :: npts     ! Number of points

       !---- Local variables ----!
       integer :: n

       !> Init
       call clear_error()

       N=Pat%npts
       if (present(npts)) N=Npts

       if (n <= 0) then
          err_CFML%IErr=1
          Err_CFML%flag=.true.
          err_CFML%Msg="Allocate_Pattern@DIFFPATT: Failed the attempt to allocate a DiffPat_Type!"
          return
       end if

       !> Allocating
       Pat%npts=N

       !> Class (DiffPat_Type)
       if (allocated(pat%x) ) deallocate(pat%x)
       allocate(pat%x(n))
       pat%x=0.0_cp

       if (allocated(pat%y) ) deallocate(pat%y)
       allocate(pat%y(n))
       pat%y=0.0_cp

       if (allocated(pat%sigma) ) deallocate(pat%sigma)
       allocate(pat%sigma(n))
       pat%sigma=0.0_cp

       !> class (DiffPat_E_Type)
       select type(Pat)
          class is (DiffPat_E_Type)
             if (allocated(pat%bgr) ) deallocate(pat%bgr)
             allocate(pat%bgr(n))
             pat%bgr=0.0_cp

             if (allocated(pat%ycalc) ) deallocate(pat%ycalc)
             allocate(pat%ycalc(n))
             pat%ycalc=0.0_cp

             if (allocated(pat%istat) ) deallocate(pat%istat)
             allocate(pat%istat(n))
             pat%istat=1

             if (allocated(pat%nd) ) deallocate(pat%nd)
             allocate(pat%nd(n))
             pat%nd=0

             Pat%al_x=.true.
             Pat%al_y=.true.
             Pat%al_sigma=.true.
             Pat%al_ycalc=.true.
             Pat%al_bgr  =.true.
             Pat%al_istat=.true.
       end select

       !> class (DiffPat_G_Type)
       select type(Pat)
          type is (DiffPat_G_Type)
             Pat%gy    =.false.
             Pat%gycalc=.false.
             Pat%gsigma=.false.
             Pat%gbgr  =.false.
       end select

    End Subroutine Allocate_Pattern

    !!----
    !!---- DEALLOCATE_PATTERN
    !!----
    !!----    De-Allocate components of the object "pat", of type Diffraction_Pattern_Type
    !!----    depending on the value of the MODE string. At present the following MODE
    !!----    values are available:
    !!----      "DATA " -> x,y remain allocated                  (purge sigma,ycalc,bgr,istat)
    !!----      "DATAS" -> x,y,sigma remain allocated            (purge ycalc,bgr,istat)
    !!----      "RIETV" -> x,y,sigma,ycalc,bgr remain allocated  (purge istat)
    !!----      "GRAPH" -> x,y,sigma,istat remain allocated      (purge ycalc, bgr)
    !!----      "PRF  " -> x,y,sigma,ycalc,bgr,istat, everything remains allocated (changed w.r.t. previous version)
    !!----
    !!----
    !!---- 30/04/2019
    !!
    Subroutine Deallocate_Pattern(Pat,Mode)
       !---- Arguments ----!
       class(DiffPat_Type), intent (in out) :: Pat       ! Pattern object
       character(len=*),    intent (in)     :: Mode      ! Type of the Deallocation

       !---- Local Variables ----!
       character(len=5) :: car

       car=adjustl(u_case(mode))
       select case (trim(car))
          case ("DATA")
             select type(Pat)
                type is (DiffPat_Type)
                   if (allocated(Pat%sigma)) deallocate(Pat%sigma)

                class is (DiffPat_E_Type)
                   if (allocated(Pat%sigma)) deallocate(Pat%sigma)
                   if (allocated(Pat%ycalc)) deallocate(Pat%ycalc)
                   if (allocated(Pat%bgr))   deallocate(Pat%bgr)
                   if (allocated(Pat%istat)) deallocate(Pat%istat)
                   !if (allocated(Pat%ND))    deallocate(Pat%ND)

                   Pat%al_sigma=.false.
                   Pat%al_ycalc=.false.
                   Pat%al_bgr  =.false.
                   Pat%al_istat=.false.
             end select

          case ("DATAS")
             select type(Pat)
                class is (DiffPat_E_Type)
                   if (allocated(Pat%ycalc)) deallocate(Pat%ycalc)
                   if (allocated(Pat%bgr))   deallocate(Pat%bgr)
                   if (allocated(Pat%istat)) deallocate(Pat%istat)
                   !if (allocated(Pat%ND))    deallocate(Pat%ND)

                   Pat%al_ycalc=.false.
                   Pat%al_bgr  =.false.
                   Pat%al_istat=.false.
             end select

          case ("RIETV")
             select type(Pat)
                class is (DiffPat_E_Type)
                   if (allocated(Pat%istat)) deallocate(Pat%istat)

                   Pat%al_istat=.false.
             end select

          case ("GRAPH")
             select type(Pat)
                type is (DiffPat_E_Type)
                   if (allocated(Pat%ycalc)) deallocate(Pat%ycalc)
                   if (allocated(Pat%bgr))   deallocate(Pat%bgr)

                   Pat%al_ycalc=.false.
                   Pat%al_bgr  =.false.

                type is (DiffPat_G_Type)
                   if (allocated(Pat%ycalc)) deallocate(Pat%ycalc)
                   if (allocated(Pat%bgr))   deallocate(Pat%bgr)

                   Pat%al_ycalc=.false.
                   Pat%al_bgr  =.false.

                   Pat%gycalc=.false.
                   Pat%gbgr  =.false.
             end select

          case ("PRF")
             ! Nothing to do
       end select
    End Subroutine Deallocate_Pattern

 End Module CFML_DiffPatt
