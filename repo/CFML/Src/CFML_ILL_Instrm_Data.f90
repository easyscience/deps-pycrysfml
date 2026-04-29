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
!!---- MODULE: CFML_ILL_Instrm_Data
!!----   INFO: Subroutines related to Instrument information from ILL
!!----
!!---- HISTORY
!!----    Created: 05/02/2006
!!----    Updated: 05/04/2012
!!----
!!--..    The default instrument cartesian frame is that defined by
!!--..       W.R.Busing & H.A.Levy (Acta Cryst. 22,457-464 (1967)
!!--..    The reciprocal Cartesian Frame used by Busing-Levy has "x" along a*
!!--..    "y" in the (a*,b*) plane and "z" perpendicular to that plane. The
!!--..    matrix passing from (h,k,l) in r.l.u. to the cartesian frames is
!!--..             | a*   b*.cos(gamm*)   c*.cos(beta*)            |
!!--..      [B] =  | 0    b*.sin(gamm*)  -c*.sin(beta*).cos(alpha) |
!!--..             ! 0          0          1/c                      |
!!--..
!!--..          [hc] = [B] [h]
!!--..
!!--..      Let U be the orthogonal matrix relating the phi-axis system
!!--..      attached to the phi-shaft of the instrument with the Cartesian crystal system
!!--..      then     [h-phi]= [U] [hc]      [h-phi] = [U] [B] [h] = [UB] [h]
!!--..
!!--..      Let us define three more cartesian systems attached to the Chi,Omega and
!!--..      Theta axes, coincident with phi-axis when all instrument angles are set
!!--..      to zero :
!!--..         [h-chi] = [PHI] [h-phi]
!!--..         [h-omg] = [CHI] [h-chi]
!!--..         [h-tet] = [OMG] [h-omg]
!!--..
!!--..               |  cos(phi)      sin(phi)      0  |             |zL-axis
!!--..       [PHI] = | -sin(phi)      cos(phi)      0  |             |
!!--..               |     0            0           1  |             |
!!--..                                             incoming beam ->__|_________yL-axis
!!--..               |  cos(chi)       0     sin(chi)  |            /
!!--..       [CHI] = |     0           1       0       |           /
!!--..               | -sin(chi)       0     cos(chi)  |          /xL-axis
!!--..
!!--..               |  cos(omg)      sin(omg)      0  |
!!--..       [OMG] = | -sin(omg)      cos(omg)      0  |
!!--..               |     0            0           1  |
!!--..
!!--..      Let us define 2 more Cartesian frames: The laboratory fixed system
!!--..      and the 2Theta-axis system attached to the 2theta shaft. All coincide
!!--..      for all angles equal to zero.
!!--..       [h-Lab] = [THE]  [h-tet] = [N] [h-omg]
!!--..       [h-tte] = [THE]t [h-tet] = [M] [h-omg]
!!--..     Where:
!!--..               |  cos(the)      sin(the)      0  |
!!--..       [THE] = | -sin(the)      cos(the)      0  |
!!--..               |     0            0           1  |
!!--..
!!--..                           |  cos(nu)      sin(nu)      0  |
!!--..       [N] = [THE] [OMG] = | -sin(nu)      cos(nu)      0  |  with nu=omg+tet
!!--..                           |     0            0         1  |
!!--..
!!--..                            |  cos(mu)      sin(mu)      0  |
!!--..       [M] = [THE]t [OMG] = | -sin(mu)      cos(mu)      0  |  with mu=omg-tet
!!--..                            |     0            0         1  |
!!--..    All angles, except Chi, are left-handed rotations about their respective axes.
!!--..
!!--..      Basic Diffractometer equations:
!!--..            q = |h| = sqrt(dot_product(hc,hc)), with [hc]=[B][h]
!!--..      Bragg equation: sin(theta) = Lambda.q/2
!!--..      The reflection [h] is in the diffraction position if
!!--..           [h-tet] = [OMG] [CHI] [PHI] [U] [B] [h]
!!--..      has the form [h-tet]t = (q, 0, 0)
!!--..
!!--..    The orientation matrix U can be obtained from two non-colinear reflections h1 & h2,
!!--..    provided the cell parameters are known
!!--..
!!--..    Two kind of Busing-Levy (BL) frames are possible:
!!--..    For both frames the origin is at sample position and the y-axis
!!--..    positive sense is along the secondary beam (from monochromator to sample)
!!--..    a)  z upward
!!--..    b)  z downward
!!--..    The x-axis makes a right-handed frame with the other axes. Positive 2theta
!!--..    angles are from y-axis towards x-axis
!!--..    A change of geometry is always possible by providing the components of the
!!--..    axes {e1,e2,e3} with respect to the standard {i,j,k} BL-frame
!!--..
!!--..    igeom=1: Bisecting (PSI=0)
!!--..    igeom=2: Bisecting - HiCHI
!!--..    igeom=3: Normal beam
!!--..    igeom=4: Parallel (PSI=90)
!!----
!!---- DEPENDENCIES
!!--++   Use CFML_GlobalDeps
!!--++   Use CFML_Maths,    only: equal_vector, locate, second_derivative, Spline_Interpol, sort, invert => Inverse_Matrix, &
!!--++                            Cross_Product
!!--++   use CFML_Strings,  only: u_case, l_case, Number_Lines, Reading_Lines, Get_Num
!!--++
!!--++   use CFML_DiffPatt, only: DiffPat_E_Type, Allocate_Pattern
!!----
!!----
!!---- VARIABLES
!!--..    Types
!!----    BASIC_NUMORC_TYPE
!!----    BASIC_NUMORI_TYPE
!!----    BASIC_NUMORR_TYPE
!!----    CALIBRATION_DETECTOR_TYPE
!!----    DIFFRACTOMETER_TYPE
!!----    GENERIC_NUMOR_TYPE
!!----    ILL_DATA_RECORD_TYPE
!!----    POWDER_NUMOR_TYPE
!!----    SXTAL_NUMOR_TYPE
!!----    SXTAL_ORIENT_TYPE
!!--..
!!----    CURRENT_INSTRM
!!--++    CURRENT_INSTRM_SET                [Private]
!!----    CURRENT_ORIENT
!!----    CYCLE_NUMBER
!!--++    GOT_ILL_DATA_DIRECTORY            [Private]
!!--++    GOT_ILL_TEMP_DIRECTORY            [Private]
!!----    ILL_DATA_DIRECTORY
!!----    ILL_TEMP_DIRECTORY
!!----    INSTRM_DIRECTORY
!!--++    INSTRM_DIRECTORY_SET              [Private]
!!--++    INSTRM_GEOMETRY_DIRECTORY_SET     [Private]
!!--++    INSTRM_INFO_ONLY                  [Private]
!!--++    IVALUES                           [Private]
!!----    MACHINE_NAME
!!--++    N_KEYTYPES                        [Private]
!!--++    NL_KEYTYPES                       [Private]
!!--++    NTEXT                             [Private]
!!--++    NVAL_F                            [Private]
!!--++    NVAL_I                            [Private]
!!--++    RVALUES                           [Private]
!!--++    TEXT_ILL                          [Private]
!!--++    UNCOMPRESSCOMMAND                 [Private]
!!----    YEAR_ILLDATA
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!--++       ADDING_NUMORS_D1A_DIFFPATTERN    [Private]
!!--++       ADDING_NUMORS_D1B_D20            [Private]
!!--++       ADDING_NUMORS_D20S_DIFFPATTERN   [Private]
!!--++       ADDING_NUMORS_D4_DIFFPATTERN     [Private]
!!----       ALLOCATE_NUMORS
!!--++       ALLOCATE_POWDER_NUMORS          [Overloaded]
!!--++       ALLOCATE_SXTAL_NUMORS           [Overloaded]
!!----       DEFINE_UNCOMPRESS_PROGRAM
!!----       GET_ABSOLUTE_DATA_PATH
!!----       GET_COUNTS
!!----       GET_NEXT_YEARCYCLE
!!----       GET_SINGLE_FRAME_2D
!!--++       INIT_POWDER_NUMOR               [Overloaded]
!!--++       INIT_SXTAL_NUMOR                [Overloaded]
!!----       INITIALIZE_DATA_DIRECTORY
!!----       INITIALIZE_NUMOR
!!--++       INITIALIZE_NUMORS_DIRECTORY     [Private]
!!--++       INITIALIZE_TEMP_DIRECTORY       [Private]
!!--++       NUMBER_KEYTYPES_ON_FILE         [Private]
!!--++       NUMORD1BD20_TO_DIFFPATTERN      [Private]
!!----       POWDERNUMORS_TO_DIFFPATTERN
!!--++       READ_A_KEYTYPE                  [Private]
!!----       READ_CALIBRATION_FILE
!!----       READ_CALIBRATION_FILE_D1A       [Private]
!!----       READ_CALIBRATION_FILE_D2B       [Private]
!!----       READ_CURRENT_INSTRM
!!--++       READ_F_KEYTYPE                  [Private]
!!--++       READ_I_KEYTYPE                  [Private]
!!--++       READ_J_KEYTYPE                  [Private]
!!----       READ_NUMOR
!!--++       READ_NUMOR_D1A                  [Private]
!!--++       READ_NUMOR_D1B                  [Private]
!!--++       READ_NUMOR_D2B                  [Private]
!!--++       READ_NUMOR_D4                   [Private]
!!--++       READ_NUMOR_D9                   [Private]
!!--++       READ_NUMOR_D10                  [Private]
!!--++       READ_NUMOR_D16                  [Private]
!!--++       READ_NUMOR_D19                  [Private]
!!--++       READ_NUMOR_D20                  [Private]
!!--++       READ_POWDER_NUMOR               [Overloaded]
!!--++       READ_R_KEYTYPE                  [Private]
!!--++       READ_S_KEYTYPE                  [Private]
!!--++       READ_V_KEYTYPE                  [Private]
!!--++       READ_SXTAL_NUMOR                [Overloaded]
!!----       SET_CURRENT_ORIENT
!!----       SET_DEFAULT_INSTRUMENT
!!----       SET_ILL_DATA_DIRECTORY
!!----       SET_INSTRM_DIRECTORY
!!----       SET_KEYTYPES_ON_FILE            [Private]
!!----       UPDATE_CURRENT_INSTRM_UB
!!----       WRITE_CURRENT_INSTRM_DATA
!!----       WRITE_HEADERINFO_NUMOR
!!--++       WRITE_HEADERINFO_POWDER_NUMOR   [Overloaded]
!!--++       WRITE_HEADERINFO_SXTAL_NUMOR    [Overloaded]
!!----       WRITE_NUMOR_INFO
!!--++       WRITE_POWDER_NUMOR              [Overloaded]
!!--++       WRITE_SXTAL_NUMOR               [Overloaded]
!!----
!!

Module CFML_ILL_Instrm_Data
   !---- Use Modules ----!

   use, intrinsic :: iso_c_binding
   Use CFML_GlobalDeps
   Use CFML_Maths,    only: equal_vector, locate, second_derivative, Spline_Interpol, sort, invert => Inverse_Matrix, &
                            Cross_Product
   use CFML_Strings,  only: u_case, l_case, Number_Lines, Reading_Lines, Get_Num

   use CFML_DiffPatt, only: DiffPat_E_Type, Allocate_Pattern

   !---- Variables ----!
   Implicit none

   private

   !---- Public Subroutines ----!
   public :: Set_Current_Orient, Read_Numor, Read_Current_Instrm, Write_Current_Instrm_data,     &
             Allocate_Numors, Set_ILL_data_directory, Set_Instrm_directory, Init_SXTAL_Numor,    &
             Update_Current_Instrm_UB, Set_Default_Instrument,Get_Single_Frame_2D,               &
             Initialize_Data_Directory, Get_Absolute_Data_Path, Get_Next_YearCycle,              &
             Write_Generic_Numor, Set_Instrm_Geometry_Directory, Write_Numor_Info,               &
             Define_Uncompress_Program, PowderNumors_To_DiffPattern, Write_HeaderInfo_Numor,     &
             Read_Calibration_File, Initialize_Numor, Get_Counts,              &
             Write_HeaderInfo_SXTAL_Numor,Write_SXTAL_Numor

   !---- Private Subroutines ----!
   private:: Initialize_Numors_Directory,Initialize_Temp_Directory,Number_Keytypes_On_File,      &
             Read_A_Keytype,Read_F_Keytype,Read_I_Keytype,Read_J_Keytype,Read_R_Keytype,         &
             Read_S_Keytype,Read_V_Keytype,Set_Keytypes_On_File, Read_Powder_Numor,              &
             Read_SXTAL_Numor, Read_Numor_Generic, Read_Numor_D1B, Read_Numor_D20,Read_Numor_D9, &
             Read_Numor_D16, Read_Numor_D19, Write_POWDER_Numor,                                 &
             Write_HeaderInfo_POWDER_Numor, Read_Numor_D2B,                                      &
             Allocate_SXTAL_numors, Allocate_Powder_Numors, Read_Numor_D1A, Read_Numor_D4,       &
             Read_Numor_D10, Init_Powder_Numor,  Read_Calibration_File_D1A,                      &
             Read_Calibration_File_D2B, Read_Calibration_File_D4, Adding_Numors_D1A_DiffPattern, &
             Adding_Numors_D20S_DiffPattern,Adding_Numors_D4_DiffPattern, Adding_Numors_D1B_D20, &
             NumorD1BD20_To_DiffPattern


   !---- Definitions ----!

   !!----
   !!---- TYPE :: BASIC_NUMC_TYPE
   !!--..
   !!---- Type, public :: Basic_NumC_Type
   !!----    integer                                      :: N        ! Number of elements in this Type
   !!----    character(len=40), dimension(:), allocatable :: NameVar  ! Name of the diferents fields
   !!----    character(len=80), dimension(:), allocatable :: CValues  ! Character Values
   !!---- End Type Basic_NumC_Type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: Basic_NumC_Type
      integer                                      :: N
      character(len=40), dimension(:), allocatable :: NameVar
      character(len=80), dimension(:), allocatable :: CValues
   End Type Basic_NumC_Type

   !!----
   !!---- TYPE :: BASIC_NUMI_TYPE
   !!--..
   !!---- Type, public :: Basic_NumI_Type
   !!----    integer                                      :: N        ! Number of elements in this Type
   !!----    character(len=40), dimension(:), allocatable :: NameVar  ! Name of the diferents fields
   !!----    integer,           dimension(:), allocatable :: IValues  ! Integer values
   !!---- End Type, private :: Basic_NumI_Type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: Basic_NumI_Type
      integer                                      :: N
      character(len=40), dimension(:), allocatable :: NameVar
      integer,           dimension(:), allocatable :: IValues
   End Type Basic_NumI_Type

   !!----
   !!---- TYPE :: BASIC_NUMR_TYPE
   !!--..
   !!---- Type, public :: Basic_NumR_Type
   !!----    integer                                      :: N        ! Number of elements in this Type
   !!----    character(len=40), dimension(:), allocatable :: NameVar  ! Name of the diferents fields
   !!----    real(kind=cp),     dimension(:), allocatable :: RValues  ! Real Values
   !!---- End Type Basic_NumR_Type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: Basic_NumR_Type
      integer                                      :: N
      character(len=40), dimension(:), allocatable :: NameVar
      real(kind=cp),     dimension(:), allocatable :: RValues
   End Type Basic_NumR_Type

   !!----
   !!---- TYPE :: CALIBRATION_DETECTOR_TYPE
   !!--..
   !!---- Type, public :: Calibration_Detector_Type
   !!----   character(len=12)                            :: Name_Instrm  ! Instrument Name
   !!----   integer                                      :: NDet         ! Number of Detectors
   !!----   integer                                      :: NPointsDet   ! Number of Points by Detector
   !!----   logical                                      :: Pos_read     ! True if PosX have been allocated and read
   !!----   logical                                      :: True_Eff     ! True if the content of Effic are really efficiencies, otherwise they are Inv_Eff
   !!----   real(kind=cp), dimension(:),   allocatable   :: PosX         ! Relative Positions of each Detector
   !!----   real(kind=cp), dimension(:,:), allocatable   :: Effic        ! Efficiency of each point detector (NpointsDetect,NDect)
   !!----   real(kind=cp), dimension(:),   allocatable   :: sPosX        ! Standard deviation of Relative Positions for each Detector
   !!----   real(kind=cp), dimension(:,:), allocatable   :: sEffic       ! Standard deviation of Efficiency for each point detector (NpointsDetect,NDect)
   !!----   logical,       dimension(:,:), allocatable   :: Active       ! Flag for active detector or not
   !!---- End Type Calibration_Detector_Type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: Calibration_Detector_Type
      character(len=12)                            :: Name_Instrm  ! Instrument Name
      integer                                      :: NDet         ! Number of Detectors
      integer                                      :: NPointsDet   ! Number of Points per Detector
      logical                                      :: Pos_read     ! True if PosX have been allocated and read
      logical                                      :: True_Eff     ! True if the content of Effic are really efficiencies, otherwise they are Inv_Eff
      real(kind=cp), dimension(:),   allocatable   :: PosX         ! Relative Positions of each Detector
      real(kind=cp), dimension(:,:), allocatable   :: Effic        ! Efficiency of each point detector (NpointsDetect,NDect)
      real(kind=cp), dimension(:),   allocatable   :: sPosX        ! Standard deviation of Relative Positions for each Detector
      real(kind=cp), dimension(:,:), allocatable   :: sEffic       ! Standard deviation of Efficiency for each point detector (NpointsDetect,NDect)
      logical,       dimension(:,:), allocatable   :: Active       ! Flag for active points on detector
   End Type Calibration_Detector_Type

   !!----
   !!---- TYPE :: DIFFRACTOMETER_TYPE
   !!--..
   !!---- Type, public :: diffractometer_type
   !!----    character(len=80)                         :: info                 !information about the instrument
   !!----    character(len=12)                         :: name_inst            !Short name of the instrument
   !!----    character(len=15)                         :: geom                 !"Eulerian_4C","Kappa_4C","Lifting_arm","Powder","Laue"
   !!----    character(len=6)                          :: BL_frame             !Kind of BL-frame: "z-up" or "z-down"
   !!----    character(len=4)                          :: dist_units           !distance units: "mm  ","cm  ","inch"
   !!----    character(len=3)                          :: r_ord                ! xyz, yxz, ... Order of detector tilt calculations: xyz means Rx Ry Rz,
   !!----                                                                      ! so the rotation around z is the first applied, then around y and finally
   !!----                                                                      ! around x.
   !!----    character(len=4)                          :: angl_units           !angle units: "rad","deg"
   !!----    character(len=30)                         :: detector_type        !"Point","Flat_rect","Cylin_ImPlate","Tube_PSD", Put ipsd=1,2,...
   !!----    real(kind=cp)                             :: dist_samp_detector   ! dist. to centre for: point, Flat_rect, Tube_PSD; radius for: Cylin_ImPlate
   !!----    real(kind=cp)                             :: wave                 !Current wavelength
   !!----    real(kind=cp)                             :: wave_min,wave_max    !Minimum and maximum wavelengths (Laue diffractometers)
   !!----    real(kind=cp)                             :: vert                 !Vertical dimension
   !!----    real(kind=cp)                             :: horiz                !Horizontal dimension
   !!----    real(kind=cp)                             :: agap                 !gap between anodes
   !!----    real(kind=cp)                             :: cgap                 !gap between cathodes
   !!----    integer                                   :: np_vert              !number of pixels in vertical direction
   !!----    integer                                   :: np_horiz             !number of pixels in horizontal direction
   !!----    logical                                   :: tilted               ! True if RD /= I
   !!----    real(kind=cp), dimension(3,3)             :: RD                   ! Rotation matrix giving the orientation of the D-frame w.r.t. the L-frame
                                                                               ! RD= Rx(tiltz_d) Ry(tilty_d) Rz(tiltz_d)  or with the permutation given by r_order
   !!----    real(kind=cp)                             :: ga_d,nu_d            ! Angles gamm and nu of the detector centre
   !!----    real(kind=cp)                             :: tiltx_d=0.0          ! Tilt of the flat detector around x-axis (normally = nu_d)
   !!----    real(kind=cp)                             :: tilty_d=0.0          ! Tilt of the flat detector around y-axis (normally = 0)
   !!----    real(kind=cp)                             :: tiltz_d=0.0          ! Tilt of the flat detector around z-axis (normally = 0)
   !!----    integer                                   :: igeom                ! 1: Bisecting (PSI=0),2: Bisecting - HiCHI, 3: Normal beam, 4:Parallel (PSI=90)
   !!----    integer                                   :: ipsd                 ! 1: Flat,2: Vertically Curved detector (used in D19amd)
   !!----    real(kind=cp),dimension(3)                :: e1                   ! Components of e1 in {i,j,k}
   !!----    real(kind=cp),dimension(3)                :: e2                   ! Components of e2 in {i,j,k}
   !!----    real(kind=cp),dimension(3)                :: e3                   ! Components of e3 in {i,j,k}
   !!----    logical                                   :: gnxz_limited         ! The limits in gamm, nu, x and z accessible in the detector have been provided
   !!----    real(kind=cp)                             :: gap_min,gap_max      ! gamm minimum , gamm  maximun  (Positive values) | This change is needed due to odd
   !!----    real(kind=cp)                             :: gan_min,gan_max      ! gamm minimum , gamm  maximun  (Negative values) | positions of flat detectors.
   !!----    real(kind=cp)                             :: nu_min,nu_max
   !!----    real(kind=cp)                             :: d_min                !Resolution limit ... if not given d_min= 2/L_min
   !!----    real(kind=cp)                             :: x_min,x_max          !in mm
   !!----    real(kind=cp)                             :: z_min,z_max
   !!----    integer                                   :: num_ang              !Number of angular motors
   !!----    integer                                   :: num_ang              !Number of angular motors
   !!----    character(len=12),dimension(15)           :: ang_names            !Name of angular motors
   !!----    real(kind=cp),dimension(15,2)             :: ang_limits           !Angular limits (up to 15 angular motors)
   !!----    real(kind=cp),dimension(15)               :: ang_offsets          !Angular offsets
   !!----    real(kind=cp),dimension(15)               :: ang_velocity         !Angular velocity of each motor in degrees/second
   !!----    integer                                   :: num_disp             !Number of displacement motors
   !!----    character(len=12),dimension(10)           :: disp_names           !Name of displacement motors
   !!----    real(kind=cp),dimension(10,2)             :: disp_limits          !Displacement limits (up to 15 displacement motors)
   !!----    real(kind=cp),dimension(10)               :: disp_offsets         !Displacement offsets
   !!----    logical                                   :: displaced            ! True if det_offsets /= (0,0,0)
   !!----    real(kind=cp),dimension(3 )               :: det_offsets          !Offsets X,Y,Z of the detector centre
   !!----    logical                                   :: rangtim              !If .true. range_time has been set
   !!----    real(kind=cp),dimension(:,:,:),allocatable:: range_time           !Curves for calculating the velocitiy of angular motors (2,np,15)
   !!----    logical                                   :: alpha_correct        !True if alpha correction is to be applied
   !!----    real(kind=cp),dimension(:,:), allocatable :: alphas               !Efficiency corrections for each pixel
   !!----    logical                                   :: shift_correct        !True if shifts correction is to be applied
   !!----    real(kind=cp),dimension(:),   allocatable :: shifts               !Horizontal Shift correction in mm of the vertical tubes
   !!----    character(len=512)                        :: alpha_file           !Name of the alpha file
   !!----    logical                                   :: resol_given          !True if the resolution curve has been provided
   !!----    integer                                   :: nGa,nNu              !Dimensions of the matrix ReSurf
   !!----    real(kind=cp),dimension(:,:),  allocatable:: ReSurf               !Resolution surface in gamm-Nu space: ReSurf(Nu,Ga)
   !!----                                                                      !ReSurf(1,1): multiplicative factor to be applied to the widths
   !!----                                                                      !ReSurf(1,2:nGa): Values of detector gamms
   !!----                                                                      !ReSurf(2:nNu,1): Values of detector Nus
   !!----                                                                      !ReSurf(Nu,Ga)  : (Nu >1, Ga >1) Values of the expected FWHM in degrees
   !!---- End Type diffractometer_type
   !!----
   !!----    Definition for Diffractometer type
   !!----
   !!---- Update: April - 2008, February -2019
   !!
   Type, public :: diffractometer_type
      character(len=80)                          :: info                 !information about the instrument
      character(len=12)                          :: name_inst            !Short name of the instrument
      character(len=15)                          :: geom                 !"Eulerian_4C","Kappa_4C","Lifting_arm","Powder","Laue"
      character(len=6)                           :: BL_frame             !Kind of BL-frame: "z-up" or "z-down"
      character(len=4)                           :: dist_units           !distance units: "mm  ","cm  ","inch"
      character(len=3)                           :: r_ord                ! xyz, yxz, ... Order of detector tilt calculations: xyz means Rx Ry Rz
      character(len=4)                           :: angl_units           !angle units: "rad","deg"
      character(len=30)                          :: detector_type        !"Point","Flat_rect","Cylin_ImPlate","Tube_PSD", Put ipsd=1,2,...
      character(len=30)                          :: data_ordering        !Data ordering in data file: 'topleftcolmajor','toprightcolmajor','bottomleftcolmajor','bottomrightcolmajor','topleftrowmajor','toprightrowmajor','bottomleftrowmajor','bottomrightrowmajor'
      real(kind=cp)                              :: dist_samp_detector   ! dist. to centre for: point, Flat_rect, Tube_PSD; radius for: Cylin_ImPlate
      real(kind=cp)                              :: wave                 !Current wavelength
      real(kind=cp)                              :: wave_min,wave_max    !Minimum and maximum wavelengths (Laue diffractometers)
      real(kind=cp)                              :: vert                 !Vertical dimension
      real(kind=cp)                              :: horiz                !Horizontal dimension
      real(kind=cp)                              :: agap                 !gap between anodes
      real(kind=cp)                              :: cgap                 !gap between cathodes
      integer                                    :: np_vert              !number of pixels in vertical direction (anodes in d19)
      integer                                    :: np_horiz             !number of pixels in horizontal direction (cathodes in d19)
      logical                                    :: tilted               ! True if RD /= I
      real(kind=cp), dimension(3,3)              :: RD                   ! Rotation matrix giving the orientation of the D-frame w.r.t. the L-frame
                                                                         ! RD= Rx(tiltz_d) Ry(tilty_d) Rz(tiltz_d)  or with the permutation given by r_order
      real(kind=cp)                              :: ga_d,nu_d            ! Angles gamm and nu of the detector centre
      real(kind=cp)                              :: tiltx_d=0.0          ! Tilt of the flat detector around x-axis (normally = nu_d)
      real(kind=cp)                              :: tilty_d=0.0          ! Tilt of the flat detector around y-axis (normally = 0)
      real(kind=cp)                              :: tiltz_d=0.0          ! Tilt of the flat detector around z-axis (normally = 0)
      integer                                    :: igeom                ! 1: Bisecting (PSI=0),2: Bisecting - HiCHI, 3: Normal beam, 4:Parallel (PSI=90)
      integer                                    :: ipsd                 ! 1: Flat,2: Vertically Curved detector (used in D19amd), 3:
      real(kind=cp),dimension(3)                 :: e1                   ! Components of e1 in {i,j,k}
      real(kind=cp),dimension(3)                 :: e2                   ! Components of e2 in {i,j,k}
      real(kind=cp),dimension(3)                 :: e3                   ! Components of e3 in {i,j,k}
      logical                                    :: gnxz_limited         ! The limits in gamm, nu, x and z accessible in the detector have been provided
      real(kind=cp)                              :: gap_min,gap_max      ! gamm minimum , gamm  maximun  (Positive values) | This change is needed due to odd
      real(kind=cp)                              :: gan_min,gan_max      ! gamm minimum , gamm  maximun  (Negative values) | positions of flat detectors.
      real(kind=cp)                              :: nu_min,nu_max        ! minimum admissible nu  and maximum admissible nu
      real(kind=cp)                              :: d_min                ! Resolution limit ... if not given d_min= 2/L_min
      real(kind=cp)                              :: x_min,x_max          ! in mm
      real(kind=cp)                              :: z_min,z_max
      integer                                    :: num_ang              !Number of angular motors
      character(len=12),dimension(15)            :: ang_names            !Name of angular motors
      real(kind=cp),dimension(15,2)              :: ang_limits           !Angular limits (up to 15 angular motors)
      real(kind=cp),dimension(15)                :: ang_offsets          !Angular offsets
      real(kind=cp),dimension(15)                :: ang_velocity         !Angular velocity of each motor in degrees/second
      integer                                    :: num_disp             !Number of displacement motors
      character(len=12),dimension(10)            :: disp_names           !Name of displacement motors
      real(kind=cp),dimension(10,2)              :: disp_limits          !Displacement limits (up to 15 displacement motors)
      real(kind=cp),dimension(10)                :: disp_offsets         !Displacement offsets
      logical                                    :: displaced            ! True if det_offsets /= (0,0,0)
      real(kind=cp),dimension(3 )                :: det_offsets          !Offsets X,Y,Z of the detector centre (roD)
      logical                                    :: rangtim              !If .true. range_time has been set
      real(kind=cp),dimension(:,:,:),allocatable :: range_time           !Curves for calculating the velocitiy of angular motors (2,np,15)
      real(kind=cp),dimension(:,:),  allocatable :: alphas               !Efficiency corrections for each pixel
      real(kind=cp),dimension(:,:),  allocatable :: sigma_alphas         !Estimated error of Efficiency corrections for each pixel
      logical                                    :: shift_correct        !True if shifts correction is to be applied
      real(kind=cp),dimension(:),   allocatable  :: shifts               !Horizontal Shift corrections for vertical tube in mm
      logical                                    :: alpha_correct        !True if alpha correction is to be applied
      logical                                    :: sigma_alpha_correct  !True if the sigma of alpha correction have been provided
      character(len=512)                         :: alpha_file           !Name of the alpha file
      logical                                    :: resol_given          !True if the resolution curve has been provided
      integer                                    :: nGa,nNu              !Dimensions of the matrix ReSurf
      real(kind=cp),dimension(:,:),  allocatable :: ReSurf               !Resolution surface in gamm-Nu space: ReSurf(Nu,Ga)
                                                                         !ReSurf(1,1): multiplicative factor to be applied to the widths
                                                                         !ReSurf(1,2:nGa): Values of detector gamms
                                                                         !ReSurf(2:nNu,1): Values of detector Nus
                                                                         !ReSurf(Nu,Ga)  : (Nu >1, Ga >1) Values of the expected FWHM in degrees
   End Type diffractometer_type

   !!----
   !!---- TYPE :: GENERIC_NUMOR_TYPE
   !!--..
   !!---- Type, public :: Generic_Numor_type
   !!----    integer                                    :: Numor       ! Numor
   !!----    character(len=4)                           :: Instr       ! Instrument on ILL
   !!----    character(len=10)                          :: ExpName     ! Experimental Name
   !!----    character(len=20)                          :: Date        ! Date
   !!----    character(len=80)                          :: Title       ! Title
   !!----    type(basic_numc_type)                      :: SampleID    ! Sample Identification
   !!----    type(basic_numr_type)                      :: DiffOpt     ! Diffractometer Optics and Reactor Parameters
   !!----    type(basic_numr_type)                      :: MonMPar     ! Monochromator Motor Parameters
   !!----    type(basic_numr_type)                      :: DiffMPar    ! Diffractometer Motor Parameters
   !!----    type(basic_numr_type)                      :: DetPar      ! Detector Parameters
   !!----    type(basic_numi_type)                      :: DACFlags    ! Data Acquisition Control
   !!----    type(basic_numr_type)                      :: DACParam    ! Data Acquisition Parameters
   !!----    type(basic_numr_type)                      :: SampleSt    ! Sample status
   !!----    type(basic_numi_type)                      :: ICounts     ! Counts as Integers
   !!----    type(basic_numr_type)                      :: RCounts     ! Counts as Reals
   !!---- End Type Generic_Numor_Type
   !!----
   !!----    Definition for Generic Numor type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: Generic_Numor_type
      integer                                    :: Numor       ! Numor
      character(len=4)                           :: Instr       ! Instrument on ILL
      character(len=10)                          :: ExpName     ! Experimental Name
      character(len=20)                          :: Date        ! Date
      character(len=80)                          :: Title       ! Title
      type(basic_numc_type)                      :: SampleID    ! Sample Identification
      type(basic_numr_type)                      :: DiffOpt     ! Diffractometer Optics and Reactor Parameters
      type(basic_numr_type)                      :: MonMPar     ! Monochromator Motor Parameters
      type(basic_numr_type)                      :: DiffMPar    ! Diffractometer Motor Parameters
      type(basic_numr_type)                      :: DetPar      ! Detector Parameters
      type(basic_numi_type)                      :: DACFlags    ! Data Acquisition Control
      type(basic_numr_type)                      :: DACParam    ! Data Acquisition Parameters
      type(basic_numr_type)                      :: SampleSt    ! Sample status
      type(basic_numi_type)                      :: ICounts     ! Counts as Integers
      type(basic_numr_type)                      :: RCounts     ! Counts as Reals
   End Type Generic_Numor_Type

   !!----
   !!---- TYPE :: ILL_DATA_RECORD_TYPE
   !!--..
   !!---- Type, public :: ILL_data_record_type
   !!----    integer                     :: numor      ! data set numor.
   !!----    integer                     :: nset_prime ! set number (groups of 100000 numor).
   !!----    integer                     :: ntran      ! (key2) 0 or numcomp => data transferred?
   !!----    character(len=4)            :: inst_ch    ! instrument name (4 characters)
   !!----    character(len=22)           :: date_ch    ! measurement date (22 characters). !it was 18
   !!----    character(len=2)            :: fill_ch    ! 2 characters (key3) leader
   !!----    character(len=6)            :: user_ch    ! user name (6 characters)
   !!----    character(len=6)            :: lc_ch      ! local contact name (6 characters)
   !!----    character(len=72)           :: text_ch    ! commentary (72characters)
   !!----    character(len=8)            :: scan_motor ! principal scan motor name. (8 characters)
   !!----    integer                     :: nvers      !ival(1),  data version number
   !!----    integer                     :: ntype      !ival(2),  data type - single/multi/powder
   !!----    integer                     :: kctrl      !ival(3),  data function type
   !!----    integer                     :: manip      !ival(4),  principle scan angle
   !!----    integer                     :: nbang      !ival(5),  number of data saved
   !!----    integer                     :: nkmes      !ival(6),  pre-calculated number of points
   !!----    integer                     :: npdone     !ival(7),  actual number of points
   !!----    integer                     :: jcode      !ival(8),  count on monitor/time
   !!----    integer                     :: icalc      !ival(9),  angle calculation type
   !!----    integer                     :: ianal      !ival(10), analyser present (d10)
   !!----    integer                     :: imode      !ival(11), 2th motor sense (d10)
   !!----    integer                     :: itgv       !ival(12), d19/d9 fast measurement
   !!----    integer                     :: iregul     !ival(13), temperature monitor function
   !!----    integer                     :: ivolt      !ival(14), voltmeter function
   !!----    integer                     :: naxe       !ival(15), d10 (number of axes)
   !!----    integer                     :: npstart    !ival(16), point starting no frag. numor (d19/16)
   !!----    integer                     :: ilasti     !ival(17), elastic measurement (d10)
   !!----    integer                     :: isa        !ival(18), analyser motor sense (d10)
   !!----    integer                     :: flgkif     !ival(19), constant ki or kf (d10)
   !!----    integer                     :: ih_sqs     !ival(20), d10 sqs variation on h
   !!----    integer                     :: ik_sqs     !ival(21), d10 sqs variation on k
   !!----    integer                     :: nbsqs      !ival(22), d10 sqs slice number
   !!----    integer                     :: nb_cells   !ival(24), multi/powder data - number of detectors
   !!----    integer                     :: nfree1     !          data control (free).
   !!----    integer,dimension(11)       :: icdesc     !
   !!----    real(kind=cp), dimension(35):: valco      !rval( 1:35)
   !!----    real(kind=cp), dimension(10):: valdef     !rval(36:45)
   !!----    real(kind=cp), dimension(5) :: valenv     !rval(46:50)
   !!---- End Type ILL_data_record_type
   !!----
   !!----    Definition for Data Record type
   !!----
   !!---- Update: April - 2008
   !!
   Type, public :: ILL_data_record_type
      integer                     :: numor      ! data set numor.
      integer                     :: nset_prime ! set number (groups of 100000 numor).
      integer                     :: ntran      ! (key2) 0 or numcomp => data transferred?
      character(len=4)            :: inst_ch    ! instrument name (4 characters)
      character(len=22)           :: date_ch    ! measurement date (22 characters). !it was 18
      character(len=2)            :: fill_ch    ! 2 characters (key3) leader
      character(len=6)            :: user_ch    ! user name (6 characters)
      character(len=6)            :: lc_ch      ! local contact name (6 characters)
      character(len=72)           :: text_ch    ! commentary (72characters)
      character(len=8)            :: scan_motor ! principal scan motor name. (8 characters)
      integer                     :: nvers      !ival(1),  data version number
      integer                     :: ntype      !ival(2),  data type - single/multi/powder
      integer                     :: kctrl      !ival(3),  data function type
      integer                     :: manip      !ival(4),  principle scan angle
      integer                     :: nbang      !ival(5),  number of data saved
      integer                     :: nkmes      !ival(6),  pre-calculated number of points
      integer                     :: npdone     !ival(7),  actual number of points
      integer                     :: jcode      !ival(8),  count on monitor/time
      integer                     :: icalc      !ival(9),  angle calculation type
      integer                     :: ianal      !ival(10), analyser present (d10)
      integer                     :: imode      !ival(11), 2th motor sense (d10)
      integer                     :: itgv       !ival(12), d19/d9 fast measurement
      integer                     :: iregul     !ival(13), temperature monitor function
      integer                     :: ivolt      !ival(14), voltmeter function
      integer                     :: naxe       !ival(15), d10 (number of axes)
      integer                     :: npstart    !ival(16), point starting no frag. numor (d19/16)
      integer                     :: ilasti     !ival(17), elastic measurement (d10)
      integer                     :: isa        !ival(18), analyser motor sense (d10)
      integer                     :: flgkif     !ival(19), constant ki or kf (d10)
      integer                     :: ih_sqs     !ival(20), d10 sqs variation on h
      integer                     :: ik_sqs     !ival(21), d10 sqs variation on k
      integer                     :: nbsqs      !ival(22), d10 sqs slice number
      integer                     :: nb_cells   !ival(24), multi/powder data - number of detectors
      integer                     :: nfree1     !          data control (free).
      integer,dimension(11)       :: icdesc     !
      real(kind=cp), dimension(35):: valco      !rval( 1:35)
      real(kind=cp), dimension(10):: valdef     !rval(36:45)
      real(kind=cp), dimension(5) :: valenv     !rval(46:50)
   End Type ILL_data_record_type

   !!----
   !!---- TYPE :: POWDER_NUMOR_TYPE
   !!--..
   !!---- Type, public :: POWDER_Numor_type
   !!----    integer                                    :: numor       ! Numor
   !!----    integer                                    :: manip       ! principle scan angle
   !!----    integer                                    :: icalc       ! angle calculation type
   !!----    character(len=32)                          :: header      ! User, local contact, date
   !!----    character(len=12)                          :: Instrm      ! Instrument name
   !!----    character(len=32)                          :: title       !
   !!----    character(len=8)                           :: Scantype    ! omega, phi, etc...
   !!----    real(kind=cp), dimension(5)                :: angles      ! Angles: phi, chi, omega, 2theta(gamm), psi
   !!----    real(kind=cp), dimension(3)                :: scans       ! scan start, scan step, scan width
   !!----    real(kind=cp)                              :: monitor     ! Average monitor Sum(Monitors)/nframes
   !!----    real(kind=cp)                              :: time        ! Total time: sum times of each frame
   !!----    real(kind=cp)                              :: wave        ! wavelength
   !!----    real(kind=cp), dimension(5)                :: conditions  ! Temp-s.pt,Temp-Regul,Temp-sample,Voltmeter,Mag.field
   !!----    integer                                    :: nbdata      ! Total number of pixels nx*ny = np_vert*np_horiz
   !!----    integer                                    :: nframes     ! Total number of frames
   !!----    integer                                    :: nbang       ! Total number of angles moved during scan
   !!----    integer, dimension(11)                     :: icdesc      ! Integer values
   !!----    real(kind=cp),allocatable,dimension(:,:)   :: tmc_ang     ! time,monitor,total counts, angles*1000
   !!----                                                              ! To be allocated as tmc_ang(nbang,nframes)
   !!----    real(kind=cp),allocatable,dimension(:,:)   :: counts      ! Counts array to be reshaped (np_vert,np_horiz,nframes) in case of 2D detectors
   !!----                                                              ! To be allocated as counts(nbdata,nframes)
   !!---- End Type POWDER_Numor_type
   !!----
   !!----    Definition for POWDER Numor type
   !!----
   !!---- Update: April - 2009
   !!
   Type, public :: POWDER_Numor_type
      integer                                    :: numor       ! Numor
      integer                                    :: manip       ! principle scan angle
      integer                                    :: icalc       ! angle calculation type
      character(len=32)                          :: header      ! User, local contact, date
      character(len=12)                          :: Instrm      ! Instrument name
      character(len=32)                          :: title       !
      character(len=8)                           :: Scantype    ! omega, phi, etc...
      real(kind=cp), dimension(5)                :: angles      ! Angles: phi, chi, omega, 2theta(gamm), psi
      real(kind=cp), dimension(3)                :: scans       ! scan start, scan step, scan width
      real(kind=cp)                              :: monitor     ! Average monitor Sum(Monitors)/nframes
      real(kind=cp)                              :: time        ! Total time: sum times of each frame
      real(kind=cp)                              :: wave        ! wavelength
      real(kind=cp), dimension(5)                :: conditions  ! Temp-s.pt,Temp-Regul,Temp-sample,Voltmeter,Mag.field
      integer                                    :: nbdata      ! Total number of pixels nx*ny = np_vert*np_horiz
      integer                                    :: nframes     ! Total number of frames
      integer                                    :: nbang       ! Total number of angles moved during scan
      integer, dimension(11)                     :: icdesc      ! Integer values
      real(kind=cp),allocatable,dimension(:,:)   :: tmc_ang     ! time,monitor,total counts, angles*1000
                                                                ! To be allocated as tmc_ang(nbang,nframes)
      real(kind=cp),allocatable,dimension(:,:)   :: counts      ! Counts array to be reshaped (np_vert,np_horiz,nframes) in case of 2D detectors
                                                                ! To be allocated as counts(nbdata,nframes)
   End type POWDER_Numor_type

   !!----
   !!---- TYPE :: SXTAL_NUMOR_TYPE
   !!--..
   !!---- Type, public :: SXTAL_Numor_type
   !!----    integer                                    :: numor       ! Numor
   !!----    integer                                    :: manip       ! principle scan angle
   !!----    integer                                    :: icalc       ! angle calculation type
   !!----    character(len=32)                          :: header      ! User, local contact, date
   !!----    character(len=12)                          :: Instrm      ! Instrument name
   !!----    character(len=32)                          :: title       !
   !!----    character(len=8)                           :: Scantype    ! omega, phi, etc...
   !!----    real(kind=cp), dimension(3)                :: hmin        ! or h,k,l for omega-scans
   !!----    real(kind=cp), dimension(3)                :: hmax        !
   !!----    real(kind=cp), dimension(5)                :: angles      ! Angles: phi, chi, omega, 2theta(gamm), psi
   !!----    real(kind=cp), dimension(3,3)              :: UB          ! UB-matrix
   !!----    real(kind=cp), dimension(3)                :: dh          ! delta_h, delta_k, delta_l
   !!----    real(kind=cp), dimension(3)                :: scans       ! scan start, scan step, scan width
   !!----    real(kind=cp)                              :: preset      !
   !!----    real(kind=cp)                              :: wave        ! wavelength
   !!----    real(kind=cp)                              :: dist        ! wavelength
   !!----    real(kind=cp)                              :: cpl_fact    ! Coupling Factor
   !!----    real(kind=cp), dimension(5)                :: conditions  ! Temp-s.pt,Temp-Regul,Temp-sample,Voltmeter,Mag.field
   !!----    integer                                    :: nbdata      ! Total number of pixels nx*ny = np_vert*np_horiz
   !!----    integer                                    :: nframes     ! Total number of frames
   !!----    integer                                    :: nbang       ! Total number of angles moved during scan
   !!----    integer, dimension(11)                     :: icdesc      ! Integer values
   !!----    real(kind=cp),allocatable,dimension(:,:)   :: tmc_ang     ! time,monitor,total counts, angles*1000
   !!----                                                              ! To be allocated as tmc_ang(nbang,nframes)
   !!----    real(kind=cp),allocatable,dimension(:,:)   :: counts      ! Counts array to be reshaped (np_vert,np_horiz,nframes) in case of 2D detectors
   !!----                                                              ! To be allocated as counts(nbdata,nframes)
   !!---- End Type SXTAL_Numor_type
   !!----
   !!----    Definition for XTAL Numor type
   !!----
   !!---- Update: April - 2008
   !!
   Type, public :: SXTAL_Numor_type
      character(len=512)                         :: filename        ! The numor filename
      integer                                    :: numor           ! Numor
      integer                                    :: manip           ! principle scan angle
      integer                                    :: icalc           ! angle calculation type
      character(len=32)                          :: header          ! User, local contact, date
      character(len=12)                          :: Instrm          ! Instrument name
      character(len=60)                          :: title           ! The title of the experiment
      character(len=8)                           :: Scantype        ! omega, phi, etc...
      real(kind=cp), dimension(3)                :: hmin            ! The hkls min
      real(kind=cp), dimension(3)                :: hmax            ! The hkls max
      real(kind=cp), dimension(5)                :: angles          ! Angles: phi, chi, omega, 2theta(gamm), psi
      real(kind=cp), dimension(3,3)              :: UB              ! UB-matrix
      real(kind=cp), dimension(3)                :: dh              ! delta_h, delta_k, delta_l
      real(kind=cp), dimension(3)                :: scans           ! scan start, scan step, scan width
      real(kind=cp)                              :: preset          !
      real(kind=cp)                              :: wave            ! wavelength
      real(kind=cp)                              :: dist            ! distance sample-detector (it may be different from that of the instrument file)
      real(kind=cp)                              :: cpl_fact        ! Coupling Factor
      real(kind=cp), dimension(5)                :: conditions      ! Temp-s.pt,Temp-Regul,Temp-sample,Voltmeter,Mag.field
      integer                                    :: nbdata          ! Total number of pixels nx*ny = np_vert*np_horiz
      integer                                    :: nframes         ! Total number of frames
      integer                                    :: nbang           ! Total number of angles moved during scan
      integer, dimension(11)                     :: icdesc          ! Integer values
      integer                                    :: header_size     ! The number of lines of the header block
      integer                                    :: frame_size      ! The number of lines of a frame block
      integer, dimension(:), allocatable         :: selected_frames ! The frames that will be stored in the structure.
      real(kind=cp),allocatable,dimension(:,:)   :: tmc_ang         ! time,monitor,total counts, angles*1000
                                                                    ! To be allocated as tmc_ang(nbang,nframes)
      real(kind=cp),allocatable,dimension(:,:)   :: counts          ! Counts array to be reshaped (np_vert,np_horiz,nframes) in case of 2D detectors
                                                                    ! To be allocated as counts(nbdata,nframes)
   End type SXTAL_Numor_type

   !!----
   !!---- TYPE :: SXTAL_ORIENT_TYPE
   !!--..
   !!---- Type, public :: SXTAL_Orient_type
   !!----    logical                      :: orient_set
   !!----    real(kind=cp)                :: wave       ! Wavelength (in Laue machines any value between Lambda_min and Lambda_max)
   !!----    real(kind=cp),dimension(3,3) :: UB         ! UB matrix in Busing-Levy setting
   !!----    real(kind=cp),dimension(3,3) :: UBINV      ! Inverse of UB-matrix
   !!----    real(kind=cp),dimension(3,3) :: CONV       ! Conversion matrix to the local setting
   !!---- End Type SXTAL_Orient_type
   !!----
   !!----    Definition for XTAL Orientation Parameters
   !!----
   !!---- Created: April - 2007
   !!---- Updated: April - 2012
   !!
   Type, public :: SXTAL_Orient_type
      logical                      :: orient_set=.false.
      real(kind=cp)                :: wave       !Wavelength (in Laue machines any value between Lambda_min and Lambda_max)
      real(kind=cp),dimension(3,3) :: UB         !UB matrix in Busing-Levy setting
      real(kind=cp),dimension(3,3) :: UBINV      !Inverse of UB-matrix
      real(kind=cp),dimension(3,3) :: CONV       !Conversion matrix to the local setting
   End type SXTAL_Orient_type

   !!----
   !!---- CURRENT_INSTRM
   !!----    type(diffractometer_type), public :: Current_Instrm
   !!----
   !!----    Define a Current Instrument varuable
   !!----
   !!---- Update: April - 2008
   !!
   type(diffractometer_type), public :: Current_Instrm

   !!--++
   !!--++ CURRENT_INSTRM_SET
   !!--++    logical, private :: Current_Instrm_set
   !!--++
   !!--++    set to .true. if the Current_Instrm has been set up by calling
   !!--++    to Read_Current_Instrm
   !!--++
   !!--++ Update: April - 2008
   !!
   logical, private :: Current_Instrm_set=.false.

   !!----
   !!---- CURRENT_ORIENT
   !!----    type(SXTAL_Orient_type), public, save :: Current_Orient
   !!----
   !!----    Define the Current Orientation variable
   !!----    from the instrument
   !!----
   !!---- Update: April - 2008
   !!
   type(SXTAL_Orient_type), public, save :: Current_Orient

   !!----
   !!---- CYCLE_NUMBER
   !!----    integer, public :: cycle_number
   !!----
   !!----    Value to give the cycle number of Reactor at ILL
   !!----
   !!---- Update: April - 2008
   !!
   integer, public  ::  cycle_number

   !!--++
   !!--++ GOT_ILL_DATA_DIRECTORY
   !!--++    logical, private:: got_ILL_data_directory
   !!--++
   !!--++    Logical variable taking the value .true. if the instrument directory
   !!--++    has been obtained from subroutine Initialize_Data_Directory
   !!--++
   !!--++ Update: March - 2009
   !!
   logical, private :: got_ILL_data_directory = .false.

   !!--++
   !!--++ GOT_ILL_TEMP_DIRECTORY
   !!--++    logical, private:: got_ILL_temp_directory
   !!--++
   !!--++    Logical variable taking the value .true. if the instrument directory
   !!--++    for temporary uncompressed data has been obtained from subroutine
   !!--++    Initialize_Data_Directory
   !!--++
   !!--++ Update: March - 2009
   !!
   logical, private :: got_ILL_temp_directory = .false.

   !!----
   !!---- ILL_DATA_DIRECTORY
   !!----    character(len=512), public :: ILL_data_directory
   !!----
   !!----    String containing information about the data directory for ILL
   !!----    Initialised (automatic save attribute) for Windows case but set
   !!----    in subroutine: Initialize_Data_Directory
   !!----
   !!---- Update: March - 2009
   !!
   character(len=512), public  ::  ILL_Data_Directory = "c:\diffraction_Windows\illdata\"

   !!----
   !!---- ILL_TEMP_DIRECTORY
   !!----    character(len=512), private :: ILL_temp_directory
   !!----
   !!----
   !!----    String containing information about the data directory for ILL
   !!----    Initialised (automatic save attribute) for Windows case but set
   !!----    In subroutine: Initialize_Data_Directory
   !!----
   !!---- Update: 10/03/2011
   !!
   character(len=512), public ::  ILL_Temp_directory = "c:\diffraction_Windows\illdata\"

   !!--++
   !!--++ IPOINT
   !!--++    integer, dimension(:), allocatable, private :: ipoint
   !!--++
   !!--++    (Private)
   !!--++    Integer vector used to reorder arrays
   !!--++
   !!--++ Update: September - 2014
   !!
   integer, dimension(:), allocatable, private :: ipoint

   !!--++
   !!--++ IVALUES
   !!--++    integer, dimension(:), allocatable, private :: ivalues
   !!--++
   !!--++    (Private)
   !!--++    Integer vector to read integer values on keytype blocks
   !!--++
   !!--++ Update: April - 2008
   !!
   integer, dimension(:), allocatable, private :: ivalues

   !!----
   !!---- INSTRM_DIRECTORY
   !!----    character(len=512), public :: Instrm_directory
   !!----
   !!----    String containing information about the data directory for specific
   !!----    instrument
   !!----
   !!---- Update: April - 2008
   !!
   character(len=512), public  :: Instrm_Directory = " "

   !!--++
   !!--++ INSTRM_DIRECTORY_SET
   !!--++    logical, private:: Instrm_directory_set
   !!--++
   !!--++    logical variable taking the value .true. if set the instrument directory
   !!--++    in correct form
   !!--++
   !!--++ Update: April - 2008
   !!
   logical, private :: Instrm_Directory_Set=.false.
   !!----
   !!---- INSTRM_GEOMETRY_DIRECTORY
   !!----    character(len=512), public :: Instrm_Geometry_directory
   !!----
   !!----    String containing information about the directory containing .geom files
   !!----    for specific instrument
   !!----
   !!---- Update: July - 2010
   !!
   character(len=512), public  :: Instrm_Geometry_Directory = " "

   !!--++
   !!--++ INSTRM_GEOMETRY_DIRECTORY_SET
   !!--++    logical, private:: Instrm_Geometry_Directory_Set
   !!--++
   !!--++    logical variable taking the value .true. if set the instrument geometry directory
   !!--++    in correct form
   !!--++
   !!--++ Update: April - 2010
   !!
   logical, private :: Instrm_Geometry_Directory_Set=.false.

   !!--++
   !!--++ INSTRM_INFO_ONLY
   !!--++    logical, private:: Instrm_Info_Only
   !!--++
   !!--++    logical variable taking the value .true. if when reading a numor
   !!--++    only the header information is needed
   !!--++
   !!--++ Update: April - 2011
   !!
   logical, private :: Instrm_Info_Only=.false.

   !!----
   !!---- MACHINE_NAME
   !!----    character(len=12), public :: machine_name
   !!----
   !!----    String containing information about the Instrument name
   !!----
   !!---- Updates: April - 2008, October 2012
   !!
   character(len=12),   public  ::  machine_name

   !!--++
   !!--++ N_KEYTYPES
   !!--++    Integer, dimension(7), private :: N_KEYTYPES
   !!--++
   !!--++    Integer containing the number of keytypes into the numor
   !!--++    according to:
   !!--++       Index   1    2    3    4    5    6    7
   !!--++     KeyType   R    A    S    F    I    J    V
   !!--++
   !!--++ Update: March - 2009
   !!
   integer, dimension(7), private :: n_keytypes

   !!--++
   !!--++ NL_KEYTYPES
   !!--++    Integer, dimension(:,:,:), allocatable, private :: NL_KEYTYPES
   !!--++
   !!--++    Integer array where it is written the initial and final lines for each
   !!--++    keytype group.
   !!--++    Index 1: [1-7]
   !!--++    Index 2: Number of Blocks of this keytype
   !!--++    Index 3: [1,2], The first the beginning and the second the end
   !!--++
   !!--++ Update: March - 2009
   !!
   integer, dimension(:,:,:), allocatable, private :: nl_keytypes

   !!--++
   !!--++ NTEXT
   !!--++    Integer, private :: NTEXT
   !!--++
   !!--++    Integer containing the number of lines of Text for KeyType
   !!--++
   !!--++ Update: March - 2009
   !!
   integer, private :: ntext

   !!--++
   !!--++ NVAL_F
   !!--++    Integer, private :: NVAL_F
   !!--++
   !!--++    Integer containing the number of real values readed on keytype blocks
   !!--++
   !!--++ Update: March - 2009
   !!
   integer, private :: nval_f

   !!--++
   !!--++ NVAL_I
   !!--++    Integer, private :: NVAL_I
   !!--++
   !!--++    Integer containing the number of integer values readed on keytype blocks
   !!--++
   !!--++ Update: March - 2009
   !!
   integer, private :: nval_i

   !!--++
   !!--++ RVALUES
   !!--++    real(kind=cp), dimension(:), allocatable, private :: rvalues
   !!--++
   !!--++    (Private)
   !!--++    Vector to read real values on keytype blocks
   !!--++
   !!--++ Update: April - 2008
   !!
   real(kind=cp), dimension(:), allocatable, private :: rvalues

   !!--++
   !!--++ TEXT_ILL
   !!--++    character(len=80), dimension(:), allocatable, private :: text_ill
   !!--++
   !!--++    String containing the info from Numors blocks
   !!--++
   !!--++ Update: March - 2009
   !!
   character(len=80), dimension(:), allocatable, private :: text_ill

   !!--++
   !!--++ UNCOMPRESSCOMMAND
   !!--++    character(len=512), private :: uncompresscommand
   !!--++
   !!--++    String containing the command for uncompressing compressed data files
   !!--++
   !!--++ Update: March - 2009
   !!
   character(len=512), private  ::  uncompresscommand=' '

   !!----
   !!---- YEAR_ILLDATA
   !!----    Integer, public :: YEAR_ILLDATA
   !!----
   !!----    Integer containing the two last figures of the "year" needed to
   !!----    find datafiles
   !!----
   !!---- Update: March - 2009
   !!
   integer,            public  ::  year_illdata

   !!--++
   !!--++ SET_CALIBRATION_DETECTOR
   !!--++    logical, private:: Set_Calibration_Detector
   !!--++
   !!--++    Logical variable taking the value .true. if the Calibration Detector
   !!--++    is readed using the respective routine.
   !!--++
   !!--++ Update: 17/03/2011
   !!
   logical, private :: Set_Calibration_Detector = .false.

   !---- Interfaces - Overloaded ----!
   Interface  Allocate_Numors
      Module Procedure Allocate_Powder_Numors
      Module Procedure Allocate_SXTAL_Numors
   End Interface

   Interface  Initialize_Numor
      Module Procedure Init_Powder_Numor
      Module Procedure Init_SXTAL_Numor
   End Interface

   Interface  Read_Numor
      Module Procedure Read_Powder_Numor
      Module Procedure Read_SXTAL_Numor
   End Interface

   Interface  Write_Numor_Info
      Module Procedure Write_Powder_Numor
      Module Procedure Write_SXTAL_Numor
   End Interface

   Interface  Write_HeaderInfo_Numor
      Module Procedure Write_HeaderInfo_Powder_Numor
      Module Procedure Write_HeaderInfo_SXTAL_Numor
   End Interface

   Interface  Get_Counts
      Module Procedure Get_Counts_int
      Module Procedure Get_Counts_real
   End Interface

 Contains

    !!--++
    !!--++ Subroutine Adding_Numors_D1A_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal)
    !!--++    type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
    !!--++    integer,                                   intent(in)  :: N          ! Number of Numors
    !!--++    logical, dimension(:),                     intent(in)  :: ActList    ! Active List to considering if Add
    !!--++    type (DiffPat_E_Type),           intent(out) :: Pat        ! Pattern Diffraction
    !!--++    real(kind=cp),                   optional, intent(in)  :: VNorm      ! Normalization value
    !!--++    type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
    !!--++
    !!--++ Adding Numors from D1A Instrument and Passing to DiffPattern
    !!--++
    !!--++ Date: 25/03/2011
    !!
    Subroutine Adding_Numors_D1A_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal)
        !---- Arguments ----!
        type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
        integer,                                   intent(in)  :: N          ! Number of Numors
        logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
        type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
        real(kind=cp), optional,                   intent(in)  :: VNorm      ! Normalization value
        type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information

        !---- Local Variables ----!
        logical                             :: correction=.false.
        integer                             :: np         ! Ptos final
        integer                             :: i,j,k,kk,nn,nc,num
        integer                             :: ndet, npoints
        integer, dimension(:), allocatable  :: ind
        real(kind=cp), dimension(:,:,:), allocatable :: x,y,d2y
        real(kind=cp)   :: fac,x1,x2,xmin,xmax,step,yfc,cnorm,tim


        !> Init
        call clear_error()

        if (N <=0) then
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg=' Number of Numors in the List was zero!'
          return
        end if

        num=count(actlist .eqv. .true.)
        if (num <=0) then
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg=' Number of active Numors in the List was zero!'
          return
        end if

        if (present(Cal)) correction=.true.

        !> Checking dimensions
        ndet=PNumors(1)%nbdata
        npoints=0
        do i=1,n
           if (.not. actlist(i)) cycle
           npoints=max(npoints,PNumors(i)%nframes)
        end do

        !> Allocating vectors...
        if (allocated(x)) deallocate(x)
        allocate(x(npoints,ndet,num))
        if (allocated(y)) deallocate(y)
        allocate(y(npoints,ndet,num))
        if (allocated(ind)) deallocate(ind)
        allocate(ind(n))
        ind=0

        cnorm=1.0
        tim=0.0
        if (present(vnorm)) cnorm=vnorm

        !> X
        x=0.0
        num=0
        do i= 1, n
           if (.not. actlist(i) ) cycle
           num=num+1
           if (num == 1 .and. .not. present(vnorm)) cnorm=PNumors(i)%tmc_ang(2,1)  ! Monitor normalization value
           ind(num)=i                                                              ! Vector for index
           tim=tim+PNumors(i)%time
           np=PNumors(i)%nframes
           do k=1,np
              x(k,:,num)=PNumors(i)%tmc_ang(3,k)
           end do
           if (correction) x(1:np,:,num)=x(1:np,:,num)+Cal%posX(1)

           do k=1,np
              if (correction) then
                 do j=2,ndet
                    x(k,ndet-j+1,num)=x(k,ndet,num)+cal%posX(j)
                 end do
              else
                 do j=ndet-1,1,-1
                    x(k,j,num)=x(k,j+1,num) -6.0
                 end do
              end if
           end do
        end do

        !> Y
        y=0.0
        num=0
        do i= 1, n
           if (.not. actlist(i) ) cycle
           num=num+1

           np=PNumors(i)%nframes
           do k=1,np
              do j=1,ndet
                 y(k,j,num)=PNumors(i)%counts(ndet-j+1,k)
              end do
           end do

           if (correction) then
              do k=1,np
                 do j=1,ndet
                    y(k,j,num)=y(k,j,num)*cnorm / (PNumors(i)%tmc_ang(2,k)*cal%effic(1,ndet-j+1))   !Effic loaded in reverse mode
                 end do
              end do
           end if
        end do

        !> Passing for Interpolation procedure only the active points
        xmin=1.0E+10
        xmax=-1.0E-10
        do i=1,num
           do j=1,ndet
              if (correction) then
                 if (.not. Cal%Active(1,j)) cycle
              end if
              np=PNumors(ind(i))%nframes
              xmin=min(xmin,minval(x(1:np,j,i)))
              xmax=max(xmax,maxval(x(1:np,j,i)))
           end do
        end do

        !> Second Derivative
        if (allocated(d2y)) deallocate(d2y)
        allocate(d2y(npoints,ndet,num))
        d2y=0.0
        do i=1,num
           do j=1,ndet
              if (correction) then
                 if (.not. Cal%Active(1,j)) cycle
              end if
              np=PNumors(ind(i))%nframes
              d2y(1:np,j,i) = second_derivative(x(1:np,j,i),y(1:np,j,i),np)
           end do
        end do

        !> DiffPattern
        step=abs(PNumors(ind(1))%tmc_ang(3,1)-PNumors(ind(1))%tmc_ang(3,2))
        np=nint((xmax-xmin)/step)+1

        call Allocate_Pattern (Pat, np)

        Pat%Monitor=cnorm
        Pat%col_time=tim
        Pat%KindRad="neutrons_cw"
        Pat%ScatVar='2theta'
        Pat%instr='D1A'
        Pat%xmin=xmin
        Pat%xmax=xmax
        Pat%step=step

        !> First active Numor on the List
        i=ind(1)
        Pat%title=trim(PNumors(i)%title)//", Instr: "//trim(PNumors(i)%instrm)//", Header:"//trim(PNumors(i)%header)
        Pat%TSet=PNumors(i)%conditions(1)
        Pat%TSample=PNumors(i)%conditions(3)
        Pat%Wave(1)=PNumors(i)%wave

        Pat%x=0.0
        Pat%y=0.0
        Pat%sigma=0.0
        Pat%nd=0
        fac=1.0 !The calculation below may be wrong ... it is commented
        do nn=1,np
           Pat%x(nn)=xmin+(nn-1)*step
           nc=0

           do i=1,num   ! From Numors
              do j=1,ndet ! Detectors
                 if (correction) then
                    if (.not. Cal%Active(1,j)) cycle
                 end if
                 k=PNumors(ind(i))%nframes
                 x1=minval(x(1:k,j,i))
                 x2=maxval(x(1:k,j,i))
                 kk=locate(x(1:k,j,i),Pat%x(nn),k)     ! Using x and not xx. It is correct!!!!
                 if (kk == 0) cycle
                 if (kk > k) cycle

                 !> double ckeck!!!
                 if (Pat%x(nn) < x1 .or. Pat%x(nn) > x2) cycle

                 nc=nc+1
                 yfc = Spline_Interpol(Pat%x(nn),x(1:k,j,i),y(1:k,j,i),d2y(1:k,j,i),k)

                 Pat%y(nn)=Pat%y(nn)+ yfc
                 !fac=cnorm/Pnumors(ind(i))%tmc_ang(2,kk)
                 if (correction) fac= fac/abs(Cal%effic(1,ndet-j+1))
                 Pat%sigma(nn)=Pat%sigma(nn)+yfc * fac
              end do
           end do

           ! control
           if (nc > 0) then
              Pat%y(nn)=Pat%y(nn)/real(nc)
              Pat%sigma(nn)=abs(Pat%sigma(nn))/real(nc*nc)
              Pat%nd(nn)=nc
           else
              Pat%y(nn)=0.0
              Pat%sigma(nn)=1.0
              Pat%nd(nn)=0
           end if
        end do

        Pat%ymin=minval(Pat%y)
        Pat%ymax=maxval(Pat%y)

    End Subroutine Adding_Numors_D1A_DiffPattern

    !!----
    !!---- Subroutine Adding_Numors_D1B_D20(PNumors,N,ActList,Numor,Cal)
    !!----    type(Powder_Numor_Type),dimension(:),      intent(in) :: PNumors    ! Powder Numors Vector
    !!----    integer,                                   intent(in) :: N          ! Number of Numors
    !!----    logical,                dimension(:),      intent(in) :: ActList    ! Active List to considering if Add
    !!----    type (Powder_Numor_Type),                  intent(out):: Numor      ! Final Numor
    !!----    type(calibration_detector_type), optional, intent(in) :: Cal        ! Calibration Information
    !!----
    !!---- Adding Numors from D1B and D20 Instrument
    !!----
    !!---- 30/04/2011
    !!
    Subroutine Adding_Numors_D1B_D20(PNumors,N,ActList,Numor,Cal)
        !---- Arguments ----!
        type(Powder_Numor_Type),dimension(:),      intent(in) :: PNumors    ! Powder Numors Vector
        integer,                                   intent(in) :: N          ! Number of Numors
        logical,                dimension(:),      intent(in) :: ActList    ! Active List to considering if Add
        type (Powder_Numor_Type),                  intent(out):: Numor      ! Final Numor
        type(calibration_detector_type), optional, intent(in) :: Cal        ! Calibration Information
        !---- Local Variables ----!
        logical      :: adding, done
        integer      :: i,num
        real(kind=cp):: a,w,diff

        call clear_error()

        !> Init Numor
        call initialize_numor(numor)

        if (N <=0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Number of Numors in the List was zero!'
          return
        end if

        ! Check if all Numors can be added directly
        done=.false.
        adding=.true.
        do i=1,N
           if (.not. actlist(i)) cycle
           if (.not. done) then
              w=PNumors(i)%wave
              done=.true.
           end if
           if (done) then
              diff=abs(w-PNumors(i)%wave)
              if (diff > 0.1) then
                 adding=.false.
                 exit
              end if
           end if
        end do

        if (.not. adding) then
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg='Impossible to add the numors selected. Not all Numors have the same Wavelength!'
           return
        end if

        done=.false.
        adding=.true.
        do i=1,N
           if (.not. actlist(i)) cycle
           if (.not. done) then
              a=PNumors(i)%scans(1)
              done=.true.
           end if
           if (done) then
              diff=abs(a-PNumors(i)%scans(1))
              if (diff > 0.1) then
                 adding=.false.
                 exit
              end if
           end if
        end do

        if (.not. adding) then
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg='Impossible to add the numors selected. Not all Numors have the same initial Angle!'
           return
        end if

        !> Add Procedure
        num=0
        done=.false.
        do i=1,N
           if (.not. actlist(i)) cycle

           ! Initialize for the first active Numor to Add
           if (.not. done) then
              numor=PNumors(i)
              if (allocated(numor%counts)) deallocate(numor%counts)
              allocate(numor%counts(PNumors(i)%nbdata,PNumors(i)%nframes))
              numor%counts=0.0

              numor%monitor=0.0
              numor%time=0.0
              done=.true.
           end if

           ! Add
           num=num+1
           numor%monitor=numor%monitor+PNumors(i)%monitor
           numor%time=numor%time+PNumors(i)%time
           if(present(cal)) then
             numor%counts(:,1)=numor%counts(:,1)+PNumors(i)%counts(:,1)/cal%effic(1,:)
           else
             numor%counts(:,1)=numor%counts(:,1)+PNumors(i)%counts(:,1)
           end if
        end do

    End Subroutine Adding_Numors_D1B_D20

      !!----
      !!---- Subroutine Adding_Numors_D20S_DiffPattern(Mach,PNumors,N,ActList,Pat,VNorm,Cal,thres)
      !!----    type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
      !!----    integer,                                   intent(in)  :: N          ! Number of Numors
      !!----    logical, dimension(:),                     intent(in)  :: ActList    ! Active List to considering if Add
      !!----    type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
      !!----    real(kind=cp),                   optional, intent(in)  :: VNorm      ! Normalization value
      !!----    type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
      !!----    Real(kind=cp),                   optional, intent(in)  :: thres      ! threshold for comparisons
      !!----
      !!---- Adding Numors from D1A Instrument and Passing to DiffPattern
      !!----
      !!---- Date: 17/05/2024
      !!
      Subroutine Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal,thres)
         !---- Arguments ----!
         type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
         integer,                                   intent(in)  :: N          ! Number of Numors
         logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
         type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
         real(kind=cp), optional,                   intent(in)  :: VNorm      ! Normalization value
         type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
         Real(kind=cp),                   optional, intent(in)  :: thres      ! threshold for comparisons

         !---- Local Variables ----!
         logical                             :: correction=.false.
         integer                             :: np         ! Ptos final
         integer                             :: i,j,k,kk,nn,nc,num,m
         integer                             :: ndet, npoints
         integer,       dimension(1)         :: jmin,jmax
         integer, dimension(:), allocatable  :: ind
         real(kind=cp), dimension(:,:,:), allocatable :: x,y
         real(kind=cp), dimension(:),     allocatable :: yv,sigv
         logical,       dimension(:),     allocatable :: exclud
         real(kind=cp)   :: fac,x1,x2,xmin,xmax,step,yfc,cnorm,tim, aver, thr


         !> Init
         call clear_error()

         if (N <= 0) then
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            Err_CFML%Msg=' Number of Numors in the List was zero!'
         return
         end if

         num=count(actlist .eqv. .true.)
         if (num <= 0) then
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            Err_CFML%Msg=' Number of active Numors in the List was zero!'
         return
         end if

         if (present(Cal)) correction=.true.
         thr=3.0
         if (present(thres)) thr=thres

         !> Checking dimensions
         ndet=PNumors(1)%nbdata
         npoints=0
         do i=1,n
            if (.not. actlist(i)) cycle
            npoints=max(npoints,PNumors(i)%nframes)
         end do

         !> Allocating vectors...
         allocate(yv(ndet), sigv(ndet), exclud(ndet))
         if (allocated(x)) deallocate(x)
         allocate(x(npoints,ndet,num))
         if (allocated(y)) deallocate(y)
         allocate(y(npoints,ndet,num))
         if (allocated(ind)) deallocate(ind)
         allocate(ind(n))
         ind=0

         cnorm=1.0
         tim=0.0
         if (present(vnorm)) cnorm=vnorm

         !> X
         x=0.0
         num=0
         do i= 1, n
            if (.not. actlist(i) ) cycle
            num=num+1
            if (num == 1 .and. .not. present(vnorm)) cnorm=PNumors(i)%tmc_ang(2,1)  ! Monitor normalization value
            ind(num)=i                                                              ! Vector for index
            tim=tim+PNumors(i)%time
            np=PNumors(i)%nframes
            do k=1,np
               x(k,:,num)=PNumors(i)%tmc_ang(4,k)
            end do
            if (correction) x(1:np,:,num)=x(1:np,:,num)

            do k=1,np
               do j= 2 , ndet
                  x(k,j,num)=x(k,j,num)+(j-1)*0.05
               end do
            end do
         end do
         !> Y
         y=0.0
         num=0
         do i = 1, n
            if (.not. actlist(i) ) cycle
            num=num+1

            np=PNumors(i)%nframes
            do k=1,np
               do j=1,ndet
                  y(k,j,num)=PNumors(i)%counts(j,k)
               end do
            end do

            if (correction) then
               if(present(VNorm)) then
                  if(VNorm < 0.0) then
                     cnorm = abs(VNorm)/PNumors(i)%time
                     Pat%Norm_Mon=PNumors(i)%monitor*cnorm
                  else
                     Pat%Norm_Mon=VNorm
                     cnorm = VNorm/Pat%monitor
                  end if
               end if
               do k=1,np
                  do j=1,ndet
                     y(k,j,num) = y(k,j,num) * cnorm / (PNumors(i)%tmc_ang(2,k)*cal%effic(1,j))
                  end do
               end do
            end if
         end do

         !> Passing for Interpolation procedure only the active points
         xmin=1.0E+10
         xmax=-1.0E-10
         do i=1,num
            do j=1,ndet
               if (correction) then
                  if (.not. Cal%Active(1,j)) cycle
               end if
               np=PNumors(ind(i))%nframes
               xmin=min(xmin,minval(x(1:np,j,i)))
               xmax=max(xmax,maxval(x(1:np,j,i)))
            end do
         end do

         !> Second Derivative
         !if (allocated(d2y)) deallocate(d2y)
         !allocate(d2y(npoints,ndet,num))
         !d2y=0.0
         !do i=1,num
         !   do j=1,ndet
         !      if (correction) then
         !         if (.not. Cal%Active(1,j)) cycle
         !      end if
         !      np=PNumors(ind(i))%nframes
         !      d2y(1:np,j,i) = second_derivative(x(1:np,j,i),y(1:np,j,i),np)
         !   end do
         !end do

         !> DiffPattern
         step=0.05
         np=nint((xmax-xmin)/step)+1

         call Allocate_Pattern (Pat, np)

         Pat%Monitor=cnorm
         Pat%col_time=tim
         Pat%KindRad="neutrons_cw"
         Pat%ScatVar='2theta'
         Pat%instr="D20"
         Pat%xmin=xmin
         Pat%xmax=xmax
         Pat%step=step
         Pat%sigvar=.true.

         !> First active Numor on the List
         i=ind(1)
         Pat%title=trim(PNumors(i)%title)//", Instr: "//trim(PNumors(i)%instrm)//", Header:"//trim(PNumors(i)%header)
         Pat%TSet=PNumors(i)%conditions(1)
         Pat%TSample=PNumors(i)%conditions(3)
         Pat%Wave(1)=PNumors(i)%wave

         Pat%x=0.0
         Pat%y=0.0
         Pat%sigma=0.0
         Pat%nd=0
         fac=1.0
         do nn=1,np
            Pat%x(nn)=xmin+(nn-1)*step
            m=0
            yv=0.0; sigv=0.0
            do i=1,num   ! From Numors/frames
               do j=1,ndet ! Detectors/wires
                  if (correction) then
                     if (.not. Cal%Active(1,j)) cycle
                  end if
                  k=PNumors(ind(i))%nframes
                  x1=minval(x(1:k,j,i))
                  x2=maxval(x(1:k,j,i))
                  kk=locate(x(1:k,j,i),Pat%x(nn),k)
                  if (kk == 0) cycle
                  if (kk > k) cycle
                  !> double ckeck!!!
                  if (Pat%x(nn) < x1 .or. Pat%x(nn) > x2) cycle
                  m=m+1
                  !yv(m) = Spline_Interpol(Pat%x(nn),x(1:k,j,i),y(1:k,j,i),d2y(1:k,j,i),k)
                  yv(m) = y(kk,j,i)
                  fac=cnorm/Pnumors(ind(i))%tmc_ang(2,kk)
                  !fac=cnorm
                  if (correction) fac = fac/abs(Cal%effic(1,j))
                  Pat%sigma(nn)=Pat%sigma(nn) + yfc * fac
               end do
            end do
            exclud=.false.
            if( m > 2) then  !Eliminate outliers only if more thant 2 different counts contributes to the point
               jmin=minloc(yv(1:m))
               jmax=maxloc(yv(1:m))
               aver=0.0; nc=0
               do j=1,m
                  if( j == jmin(1) .or. j == jmax(1))  cycle
                  nc=nc+1
                  aver=aver+yv(j)
               end do
               if(nc > 0) then
                  aver=aver/nc
                     do j=1,m
                        if(abs((yv(j)-aver)) > thr * aver) exclud(j) = .true.
                     end do
                  end if
            end if
            nc=0
            do j=1,m   !Exclude outliers
               if(exclud(j)) cycle
               nc=nc+1
               Pat%y(nn)=Pat%y(nn)+yv(j)
            end do
            Pat%sigma(nn) = Pat%y(nn)
            ! control
            if (nc > 0) then
               Pat%sigma(nn)=sqrt(Pat%sigma(nn))/real(nc)
               Pat%y(nn)=Pat%y(nn)/real(nc)
               Pat%nd(nn)=nc
            else
               Pat%y(nn)=0.0
               Pat%sigma(nn)=1.0
               Pat%nd(nn)=0
            end if
         end do

         Pat%ymin=minval(Pat%y)
         Pat%ymax=maxval(Pat%y)

   End Subroutine Adding_Numors_D20S_DiffPattern


    !!--++
    !!--++ Subroutine Adding_Numors_D4_DiffPattern(PNumors,N,ActList,Pat,VNorm,Detect,Cal)
    !!--++    type(Powder_Numor_Type),dimension(:),      intent(in) :: PNumors    ! Powder Numors Vector
    !!--++    integer,                                   intent(in) :: N          ! Number of Numors
    !!--++    logical, dimension(:),                     intent(in) :: ActList    ! Active List to considering if Add
    !!--++    type (DiffPat_E_Type),           intent(out):: Pat        ! Pattern Diffraction
    !!--++    real(kind=cp),                   optional, intent(in) :: VNorm      ! Normalization value
    !!--++    integer,                         optional, intent(in) :: Detect     ! Selected Detector
    !!--++    type(calibration_detector_type), optional, intent(in) :: Cal        ! Calibration Information
    !!--++
    !!--++ Adding Numors from D4 Instrument and Passing to DiffPattern
    !!--++
    !!--++ 21/03/2011
    !!
    Subroutine Adding_Numors_D4_DiffPattern(PNumors,N,ActList,Pat,VNorm,Detect,Cal)
        !---- Arguments ----!
        type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
        integer,                                   intent(in)  :: N          ! Number of Numors
        logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
        type (DiffPat_E_Type),           intent(out) :: Pat        ! Pattern Diffraction
        real(kind=cp), optional,                   intent(in)  :: VNorm      ! Normalization value
        integer, optional,                         intent(in)  :: Detect     ! Save scan for a particular Detector
        type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information

        !---- Local Variables ----!
        logical                             :: correction=.false.
        integer, parameter                  :: ndet=9     ! Number of Detectors
        integer, parameter                  :: ncell=64   ! Number of Points by Detector
        integer                             :: np         ! Ptos final
        integer                             :: i,j,k,kk,nn,nc,num
        integer, dimension(:), allocatable  :: ncc,ind
        real(kind=cp), dimension(:,:,:), allocatable :: x,y,xx,yy,d2y
        real(kind=cp)  :: fac,x1,x2,xmin,xmax,step,yfc, cnorm, tim

        !> Init
        call clear_error()

        if (N <=0) then
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg=' Number of Numors in the List was zero!'
          return
        end if

        num=count(actlist .eqv. .true.)
        if (num <=0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Number of active Numors in the List was zero!'
          return
        end if

        if (present(Cal)) correction=.true.

        !> Allocating vectors...
        if (allocated(x)) deallocate(x)
        allocate(x(ncell,ndet,num))
        if (allocated(y)) deallocate(y)
        allocate(y(ncell,ndet,num))
        if (allocated(ind)) deallocate(ind)
        allocate(ind(n))
        ind=0
        cnorm=1.0
        if (present(vnorm)) cnorm=vnorm
        tim=0.0
        !> X
        x=0.0
        num=0
        do i= 1, n
           if (.not. actlist(i) ) cycle
           num=num+1
           if (num==1 .and. .not. present(vnorm)) cnorm=PNumors(i)%monitor       ! Monitor normalization value
           ind(num)=i                                                            ! Vector for index
           tim=tim+PNumors(i)%time
           do j=1,ndet
              fac=0.0
              if (correction) fac=Cal%PosX(j)
              x1=(j-1)*15.0 + PNumors(i)%tmc_ang(3,1) + fac
              do k=1,ncell
                 x(k,j,num)=x1 + (k-1)*0.125
              end do
           end do
        end do

        !> Y
        y=0.0
        num=0
        do i= 1, n
           if (.not. actlist(i) ) cycle
           num=num+1
           do j=1,ndet
              do k=1,ncell
                 fac=cnorm/Pnumors(i)%monitor
                 if (correction) fac=fac/abs(Cal%effic(k,j))
                 y(k,j,num)=PNumors(i)%counts(k,j)*fac
              end do
           end do
        end do

        !> Passing for Interpolation procedure only the active points
        if (allocated(ncc)) deallocate(ncc)
        allocate(ncc(ndet))
        ncc=ncell

        if (correction) then
           do j=1,ndet
              ncc(j)=count(Cal%Active(:,j) .eqv..true.)
           end do
        end if

        if (allocated(xx)) deallocate(xx)
        if (allocated(yy)) deallocate(yy)
        allocate(xx(ncell,ndet,num))
        allocate(yy(ncell,ndet,num))
        xx=0.0
        yy=0.0

        xmin=1.0E+10
        xmax=-1.0E-10
        do i=1,num
           do j=1,ndet
              kk=0
              do k=1,ncell
                 if (correction) then
                    if (.not. Cal%Active(k,j)) cycle
                 end if
                 kk=kk+1
                 xx(kk,j,i)=x(k,j,i)
                 yy(kk,j,i)=y(k,j,i)
                 xmin=min(xmin,xx(kk,j,i))
                 xmax=max(xmax,xx(kk,j,i))
              end do
           end do
        end do

        !> Second Derivative
        if (allocated(d2y)) deallocate(d2y)
        allocate(d2y(ncell,ndet,num))               ! Cell points, Detector, Number of Numors
        d2y=0.0
        do i=1,num
           do j=1,ndet
              d2y(1:ncc(j),j,i)=second_derivative(xx(1:ncc(j),j,i),yy(1:ncc(j),j,i),ncc(j))
           end do
        end do

        !> DiffPattern
        step=0.125
        np=nint((xmax-xmin)/step)+1
        call Allocate_Pattern (Pat, np)

        Pat%Monitor=cnorm
        Pat%col_time=tim
        Pat%KindRad="neutrons_cw"
        Pat%ScatVar='2theta'
        Pat%instr='D4'
        Pat%xmin=xmin
        Pat%xmax=xmax
        Pat%step=step

        !> First active Numor on the List
        i=ind(1)
        Pat%title=trim(PNumors(i)%title)//", Instr: "//trim(PNumors(i)%instrm)//", Header:"//trim(PNumors(i)%header)
        Pat%TSet=PNumors(i)%conditions(1)
        Pat%Tsample=PNumors(i)%conditions(3)
        Pat%wave(1)=PNumors(i)%wave

        Pat%x=0.0
        Pat%y=0.0
        Pat%sigma=0.0
        Pat%nd=0
        Pat%npts=np

        do nn=1,np
           Pat%x(nn)=xmin+(nn-1)*step
           nc=0

           do i=1,num   ! From Numors
              do j=1,ndet ! Detectors
                 if (present(detect)) then
                    if (j /= detect) cycle
                 end if
                 x1=minval(xx(1:ncc(j),j,i))
                 x2=maxval(xx(1:ncc(j),j,i))
                 k=locate(x(:,j,i),Pat%x(nn),ncell)     ! Using x and not xx. It is correct!!!!
                 if (k == 0) cycle
                 if (k > ncell) cycle
                 if (correction) then
                    if (.not. Cal%Active(k,j)) cycle
                 end if

                 !> double ckeck!!!
                 if (Pat%x(nn) < x1 .or. Pat%x(nn) > x2) cycle

                 nc=nc+1

                 yfc = Spline_Interpol(Pat%x(nn),xx(1:ncc(j),j,i),y(1:ncc(j),j,i),d2y(1:ncc(j),j,i),ncc(j))
                 Pat%y(nn)=Pat%y(nn)+ yfc
                 fac=cnorm/Pnumors(ind(i))%monitor
                 if (correction) fac= fac/abs(Cal%effic(k,j))
                 Pat%sigma(nn)=Pat%sigma(nn)+yfc * fac
              end do
           end do

           ! control
           if (nc > 0) then
              Pat%y(nn)=Pat%y(nn)/real(nc)
              Pat%sigma(nn)=abs(Pat%sigma(nn))/real(nc*nc)
              Pat%nd(nn)=nc
           else
              Pat%y(nn)=0.0
              Pat%sigma(nn)=1.0
              Pat%nd(nn)=0
           end if
        end do

        Pat%ymin=minval(Pat%y)
        Pat%ymax=maxval(Pat%y)

    End Subroutine Adding_Numors_D4_DiffPattern

    !!----
    !!---- Subroutine Allocate_POWDER_numors(num_max,ndata,num_ang,nframes,Num)
    !!----    integer,                                            intent(in)    :: num_max
    !!----    integer,                                            intent(in)    :: ndata
    !!----    integer,                                            intent(in)    :: num_ang
    !!----    integer,                                            intent(in)    :: nframes
    !!----    type(POWDER_Numor_type), allocatable, dimension(:), intent(in out):: Num
    !!----
    !!--<<    Subroutine allocating and initializing the array 'Num' of type
    !!----    Powder_Numor_type. The input arguments are:
    !!----        num_max : number of components of the array (dimension of Num)
    !!----        ndata   : number of pixels of a single frame
    !!----        num_ang : number of angles moved simultaneously during the scan
    !!----        nframes : number of frames (number of scan points)
    !!-->>
    !!---- Update: March - 2005
    !!
    Subroutine Allocate_POWDER_Numors(Num_Max,Ndata,Num_Ang,Nframes,Num)
       !---- Arguments ----!
       integer, intent(in)                                                :: num_max
       integer, intent(in)                                                :: ndata
       integer, intent(in)                                                :: num_ang
       integer, intent(in)                                                :: nframes
       type(Powder_Numor_type), allocatable, dimension(:), intent(in out) :: Num

       !---- Local variables ----!
       integer :: i

       if (allocated(Num)) deallocate(Num)
       allocate(Num(num_max))

       do i=1, num_max
          call init_powder_numor(Num(i),num_ang,ndata,nframes)
       end do

    End Subroutine Allocate_POWDER_Numors

    !!----
    !!---- Subroutine Allocate_SXTAL_Numors(Num_Max,Ndata,Num_Ang,Nframes,Num)
    !!----    integer,                                           intent(in)     :: num_max
    !!----    integer,                                           intent(in)     :: ndata
    !!----    integer,                                           intent(in)     :: num_ang
    !!----    integer,                                           intent(in)     :: nframes
    !!----    type(SXTAL_Numor_type), allocatable, dimension(:), intent(in out) :: Num
    !!----
    !!--<<    Subroutine allocating and initializing the array 'Num' of type SXTAL_Numor_type
    !!----    The input arguments are:
    !!----    num_max : number of components of the array (dimension of Num)
    !!----    ndata   : number of pixels of a single frame
    !!----    num_ang : number of angles moved simultaneously during the scan
    !!-->>    nframes : number of frames (number of scan points)
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Allocate_SXTAL_numors(num_max,ndata,num_ang,nframes,Num)
       !---- Arguments ----!
       integer,                                           intent(in)     :: num_max
       integer,                                           intent(in)     :: ndata
       integer,                                           intent(in)     :: num_ang
       integer,                                           intent(in)     :: nframes
       type(SXTAL_Numor_type), allocatable, dimension(:), intent(in out) :: Num

       !---- Local variables ----!
       integer :: i

       if(allocated(Num)) deallocate(Num)
       allocate(Num(num_max))

       do i=1, num_max
          call init_sxtal_numor(Num(i),num_ang,ndata,nframes)
       end do

    End Subroutine Allocate_SXTAL_Numors

    !!----
    !!---- Subroutine Define_Numor_Header_Frame_Size(filename,header_size,frame_size)
    !!----    character(len=*)    , intent(in)  :: filename    ! The input numor
    !!----    integer             , intent(out) :: header_size ! The number of lines of the header block
    !!----    integer             , intent(out) :: frame_size  ! The number of lines of the frame block
    !!----
    !!---- Routine that returns the number of lines of the header and frame blocks of a
    !!---- given numor.
    !!----
    !!---- Update:  June - 2012
    !!
    Subroutine Define_Numor_Header_Frame_Size(filename,header_size,frame_size)
       !---- Argument ----!
       character(len=*), intent(in)  :: filename
       integer         , intent(out) :: header_size
       integer         , intent(out) :: frame_size

       !---- Loval variables ----!
       character(len=80) :: line
       integer           :: ier, lun
       logical           :: info

       ! The error flags are initialized.
       call clear_error()

       ! The header and frame counters are initialized.
       header_size = 0
       frame_size  = 1

       ! Flag used for inquiring the input file.
       info=.false.

       ! Check first that the input file exists.
       inquire (file=filename,exist=info)

       ! If the input file does not exist, stop here.
       if (.not. info) then
           err_CFML%Ierr = 1
           Err_CFML%Flag=.true.
           Err_CFML%Msg = " The file "//trim(filename)//" does not exist."
           return
       end if

       ! Check whether the input file is opned or not.
       inquire (file=filename,opened=info)

       ! If the input file is opened, get its logical unit and rewind the file.
       if (info) then
          inquire(file=filename,number=lun)
          rewind(unit=lun)
       ! If the input file is not opened, open it.
       else
          open(newunit=lun,file=filename, status="old",action="read", position="rewind")
       end if

       ! Loop used to determine the number of lines of the header block.
       do
          ! Read a new line.
          read(lun,'(a)',iostat=ier) line
          ! If the line is a SSSSS line then it is the beginning of a frame block.
          if (line(1:10) == repeat('S',10) .or. ier < 0) exit
          ! Increment the header block counter.
          header_size = header_size + 1
       end do

       ! If the header block is empty, stops here. A numor must have a header.
       if (header_size == 0) then
           err_CFML%Flag = .true.
           Err_CFML%Msg = " "//trim(filename)//" has no header."
           return
       end if

       ! Loop used to determine the number of lines of the first frame block (all the frames have the same size).
       do
          ! Read a new line.
          read(lun,'(a)', iostat=ier) line

          ! If we are at the eof, the numor contains just one frame.
          if (ier < 0) exit

          ! If the line is a SSSSS line then it is the beginning of the next frame block.
          if (line(1:10) == repeat('S',10)) exit

          ! Increment the frame block counter.
          frame_size = frame_size + 1
       end do

       ! If the frame block is empty, stops here. A numor must have at least one frame.
       if (frame_size == 1) then
           err_CFML%Flag = .true.
           Err_CFML%Msg = " "//trim(filename)//" has no frame."
           return
       end if

       ! Rewind the opened numor.
       rewind(lun)

    End Subroutine Define_Numor_Header_Frame_Size

    !!----
    !!---- Subroutine Define_Uncompress_Program(ProgName)
    !!----    character(len=*), intent(in) :: ProgName
    !!----
    !!---- Routine that define the uncompress program that you wants to use
    !!----
    !!---- Update:  April - 2009
    !!
    Subroutine Define_Uncompress_Program(ProgName)
       !---- Argument ----!
       character(len=*), intent(in) :: ProgName

       uncompresscommand=ProgName

    End Subroutine Define_Uncompress_Program

    !!----
    !!---- Subroutine Get_Absolute_Data_Path(Numor,Instrm,Path,Iyear,Icycle, Actual_Path)
    !!----    integer,           intent(in)            :: numor
    !!----    character(len=*),  intent(in)            :: instrm
    !!----    character(len=*),  intent(out)           :: path
    !!----    integer, optional, intent(in)            :: iyear
    !!----    integer, optional, intent(in)            :: icycle
    !!----    character(len=*),  intent(out), optional :: Actual_Path
    !!----
    !!----    Finds the absolute path to any numor. The base directory
    !!----    is set by a call to 'initialize_data_directory'. The subroutine
    !!----    then searches for the numor in the following order:
    !!----    1. In the subdirectory defined by the year and cycle if passed
    !!----       as arguments to the subroutine (i.e args iyear, icycle).
    !!----    2. In the subdirectory defined by the year and cycle of the
    !!----       previous call to get_absolute_data_path (since numors are
    !!----       likely to be adjacent).
    !!----    3. In the 'data' subdirectory (since likely to process recent
    !!----       data).
    !!----    4. In the 'data-1' subdirectory (same logic as above)
    !!----    5. Working from the current year and most recent cycle and
    !!----       working back through cycles and year until the stopping
    !!----       at the first cycle of 1973, when the first data were archived
    !!----
    !!--..    Tries to find an uncompress numor first and then tries to find
    !!--..    a compressed numor (.Z extension). If found the numor is uncomp-
    !!--..    ressed in the a temporary directory if defined (see subroutine
    !!--..    'initialize_data_directory') or else into the current directory.
    !!--..
    !!--..    Nb At present no attempt is made to tidy up these uncompressed
    !!--..    numors, which could potentially litter the current directory.
    !!----
    !!---- Update: March - 2009
    !!
    Subroutine Get_Absolute_Data_Path(Numor, Instrm, Path, Iyear, Icycle, Actual_Path)

       !---- Arguments ----!
       integer,           intent(in)            :: numor
       character(len=*),  intent(in)            :: instrm
       character(len=*),  intent(out)           :: path
       integer, optional, intent(in)            :: iyear
       integer, optional, intent(in)            :: icycle
       character(len=*),  intent(out), optional :: Actual_Path

       !---- Local Variables ----!
       character(len=*),parameter :: Current_Data = 'data'
       character(len=*),parameter :: Previous_Data = 'data-1'
       character(len=*),parameter :: Extension = '.Z'

       character(len=6) :: numstr
       character(len=8) :: inst
       character(len=5) :: yearcycle
       character(len=7) :: subdir
       logical          :: exists

       ! Init value
       path=" "
       if (present(actual_path)) actual_path = path

       ! Some compilers are unable to test the existence of file: data_diretory//ops_sep//"." !!!!!
       ! so, got_ILL_data_directory may still be .false., ILL_data_directory has been set anyway
       ! in initialize_data_directory(), so let us continue and set got_ILL_data_directory=.true.
       ! if the associated numor file exist anywhere
       if (.not. got_ILL_data_directory) call initialize_data_directory()

       ! At this point the ILL Data and Temporal directories must be defined
       if (.not. got_ILL_Data_directory) return
       if (.not. got_ILL_Temp_Directory) return

       ! Uncompress program must be defined
       if (len_trim(uncompresscommand) == 0) then
          if (Ops == 1) then
             call define_uncompress_program('7z e -y -so')
          else
             call define_uncompress_program('gzip -q -d -c')
          end if
       end if

       ! Numor character
       write(unit=numstr,fmt='(i6.6)') numor

       ! Instrument
       inst=adjustl(instrm)
       inst= l_case(inst)

       ! Using Instrument Information
       select case(inst)
          case("d1b","d20","d9","d15","d19","d16")
             subdir = trim(inst)//"_"//numstr(2:2)//ops_sep

          case default ! d10, d3 etc don't divide numors up into sub directories
             subdir = ""
       end select

       if (present(iyear) .and. present(icycle)) then
          !> Using Year+Cycle
          write(unit=yearcycle,fmt="(i4.4,i1.1)") iyear,icycle
          yearcycle = yearcycle(len_trim(yearcycle)-2:len_trim(yearcycle))
          path = trim(ILL_data_directory)//trim(yearcycle)//ops_sep//trim(inst)//ops_sep//trim(subdir)//numstr
          if (present(actual_path)) actual_path = path
          inquire(file=trim(path),exist=exists)
          if (exists) return ! found numor so return

          !> Using previous + compressed data
          path = trim(path)//Extension
          inquire(file=trim(path),exist=exists)
          if (exists) then ! uncompress into temp directory
             call execute_command_line(trim(uncompresscommand)//' '//trim(path)//' > '//trim(ILL_temp_directory)//numstr)
             if (present(actual_path)) actual_path = path
             path = trim(ILL_temp_directory)//numstr
             return ! found numor so return
          end if
       end if

       !> Using Current_data
       path = trim(ILL_data_directory)//Current_Data//ops_sep//trim(inst)//ops_sep//trim(subdir)//numstr
       if (present(actual_path)) actual_path = path
       inquire(file=trim(path),exist=exists)
       if (exists) return ! found numor so return

       ! Using Previous data
       path = trim(ILL_data_directory)//Previous_Data//ops_sep//trim(inst)//ops_sep//trim(subdir)//numstr
       if (present(actual_path)) actual_path = path
       inquire(file=trim(path),exist=exists)
       if (exists) return ! found numor so return

       ! start from the most recent yearcycle and work search backwards
       call get_next_yearcycle(yearcycle,.true.)
       do
          if (yearcycle == "") exit

          path = trim(ILL_data_directory)//trim(yearcycle)//ops_sep//trim(inst)//ops_sep//trim(subdir)//numstr
          if (present(actual_path)) actual_path = path
          inquire(file=trim(path),exist=exists)
          if (exists) return

          ! Using previous + compressed data
          path = trim(path)//Extension
          inquire(file=trim(path),exist=exists)
          if (exists) then ! uncompress into temp directory
             call execute_command_line(trim(uncompresscommand)//' '//trim(path)//' > '//trim(ILL_temp_directory)//numstr)
             if (present(actual_path)) actual_path = path
             path = trim(ILL_temp_directory)//numstr
             return ! found numor so return
          end if

          call get_next_yearcycle(yearcycle)
       end do

       ! the numor wasn't found compressed or uncompressed anywhere
       path = " "
       if (present(actual_path)) actual_path = path

    End Subroutine Get_Absolute_Data_Path


    !!----Subroutine Get_Counts(np,snum,machineName,icnt)
    !!----  integer,                 intent(In)     :: np
    !!----  Type(Sxtal_Numor_Type),  intent(In)     :: snum
    !!----  character(len=*),        intent(in)     :: machineName
    !!----  integer, dimension(:,:), intent(In Out) :: icnt
    !!----
    !!----   Read counts of frame "np" from numor "snum" of machine "machineName"
    !!----   and store them in the 2D matrix icnt
    !!----   Update: March - 2019
    !!----
    !!----
    Subroutine Get_Counts_int(np,snum,machineName,icnt)
        integer,                 intent(In)     :: np
        Type(Sxtal_Numor_Type),  intent(In)     :: snum
        character(len=*),        intent(in)     :: machineName
        integer, dimension(:,:), intent(In Out) :: icnt
        !
        integer :: ik1,ik2,ix,iy,idx,iyear, xsize, ysize

        xsize = Size(icnt,1)
        ysize = Size(icnt,2)

        ! Set icnt to zero (required for border)
        icnt = 0
        ! How to read in the data depends on the machine
        ! The raw data is stored in snum%counts in the order it is written in the numor
        ! Ideally the data needs to be put into 'icnt' from the samples point of view
        ! Instead of machine name, a number can be provided in order to read the pixels
        ! in a particular way.
        ! 1: Column by column with zero at high nu, high gamm (e.g. D19 banana)
        ! 2: Row by Row with zero at high nu, high gamm (e.g. D9 and D10)
        ! 3: Row wire by Row with zero at high nu, low gamm (e.g. D19 bidim?)
        ! 4: Column by column with zero at low nu, low gamm (e.g. D19 bidim on DB21)
        ! 5: Row by Row wire with zero at high nu, low gamm (e.g. D19 bidim on DB21)

        select case(l_case(trim(machineName)))
            case('d9','d10')
                read(snum%header(22:23),"(i2)") iyear
                if (iyear > 70 .AND. iyear < 99) then
                    ik1 = 1
                    ik2 = 0
                else
                    ik1 = 0
                    ik2 = 1
                end if
                do ix=1,xsize
                    do iy=1,ysize
                        idx=(ix+(iy-1)*xsize)*ik1 + (iy+(ix-1)*ysize)*ik2
                        icnt(ix,iy)=snum%counts(idx,np)  !icou(i)
                    end do
                end do
            case('d19_vb')
                Err_CFML%Msg= 'Can''t read D19 Vertical Banana Detector data!'
                Err_CFML%Ierr=1
                Err_CFML%flag=.true.
                return
            case ('d19_hb','d19','db21','d16','1')
                !do ix=1,xsize
                !    do iy=1,ysize
                !        idx = iy+(ix-1)*ysize
                !        icnt(ix,iy) = snum%counts(idx,np)
                !    end do
                !end do
                icnt=reshape(nint(snum%counts(:,np)),(/xsize,ysize/))
            case default
                Err_CFML%Msg= 'Error in Get_Counts: Unknown machine!'
                Err_CFML%Ierr=1
                Err_CFML%flag=.true.
                return
        end select

    End Subroutine Get_Counts_int


    !!----Subroutine Get_Counts(np,snum,machineName,cnt)
    !!----  integer,                       intent(In)     :: np
    !!----  Type(Sxtal_Numor_Type),        intent(In)     :: snum
    !!----  character(len=*),              intent(in)     :: machineName
    !!----  real(kind=cp), dimension(:,:), intent(In Out) :: cnt
    !!----
    !!----   Read counts of frame "np" from numor "snum" of machine "machineName"
    !!----   and store them in the 2D matrix icnt
    !!----   Update: March - 2019
    !!----
    !!----
    Subroutine Get_Counts_real(np,snum,machineName,cnt)
        integer,                       intent(In)     :: np
        Type(Sxtal_Numor_Type),        intent(In)     :: snum
        character(len=*),              intent(in)     :: machineName
        real(kind=cp), dimension(:,:), intent(In Out) :: cnt
        !
        integer :: ik1,ik2,ix,iy,idx,iyear, xsize, ysize

        xsize = Size(cnt,1)
        ysize = Size(cnt,2)

        cnt = 0.0
        ! How to read in the data depends on the machine
        ! The raw data is stored in snum%counts in the order it is written in the numor
        ! Ideally the data needs to be put into 'icnt' from the samples point of view
        ! Instead of machine name, a number can be provided in order to read the pixels
        ! in a particular way.
        ! 1: Column by column with zero at high nu, high gamm (e.g. D19 banana)
        ! 2: Row by Row with zero at high nu, high gamm (e.g. D9 and D10)
        ! 3: Row wire by Row with zero at high nu, low gamm (e.g. D19 bidim?)
        ! 4: Column by column with zero at low nu, low gamm (e.g. D19 bidim on DB21)
        ! 5: Row by Row wire with zero at high nu, low gamm (e.g. D19 bidim on DB21)

        select case(l_case(trim(machineName)))
            case('d9','d10')
                read(snum%header(22:23),"(i2)") iyear
                if (iyear > 70 .AND. iyear < 99) then
                    ik1 = 1
                    ik2 = 0
                else
                    ik1 = 0
                    ik2 = 1
                end if
                do ix=1,xsize
                    do iy=1,ysize
                        idx=(ix+(iy-1)*xsize)*ik1 + (iy+(ix-1)*ysize)*ik2
                        cnt(ix,iy)=snum%counts(idx,np)
                    end do
                end do
            case('d19_vb')
                Err_CFML%Msg= 'Can''t read D19 Vertical Banana Detector data!'
                Err_CFML%Ierr=1
                Err_CFML%Flag=.true.
                return
            case ('d19_hb','d19','db21','d16','1')
                !do ix=1,xsize
                !    do iy=1,ysize
                !        idx = iy+(ix-1)*ysize
                !        cnt(ix,iy) = snum%counts(idx,np)
                !    end do
                !end do
                cnt=reshape(snum%counts(:,np),[xsize,ysize])
            case default
                Err_CFML%Msg= 'Error in Get_Counts: Unknown machine!'
                Err_CFML%Ierr=1
                Err_CFML%flag=.true.
                return
        end select

    End Subroutine Get_Counts_real

    !!---- Subroutine Get_Next_YearCycle(YearCycle,Reset_To_Most_Recent)
    !!----    character(len=*), intent(out) :: yearcycle
    !!----    logical, optional, intent(in) :: reset_to_most_recent
    !!----
    !!----    Works back through the cycles and years, returning the previous
    !!----    yearcycle combination as a 3 character string i.e.
    !!----       if year_illdata = 6 and cycle_number = 5, returns '064'
    !!----       if year_illdata = 6 and cycle_number = 1, returns '057'
    !!----
    !!----    The reset_to_most_recent flag allows the year_illdata and cycle_number to
    !!----    be set to the most recent possible. If asked for a yearcycle before
    !!----    '731' (first cycle of 1973) then returns "", since no data was archived
    !!----    before this date.
    !!----    (Original code from Mike Turner)
    !!----
    !!---- Update: March - 2009
    !!
    Subroutine Get_Next_YearCycle(Yearcycle, reset_to_most_recent)
       !---- Argument ----!
       character(len=*), intent(out) :: yearcycle
       logical, optional, intent(in) :: reset_to_most_recent

       !---- Local Variables ----!
       integer, parameter    :: Maxcycle = 7
       integer, dimension(8) :: dt
       character(len=10)     :: date,time,zone

       if (present(reset_to_most_recent)) then
          if (reset_to_most_recent) then
             call date_and_time (date, time, zone, dt)
             year_illdata = mod(dt(1),100) ! last two digits of year
             cycle_number = Maxcycle
          end if
       else
          if (cycle_number /= 1) then
             cycle_number = cycle_number -1
          else
             if (year_illdata == 0) then
                year_illdata = 99
             else
                year_illdata = year_illdata - 1
             end if
             cycle_number = Maxcycle
          end if
       end if

       if (year_illdata == 72) then ! no more point searching
          yearcycle = ""
       else
          write(unit=yearcycle,fmt="(i4.4,i1.1)") year_illdata,cycle_number
          yearcycle = yearcycle(len_trim(yearcycle)-2:len_trim(yearcycle))

       end if

    End Subroutine Get_Next_YearCycle

    !!----
    !!---- Subroutine Get_Single_Frame_2D(nfr,iord,snum,dat_2D,appl_alphas)
    !!----    integer,                       intent(in)  :: nfr,iord
    !!----    type(SXTAL_Numor_type),        intent(in)  :: snum
    !!----    real(kind=cp), dimension(:,:), intent(out) :: dat_2D
    !!----    logical, optional,             intent(in)  :: appl_alphas
    !!----
    !!--<<    Extracts into the real two-dimensional array dat_2D the counts
    !!----    of the frame number 'nfr' of the numor object 'snum', applying
    !!----    the efficiency corrections depending of the optional argument
    !!----    'appl_alphas'. The value of iord selects the type of ordering
    !!----    of counts in 'snum'
    !!----
    !!----    IORD = 1 for D19 Banana, IORD = 2 for D9/D10, IORD = 3 for D19 Flat
    !!-->>    IORD = 4 for ID20 trial.
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Get_Single_Frame_2D(nfr,iord,snum,dat_2D,appl_alphas)
       !---- Arguments ----!
       integer,                       intent(in)  :: nfr,iord
       type(SXTAL_Numor_type),        intent(in)  :: snum
       real(kind=cp), dimension(:,:), intent(out) :: dat_2D
       logical, optional,             intent(in)  :: appl_alphas

       !---- IA = anodes (1-NANO), IC = cathodes (1-NCAT)
       !   IORD = 1 for D19 Banana, IORD = 2 for D9/D10, IORD = 3 for D19 Flat.
       !   IORD = 4 for ID20 trial.
       integer :: k,ia,ic,ncat,nano

       Select Case(iord)

         Case(1)   ! D19
           ! Read vertical-wire by vertical-wire, with zero at high nu, high gamm.
           ! D19 Banana
           ncat =  Current_instrm%np_vert
           nano =  Current_instrm%np_horiz
           k=0
           do ic=1,ncat
             do ia=1,nano
               k=k+1
               dat_2D(ia,ic)=snum%counts(k,nfr)
             end do
           end do

         Case(2)   ! D9/D10
           ! Read horizontal-wire by horizontal-wire, with zero at high nu, high gamm?
           ! D9, D10
           ncat =  Current_instrm%np_vert
           nano =  Current_instrm%np_horiz
           k=0
           do ic=1,ncat
             do ia=1,nano
               k=k+1
               dat_2D(ia,ic)=snum%counts(k,nfr)
             end do
           end do

         Case(3)   ! D19-Flat
           ! Read horizontal-wire by horizontal-wire, with zero at high nu, high gamm.
           ! D19 flat detector (but we reverse IA because of the funny different
           ! conversion for flat and curved detectors in subroutine d19amd.
           ncat =  Current_instrm%np_vert
           nano =  Current_instrm%np_horiz
           k=0
           do ia=nano,1,-1
             do ic=1,ncat
               k=k+1
               dat_2D(ia,ic)=snum%counts(k,nfr)
             end do
           end do

         Case(4)   ! ID20-trial
           ! Read vertical-wire by vertical-wire, with zero at low nu, high gamm.
           ! ID20 flat detector (but we reverse IA because of the funny different
           ! conversion for flat and curved detectors in subroutine d19amd).
           ncat =  Current_instrm%np_vert
           nano =  Current_instrm%np_horiz
           k=0
           do ic=ncat,1,-1
             do ia=nano,1,-1
               k=k+1
               dat_2D(ia,ic)=snum%counts(k,nfr)
             end do
           end do

       End Select

       if(present(appl_alphas)) then
         if(appl_alphas) dat_2D(:,:)= dat_2D(:,:)*Current_instrm%alphas(:,:)
       end if

    End Subroutine Get_Single_Frame_2D

    !!----
    !!---- Subroutine Init_Powder_Numor(Numor,NBAng, NBData, NFrames)
    !!----
    !!---- Initialize the Type Numor. If NBAng, NBData and NFrames are > 0 then
    !!---- allocate the respective arrays into the type object-
    !!----
    !!---- 21/03/2011
    !!
    Subroutine Init_Powder_Numor(Numor,NBAng, NBData, NFrames)
        !---- Arguments ----!
        type(Powder_Numor_type), intent(out) :: Numor
        integer, optional,       intent(in)  :: NBAng
        integer, optional,       intent(in)  :: NBData
        integer, optional,       intent(in)  :: NFrames

        !---- Local Variables ----!
        Numor%numor=0
        Numor%manip=0
        Numor%icalc=0
        Numor%header=" "
        Numor%instrm=" "
        Numor%title=" "
        Numor%scantype=" "
        Numor%angles=0.0
        Numor%scans=0.0
        Numor%monitor=0.0
        Numor%time=0.0
        Numor%wave=0.0
        Numor%conditions=0.0
        Numor%ICDesc=0

        Numor%nbdata=0
        Numor%nbang=0
        Numor%nframes=0
        if (present(nframes)) Numor%nframes=nframes
        if (present(nbdata))  Numor%nbdata=nbdata
        if (present(nbang))   Numor%nbang=nbang

        if (allocated(Numor%tmc_ang)) deallocate(Numor%tmc_ang)
        if (allocated(Numor%counts)) deallocate(Numor%counts)

        if (present(nframes)) then
           if (present(nbang)) then
              if (nframes > 0 .and. nbang > 0) then
                 allocate(Numor%tmc_ang(3+nbang,nframes))
                 Numor%tmc_ang=0.0
              end if
           end if

           if (present(nbdata)) then
              if (nframes > 0 .and. nbdata > 0) then
                 allocate(Numor%counts(nbdata,nframes))
                 Numor%counts=0.0
              end if
           end if
        end if

    End Subroutine Init_Powder_Numor

    !!----
    !!---- Subroutine Init_Current_Instrument()
    !!----
    !!---- Initialize the Current Instrument.
    !!----
    !!---- 9/9/2018
    !!
    Subroutine Init_Current_Instrument()
      Current_Instrm%info="Current_Instrument"    !information about the instrument
      Current_Instrm%name_inst=" "                !Short name of the instrument
      Current_Instrm%geom="Eulerian_4C"           !"Eulerian_4C","Kappa_4C","Lifting_arm","Powder","Laue"
      Current_Instrm%BL_frame="z-up"              !Kind of BL-frame: "z-up" or "z-down"
      Current_Instrm%data_ordering="unknown"
      Current_Instrm%dist_units="mm"              !distance units: "mm  ","cm  ","inch"
      Current_Instrm%r_ord="xyz"                  ! xyz, yxz, ... Order of detector tilt calculations: xyz means Rx Ry Rz
      Current_Instrm%angl_units="deg"             !angle units: "rad","deg"
      Current_Instrm%detector_type=" "            !"Point","Flat_rect","Cylin_ImPlate","Tube_PSD", Put ipsd=1,2,...
      Current_Instrm%dist_samp_detector=0.0       ! dist. to centre for: point, Flat_rect, Tube_PSD; radius for: Cylin_ImPlate
      Current_Instrm%wave=0.5                     !Minimum and maximum wavelengths (Laue diffractometers)
      Current_Instrm%wave_min=0.5                 !Minimum and maximum wavelengths (Laue diffractometers)
      Current_Instrm%wave_max=0.5                 !Minimum and maximum wavelengths (Laue diffractometers)
      Current_Instrm%vert=0.0                     !Vertical dimension
      Current_Instrm%horiz=0.0                    !Horizontal dimension
      Current_Instrm%agap=0.0                     !gap between anodes
      Current_Instrm%cgap=0.0                     !gap between cathodes
      Current_Instrm%np_vert=0                    !number of pixels in vertical direction
      Current_Instrm%np_horiz=0                   !number of pixels in horizontal direction
      Current_Instrm%tilted=.false.               ! True if RD /= I
      Current_Instrm%RD=reshape([1.0,0.0,0.0, & ! Rotation matrix giving the orientation of the D-frame
                                  0.0,1.0,0.0, & ! w.r.t. the L-frame
                                  0.0,0.0,1.0],[3,3])
      Current_Instrm%ga_d=0.0                     ! Angles gamm and nu of the detector centre
      Current_Instrm%nu_d=0.0                     ! Angles gamm and nu of the detector centre
      Current_Instrm%tiltx_d=0.0                  ! Tilt of the flat detector around x-axis (normally = nu_d)
      Current_Instrm%tilty_d=0.0                  ! Tilt of the flat detector around y-axis (normally = 0)
      Current_Instrm%tiltz_d=0.0                  ! Tilt of the flat detector around z-axis (normally = 0)
      Current_Instrm%igeom=0                      ! 1: Bisecting (PSI=0),2: Bisecting - HiCHI, 3: Normal beam, 4:Parallel (PSI=90)
      Current_Instrm%ipsd=0                       ! 1: Flat,2: Vertically Curved detector (used in D19amd), 3:
      Current_Instrm%e1=[1.0,0.0,0.0]            ! Components of e1 in {i,j,k}
      Current_Instrm%e2=[0.0,1.0,0.0]            ! Components of e2 in {i,j,k}
      Current_Instrm%e3=[0.0,0.0,1.0]            ! Components of e3 in {i,j,k}
      Current_Instrm%gnxz_limited=.false.         ! The limits in gamm, nu, x and z accessible in the detector have been provided
      Current_Instrm%gap_min=0.0                  ! gamm minimum , gamm  maximun  (Positive values) | This change is needed due to odd
      Current_Instrm%gap_max=140.0                ! gamm minimum , gamm  maximun  (Positive values) | This change is needed due to odd
      Current_Instrm%gan_min=0.0                  ! gamm minimum , gamm  maximun  (Negative values) | positions of flat detectors.
      Current_Instrm%gan_max=0.0                  ! gamm minimum , gamm  maximun  (Negative values) | positions of flat detectors.
      Current_Instrm%nu_min=-15.0                 ! minimum admissible nu  and maximum admissible nu
      Current_Instrm%nu_max=15.0                  ! minimum admissible nu  and maximum admissible nu
      Current_Instrm%d_min=0.5                    ! Resolution limit ... if not given d_min= 2/L_min
      Current_Instrm%x_min=0.0                    ! in mm
      Current_Instrm%x_max=40.0                   ! in mm
      Current_Instrm%z_min=0.0
      Current_Instrm%z_max=40.0
      Current_Instrm%num_ang=0                    !Number of angular motors
      Current_Instrm%ang_names=" "                !Name of angular motors
      Current_Instrm%ang_limits=0.0               !Angular limits (up to 15 angular motors)
      Current_Instrm%ang_velocity=1.0             !Velocity assumed 1degree/second
      Current_Instrm%ang_offsets=0.0              !Angular offsets
      Current_Instrm%num_disp=0                   !Number of displacement motors
      Current_Instrm%disp_names=" "               !Name of displacement motors
      Current_Instrm%disp_limits=0.0              !Displacement limits (up to 15 displacement motors)
      Current_Instrm%disp_offsets=0.0             !Displacement offsets
      Current_Instrm%displaced=.false.            ! True if det_offsets /= (0,0,0)
      Current_Instrm%det_offsets=0.0              !Offsets X,Y,Z of the detector centre (roD)
      Current_Instrm%rangtim=.false.
      Current_Instrm%alpha_correct=.false.        !Alpha correction not applied
      Current_Instrm%sigma_alpha_correct=.false.  !Sigmas of Alpha correction not applied
      Current_Instrm%shift_correct=.false.        !Shifts correction not applied
      Current_Instrm%resol_given=.false.          !Resolution curve not given
      Current_Instrm%alpha_file=" "
    End Subroutine Init_Current_Instrument
    !!----
    !!---- Subroutine Init_SXTAL_Numor(Numor,NBAng, NBData, NFrames)
    !!----
    !!---- Initialize the Type Numor. If NBAng, NBData and NFrames are > 0 then
    !!---- allocate the respective arrays into the type object-
    !!----
    !!---- 21/03/2011
    !!
    Subroutine Init_SXTAL_Numor(Numor,NBAng, NBData, NFrames)
        !---- Arguments ----!
        type(SXTAL_Numor_type),  intent(out) :: Numor
        integer, optional,       intent(in)  :: NBAng
        integer, optional,       intent(in)  :: NBData
        integer, optional,       intent(in)  :: NFrames

        !---- Local Variables ----!
        Numor%filename = ""
        Numor%header_size = 0
        Numor%frame_size = 0
        Numor%numor=0
        Numor%manip=0
        Numor%icalc=0
        Numor%header=" "
        Numor%instrm=" "
        Numor%title=" "
        Numor%scantype=" "
        Numor%hmin=0.0
        Numor%hmax=0.0
        Numor%angles=0.0
        Numor%ub=0.0
        Numor%dh=0.0
        Numor%scans=0.0
        Numor%preset=0.0
        Numor%wave=0.0
        Numor%dist=0.0
        Numor%cpl_fact=0.0
        Numor%conditions=0.0
        Numor%ICDesc=0

        Numor%nbdata=0
        Numor%nbang=0
        Numor%nframes=0

        if (present(nframes)) Numor%nframes=nframes
        if (present(nbdata))  Numor%nbdata=nbdata
        if (present(nbang))   Numor%nbang=nbang

        if (allocated(Numor%selected_frames)) deallocate(Numor%selected_frames)
        if (allocated(Numor%tmc_ang)) deallocate(Numor%tmc_ang)
        if (allocated(Numor%counts)) deallocate(Numor%counts)

        if (present(nframes)) then
           if (present(nbang)) then
              if (nframes > 0 .and. nbang > 0) then
                 allocate(Numor%tmc_ang(nbang,nframes))
                 Numor%tmc_ang=0.0
              end if
           end if

           if (present(nbdata)) then
              if (nframes > 0 .and. nbdata > 0) then
                 allocate(Numor%counts(nbdata,nframes))
                 Numor%counts=0.0
              end if
           end if
        end if

    End Subroutine Init_SXTAL_Numor

    !!----
    !!---- Subroutine Initialize_Data_Directory()
    !!----
    !!----    Call two subroutines: Initialize_Numors_Directory and Initialize_Temp_Directory .
    !!----    The first one is to set the ILL data directory and
    !!----    the second one to set the temporary directory.
    !!----    Both subroutines were originally coded by Mike Turner.
    !!----
    !!----
    !!---- Update: January - 2010
    !!
    Subroutine Initialize_Data_Directory()

       Call Initialize_Numors_Directory()
       Call Initialize_Temp_Directory()

    End Subroutine Initialize_Data_Directory

    !!----
    !!---- Subroutine Initialize_Numors_Directory()
    !!----
    !!....    Original code from Mike Turner (as well as the following comments)
    !!....    Depending on the operating system as reported by winteracter routine
    !!....    InfoOpSystem, assigns values to the following global public variables:
    !!....    sep, the path separator. "sep" has been replaced by "ops_sep" from
    !!....    CFML_GlobalDeps. InfoOpSystem is a function from Winteracter: replaced by
    !!....    OPS integer from CFML_GlobalDeps
    !!....
    !!----    Subroutine assigning values to the following global public variables:
    !!----    ILL_Data_Directory: Base directory for the ILL data. If found then
    !!----                        the flag got_ILL_data_directory is set to true.
    !!----
    !!--..    Original code from Mike Turner, changed to make the subroutine independent
    !!--..    of the Winteracter library.
    !!--..
    !!--..    The subroutine ask for enviroment variables TEMP and ILLDATA in first place and
    !!--..    and if it return a blank then the default is set as following:
    !!--..    ILL_Data_Directory -> \\Serdon\illdata
    !!--..    ILL_Data_Directory -> /net/serdon/illdata
    !!----
    !!----
    !!---- Update: January - 2010
    !!
    Subroutine Initialize_Numors_Directory()
       !---- Local Variables ----!
       Logical                     :: Exists
       Character(Len=*), Parameter :: Envvar = 'ILLDATA'
       Integer                     :: Nlong

       Exists = .False.

       !> Exist ILLDATA environment variable?
       Call Get_Environment_Variable(Envvar, Ill_Data_Directory)

       If (Len_Trim(Ill_Data_Directory) == 0) Then
          If (Ops == 1) Then
             Ill_Data_Directory = '\\Serdon\illdata'                     ! For Windows
          Else
             Ill_Data_Directory = '/net/serdon/illdata'                  ! For Linux/MACoS
          End If
       End If

       nlong = Len_Trim(Ill_Data_Directory)
       if (nlong > 0 ) then
          If (Ill_Data_Directory(nlong:nlong) /= Ops_Sep) Ill_Data_Directory = trim(Ill_Data_Directory)//Ops_Sep
       end if
       Got_Ill_Data_Directory = Directory_Exists(Trim(Ill_Data_Directory))

    End Subroutine Initialize_Numors_Directory

    !!----
    !!---- Subroutine Initialize_Temp_Directory()
    !!----
    !!....    Original code from Mike Turner (as well as the following comments)
    !!....    Depending on the operating system as reported by winteracter routine
    !!....    InfoOpSystem, assigns values to the following global public variables:
    !!....    sep, the path separator. "sep" has been replaced by "ops_sep" from
    !!....    CFML_GlobalDeps. InfoOpSystem is a function from Winteracter: replaced by
    !!....    OPS integer from CFML_GlobalDeps
    !!....
    !!----    Subroutine assigning values to the following global public variables:
    !!----    ILL_Temp_Directory: A temporary directory (used for uncompressing). If
    !!----                        found the flag got_ILL_temp_directory is set to true.
    !!----
    !!--..    Original code from Mike Turner, changed to make the subroutine independent
    !!--..    of the Winteracter library.
    !!--..
    !!--..    The subroutine ask for enviroment variables TEMP and ILLDATA in first place and
    !!--..    and if it return a blank then the default is set as following:
    !!--..    Windows: ILL_Temp_Directory -> C:\Temp
    !!--..    Linux:   ILL_Temp_Directory -> $HOME/tmp
    !!----
    !!----
    !!---- Update: January - 2010
    !!
    Subroutine Initialize_Temp_Directory()

       !---- Parameters ----!
       Character (Len=*), Parameter :: Envvar1 = 'TEMP'
       Character (Len=*), Parameter :: Envvar2 = 'TMP'

       !---- Local Variables ----!
       Integer                      :: I

       !> Is defined TEMP environment variable?
       Call Get_Environment_Variable(Envvar1, Ill_Temp_Directory)

       If (Len_Trim(Ill_Temp_Directory) == 0) Then
          !> Is defined TMP environment variable?
          Call Get_Environment_Variable(Envvar2, Ill_Temp_Directory)
          If (Len_Trim(Ill_Temp_Directory) == 0) Then
             !> Default values set to Home user
             If (Ops == 1) Then
                Call Get_Environment_Variable('USERPROFILE', Ill_Temp_Directory)
             Else
                Call Get_Environment_Variable('HOME', Ill_Temp_Directory)
             End If
          End If
       End If

       I = Len_Trim(Ill_Temp_Directory)
       If (Ill_Temp_Directory(I:I) /= Ops_Sep) Ill_Temp_Directory = Trim(Ill_Temp_Directory)//Ops_Sep

       ! Temporal
       Got_Ill_Temp_Directory = Directory_Exists(Trim(Ill_Temp_Directory))

    End Subroutine Initialize_Temp_Directory


    !!--++
    !!--++ Subroutine Number_KeyTypes_on_File(filevar, nlines)
    !!--++    character(len=*),dimension(:), intent(in) :: filevar
    !!--++    integer,                       intent(in) :: nlines
    !!--++
    !!--++    1:R, 2:A, 3:S, 4:F, 5:I, 6:J, 7:V
    !!
    Subroutine Number_KeyTypes_on_File(filevar, nlines)
       !---- Arguments ----!
       character(len=*),dimension(:), intent(in) :: filevar
       integer,                       intent(in) :: nlines

       !---- Local Variables ----!
       integer :: i

       ! Init output variables
       n_keytypes=0

       do i=1,nlines
          select case (filevar(i)(1:10))
             case ('RRRRRRRRRR')
                n_keytypes(1)=n_keytypes(1)+1
             case ('AAAAAAAAAA')
                n_keytypes(2)=n_keytypes(2)+1
             case ('SSSSSSSSSS')
                n_keytypes(3)=n_keytypes(3)+1
             case ('FFFFFFFFFF')
                n_keytypes(4)=n_keytypes(4)+1
             case ('IIIIIIIIII')
                n_keytypes(5)=n_keytypes(5)+1
             case ('JJJJJJJJJJ')
                n_keytypes(6)=n_keytypes(6)+1
             case ('VVVVVVVVVV')
                n_keytypes(7)=n_keytypes(7)+1
          end select
       end do

    End Subroutine Number_KeyTypes_on_File

    !!--++
    !!--++ Subroutine NumorD1BD20_To_DiffPattern(N, Pat, VNorm, Cal,angcor,perm)
    !!--++    type(powder_numor_type),                   intent(in)  :: N
    !!--++    type(DiffPat_E_Type),            intent(out) :: Pat
    !!--++    real(kind=cp),  optional,                  intent(in)  :: VNorm
    !!--++    type(calibration_detector_type), optional, intent(in)  :: Cal
    !!--++    logical, optional,                         intent(in)  :: angcor,perm
    !!--++
    !!--++ Pass the information from D1B/D20 Numor to DiffPat object
    !!--++ angcor: correction of wires positions applied (perm is assumed in this case)
    !!--++ per: permutation of alphas for wires positions applied even if the angles positios are not corrected
    !!--++
    !!--++ Updated: 13/10/2013
    !!
    Subroutine NumorD1BD20_To_DiffPattern(N, Pat, VNorm,Cal,angcor,perm)
       !---- Arguments ----!
       type(powder_numor_type),                   intent(in)  :: N
       type(DiffPat_E_Type),            intent(out) :: Pat
       real(kind=cp),  optional,                  intent(in)  :: VNorm
       type(calibration_detector_type), optional, intent(in)  :: Cal
       logical, optional,                         intent(in)  :: angcor,perm

       !---- Local Variables ----!
       integer                           :: i,j
       integer                           :: np
       real(kind=cp)                     :: xmin,xmax,xstep,cnorm

       np=n%nbdata
       !write(*,*)  "  Allocation of Diffraction pattern with ",np," points"
       call Allocate_Pattern (Pat, np)
       xmin=n%scans(1)
       xstep=n%scans(2)

       if(present(Cal)) then
         if(present(angcor)) then
            do i=1,np    ! Points
               j=ipoint(i)   !The pointer has been set on Read_Calibration_File_D20
               Pat%x(i)=xmin + Cal%PosX(j)
               Pat%y(i)=n%counts(j,1)
               Pat%sigma(i)=Pat%y(i)
            end do
         else
            if(present(perm)) then
                do i=1,np    ! Points
                   j=ipoint(i)   !The pointer has been set on Read_Calibration_File_D20
                   Pat%x(i)=xmin + (i-1)*xstep !Cal%PosX(j)
                   Pat%y(i)=n%counts(j,1)
                   Pat%sigma(i)=Pat%y(i)
                end do
            else
                do i=1,np
                   Pat%x(i)=xmin + (i-1)*xstep
                   Pat%y(i)=n%counts(i,1)
                   Pat%sigma(i)=Pat%y(i)
                end do
            end if
         end if
       else
         do i=1,np    ! Points
            Pat%x(i)=xmin + (i-1)*xstep
            Pat%y(i)=n%counts(i,1)
            Pat%sigma(i)=Pat%y(i)
         end do
       end if

       xmax=Pat%x(np)
       Pat%title=trim(n%title)//", Instr: "//trim(n%instrm)//", Header:"//trim(n%header)
       Pat%KindRad="neutrons_cw"
       Pat%ScatVar='2theta'
       Pat%instr=n%instrm
       Pat%Monitor=n%monitor
       Pat%col_time=n%time
       Pat%xmin=xmin
       Pat%xmax=xmax
       Pat%ymin=minval(Pat%y)
       Pat%ymax=maxval(Pat%y)
       Pat%step=xstep
       Pat%npts=np
       Pat%nd=1
       Pat%TSet=n%conditions(1)
       Pat%Tsample=n%conditions(3)
       Pat%wave(1)=n%wave

       if(present(VNorm)) then
         if(VNorm < 0.0) then
           cnorm = abs(VNorm)/n%time
           Pat%Norm_Mon=n%monitor*cnorm
         else
           Pat%Norm_Mon=VNorm
           cnorm = VNorm/Pat%monitor
         end if
         Pat%y=Pat%y*cnorm
         Pat%sigma=Pat%sigma*cnorm*cnorm
       else
          Pat%Norm_Mon=n%monitor
       end if

    End Subroutine NumorD1BD20_To_DiffPattern


    !!----
    !!---- Subroutine PowderNumors_To_DiffPattern(PNumors, N, ActList, Pat, VNorm, Cal, Cal,angcor, perm)
    !!----    type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
    !!----    integer,                                   intent(in)  :: N          ! Number of Numors
    !!----    logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
    !!----    type (DiffPat_E_Type),                     intent(out) :: Pat        ! Pattern Diffraction
    !!----    real(kind=cp), optional,                   intent(in)  :: VNorm      ! Normalization value
    !!----    type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
    !!----    logical, optional,                         intent(in)  :: angcor,perm
    !!----
    !!---- Pass the information from Powder_Numor_Type to DiffPat_E_Type
    !!----
    !!----
    !!---- Date: 25/03/2011
    !!
    Subroutine PowderNumors_To_DiffPattern(PNumors,N,ActList,Pat, VNorm, Detect,Cal,angcor,perm,thres)
       !---- Arguments ----!
       type(Powder_Numor_Type),dimension(:),      intent(in)  :: PNumors    ! Powder Numors Vector
       integer,                                   intent(in)  :: N          ! Number of Numors
       logical,dimension(:),                      intent(in)  :: ActList    ! Active list for Numors
       type (DiffPat_E_Type),                     intent(out) :: Pat        ! Diffraction Pattern
       real(kind=cp), optional,                   intent(in)  :: VNorm      ! Normalization value
       integer, optional ,                        intent(in)  :: Detect     ! Select a particular detector (for D4)
       type(calibration_detector_type), optional, intent(in)  :: Cal        ! Calibration Information
       logical, optional,                         intent(in)  :: angcor,perm
       real(kind=cp), optional,                   intent(in)  :: thres

       !---- Local Variables ----!
       character(len=4)        :: inst
       character(len=50)       :: textnum
       integer                 :: i,num
       integer, dimension(N)   :: ind
       type(POWDER_Numor_type) :: PPNum

       !> Initialize
       call clear_error()

       !> Init
       if (N <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Number of Numors in the List was zero!'
          return
       end if

       num=count(actlist .eqv. .true.)
       if (num <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Number of active Numors in the List was zero!'
          return
       end if

       num=0
       ind=0
       do i=1,n
          if (.not. actlist(i) ) cycle
          num=num+1
          ind(num)=i
       end do
       inst=PNumors(ind(1))%Instrm
       inst=u_case(adjustl(inst))
       if (len_trim(inst) <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' The Instrument name for the first active numor was empty!'
          return
       end if
       if(trim(inst) == "D20" .and. PNumors(1)%nframes > 1) inst="D20S"
       select case (trim(inst))
          case ('D1B','D20')
             if(present(cal)) then
               call Adding_Numors_D1B_D20(PNumors,N,ActList,PPNum,Cal)
               if (present(VNorm)) then
                  if(present(angcor)) then
                    call NumorD1BD20_to_DiffPattern(PPNum, Pat,VNorm,Cal,angcor)
                  else
                    if(present(perm)) then
                      call NumorD1BD20_to_DiffPattern(PPNum, Pat,VNorm,Cal,perm=perm)
                    else
                      call NumorD1BD20_to_DiffPattern(PPNum, Pat,VNorm,Cal)
                    end if
                  end if
               else
                  if(present(angcor)) then
                    call NumorD1BD20_to_DiffPattern(PPNum, Pat,Cal=Cal,angcor=angcor)
                  else
                    if(present(perm)) then
                      call NumorD1BD20_to_DiffPattern(PPNum, Pat,Cal=Cal,perm=perm)
                    else
                      call NumorD1BD20_to_DiffPattern(PPNum, Pat,Cal=Cal)
                    end if
                  end if
               end if
             else
               call Adding_Numors_D1B_D20(PNumors,N,ActList,PPNum)
               if (present(VNorm)) then
                  call NumorD1BD20_to_DiffPattern(PPNum, Pat,VNorm)
               else
                  call NumorD1BD20_to_DiffPattern(PPNum, Pat)
               end if
             end if

          case ('D1A')
             if (present(Cal)) then
                if (present(VNorm)) then
                   call Adding_Numors_D1A_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal)
                else
                   call Adding_Numors_D1A_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat,Cal=Cal)
                end if
             else
                if (present(VNorm)) then
                   call Adding_Numors_D1A_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat,VNorm=VNorm)
                else
                   call Adding_Numors_D1A_DiffPattern(PNumors,N,ActList,Pat)
                end if
             end if

         case ('D20S')
            if (present(Cal)) then
               if (present(VNorm)) then
                  if(present(thres)) then
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal,thres)
                  else
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,VNorm,Cal)
                  end if
               else
                  if(present(thres)) then
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,Cal=Cal,thres=thres)
                  else
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,Cal=Cal,thres=thres)
                  end if
               end if
            else
               if (present(VNorm)) then
                  if(present(thres)) then
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,VNorm,thres=thres)
                  else
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,VNorm)
                  end if
               else
                  if(present(thres)) then
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat,thres=thres)
                  else
                     call Adding_Numors_D20S_DiffPattern(PNumors,N,ActList,Pat)
                  end if
               end if
            end if

          case ('D2B')

          case ('D4')
             if (.not. present(Cal)) then

                if (.not. present(VNorm)) then
                   if (.not. present(Detect)) then
                      call Adding_Numors_D4_DiffPattern(PNumors,N,ActList,Pat)
                   else
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat, Detect=Detect)
                   end if
                else
                   if (.not. present(Detect)) then
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat,VNorm=VNorm)
                   else
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat,VNorm=VNorm,Detect=Detect)
                   end if
                end if
             else

                if (.not. present(VNorm)) then
                   if (.not. present(Detect)) then
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat, Cal=Cal)
                   else
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat, Detect=Detect, Cal=Cal)
                   end if
                else
                   if (.not. present(Detect)) then
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=N,ActList=ActList,Pat=Pat,VNorm=VNorm, Cal=Cal)
                   else
                      call Adding_Numors_D4_DiffPattern(PNumors=PNumors,N=Num,ActList=ActList,Pat=Pat,VNorm=VNorm, &
                                                        Detect=Detect,Cal=Cal)
                   end if
                end if
             end if

       end select
       !Completing the title of the powder pattern including the sumed numors
       write(unit=textnum,fmt="(2(a,i7))") " Numors added from ",PNumors(ind(1))%numor, " to ",PNumors(ind(num))%numor
       Pat%title=trim(Pat%Title)//" "//trim(textnum)

    End Subroutine PowderNumors_To_DiffPattern

    !!--++
    !!--++ Subroutine Read_A_KeyType(filevar, n_ini, n_end, nchars, charline)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++    integer,                        intent(out) :: nchars    ! Number of characters to be read from the next data
    !!--++    character(len=*),               intent(out) :: charline  ! String to be read
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_A_KeyType(filevar, n_ini, n_end, nchars, charline)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end
       integer,                        intent(out) :: nchars
       character(len=*),               intent(out) :: charline

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j,n
       integer, dimension(10) :: ivet

       ! Init output values
       call clear_error()

       nchars =0
       charline=' '
       ntext=0
       if (allocated(text_ill)) deallocate(text_ill)

       ! Check the correct KeyType A
       line=filevar(n_ini)
       if (line(1:10) /= 'AAAAAAAAAA') then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' A bad Block A-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       nchars =ivet(1)
       ntext=ivet(2)

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+2)+i
             if (j > n_end) then
                Err_CFML%Ierr=1
                Err_CFML%flag=.true.
                Err_CFML%Msg=' Impossible to read a line for this block!'
                exit
             end if
             text_ill(i)=trim(filevar(j))
          end do
       end if

       if (nchars > 0) then
          charline=' '
          n=nchars/80
          if (mod(nchars,80) /= 0) n=n+1
          do i=1,n
             charline=trim(charline)//filevar(n_ini+ntext+1+i)//char(10)
          end do
       end if

    End Subroutine Read_A_KeyType

    !!----
    !!---- Subroutine Read_Current_Instrm(filenam)
    !!----    character(len=*),  intent(in) :: filenam
    !!----
    !!----    Subroutine reading the file 'filenam' where the characteristics
    !!----    of the current instrument are written. The global Current_Instrm
    !!----    variable is filled after returning from this subroutine.
    !!----    In case of error the subroutine puts Err_CFML%Ierr=1
    !!----    and fills the error message variable Err_CFML%Msg.
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Read_Current_Instrm(filenam)
       !---- Argument ----!
       character(len=*),  intent(in) :: filenam

       !---- Local variables ----!
       character(len=120)             :: line
       character(len=512)             :: alpha_file
       character(len=20)              :: keyv, kshift
       integer                        :: i, j, lun, ier,npx,npz, n1,n2,n3,ial
       real(kind=cp), dimension(3,3)  :: set, ub
       real(kind=cp)                  :: wave

       logical                        :: esta,read_set, read_ub, g_lim,n_lim,x_lim,z_lim, r_velo

       npx=32  !default values
       npz=32

       open(newunit=lun,file=trim(filenam),status="old", action="read", position="rewind",iostat=ier)
       if (ier /= 0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg="Error opening the file: "//trim(filenam)
          return
       end if

       read_set    = .false.
       read_ub     = .false.
       g_lim       = .false.
       n_lim       = .false.
       x_lim       = .false.
       z_lim       = .false.
       r_velo      = .false.

       call Init_Current_Instrument()

       n1=0
       n2=0

       do
         read(unit=lun,fmt="(a)",iostat=ier) line
         if(ier /= 0) exit

         line=adjustl(line)
         if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
         i=index(line," ")
         keyv=u_case(line(1:i-1))

         Select Case(keyv)
            Case("INFO")
                Current_Instrm%info= adjustl(line(i+1:))

            Case("NAME")
                Current_Instrm%name_inst= adjustl(line(i+1:))

            Case("GEOM")
                Current_Instrm%geom= adjustl(line(i+1:))
                read(unit=Current_Instrm%geom,fmt=*) Current_Instrm%igeom
                j=index(Current_Instrm%geom," ")
                Current_Instrm%geom=adjustl(Current_Instrm%geom(j+1:))

            Case("BLFR")
                Current_Instrm%BL_frame = adjustl(line(i+1:))

            Case("DIST_UNITS")
                Current_Instrm%dist_units = adjustl(line(i+1:))

            Case("ANGL_UNITS")
                Current_Instrm%angl_units = adjustl(line(i+1:))

            Case("DATA_ORDERING")
                Current_Instrm%data_ordering = adjustl(line(i+1:))

            Case("DET_TYPE")
                Current_Instrm%detector_type = adjustl(line(i+1:))
                Current_Instrm%ipsd=2 !Flat detector
                 j=index(Current_Instrm%detector_type,"ipsd")
                if( j /= 0)  &
                read(unit=Current_Instrm%detector_type(j+4:),fmt=*,iostat=ier) Current_Instrm%ipsd
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the type of detector (ipsd)"
                  return
                end if

            Case("DIST_DET")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%dist_samp_detector
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the distance sample-detector"
                  return
                end if

            Case("WAVE")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%wave
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the wavelength"
                  return
                else
                  Current_Instrm%wave_min=Current_Instrm%wave
                  Current_Instrm%wave_max=Current_Instrm%wave
                  wave=Current_Instrm%wave
                end if

            Case("WAVE_LIMITS","WAV_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%wave_min, Current_Instrm%wave_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the wavelength limits"
                  return
                end if
                wave=0.5*(Current_Instrm%wave_max-Current_Instrm%wave_min)

            Case("DIM_XY","DIM_XZ","DIM_XZP")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%horiz,    Current_Instrm%vert, &  !dimensions in milimeters
                                                       Current_Instrm%np_horiz, Current_Instrm%np_vert  !number of  cathodes and anodes
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the dimensions of the detector"
                  return
                end if
                npx = Current_Instrm%np_horiz
                npz = Current_Instrm%np_vert

            Case("GAPS_DET","GAPS_CA_AN")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%cgap, Current_Instrm%agap  !Gaps between cathodes and between anodes
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg=&
                  "Error in file: "//trim(filenam)//", reading the gaps between anodes and between cathodes"
                  return
                end if

            Case("GN_CENTRE","gamm_NU_CENTRE")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%ga_d, Current_Instrm%nu_d
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg= "Error in file: "//trim(filenam)//", reading gamm and Nu of detector centre"
                  return
                end if

            Case("ROT_ORDER")
                Current_Instrm%r_ord=adjustl(line(i+1:))

            Case("TILT_ANGLES")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%Tiltx_d, Current_Instrm%Tilty_d, Current_Instrm%Tiltz_d
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the Tilt Angles"
                  return
                end if
                if (abs(Current_Instrm%Tiltx_d) > 0.0001 .or. abs(Current_Instrm%Tilty_d) > 0.0001  &
                   .or. abs(Current_Instrm%Tiltz_d) > 0.0001 ) Current_Instrm%Tilted=.true.
            ! Limits

            Case("RESOL")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%d_min
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the resolution d_min value"
                  return
                end if

            Case("GAM+_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%gap_min, Current_Instrm%gap_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg= "Error in file: "//trim(filenam)//", reading the gamm(positive) limits"
                  return
                end if
                g_lim= .true.


            Case("GAM-_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%gan_min, Current_Instrm%gan_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg= "Error in file: "//trim(filenam)//", reading the gamm(negative) limits"
                  return
                end if
                g_lim= .true.

            Case("NU_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%nu_min, Current_Instrm%nu_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg= "Error in file: "//trim(filenam)//", reading the Nu limits"
                  return
                end if
                n_lim= .true.

            Case("X_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%x_min, Current_Instrm%x_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the X limits"
                  return
                end if
                x_lim = .true.

            Case("Z_LIMITS")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%z_min, Current_Instrm%z_max
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the Z limits"
                  return
                end if
                z_lim = .true.

            Case("UBMAT")
                do j=1,3
                  read(unit=lun,fmt=*,iostat=ier) ub(j,:)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the UB-matrix"
                    return
                  end if
                end do
                read_ub=.true.

            Case("SETTING")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%e1, Current_Instrm%e2, Current_Instrm%e3
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the setting vectors"
                  return
                end if
                set(:,1)=Current_Instrm%e1
                set(:,2)=Current_Instrm%e2
                set(:,3)=Current_Instrm%e3
                read_set=.true.

            Case("NUM_ANG")
                 read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%num_ang
                 n1 = Current_Instrm%num_ang

            Case("NUM_DISP")
                 read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%num_disp
                 n2 = Current_Instrm%num_disp

            Case("ANG_LIMITS")
                if(n1 == 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", Number of angular motors missing!"
                  return
                end if
                do j=1,n1
                  read(unit=lun,fmt=*,iostat=ier) Current_Instrm%ang_names(j), Current_Instrm%ang_Limits(j,1:2), &
                                                  Current_Instrm%ang_offsets(j),Current_Instrm%ang_velocity(j)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the angular names, limits, offsets and velocities"
                    return
                  end if
                end do

            Case("DISP_LIMITS")
                if(n2 == 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", Number of displacement motors missing!"
                  return
                end if
                do j=1,n2
                  read(unit=lun,fmt=*,iostat=ier) Current_Instrm%disp_names(j),Current_Instrm%disp_Limits(j,1:2), &
                                                  Current_Instrm%disp_offsets(1:n2)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the displacement limits"
                    return
                  end if
                end do

            Case("DET_OFF","DISP_CENTRE")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%det_offsets
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the detector offsets"
                  return
                end if

            Case("RANGE_TIME")
                if(n1 == 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)// &
                      ", the number of angular motors is needed before giving range_time values"
                  return
                end if
                Current_Instrm%rangtim=.false.
                if(allocated(Current_Instrm%range_time)) deallocate(Current_Instrm%range_time)
                allocate(Current_Instrm%range_time(2,10,n1))
                Current_Instrm%range_time=0.0
                do j=1,n1
                  do
                    read(unit=lun,fmt="(a)",iostat=ier) line ! Name of the motor and number of pairs range - time
                    if(ier /= 0) then
                      Err_CFML%Ierr=1
                      Err_CFML%flag=.true.
                      Err_CFML%Msg="Error in file: "//trim(filenam)//", reading section of pairs range - time"
                      return
                    end if
                    line=adjustl(line)
                    if(len_trim(line) == 0 .or. line(1:1) == "!".or. line(1:1) == "#") cycle
                    exit
                  end do
                  i=index(line," ")
                  read(unit=line(i:),fmt=*,iostat=ier) n3
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading name of motor and number of pairs range - time"
                    return
                  end if
                  do i=1,n3
                     read(unit=lun,fmt=*,iostat=ier) Current_Instrm%range_time(:,i,j)
                     if(ier /= 0) then
                      Err_CFML%Ierr=1
                      Err_CFML%flag=.true.
                      Err_CFML%Msg="Error in file: "//trim(filenam)//", reading pairs range - time for motor: "//trim(line)
                      return
                     end if
                  end do
                end do
                Current_Instrm%rangtim=.true.

            Case("ALPHA_FILE")

                alpha_file=adjustl(line(11:))
                inquire(file=trim(alpha_file),exist=esta)
                if(esta) then
                  open(newunit=ial,file=trim(alpha_file),status="old", action="read", position="rewind",iostat=ier)
                  if (ier /= 0) then
                     Err_CFML%Ierr=1
                     Err_CFML%flag=.true.
                     Err_CFML%Msg="Error opening the file: "//trim(alpha_file)
                     return
                  end if
                  if(allocated(Current_Instrm%alphas)) deallocate(Current_Instrm%alphas)
                  allocate(Current_Instrm%alphas(npz,npx))
                  read(unit=ial,fmt=*)
                  do j=1,npz
                    read(unit=ial,fmt=*,iostat=ier) Current_Instrm%alphas(j,1:npx)
                    if(ier /= 0) then
                      Err_CFML%Ierr=1
                      Err_CFML%flag=.true.
                      Err_CFML%Msg="Error in file: "//trim(alpha_file)//", reading the efficiency of 2D-detector, efficiencies not applied!"
                      return
                    end if
                  end do

                  !Try to read sigma of alphase
                  read(unit=ial,fmt="(a)",iostat=ier) kshift !sigma of alphas
                  if(ier == 0) then
                    if(index(L_case(kshift),"sigma") /= 0) then
                       if(allocated(Current_Instrm%sigma_alphas)) deallocate(Current_Instrm%sigma_alphas)
                       allocate(Current_Instrm%sigma_alphas(npz,npx))
                       read(unit=ial,fmt=*)
                       do j=1,npz
                         read(unit=ial,fmt=*,iostat=ier) Current_Instrm%sigma_alphas(j,1:npx)
                         if(ier /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%flag=.true.
                           Err_CFML%Msg="Error in file: "//trim(alpha_file)//", reading the sigma of efficiency of 2D-detector, sigma-efficiencies not applied!"
                           return
                         end if
                       end do
                       Current_Instrm%sigma_alpha_correct =.true.
                    end if
                  end if

                  !Try to read pixel shifts
                  if(Current_Instrm%sigma_alpha_correct) then !If sigmas have been read, read another line containing shifts
                    read(unit=ial,fmt="(a)",iostat=ier) kshift !horizontal shifts
                  else
                    ier=0  !The previous line may be shifts
                  end if
                  if(ier == 0) then
                    if(index(L_case(kshift),"shift") /= 0) then
                      if(allocated(Current_Instrm%shifts)) deallocate(Current_Instrm%shifts)
                      allocate(Current_Instrm%shifts(npx))
                      Current_Instrm%shifts=0.0
                      Current_Instrm%shift_correct=.true.
                      read(unit=ial,fmt=*,iostat=ier) Current_Instrm%shifts(1:npx)
                      if(ier /= 0) then
                         Current_Instrm%shift_correct=.false.
                         exit
                      end if
                    end if
                  end if
                else

                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error the alpha file: "//trim(alpha_file)//", does not exist!"
                  return

                end if

                Current_Instrm%alpha_file=alpha_file
                Current_Instrm%alpha_correct=.true.


            Case("DET_ALPHAS")
                if(allocated(Current_Instrm%alphas)) deallocate(Current_Instrm%alphas)
                allocate(Current_Instrm%alphas(npz,npx))

                do j=1,npz
                  read(unit=lun,fmt=*,iostat=ier) Current_Instrm%alphas(j,1:npx)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the efficiency of 2D-detector"
                   return
                  end if
                end do
                Current_Instrm%alpha_correct=.true.

            Case("DET_SIGMA_ALPHAS")
                if(allocated(Current_Instrm%sigma_alphas)) deallocate(Current_Instrm%sigma_alphas)
                allocate(Current_Instrm%sigma_alphas(npz,npx))

                do j=1,npz
                  read(unit=lun,fmt=*,iostat=ier) Current_Instrm%sigma_alphas(j,1:npx)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the sigma of efficiencies of 2D-detector"
                   return
                  end if
                end do
                Current_Instrm%sigma_alpha_correct=.true.

            Case("DET_SHIFTS")
                if(Current_Instrm%alpha_correct) then
                   if(allocated(Current_Instrm%shifts)) deallocate(Current_Instrm%shifts)
                   allocate(Current_Instrm%shifts(npx))
                   read(unit=lun,fmt=*,iostat=ier) kshift
                   read(unit=lun,fmt=*,iostat=ier) Current_Instrm%shifts(1:npx)
                   if(ier /= 0) then
                     Err_CFML%Ierr=1
                     Err_CFML%flag=.true.
                     Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the pixel-shifts of 2D-detector"
                    return
                   end if
                   Current_Instrm%shift_correct=.true.
                end if

            Case("RESOLUTION_SURFACE")
                read(unit=line(i+1:),fmt=*,iostat=ier) Current_Instrm%nNu, Current_Instrm%nGa
                if(ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the dimension of resolution surface of the 2D-detector"
                 return
                end if
                if(allocated(Current_Instrm%ReSurf)) deallocate(Current_Instrm%ReSurf)
                allocate(Current_Instrm%ReSurf(Current_Instrm%nNu,Current_Instrm%nGa))
                do j=1,Current_Instrm%nNu
                  read(unit=lun,fmt=*,iostat=ier) Current_Instrm%ReSurf(j,1:Current_Instrm%nGa)
                  if(ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%flag=.true.
                    Err_CFML%Msg="Error in file: "//trim(filenam)//", reading the resolution surface of 2D-detector"
                   return
                  end if
                end do
                Current_Instrm%resol_given=.true.

         End Select
       End do
       if(g_lim .and. n_lim .and. x_lim .and. z_lim) Current_Instrm%gnxz_limited=.true.
       close(unit=lun)

       if(read_ub) then
         if (read_set) then
            call Set_Current_Orient(wave,ub,set)
         else
            call Set_Current_Orient(wave,ub)
         end if
       end if
       Current_Instrm_set=.true.

    End Subroutine Read_Current_Instrm

    !!--++
    !!--++ Subroutine Read_F_KeyType(filevar, n_ini, n_end)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_F_KeyType(filevar, n_ini, n_end)
       !---- Arguments----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end

       !---- Local Variables ----!
       character(len=80)           :: line
       integer                     :: i,j,k,nl
       integer, dimension(10)      :: ivet
       real(kind=cp), dimension(5) :: vet

       ! Init output values
       call clear_error()
       nval_f=0
       ntext=0

       if (allocated(text_ill)) deallocate(text_ill)
       if (allocated(rvalues)) deallocate(rvalues)

       ! Check the correct KeyType F
       line=filevar(n_ini)
       if (line(1:10) /= 'FFFFFFFFFF') then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' A bad Block F-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       nval_f =ivet(1)
       ntext=ivet(2)

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+1)+i
             text_ill(i)=filevar(j)
          end do
       end if

       if (nval_f > 0) then
          nl=nval_f/5
          if (mod(nval_f,5) /= 0) nl=nl+1
          allocate(rvalues(nval_f))
          rvalues=0.0

          j=1
          do i=n_ini+ntext+2, n_end
             read(unit=filevar(i),fmt='(5E16.8)') vet
             if (i /= n_end) then
                rvalues(j:j+4)=vet
                j=j+5
             else
                k=nval_f-(nl-1)*5
                rvalues(j:j+k-1)=vet(1:k)
             end if
          end do
       end if

    End Subroutine Read_F_KeyType

    !!--++
    !!--++ Subroutine Read_I_KeyType(filevar, n_ini, n_end)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_I_KeyType(filevar, n_ini, n_end)
       !---- Arguments----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j,k, nl
       integer, dimension(10) :: ivet

       ! Init output values
       call clear_error()

       nval_i=0
       ntext=0
       if (allocated(text_ill)) deallocate(text_ill)
       if (allocated(ivalues)) deallocate(ivalues)

       ! Check the correct KeyType I
       line=filevar(n_ini)
       if (line(1:10) /= 'IIIIIIIIII') then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' A bad Block I-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       nval_i =ivet(1)
       ntext=ivet(2)

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+1)+i
             text_ill(i)=filevar(j)
          end do
       end if

       if (nval_i > 0) then
          nl=nval_i/10
          if (mod(nval_i,10) /= 0) nl=nl+1
          allocate(ivalues(nval_i))
          ivalues=0

          j=1
          do i=n_ini+ntext+2, n_end
             read(unit=filevar(i),fmt='(10i8)') ivet
             if (i /= n_end) then
                ivalues(j:j+9)=ivet
                j=j+10
             else
                k=nval_i-(nl-1)*10
                ivalues(j:j+k-1)=ivet(1:k)
             end if
          end do
       end if

    End Subroutine Read_I_KeyType

    !!--++
    !!--++ Subroutine Read_J_KeyType(filevar, n_ini, n_end)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_J_KeyType(filevar, n_ini, n_end)
       !---- Arguments----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j,k,nl
       integer, dimension(8)  :: ivet

       ! Init output values
       call clear_error()

       nval_i=0
       ntext=0
       if (allocated(text_ill)) deallocate(text_ill)
       if (allocated(ivalues)) deallocate(ivalues)

       ! Check the correct KeyType J
       line=filevar(n_ini)
       if (line(1:10) /= 'JJJJJJJJJJ') then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' A bad Block J-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       nval_i =ivet(1)
       ntext=ivet(2)

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+1)+i
             text_ill(i)=filevar(j)
          end do
       end if

       if (nval_i > 0) then
          nl=nval_i/8
          if (mod(nval_i,8) /= 0) nl=nl+1
          allocate(ivalues(nval_i))
          ivalues=0

          j=1
          do i=n_ini+ntext+2, n_end
             read(unit=filevar(i),fmt='(8i10)') ivet
             if (i /= n_end) then
                ivalues(j:j+7)=ivet
                j=j+8
             else
                k=nval_i-(nl-1)*8
                ivalues(j:j+k-1)=ivet(1:k)
             end if
          end do

       end if

    End Subroutine Read_J_KeyType

    !!----
    !!---- Subroutine Read_Numor_D1A(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(powder_numor_type), intent(out) :: n
    !!----
    !!---- Subroutine to read a Numor of D1A Instrument at ILL
    !!----
    !!---- Update: 15/03/2011
    !!
    Subroutine Read_Numor_D1A(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(powder_numor_type), intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       integer                                      :: nlines
       integer                                      :: numor,idum
       integer                                      :: i,j

       call clear_error()

       ! Detecting numor
       nlines=Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D1A Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Check format for D1A
       call Number_KeyTypes_on_File(filevar,nlines)
       if (.not. equal_vector(n_keytypes,[1,2,1,1,2,0,0],7)) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond with D1A Format'
          return
       end if

       ! Defining the different blocks and load information on nl_keytypes
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if
       if (index(u_case(n%instrm),'D1A') <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond with D1A Format'
          return
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line)
       end if

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0) then
          n%manip=ivalues(4)
          n%nbang=ivalues(5)
          n%nframes=ivalues(7)
          n%nbdata=ivalues(24)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%scantype='2theta'
          n%wave=rvalues(18)
          n%conditions(1:5)=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample
       end if

       if(Instrm_Info_only) return

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+2,n%nframes))
       n%tmc_ang=0.0

       ! Counts
       call read_I_keyType(filevar,nl_keytypes(5,2,1),nl_keytypes(5,2,2))
       if (nval_i > 0) then
          if (nval_i /= (n%nbdata+n%nbang+2)*n%nframes) then
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='Counts problems in Numor format for D1A Instrument'
             return
          end if
          i=0
          n%time=0.0
          n%monitor=0.0
          do j=1,n%nframes
             n%tmc_ang(1,j)=real(ivalues(i+2))*0.001    ! Time (s)
             n%tmc_ang(2,j)=real(ivalues(i+1))          ! Monitor
             n%tmc_ang(3,j)=real(ivalues(i+3))*0.001    ! Angles
             n%counts(:,j)=real(ivalues(i+5:i+29))
             i=i+29
             n%time=n%time+n%tmc_ang(1,j)
             n%monitor=n%monitor+n%tmc_ang(2,j)
          end do
          n%monitor=n%monitor/real(n%nframes)
       end if

    End Subroutine Read_Numor_D1A

    !!----
    !!---- Subroutine Read_Numor_D1B(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(generic_numor_type), intent(out) :: n
    !!----
    !!---- Subroutine to read a Numor of D1B Instrument at ILL
    !!----
    !!---- Update: April - 2009
    !!
    Subroutine Read_Numor_D1B(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(powder_numor_type), intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       integer                                      :: nlines
       integer                                      :: numor,idum
       logical                                      :: new_form, very_old,old

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D1B Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Check format for D1B
       call Number_KeyTypes_on_File(filevar,nlines)

       new_form = .false.; very_old=.false.; old=.false.

                                    !R A S F I J V
       if (equal_vector(n_keytypes,[1,2,1,2,2,0,0],7)) then
           new_form = .true.
       else if (equal_vector(n_keytypes,[1,2,1,1,2,0,0],7)) then
           old=.true.
       else if (equal_vector(n_keytypes,[1,2,1,2,1,0,0],7)) then
           very_old=.true.
       else
           Err_CFML%Ierr=1
           Err_CFML%flag=.true.
           Err_CFML%Msg='This numor does not correspond with D1B Format'
           return
       end if


       ! Defining the different blocks and load information on nl_keytypes
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Initialize Numor
       call init_powder_numor(n)

       ! Fixing some values
       n%nframes=1
       n%scantype='2theta'

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor
       if(n%numor >= 102577) then   !Hardcoded here waiting for a proper database
         n%scans(2)=0.1
       else
         n%scans(2)=0.2
       end if

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line)
       end if

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0 .and. .not. new_form) then
          n%icdesc(1)=ivalues(8)    ! 0:Monitor ; 1:Time
       end if
       if (nval_i > 0 .and. new_form) then  !Like D2B
          n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
          n%nbang=ivalues(5)              ! Total number of angles moved during scan
          n%icalc=ivalues(9)
          n%nbdata=ivalues(24)            ! Total number of points per frame: D2B-> 128x128=16384
          n%icdesc(1:7)=ivalues(25:31)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0 .and. old ) then
          n%wave=rvalues(18)
          n%conditions(1:3)=rvalues(46:48)  ! Temp-s, Temp-r, Temp-sample
       end if

       if (nval_f > 0 .and. very_old ) then
          n%monitor=rvalues(1)
          n%time=rvalues(2)
          n%scans(1)=rvalues(3)             ! Initial 2theta
          n%conditions(1:3)=rvalues(15:17)  ! Temp-s, Temp-r, Temp-sample
          n%wave=rvalues(18)  !Not sure
          !write(*,*) " Monitor, time, 2theta_zero, T-set T-reg, T-samp"
          !write(*,*) n%monitor,n%time,n%scans(1),n%conditions(1:3)
      end if

       if (nval_f > 0 .and. new_form) then
          n%wave=rvalues(18)           ! Wavelength
          n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
          n%scans(1)=rvalues(7)        ! Initial 2theta
       end if

       if(new_form) then

         ! Allocating
         if (allocated(n%counts)) deallocate(n%counts)
         allocate(n%counts(n%nbdata,n%nframes))
         n%counts=0.0

         ! Loading Frames
         n%monitor=0.0
         n%time=0.0

         !Time/Monitor/Counts/Angles
         call read_F_keyType(filevar,nl_keytypes(4,2,1),nl_keytypes(4,2,2))
         if (nval_f > 0 ) then
            !write(*,*) " Allocating tmc_ang with ",n%nbang+nval_f,""
            if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
            allocate(n%tmc_ang(n%nbang+nval_f,n%nframes))
            n%tmc_ang=0.0
            n%tmc_ang(1,1)=rvalues(1)*0.001 ! Time (s)
            n%tmc_ang(2:3,1)=rvalues(2:3)   ! Monitor and Total Counts
            n%tmc_ang(4:nval_f,1)=rvalues(4:nval_f)*0.001  ! Angles in degrees
            n%time=n%tmc_ang(1,1)
            n%monitor=n%tmc_ang(2,1)
         else
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            write(unit=Err_CFML%Msg,fmt="(a,i6.6)") 'Problem reading Time, Monitor, Counts, Angles' &
                              //' parameters in Numor:',n%numor
            return
         end if

         if(Instrm_Info_only) return

         ! Counts
         call read_I_keyType(filevar,nl_keytypes(5,2,1),nl_keytypes(5,2,2))
         if (nval_i /= n%nbdata) then
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            write(unit=Err_CFML%Msg,fmt="(a,i6.6)") 'Problem reading Counts in Numor:',n%numor
            return
         end if
         if (nval_i > 0) then
            n%counts(:,1)=ivalues(1:n%nbdata)
         end if

       Else if (old) then !Below old format

         ! Counts
         if (allocated(n%counts)) deallocate(n%counts)
         call read_I_keyType(filevar,nl_keytypes(5,2,1),nl_keytypes(5,2,2))
         if (nval_i > 0) then
            n%nbdata=nval_i-3
            n%monitor=real(ivalues(1))
            n%time=real(ivalues(2))*0.001
            n%scans(1)=real(ivalues(3))*0.001
            if(Instrm_Info_only) return    !This is placed here waiting for a proper database
            if (n%nbdata > 0) then
               allocate(n%counts(n%nbdata,1))
               n%counts(:,1)=real(ivalues(4:nval_i))
            end if
         end if

       Else if (very_old) then !Below  very old format

         ! Counts
         if (allocated(n%counts)) deallocate(n%counts)
         !write(*,*) " Reading F-types between lines ",nl_keytypes(4,2,1),nl_keytypes(4,2,2)
         call read_F_keyType(filevar,nl_keytypes(4,2,1),nl_keytypes(4,2,2))
         if (nval_f > 0) then
            n%nbdata=nval_f
            if(Instrm_Info_only) return    !This is placed here waiting for a proper database
            if (n%nbdata > 0) then
               allocate(n%counts(n%nbdata,1))
               n%counts(:,1)=rvalues(1:nval_f)
            end if
         end if

       end if

    End Subroutine Read_Numor_D1B

    !!----
    !!---- Subroutine Read_Numor_D2B(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(Powder_numor_type), intent(out)  :: n
    !!----
    !!---- Subroutine to read a Numor of D2B Instrument at ILL
    !!----
    !!---- Counts: 128 x 128
    !!----
    !!---- Update: 15/03/2011
    !!
    Subroutine Read_Numor_D2B(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(Powder_numor_type),  intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i,nlines
       integer                                      :: numor,idum

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D2B Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Defining the different blocks and load information on nl_keytypes
       call Number_KeyTypes_on_File(filevar,nlines)
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Check format for D2B
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (index(u_case(line(1:4)),'D2B') <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond to D2B Format'
          return
       end if

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line(1:60))
          n%scantype=trim(line(73:))
       end if

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0) then
          n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
          n%nbang=ivalues(5)              ! Total number of angles moved during scan
          n%nframes=ivalues(7)            ! Number of Frames. The same as required in general
          n%icalc=ivalues(9)
          n%nbdata=ivalues(24)            ! Total number of points per frame: D2B-> 128x128=16384
          n%icdesc(1:7)=ivalues(25:31)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%wave=rvalues(18)           ! Wavelength
          n%scans=rvalues(36:38)       ! Scan start, Scan step, Scan width
          n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
       end if

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+3,n%nframes))
       n%tmc_ang=0.0

       ! Loading Frames
       n%monitor=0.0
       n%time=0.0
       do i=1,n%nframes

          !Time/Monitor/Counts/Angles
          call read_F_keyType(filevar,nl_keytypes(4,i+1,1),nl_keytypes(4,i+1,2))
          if (nval_f > 0 .and. nval_f == (n%nbang+3)) then
             n%tmc_ang(1,i)=rvalues(1)*0.001 ! Time (s)
             n%tmc_ang(2:3,i)=rvalues(2:3)   ! Monitor and Total Counts
             n%tmc_ang(4:nval_f,i)=rvalues(4:nval_f)*0.001  ! Angles in degrees
             n%time=n%time+n%tmc_ang(1,i)
             n%monitor=n%monitor+n%tmc_ang(2,i)
          else
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             write(unit=Err_CFML%Msg,fmt="(a,i6.6)") 'Problem reading Time, Monitor, Counts, Angles' &
                               //' parameters in Frame: '//trim(car)//" Numor:",n%numor
             return
          end if

          if(Instrm_Info_only) return

          ! Counts
          call read_I_keyType(filevar,nl_keytypes(5,i+1,1),nl_keytypes(5,i+1,2))
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='Problem reading Counts in Frame: '//trim(car)
             return
          end if
          if (nval_i > 0) then
             n%counts(:,i)=ivalues(1:n%nbdata)
          end if
       end do
       n%monitor=n%monitor/real(n%nframes)

    End Subroutine Read_Numor_D2B

    !!----
    !!---- Subroutine Read_Numor_D4(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(SXTAL_numor_type),  intent(out) :: n
    !!----
    !!---- Subroutine to read a Numor of D4 Instrument at ILL
    !!----
    !!---- 9 Detectors x 64 cells
    !!----   Each cell every 0.125�  (Total 8� by detector)
    !!----   Angular space between detectors is 7�
    !!----
    !!---- Update: 18/03/2011
    !!
    Subroutine Read_Numor_D4(fileinfo,N)
       !---- Arguments ----!
       character(len=*),          intent(in)   :: fileinfo
       type(Powder_numor_type),   intent(out)  :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i,nlines
       integer                                      :: numor,idum
       integer                                      :: npos, npos1,npos2

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D4 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Defining the different blocks and load information on nl_keytypes
       call Number_KeyTypes_on_File(filevar,nlines)
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Check format for D4
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (index(u_case(line(1:4)),'D4') <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond with D4 Format'
          return
       end if

       ! Fixed parameters
       n%nframes=9    ! Number of Detectors
       n%nbdata=64    ! Cells for each Detector

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(3+6,n%nframes))
       n%tmc_ang=0.0

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=u_case(line(1:4))
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          npos1=index(line,'Title')
          npos2=index(line,'Subtitle')
          if (npos1 > 0) then
             line=adjustl(line(npos1+5:npos2-1))
             npos=index(line,':')
             if (npos > 0) n%title=trim(line(npos+1:))
          end if
          n%scantype='2Theta'
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%monitor=rvalues(1)
          n%time=rvalues(2)
          n%wave=rvalues(5)
          n%scans(1)=rvalues(12)
          n%tmc_ang(1,1)=rvalues(2)           ! Time
          n%tmc_ang(2,1)=rvalues(1)           ! Monitor
          n%tmc_ang(3,1)=rvalues(11)          ! Initial Angle
          n%tmc_ang(4:12,1)=rvalues(31:39)    ! Total Counts for each detector (Monitor)
       end if

       call read_F_keyType(filevar,nl_keytypes(4,2,1),nl_keytypes(4,2,2))
       if (nval_f > 0) then
          n%conditions(3)=rvalues(1)   ! T-Sample
          n%conditions(2)=rvalues(2)   ! T-reg
          n%conditions(1)=rvalues(3)   ! T-Set
       end if

       if(Instrm_Info_only) return

       ! Loading Frames
       do i=1,n%nframes
          call read_I_keyType(filevar,nl_keytypes(5,i,1),nl_keytypes(5,i,2))
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i3)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='Problem reading Counts for Detector '//trim(car)//' for D4 Instrument'
             return
          end if
          n%counts(:,i)=real(ivalues(1:nval_i))
       end do

    End Subroutine Read_Numor_D4

    !!----
    !!---- Subroutine Read_Numor_D9(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(SXTAL_numor_type),  intent(out)  :: n
    !!----
    !!---- Subroutine to read a Numor of D9 Instrument at ILL
    !!----
    !!---- Counts: 32 x 32 = 1024
    !!----
    !!---- Update: 14/03/2011
    !!
    Subroutine Read_Numor_D9(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(SXTAL_numor_type),   intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i,nlines
       integer                                      :: numor,idum
       logical                                      :: check_qscan

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D9 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Defining the different blocks and load information on nl_keytypes
       call Number_KeyTypes_on_File(filevar,nlines)
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Check format for D9
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (index(line(1:4),'D9') <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond with D9 Format'
          return
       end if

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line(1:60))
          n%scantype=trim(line(73:))
          if(len_trim(n%scantype) == 0) n%scantype='q-scan'
       end if
       check_qscan=.false.

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0) then
          n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
          n%nbang=ivalues(5)              ! Total number of angles moved during scan
          n%nframes=ivalues(7)            ! Frames medidos. En general igual que los solicitados
          n%icalc=ivalues(9)
          n%nbdata=ivalues(24)            ! Number of Points
          n%icdesc(1:7)=ivalues(25:31)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%HMin=rvalues(1:3)          ! HKL min
          n%angles=rvalues(4:8)        ! Phi, Chi, Omega, 2Theta, Psi
          n%ub(1,:)=rvalues(9:11)      !
          n%ub(2,:)=rvalues(12:14)     ! UB Matrix
          n%ub(3,:)=rvalues(15:17)     !
          n%wave=rvalues(18)           ! Wavelength
          n%HMax=rvalues(22:24)        ! HKL max
          n%dh=rvalues(25:27)          ! Delta HKL
          n%dist=rvalues(30)           ! distance
          n%scans=rvalues(36:38)       ! Scan start, Scan step, Scan width
          n%preset=rvalues(39)         ! Preset
          n%cpl_fact=rvalues(43)       ! Coupling factor
          n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
       end if

       if(Instrm_Info_only) return

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+3,n%nframes))
       n%tmc_ang=0.0

       ! Loading Frames
       do i=1,n%nframes

          !Time/Monitor/Counts/Angles
          call read_F_keyType(filevar,nl_keytypes(4,i+1,1),nl_keytypes(4,i+1,2))
          if (nval_f > 0) then
             select case (nval_f)
                case (4)
                   n%tmc_ang(1,i)=rvalues(1)*0.001 ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)
                   n%tmc_ang(4,i)=rvalues(4)*0.001  ! Angle (that of the scan ..)

                case (5)
                   n%tmc_ang(1,i)=rvalues(1)*0.001      ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)        ! Monitor and total counts
                   n%tmc_ang(4:5,i)=rvalues(4:5)*0.001  ! Angle (gamm omega?)

                case (6:)
                   n%tmc_ang(1,i)=rvalues(1)*0.001      ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)        ! Monitor and total counts
                   n%tmc_ang(4:nval_f,i)=rvalues(4:nval_f)*0.001  ! Angles: gamm, omega, Chi,phi, psi?
                   check_qscan=.true.

                case default
                   write(unit=car,fmt='(i5)') i
                   car=adjustl(car)
                   Err_CFML%Ierr=1
                   Err_CFML%flag=.true.
                   write(unit=Err_CFML%Msg,fmt="(a,i6.6)")'Problem reading Time, Monitor, Counts, Angles' &
                                    //' parameters in the Frame: '//trim(car)//", Numor:",n%numor
                   return
             end select
          end if

          ! Counts
          call read_I_keyType(filevar,nl_keytypes(5,i+1,1),nl_keytypes(5,i+1,2))
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='Problem reading Counts in the Frame: '//trim(car)
             return
          end if
          if (nval_i > 0) then
             n%counts(:,i)=ivalues(1:n%nbdata)
          end if
       end do
       if(check_qscan) then
         n%scantype='q-scan'
       end if
       return

    End Subroutine Read_Numor_D9

    !!----
    !!---- Subroutine Read_Numor_D10(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(SXTAL_numor_type),  intent(out) :: n
    !!----
    !!---- Subroutine to read a Numor of D10 Instrument at ILL
    !!----
    !!---- Counts: 32 x 32 = 1024 (Bidimensional)
    !!----
    !!---- Update: 18/03/2011 18:32:52
    !!
    Subroutine Read_Numor_D10(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(SXTAL_numor_type),   intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i,nlines
       integer                                      :: numor,idum
       logical                                      :: check_qscan

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=' Problems trying to read the Numor for D10 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Defining the different blocks and load information on nl_keytypes
       call Number_KeyTypes_on_File(filevar,nlines)
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Check format for D10
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (index(line(1:4),'D10') <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg='This numor does not correspond with D10 Format'
          return
       end if

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line(1:60))
          n%scantype=trim(line(73:))
          if(len_trim(n%scantype) == 0) n%scantype='q-scan'
       end if
       check_qscan=.false.

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0) then
          if (ivalues(2) /= 2) then
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='This numor was made using Point detector in the D10 Instrument'
             return
          end if
          n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
          n%nbang=ivalues(5)              ! Total number of angles moved during scan
          n%nframes=ivalues(7)            ! Measured Frames. In general equal to those prescripted
          n%icalc=ivalues(9)
          n%nbdata=ivalues(24)            ! Number of Points
          n%icdesc(1:7)=ivalues(25:31)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%HMin=rvalues(1:3)          ! HKL min
          n%angles=rvalues(4:8)        ! Phi, Chi, Omega, 2Theta, Psi
          n%ub(1,:)=rvalues(9:11)      !
          n%ub(2,:)=rvalues(12:14)     ! UB Matrix
          n%ub(3,:)=rvalues(15:17)     !
          n%wave=rvalues(18)           ! Wavelength
          n%HMax=rvalues(22:24)        ! HKL max
          n%dh=rvalues(25:27)          ! Delta HKL
          n%dist=rvalues(30)           ! distance
          n%scans=rvalues(36:38)       ! Scan start, Scan step, Scan width
          n%preset=rvalues(39)         ! Preset
          n%cpl_fact=rvalues(43)       ! Coupling factor
          n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
       end if

       if(Instrm_Info_only) return

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+3,n%nframes))
       n%tmc_ang=0.0

       ! Loading Frames
       do i=1,n%nframes

          !Time/Monitor/Counts/Angles
          call read_F_keyType(filevar,nl_keytypes(4,i+1,1),nl_keytypes(4,i+1,2))
          if (nval_f > 0) then
             select case (nval_f)
                case (4)
                   n%tmc_ang(1,i)=rvalues(1)*0.001 ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)
                   n%tmc_ang(4,i)=rvalues(4)*0.001  ! Angle

                case (5)
                   n%tmc_ang(1,i)=rvalues(1)*0.001      ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)        ! Monitor and total counts
                   n%tmc_ang(4:5,i)=rvalues(4:5)*0.001  ! Angle (gamm omega?)

                case (6:)
                   n%tmc_ang(1,i)=rvalues(1)*0.001      ! Time (s)
                   n%tmc_ang(2:3,i)=rvalues(2:3)        ! Monitor and total counts
                   n%tmc_ang(4:nval_f,i)=rvalues(4:nval_f)*0.001  ! Angles: gamm, omega, Chi,phi, psi?
                   check_qscan=.true.

                case default
                   write(unit=car,fmt='(i5)') i
                   car=adjustl(car)
                   Err_CFML%Ierr=1
                   Err_CFML%Flag=.true.
                   write(unit=Err_CFML%Msg,fmt="(a,i6.6)")'Problem reading Time, Monitor, Counts, Angles' &
                                    //' parameters in the Frame: '//trim(car)//", Numor:",n%numor
                   return
             end select
          end if

          ! Counts
          call read_I_keyType(filevar,nl_keytypes(5,i+1,1),nl_keytypes(5,i+1,2))
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%Flag=.true.
             Err_CFML%Msg='Problem reading Counts in the Frame: '//trim(car)
             return
          end if
          if (nval_i > 0) then
             n%counts(:,i)=ivalues(1:n%nbdata)
          end if
       end do
       if(check_qscan) then
         n%scantype='q-scan'
       end if

    End Subroutine Read_Numor_D10

    !!----
    !!---- Subroutine Read_Numor_D16(filevar,N)
    !!----    character(len=*),        intent(in)   :: fileinfo
    !!----    type(SXTAL_numor_type),  intent(out)  :: n
    !!----
    !!---- Subroutine to read a Numor of D16 Instrument at ILL
    !!----
    !!---- Counts: 320 x 320 = 102400 (Bidimensional)
    !!----
    !!---- Update: 04/04/2012
    !!
    Subroutine Read_Numor_D16(fileinfo,N)
       !---- Arguments ----!
       character(len=*),         intent(in)   :: fileinfo
       type(SXTAL_numor_type),   intent(out)   :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i,nlines
       integer                                      :: numor,idum

       call clear_error()

       ! Detecting numor
       nlines=Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg=' Problems trying to read the Numor for D16 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Defining the different blocks and load information on nl_keytypes
       call Number_KeyTypes_on_File(filevar,nlines)
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Check format for D16
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (index(line(1:4),'D16') <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%flag=.true.
          Err_CFML%Msg='This numor does not correspond with D16 Format'
          return
       end if

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if

       ! Title/Sample
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          n%title=trim(line(1:60))
          n%scantype=trim(line(73:))
          if(len_trim(n%scantype) == 0) n%scantype='q-scan'
       end if

       ! Control Flags
       call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
       if (nval_i > 0) then
          if (ivalues(2) /= 2) then
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='This numor was made using Point detector in the D16 Instrument'
             return
          end if
          n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
          n%nbang=ivalues(5)              ! Total number of angles moved during scan
          n%nframes=ivalues(7)            ! Measured Frames. In general equal to those prescripted
          n%icalc=ivalues(9)
          n%nbdata=ivalues(24)            ! Number of Points
          n%icdesc(1:7)=ivalues(25:31)
       end if

       ! Real values
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%HMin=rvalues(1:3)          ! HKL min
          n%angles=rvalues(4:8)        ! Phi, Chi, Omega, 2Theta, Psi
          n%ub(1,:)=rvalues(9:11)      !
          n%ub(2,:)=rvalues(12:14)     ! UB Matrix
          n%ub(3,:)=rvalues(15:17)     !
          n%wave=rvalues(18)           ! Wavelength
          n%HMax=rvalues(22:24)        ! HKL max
          n%dh=rvalues(25:27)          ! Delta HKL
          n%dist=rvalues(30)           ! distance
          n%scans=rvalues(36:38)       ! Scan start, Scan step, Scan width
          n%preset=rvalues(39)         ! Preset
          n%cpl_fact=rvalues(43)       ! Coupling factor
          n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
       end if

       if(Instrm_Info_only) return

       ! Allocating
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n%nframes))
       n%counts=0.0

       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+3,n%nframes))
       n%tmc_ang=0.0

       ! Loading Frames
       do i=1,n%nframes

          !Time/Monitor/Counts/Angles
          call read_F_keyType(filevar,nl_keytypes(4,i+1,1),nl_keytypes(4,i+1,2))
          if (nval_f > 0 .and. nval_f == (n%nbang+3)) then
             n%tmc_ang(1,i)=rvalues(1)*0.001 ! Time (s)
             n%tmc_ang(2:3,i)=rvalues(2:3)
             n%tmc_ang(4:nval_f,i)=rvalues(4:nval_f)*0.001  ! Angle
          else
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             write(unit=Err_CFML%Msg,fmt="(a,i6.6)")'Problem reading Time, Monitor, Counts, Angles' &
                                    //' parameters in the Frame: '//trim(car)//", Numor:",n%numor
             return
          end if

          ! Counts
          call read_I_keyType(filevar,nl_keytypes(5,i+1,1),nl_keytypes(5,i+1,2))
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i5)') i
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='Problem reading Counts in the Frame: '//trim(car)
             return
          end if
          if (nval_i > 0) then
             n%counts(:,i)=ivalues(1:n%nbdata)
          end if
       end do

    End Subroutine Read_Numor_D16

    !!----
    !!---- Subroutine Read_Numor_D19(filename,n,frames)
    !!----    character(len=*)               , intent(in)    :: filename ! The input numor
    !!----    type(SXTAL_numor_type)         , intent(inout) :: n        ! The output numor structure
    !!----    integer, optional, dimension(:), intent(in)    :: frames   ! The frames to include in the numor structure
    !!----
    !!---- Subroutine to read a Numor of D19 Instrument at ILL
    !!----
    !!---- Counts: 640 x 256 = 163840
    !!----
    !!---- Update: 14/03/2011
    !!
    Subroutine Read_Numor_D19(filename,n,frames)
       !---- Arguments ----!
       character(len=*)               , intent(in)    :: filename
       type(SXTAL_numor_type)         , intent(inout) :: n
       integer, optional, dimension(:), intent(in)    :: frames


       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       integer, dimension(:), allocatable           :: temp_frames
       character(len=80)                            :: line
       character(len=5)                             :: car
       integer                                      :: i, j, n_skip, lun, previous_frame, n_selected_frames
       integer                                      :: numor,idum
       logical                                      :: info

       ! The error flags are initialized.
       call clear_error()

       ! Flag used for inquiring the input file.
       info=.false.

       ! Check first that the input file exists.
       inquire (file=filename,exist=info)

       ! If the input file does not exist, stop here.
       if (.not. info) then
           err_CFML%Flag = .true.
           err_CFML%Ierr = 1
           Err_CFML%Msg = " The file "//trim(filename)//" does not exist."
           return
       end if

       ! Check whether the input file is opned or not.
       inquire (file=filename,opened=info)

       ! If the input file is opened, get its logical unit.
       if (info) then
          inquire(file=filename,number=lun)
       ! If the input file is not opened, open it.
       else
          open(newunit=lun,file=filename,status="old",action="read",position="rewind")
       end if

       ! If the numor to read is different from the one stored in the numor structure, reprocess the header.
       if (trim(filename) /= trim(n%filename)) then

          ! Intiliaze the numor.
          call init_sxtal_numor(n)

          ! Store the input numor filename in the numor structure.
          n%filename = trim(filename)
          ! Define the number of lines of the header and frame blocks.
          call define_numor_header_frame_size(trim(filename),n%header_size,n%frame_size)
          ! If an error occured, stop here.
          if (err_CFML%Flag) return

          ! Allocating a character array for storing the header block line by line.
          if (allocated(filevar)) deallocate(filevar)
          allocate(filevar(n%header_size))

          ! Read the header and put it in the array.
          do i = 1, n%header_size
             read(lun,'(a)') filevar(i)
          end do

          ! Define the different blocks (i.e. RRRRRRR, IIIIIII ...) the header is made of.
          call Number_KeyTypes_on_File(filevar,n%header_size)

          ! Load information on nl_keytypes
          call Set_KeyTypes_on_File(filevar,n%header_size)

          ! Check format for D19
          call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)

          ! If the numor is not a D19 numor, stop here.
          if (index(u_case(line(1:4)),'D19') <= 0) then
             Err_CFML%Ierr=1
             Err_CFML%flag=.true.
             Err_CFML%Msg='This numor does not match with D19 Format'
             return
          end if

          ! Get the numor id.
          call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
          n%numor=numor
          ! Instr/Experimental Name/ Date
          call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
          if (idum > 0) then
             n%instrm=line(1:4)
             n%header=line(5:14)//"   "//line(15:32)
          end if
          ! Title/Sample
          call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
          if (idum > 0) then
             n%title=trim(line(1:60))
             n%scantype=trim(line(73:))
          end if

          ! Control Flags
          call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))
          if (nval_i > 0) then
             n%manip=ivalues(4)              ! 1: 2Theta, 2: Omega, 3:Chi, 4: Phi
             n%nbang=ivalues(5)              ! Total number of angles moved during scan
             n%nframes=ivalues(7)            ! Measured Frames. In general equal to those prescripted
             n% icalc=ivalues(9)
             n%nbdata=ivalues(24)            ! Number of Points
             n%icdesc(1:7)=ivalues(25:31)
          end if
          ! Real values
          call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
          if (nval_f > 0) then
             n%HMin=rvalues(1:3)          ! HKL min
             n%angles=rvalues(4:8)        ! Phi, Chi, Omega, 2Theta, Psi
             n%ub(1,:)=rvalues(9:11)      !
             n%ub(2,:)=rvalues(12:14)     ! UB Matrix
             n%ub(3,:)=rvalues(15:17)     !
             n%wave=rvalues(18)           ! Wavelength
             n%HMax=rvalues(22:24)        ! HKL max
             n%dh=rvalues(25:27)          ! Delta HKL
             n%dist=rvalues(30)           ! distance
             n%scans=rvalues(36:38)       ! Scan start, Scan step, Scan width
             n%preset=rvalues(39)         ! Preset
             n%cpl_fact=rvalues(43)       ! Coupling factor
             n%conditions=rvalues(46:50)  ! Temp-s, Temp-r, Temp-sample. Voltmeter, Mag.Field
          end if

       end if
       ! If the numor was only opened for reading the header, stops here.
       if(Instrm_Info_only) return

       if (allocated(n%selected_frames)) deallocate(n%selected_frames)

       ! Case where the user only wants a subset of frames to be included in the numor structure.
       if (present(frames)) then
          ! This temporary array will store only those of the selected frames that are in [1,nframes].
          allocate(temp_frames(n%nframes))
          n_selected_frames = 0
          do i = 1, size(frames)
             if (frames(i) < 1 .or. frames(i) > n%nframes) cycle
             n_selected_frames = n_selected_frames + 1
             temp_frames(n_selected_frames) = frames(i)
          end do

          ! If none of the selected frame fall in [1,nframes], stops here.
          if (n_selected_frames <= 0) then
             Err_CFML%Ierr = 1
             Err_CFML%Flag = .true.
             Err_CFML%Msg = " Invalid frames selection."
             return
          end if

          ! Sets the selected_frames field of the numor structure with the selected frames within [1,nframes].
          allocate(n%selected_frames(n_selected_frames))
          n%selected_frames(1:n_selected_frames) = temp_frames
          deallocate(temp_frames)
       ! Case where no frame selection was provided by the user. All the frames will be included in the numor structure.
       else
          allocate(n%selected_frames(n%nframes))
          n%selected_frames = [(i, i=1,n%nframes)]
          n_selected_frames = n%nframes
       end if

       ! At this point, we should normally be at the beginning of a frame block.
       read(lun,'(a)') line

       ! If so, read the line just after the SSSSSS line to get the id of the frame the file pointer is locaed on.
       if (line(1:10) /= repeat('S',10)) then
          ! The frame id is read.
          read(lun,*) previous_frame
          ! The file pointer is replaced just before the beginning of the corresponding frame block.
          backspace(lun)
          backspace(lun)
       ! If not so, the file pointer is replaced just before the beginning of the first frame block.
       else
          rewind(lun)
          do i = 1, n%header_size
             read(lun,'(a)') line
          end do
          previous_frame = 1
       end if

       ! Allocate the count array.
       if (allocated(n%counts)) deallocate(n%counts)
       allocate(n%counts(n%nbdata,n_selected_frames))
       n%counts=0.0
       ! Allocate the motor angles array.
       if (allocated(n%tmc_ang)) deallocate(n%tmc_ang)
       allocate(n%tmc_ang(n%nbang+3,n_selected_frames))
       n%tmc_ang=0.0

       ! Loop over the selected frames.
       do i=1,n_selected_frames

          ! This gives the number of frame block to skip to arrive just before the frame to read.
          n_skip = abs(n%selected_frames(i) - previous_frame - 1)

          ! Case where the frame to read is after the previous one.
          if (n%selected_frames(i) > previous_frame) then
             do j = 1, n_skip*n%frame_size
                read(lun,*) line
             end do
          ! Case where the frame to read is before the previous one.
          elseif (n%selected_frames(i) < previous_frame) then
             do j = 1, n_skip*n%frame_size
                backspace(lun)
             end do
          end if

          ! Allocating a character array for storing the frame block line by line.
          if (allocated(filevar)) deallocate(filevar)
          allocate(filevar(n%frame_size))

          ! Read the frame and put it in the array.
          do j = 1, n%frame_size
             read(lun,'(a)') filevar(j)
          end do

          ! Define the different blocks (i.e. RRRRRRR, IIIIIII ...) the frame is made of.
          call Number_KeyTypes_on_File(filevar,n%frame_size)

          ! Load information on nl_keytypes
          call Set_KeyTypes_on_File(filevar,n%frame_size)

          ! Time/Monitor/Counts/Angles
          call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))

          ! Check that the number of motors angles is correct.
          if (nval_f > 0 .and. nval_f == (n%nbang+3)) then
             n%tmc_ang(1,i)=rvalues(1)*0.001 ! Time (s)
             n%tmc_ang(2:3,i)=rvalues(2:3)
             n%tmc_ang(4:nval_f,i)=rvalues(4:nval_f)*0.001  ! Angle


          ! Case where the number of motors angles is incorrect. Stops here.
          else
             write(unit=car,fmt='(i5)') n%selected_frames(i)
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%Flag=.true.
             write(unit=Err_CFML%Msg,fmt="(a,i6.6)")'Problem reading Time, Monitor, Counts, Angles' &
                    //' parameters in the Frame: '//trim(car)//", Numor:",n%numor
             return
          end if

          ! Reads the count matrix.
          call read_I_keyType(filevar,nl_keytypes(5,1,1),nl_keytypes(5,1,2))

          ! Case of a mismatch between the number of elements of the matrix and its size indicated in the numor, stops here.
          if (nval_i /= n%nbdata) then
             write(unit=car,fmt='(i5)') n%selected_frames(i)
             car=adjustl(car)
             Err_CFML%Ierr=1
             Err_CFML%Flag=.true.
             Err_CFML%Msg='Problem reading Counts in the Frame: '//trim(car)
             return
          end if

          if (nval_i > 0) n%counts(:,i)=ivalues(1:n%nbdata)

          ! The id of frame just processed is set as the previous one.
          previous_frame = n%selected_frames(i)

       end do

       return

    End Subroutine Read_Numor_D19

    !!----
    !!---- Subroutine Read_Numor_Generic(filevar,N)
    !!----    character(len=*), intent(in) :: fileinfo
    !!----    type(generic_numor_type), intent(out) :: n
    !!----
    !!---- Read a Numor (Not yet operative)
    !!----
    !!---- Update: 11/03/2011
    !!
    Subroutine Read_Numor_Generic(fileinfo,N)
       !---- Arguments ----!
       character(len=*), intent(in) :: fileinfo
       type(generic_numor_type), intent(out) :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=40), dimension(5)              :: dire
       character(len=1024)                          :: line
       character(len=80)                            :: linec
       integer                                      :: nlines
       integer                                      :: i,j,numor,idum,nl

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg=' Problems trying to read the numor for D20 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Check format for D20
       call Number_KeyTypes_on_File(filevar,nlines)
       if (.not. equal_vector(n_keytypes,[1,2,1,6,0,1,0],7)) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg='This numor: '//trim(fileinfo)//' does not correspond with D20 Format'
          return
       end if

       ! Defining the different blocks
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instr=line(1:4)
          n%expname=line(5:14)
          n%date=line(15:32)
       end if

       ! Title/Sample
       if (allocated(n%sampleid%namevar)) deallocate(n%sampleid%namevar)
       if (allocated(n%sampleid%cvalues)) deallocate(n%sampleid%cvalues)

       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       select case (idum)
          case (1:80)
             n%SampleID%n=4
             allocate(n%sampleid%namevar(4))
             allocate(n%sampleid%cvalues(4))

             ! Given names
             n%sampleid%namevar=' '
             n%sampleid%namevar(1)='Name'
             n%sampleid%namevar(2)='Local Contact'
             n%sampleid%namevar(3)='Experimentalist'
             n%sampleid%namevar(4)='Proposal Number'

             ! Load values
             n%sampleid%cvalues=' '
             n%title=trim(line)
             n%sampleid%cvalues(1)=trim(line)

          case (81:)
             nl=(idum/80)+mod(idum,80)
             n%SampleID%n=nl
             allocate(n%sampleid%namevar(nl))
             allocate(n%sampleid%cvalues(nl))

             ! Give names
             do i=1,nl
                j=index(line,char(9))
                linec=line(1:j-1)
                line=line(j+1:)

                j=index(linec,':')
                n%sampleid%namevar(i)=linec(1:j-1)
                n%sampleid%cvalues(i)=trim(linec(j+1:))
             end do
       end select

       ! Diffractometers Optics / Reactor parameters
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%diffopt%n=nval_f
          if (allocated(n%diffopt%namevar)) deallocate(n%diffopt%namevar)
          if (allocated(n%diffopt%rvalues)) deallocate(n%diffopt%rvalues)
          allocate(n%diffopt%namevar(nval_f))
          allocate(n%diffopt%rvalues(nval_f))

          ! Given names
          n%diffopt%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%diffopt%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%diffopt%rvalues=rvalues(1:nval_f)
       end if

       ! Monochromator Motor Parameters
       call read_F_keyType(filevar,nl_keytypes(4,2,1),nl_keytypes(4,2,2))
       if (nval_f > 0) then
          n%monmpar%n=nval_f
          if (allocated(n%monmpar%namevar)) deallocate(n%monmpar%namevar)
          if (allocated(n%monmpar%rvalues)) deallocate(n%monmpar%rvalues)
          allocate(n%monmpar%namevar(nval_f))
          allocate(n%monmpar%rvalues(nval_f))

          ! Given names
          n%monmpar%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%monmpar%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%monmpar%rvalues=rvalues(1:nval_f)
       end if

       ! Diffractometer Motor Parameters
       call read_F_keyType(filevar,nl_keytypes(4,3,1),nl_keytypes(4,3,2))
       if (nval_f > 0) then
          n%diffmpar%n=nval_f
          if (allocated(n%diffmpar%namevar)) deallocate(n%diffmpar%namevar)
          if (allocated(n%diffmpar%rvalues)) deallocate(n%diffmpar%rvalues)
          allocate(n%diffmpar%namevar(nval_f))
          allocate(n%diffmpar%rvalues(nval_f))

          ! Given names
          n%diffmpar%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%diffmpar%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%diffmpar%rvalues=rvalues(1:nval_f)
       end if

       ! Detector and DAS Parameters
       call read_F_keyType(filevar,nl_keytypes(4,4,1),nl_keytypes(4,4,2))
       if (nval_f > 0) then
          n%detpar%n=nval_f
          if (allocated(n%detpar%namevar)) deallocate(n%detpar%namevar)
          if (allocated(n%detpar%rvalues)) deallocate(n%detpar%rvalues)
          allocate(n%detpar%namevar(nval_f))
          allocate(n%detpar%rvalues(nval_f))

          ! Given names
          n%detpar%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%detpar%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%detpar%rvalues=rvalues(1:nval_f)
       end if

       ! Data Acquisition Control Parameters
       call read_F_keyType(filevar,nl_keytypes(4,5,1),nl_keytypes(4,5,2))
       if (nval_f > 0) then
          n%dacparam%n=nval_f
          if (allocated(n%dacparam%namevar)) deallocate(n%dacparam%namevar)
          if (allocated(n%dacparam%rvalues)) deallocate(n%dacparam%rvalues)
          allocate(n%dacparam%namevar(nval_f))
          allocate(n%dacparam%rvalues(nval_f))

          ! Given names
          n%dacparam%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%dacparam%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%dacparam%rvalues=rvalues(1:nval_f)
       end if

       ! Sample Status
       call read_F_keyType(filevar,nl_keytypes(4,6,1),nl_keytypes(4,6,2))
       if (nval_f > 0) then
          n%samplest%n=nval_f
          if (allocated(n%samplest%namevar)) deallocate(n%samplest%namevar)
          if (allocated(n%samplest%rvalues)) deallocate(n%samplest%rvalues)
          allocate(n%samplest%namevar(nval_f))
          allocate(n%samplest%rvalues(nval_f))

          ! Given names
          n%samplest%namevar=' '
          j=0
          do i=2,ntext
             read(unit=text_ill(i),fmt='(5a16)') dire
             do nl=1,5
                j=j+1
                n%samplest%namevar(j)=dire(nl)
             end do
          end do

          ! Load values
          n%samplest%rvalues=rvalues(1:nval_f)
       end if

       ! Counts Information
       call read_J_keyType(filevar,nl_keytypes(6,1,1),nl_keytypes(6,1,2))

       if (nval_i > 0) then
          n%icounts%n=nval_i
          if (allocated(n%icounts%namevar)) deallocate(n%icounts%namevar)
          if (allocated(n%icounts%ivalues)) deallocate(n%icounts%ivalues)
          allocate(n%icounts%namevar(1))
          allocate(n%icounts%ivalues(nval_i))

          ! Given names
          n%icounts%namevar(1)=' N. of Counts'

          ! Load values
          n%icounts%ivalues=ivalues(1:nval_i)
       end if

    End Subroutine Read_Numor_Generic

    !!----
    !!---- Subroutine Read_Numor_D20(filevar,N)
    !!----    character(len=*), intent(in) :: fileinfo
    !!----    type(generic_numor_type), intent(out) :: n
    !!----
    !!---- Read a Numor for D20 Instrument at ILL
    !!----
    !!---- Update: 11/03/2011
    !!
    Subroutine Read_Numor_D20(fileinfo,N)
       !---- Arguments ----!
       character(len=*), intent(in) :: fileinfo
       type(powder_numor_type), intent(out) :: n

       !---- Local Variables ----!
       character(len=80), dimension(:), allocatable :: filevar
       character(len=1024)                          :: line
       integer                                      :: nlines
       integer                                      :: i,j,numor,idum

       call clear_error()

       ! Detecting numor
       nlines= Number_Lines(fileinfo)
       if (nlines <=0) then
          Err_CFML%Ierr = 1
          Err_CFML%Flag = .true.
          Err_CFML%Msg=' Problems trying to read the numor for D20 Instrument in file '//trim(fileinfo)
          return
       end if

       ! Allocating variables
       if (allocated(filevar)) deallocate(filevar)
       allocate(filevar(nlines))
       call Reading_Lines(fileinfo,nlines,filevar)

       ! Check format for D20
       call Number_KeyTypes_on_File(filevar,nlines)
       if (.not. equal_vector(n_keytypes,[1,2,1,6,0,1,0],7)) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg='This numor: '//trim(fileinfo)//' does not correspond with D20 Format'
          return
       end if

       ! Defining the different blocks
       call Set_KeyTypes_on_File(filevar,nlines)

       ! Initialize Numor
       call init_powder_numor(n)

       ! Fixed Some values
       n%nframes=1
       !n%nbdata=1600
       n%scantype='2theta'

       ! Numor
       call read_R_keyType(filevar,nl_keytypes(1,1,1),nl_keytypes(1,1,2),numor,idum)
       n%numor=numor

       ! Instr/Experimental Name/ Date
       call read_A_keyType(filevar,nl_keytypes(2,1,1),nl_keytypes(2,1,2),idum,line)
       if (idum > 0) then
          n%instrm=line(1:4)
          n%header=line(5:14)//"   "//line(15:32)
       end if


       ! Title
       call read_A_keyType(filevar,nl_keytypes(2,2,1),nl_keytypes(2,2,2),idum,line)
       if (idum > 0) then
          i=index(line,'Title :')
          j=index(line,'Local')
          if (i > 0) then
             n%title=line(i+7:j-2)
             n%title=trim(n%title)
          end if
          i=index(line,'SAMPLE             :')
          j=index(line,'Experimentalist')
          if( i > 0) then
             n%title=trim(line(i+20:j-2))//"--"//trim(n%title)
          end if
       end if

       ! Wave
       call read_F_keyType(filevar,nl_keytypes(4,1,1),nl_keytypes(4,1,2))
       if (nval_f > 0) then
          n%wave=rvalues(4)
       end if

       ! Monochromator Motor Parameters
       !call read_F_keyType(filevar,nl_keytypes(4,2,1),nl_keytypes(4,2,2))
       !if (nval_f > 0) then
       !end if

       ! Diffractometer Motor Parameters
       !call read_F_keyType(filevar,nl_keytypes(4,3,1),nl_keytypes(4,3,2))
       !if (nval_f > 0) then
       !   ! Zero 2Theta?
       !end if

       ! Detector and DAS Parameters
       !call read_F_keyType(filevar,nl_keytypes(4,4,1),nl_keytypes(4,4,2))
       !if (nval_f > 0) then
       !end if

       ! Data Acquisition Control Parameters
       call read_F_keyType(filevar,nl_keytypes(4,5,1),nl_keytypes(4,5,2))
       if (nval_f > 0) then
          n%monitor=rvalues(51)
          n%time=rvalues(52)
       end if

       ! Sample Status
       call read_F_keyType(filevar,nl_keytypes(4,6,1),nl_keytypes(4,6,2))
       if (nval_f > 0) then
          n%conditions(1:3)=rvalues(2:4)
          n%scans(1)=rvalues(5)
       end if

       if(Instrm_Info_only) return

       ! S Block

       ! Counts Information
       call read_J_keyType(filevar,nl_keytypes(6,1,1),nl_keytypes(6,1,2))
       n%nbdata = nval_i
       n%scans(2)=160.0/real(nval_i)

       if (nval_i > 0) then
          if (n%nbdata == nval_i) then
             allocate(n%counts(n%nbdata,1))
             n%counts(:,1)=real(ivalues(1:nval_i))
          end if
       end if

    End Subroutine Read_Numor_D20

    !!----
    !!---- Subroutine Read_Powder_Numor(PathNumor,Instrument,Num,inf)
    !!----    character(len=*),            intent(in)    :: PathNumor
    !!----    character(len=*),            intent(in)    :: Instrument
    !!----    type(Powder_Numor_type),     intent(out)   :: Num
    !!-----   logical, optional,           intent(in)    :: inf
    !!----
    !!----    Read a Powder numor from the ILL database.
    !!----    In case of error the subroutine puts Err_CFML%Ierr=1
    !!----    and fils the error message variable Err_CFML%Msg.
    !!----    If inf is present and .true. only the heaede information
    !!----    is read, the data are not loaded in num.
    !!----
    !!---- Update: 29/04/2011
    !!
    Subroutine Read_Powder_Numor(PathNumor,Instrument,Num,inf)
       !---- Arguments ----!
       character(len=*),           intent(in)    :: PathNumor
       character(len=*),           intent(in)    :: Instrument
       type(Powder_Numor_Type),    intent(out)   :: Num
       logical, optional,          intent(in)    :: inf

       !---- Local variables ----!
       character(len=512)     :: Path
       character(len=80)      :: Filename
       character(len=4)       :: Instr
       integer                :: n

       ! Initialize
       call clear_error()
       ! Check
       if (len_trim(PathNumor) <= 0 .or. len_trim(Instrument) <=0 ) return
       instr=u_case(adjustl(Instrument))

       ! Path + Numor
       n=index(pathnumor,ops_sep, back=.true.)
       if (n > 0) then
          path=pathnumor(:n)
          filename=pathnumor(n+1:)
       else
          path=' '
          filename=trim(pathnumor)
       end if

       ! Compressed Numor
       n=index(filename,'.Z')
       if (n > 0) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg= " Numor file is compressed. Please uncompress the numor before to use this routine"
          return
       end if

       ! Read Numor

       if(present(inf)) then
         if(inf) then
           Instrm_Info_only=.true.   !Temporarily set to .true.
         end if
       end if

       select case (trim(u_case(Instr)))
          case ('D1A')
             call Read_Numor_D1A(trim(path)//trim(filename),Num)

          case ('D1B')
             call Read_Numor_D1B(trim(path)//trim(filename),Num)

          case ('D2B')
             call Read_Numor_D2B(trim(path)//trim(filename),Num)

          case ('D4')
             call Read_Numor_D4(trim(path)//trim(filename),Num)

          case ('D20')
             call Read_Numor_D20(trim(path)//trim(filename),Num)

          case default
             Err_CFML%Ierr=1
             Err_CFML%Flag=.true.
             Err_CFML%Msg= " Not Implemented for the Powder Instrument name: "//trim(instrument)
       end select

       if(present(inf)) then
         if(inf) then
           Instrm_Info_only=.false.   !revert to .false.
         end if
       end if

    End Subroutine Read_Powder_Numor

    !!----
    !!---- Subroutine Read_SXTAL_Numor(PathNumor,Instrument,Num,inf)
    !!----    character(len=*),            intent(in)    :: PathNumor
    !!----    character(len=*),            intent(in)    :: Instrument
    !!----    type(SXTAL_Numor_type),      intent(out)   :: Num
    !!----    logical, optional,           intent(in)    :: inf
    !!----
    !!----    Read a SXTAL numor from the ILL database.
    !!----    In case of error the subroutine puts Err_CFML%Ierr=1
    !!----    and fils the error message variable Err_CFML%Msg.
    !!----
    !!---- Update: 29/04/2011
    !!
    Subroutine Read_SXTAL_Numor(filename,instrument,num,inf,frames)
       !---- Arguments ----!
       character(len=*)               , intent(in)    :: filename
       character(len=*)               , intent(in)    :: Instrument
       type(SXTAL_Numor_Type)         , intent(in out):: Num
       logical, optional              , intent(in)    :: inf
       integer, optional, dimension(:), intent(in)    :: frames

       !---- Local variables ----!
       character(len=4)       :: instr
       integer                :: n

       !write (*,*)  ' Read_SXTAL_Numor filename ',filename
       !write (*,*)  ' Read_SXTAL_Numor instrument ',instrument

       ! Initialize
       call clear_error()
       ! Check
       if (len_trim(filename) <= 0 .or. len_trim(instrument) <=0 ) return
       instr=u_case(adjustl(instrument))

       ! Compressed Numor
       n = index(filename,'.Z',back=.true.)
       if (n > 0) then
          Err_CFML%Ierr=1
          Err_CFML%Flag = .true.
          Err_CFML%Msg= " Numor file is compressed. Please uncompress the numor before to use this routine"
          return
       end if

       ! Read Numor
       if(present(inf)) then
         if(inf) then
           Instrm_Info_only=.true.   !Temporarily set to .true.
         end if
       end if

       select case (trim(Instr))
          case ('D9')
             call Read_Numor_D9(trim(filename),num)

          case ('D10')
             call Read_Numor_D10(trim(filename),num)

          case ('D16')
             call Read_Numor_D16(trim(filename),num)

          case ('D19','D19_HB')
             write(*,"(a)") ' => Reading ASCII files'
             call Read_Numor_D19(trim(filename),num,frames)
          case default
             Err_CFML%Ierr=1
             Err_CFML%Flag=.true.
             Err_CFML%Msg= " Not Implemented for the SXTAL Instrument name: "//trim(instrument)
       end select

       if(present(inf)) then
         if(inf) then
           Instrm_Info_only=.false.   !revert to .false.
         end if
       end if

    End Subroutine Read_SXTAL_Numor

    !!--++
    !!--++ Subroutine Read_R_KeyType(filevar, n_ini, n_end, nrun, nvers)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++    integer,                        intent(out) :: nrun      ! Run number of the data
    !!--++    integer,                        intent(out) :: nvers     ! Version data
    !!--++
    !!--++ (Private)
    !!--++ Read the Blocktype R on Numor at ILL.
    !!--++ If text is readen, then it is saved on Text_ILL variable and the number of lines is
    !!--++ saved on ntext
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_R_KeyType(filevar, n_ini, n_end, nrun, nvers)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end
       integer,                        intent(out) :: nrun
       integer,                        intent(out) :: nvers

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j
       integer, dimension(10) :: ivet

       ! Init output values
       call clear_error()

       nrun =0
       nvers=0
       ntext=0
       if (allocated(text_ill)) deallocate(text_ill)

       ! Check the correct KeyType R
       line=filevar(n_ini)
       if (line(1:10) /= 'RRRRRRRRRR') then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg=' A bad Block R-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       nrun =ivet(1)   ! Numor
       ntext=ivet(2)   ! Number of lines of descriptive text
       nvers=ivet(3)   ! Version of the data

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+1)+i
             if (j > n_end) then
                Err_CFML%Ierr=1
                Err_CFML%Flag=.true.
                Err_CFML%Msg='  Impossible to read a line for this block!'
                exit
             end if
             text_ill(i)=trim(filevar(j))
          end do
       end if

    End Subroutine Read_R_KeyType

    !!--++
    !!--++ Subroutine Read_S_KeyType(filevar, n_ini, n_end, ispec, nrest, ntot, nrun, npars)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++    integer,                        intent(out) :: ispec     ! Sub-spectrum number
    !!--++    integer,                        intent(out) :: nrest     ! Number of subspectra remaining after ispec
    !!--++    integer,                        intent(out) :: ntot      ! Total number of subspectra in the run
    !!--++    integer,                        intent(out) :: nrun      ! Current run number
    !!--++    integer,                        intent(out) :: npars     ! Number of parameters sections
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_S_KeyType(filevar, n_ini, n_end, ispec, nrest, ntot, nrun, npars)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end
       integer,                        intent(out) :: ispec
       integer,                        intent(out) :: nrest
       integer,                        intent(out) :: ntot
       integer,                        intent(out) :: nrun
       integer,                        intent(out) :: npars

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j
       integer, dimension(10) :: ivet

       ! Init output variables
       call clear_error()

       ispec=0
       nrest=0
       ntot=0
       nrun=0
       ntext=0
       npars=0
       if (allocated(text_ill)) deallocate(text_ill)

       ! Check the correct KeyType S
       line=filevar(n_ini)
       if (line(1:10) /= 'SSSSSSSSSS') then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg=' A bad Block S-Type has been found'
          return
       end if

       ! Getting information
       read(unit=filevar(n_ini+1),fmt='(10i8)') ivet
       ispec=ivet(1)
       nrest=ivet(2)
       ntot =ivet(3)
       nrun =ivet(4)
       ntext=ivet(5)
       npars=ivet(6)

       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          do i=1,ntext
             j=(n_ini+1)+i
             if (j > n_end) then
                Err_CFML%Ierr=1
                Err_CFML%Flag=.true.
                Err_CFML%Msg=' Impossible to read a line for this block!'
                exit
             end if
             text_ill(i)=trim(filevar(j))
          end do
       end if

    End Subroutine Read_S_KeyType

    !!--++
    !!--++ Subroutine Read_V_KeyType(filevar, n_ini, n_end)
    !!--++    character(len=*), dimension(:), intent(in)  :: filevar   ! Input information
    !!--++    integer,                        intent(in)  :: n_ini     ! First line
    !!--++    integer,                        intent(in)  :: n_end     ! Last line to be read
    !!--++
    !!--++ Update: April - 2009
    !!
    Subroutine Read_V_KeyType(filevar, n_ini, n_end)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)  :: filevar
       integer,                        intent(in)  :: n_ini
       integer,                        intent(in)  :: n_end

       !---- Local Variables ----!
       character(len=80)      :: line
       integer                :: i,j

       ! Init output values
       call clear_error()
       ntext=0
       if (allocated(text_ill)) deallocate(text_ill)

       ! Check the correct KeyType V
       line=filevar(n_ini)
       if (line(1:10) /= 'VVVVVVVVVV') then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg=' A bad Block V-Type has been found'
          return
       end if

       ! Getting information
       ntext=n_end-n_ini-1
       if (ntext > 0) then
          allocate(text_ill(ntext))
          text_ill=' '
          j=0
          do i=n_ini+1,n_end
             j=j+1
             text_ill(j)=filevar(i)
          end do
       end if

    End Subroutine Read_V_KeyType

    !!----
    !!---- Subroutine Set_Current_Orient(wave,ub,setting)
    !!----   real(kind=cp),                         intent(in)   :: wave
    !!----   real(kind=cp), dimension(3,3),         intent(in)   :: ub
    !!----   real(kind=cp), dimension(3,3),optional,intent(in)   :: setting
    !!----
    !!----    Subroutine setting the Current_Orient global variable
    !!----    If the final UB matrix is singular an error is rised
    !!----    by putting Err_CFML%Ierr=1 and filling the
    !!----    error message variable Err_CFML%Msg.
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Set_Current_Orient(wave,ub,setting)
       !---- Argument ----!
       real(kind=cp),                         intent(in)   :: wave
       real(kind=cp), dimension(3,3),         intent(in)   :: ub
       real(kind=cp), dimension(3,3),optional,intent(in)   :: setting

       !--- Local variables ---!
       !real(kind=cp)                 :: det
       real(kind=cp), dimension(3,3) :: mat

       Current_Orient%conv=reshape( [1.0,0.0,0.0,  0.0,1.0,0.0,  0.0,0.0,1.0],[3,3])

       if (present(setting)) then
          mat=matmul(setting,ub)  !check that point
          Current_Orient%conv=setting
       else
          mat=ub
       end if

       Current_Orient%orient_set=.true.
       Current_Orient%wave=wave
       Current_Orient%ub=mat
       Current_Orient%ubinv=Invert(mat)

    End Subroutine Set_Current_Orient

    !!----
    !!---- Subroutine Set_Default_Instrument(typ,wav)
    !!----    Character(len=*),            optional, intent(in) :: typ  !"Laue" for a Laue diffractometer
    !!----    real(kind=cp), dimension(2), optional, intent(in) :: wav  ! Lambda min and Lambda max
    !!----
    !!----    Construct the Current_Instrument as a default 4C diffractometer
    !!----    (or a Laue diffractometer if Typ and wav are provided)
    !!----    The UB matrix is set to a real matrix corresponding to a measurement
    !!----    done on D9. The characteristics of the diffractometer correspond to
    !!----    those of D9
    !!----
    !!---- Update: May - 2007
    !!
    Subroutine Set_Default_Instrument(typ,wav)
       Character(len=*),            optional, intent(in) :: typ
       real(kind=cp), dimension(2), optional, intent(in) :: wav
       !---- Local Variables ----!
       real(kind=cp)                 :: wave
       integer                       :: npx, npz
       real(kind=cp), dimension(3,3) :: ub

       Current_Instrm%det_offsets=0.0
       Current_Instrm%disp_offsets=0.0
       Current_Instrm%disp_Limits=0.0
       Current_Instrm%ang_Limits=0.0
       Current_Instrm%disp_names=" "
       Current_Instrm%ang_names=" "
       Current_Instrm%e1=[1.0,0.0,0.0]
       Current_Instrm%e2=[0.0,1.0,0.0]
       Current_Instrm%e3=[0.0,0.0,1.0]
       if(present(typ)) then
         Current_Instrm%info= "Default Laue diffractrometer"
         Current_Instrm%name_inst= "LaueDiff"
         Current_Instrm%geom="Laue"
         if(present(wav)) then
           Current_Instrm%wave_min=wav(1)
           Current_Instrm%wave_max=wav(2)
         else
           Current_Instrm%wave_min=0.7
           Current_Instrm%wave_max=5.5
         end if
         Current_Instrm%igeom=3
         wave=2.0
         Current_Instrm%dist_samp_detector=488.0
         Current_Instrm%np_horiz= 2560
         Current_Instrm%np_vert=  1980
         Current_Instrm%horiz= 256.0
         Current_Instrm%vert=  198.0
         npx = 2560
         npz = 1980
         Current_Instrm%agap=0
         Current_Instrm%cgap=0
         Current_Instrm%ang_names(1) ="gamm"
         Current_Instrm%ang_Limits(1,1:2)=[2.0,175.0]
       else
         Current_Instrm%info= "Default 4-cercles diffractrometer"
         Current_Instrm%name_inst= "4C-Diff"
         Current_Instrm%geom="4C-Diff High-Chi, Eulerian cradle"
         Current_Instrm%igeom=2
         wave=0.71
         Current_Instrm%dist_samp_detector=488.0
         Current_Instrm%np_horiz= 32
         Current_Instrm%np_vert= 32
         Current_Instrm%horiz= 64.0
         Current_Instrm%vert=  64.0
         npx = 32
         npz = 32
         Current_Instrm%agap=2
         Current_Instrm%cgap=2
         Current_Instrm%ang_names(1) ="2Theta"
         Current_Instrm%ang_Limits(1,1:2)=[2.0,130.0]
       end if
       Current_Instrm%BL_frame="z-up"
       Current_Instrm%dist_units = "mm"
       Current_Instrm%angl_units = "deg"
       Current_Instrm%detector_type = "Flat_rect"
       Current_Instrm%ipsd=2          !Flat detector
       ub=reshape([-0.0989455,   0.0671905,  -0.1005396, &
                    0.0045075,  -0.1487497,  -0.0642365, &
                   -0.1588914,  -0.0460609,   0.0607861],[3,3])
       Current_Instrm%num_ang=4
       Current_Instrm%num_disp=0
       Current_Instrm%ang_names(2) ="Omega"
       Current_Instrm%ang_names(3) ="Chi"
       Current_Instrm%ang_names(4) ="Phi"
       Current_Instrm%ang_Limits(2,1:2)=[1.0,49.0]
       Current_Instrm%ang_Limits(3,1:2)=[77.0,202.0]
       Current_Instrm%ang_Limits(4,1:2)=[-180.0,180.0]
       if (allocated(Current_Instrm%alphas)) deallocate(Current_Instrm%alphas)
       allocate(Current_Instrm%alphas(npz,npx))
       Current_Instrm%alphas(:,:)=1.0
       if (allocated(Current_Instrm%shifts)) deallocate(Current_Instrm%shifts)
       allocate(Current_Instrm%shifts(npx))
       Current_Instrm%shifts(:)=0.0
       call Set_Current_Orient(wave,ub)
       Current_Instrm_set=.true.

    End Subroutine Set_Default_Instrument

    !!----
    !!---- Subroutine Set_ILL_Data_Directory(Filedir)
    !!----    character(len=*), intent(in) :: Filedir  !proposed location of ILL data
    !!----
    !!----    Assign the global public variable: ILL_data_directory
    !!----    If the directory doesn't exist the subroutine rises an error condition
    !!----    by putting Err_CFML%Ierr=1 and filling the error message
    !!----    variable Err_CFML%Msg.
    !!----
    !!---- Update: 08/03/2011
    !!
    Subroutine Set_ILL_Data_Directory(Filedir)
       !---- Arguments ----!
       character(len=*), intent(in) :: Filedir

       !---- Local Variables ----!
       integer            :: i

       !> Initialize
       call clear_error()
       Err_CFML%Msg=' '

       if (len_trim(filedir) == 0) then
          ILL_data_directory =" "      !data are in the current directory
          got_ILL_data_directory=.true.
          return
       end if

       !> Add separator if absent and ILL_data_directory is not the current directory
       i=len_trim(filedir)
       if (filedir(i:i) /= ops_sep) then
          ILL_data_directory=trim(filedir)//ops_sep
       else
          ILL_data_directory=trim(filedir)
       end if

       !> Check that the directory exist, otherwise rise an error condition
       if (.not. directory_exists(trim(ILL_data_directory))) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The ILL directory: '"//trim(ILL_data_directory)//"' doesn't exist"
          got_ILL_data_directory=.false.
       else
          got_ILL_data_directory=.true.
       end if

    End Subroutine Set_ILL_Data_Directory

    !!----
    !!---- Subroutine Set_Instrm_directory(working_dir,instrm, iyear, icycle)
    !!----    character(len=*),  intent(in), optional :: instrm         ! name of the diffractometer
    !!----    character(len=*),  intent(in), optional :: working_dir    ! Data directory to search
    !!----    character(len=*),  intent(in), optional :: iyear          ! Year for search Numor
    !!----    character(len=*),  intent(in), optional :: icycle         ! Cycle for Search Numor
    !!----
    !!----    Assign the global public variable: Instrm_directory
    !!----    It is assumed that the subroutine Set_ILL_data_directory has already been called.
    !!----
    !!----    1- Look at the working_dir to define Instrm_directory
    !!----    2- Arguments instrm, iyear and icycle is used with ILL_Data_Directory
    !!----    3- If not year and cycle then use "data"
    !!----    If the directory doesn't exist the subroutine rises an error condition
    !!----    by putting Err_CFML%Ierr=1 and filling the error message
    !!----    variable Err_CFML%Msg.
    !!----
    !!---- Update: 08/03/2011
    !!
    Subroutine Set_Instrm_Directory(working_dir, instrm, iyear, icycle)
       !---- Argument ----!
       character(len=*),  intent(in), optional :: working_dir
       character(len=*),  intent(in), optional :: instrm  !Name of the instrument
       integer         ,  intent(in), optional :: iyear
       integer         ,  intent(in), optional :: icycle

       !---- Local Variables ----!
       integer          :: i
       character(len=5) :: yearcycle

       ! Initialize
       call clear_error()
       Err_CFML%Msg=""

       ! If a working directory is given as an argument, then use it directly as the instrument
       ! directory.
       if ( Present(working_dir) ) then
           Instrm_directory = trim(working_dir)
           i=len_trim(Instrm_directory)
           if ( i > 0) then
              if (Instrm_directory(i:i) /= ops_sep) Instrm_directory=trim(Instrm_directory)//ops_sep
           else
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg="Provided working directory string is empty"
              return
           end if
       else
           if ( Present(instrm) ) then
              ! If an instrument name is given as argument, then build the instrument directory
              ! using the path of the ILL internal database.
              ! If the year and the cycle are given as arguments, then construct the yearcycle directory.
              i=len_trim(instrm)
              if (i > 0) then
                 if ( Present(iyear) .and. Present(icycle) ) then
                    Write(Unit=Yearcycle, fmt='(i4.4,i1.1)') iyear, icycle
                    yearcycle = yearcycle(len_trim(yearcycle)-2:len_trim(yearcycle))
                    Instrm_directory = trim(ILL_data_directory)//trim(yearcycle)//ops_sep//trim(instrm)//ops_sep
                 else
                    ! Otherwise, use the current data directory as the base location for the instrument directory.
                    Instrm_directory = trim(ILL_data_directory)//'data'//ops_sep//trim(instrm)//ops_sep
                 end if
              else
                 Err_CFML%Ierr=1
                 Err_CFML%Flag=.true.
                 Err_CFML%Msg="An instrument name must be at least provided"
                 return
              end if
           else
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg="A working directory or an instrument name must be at least provided."
              return
           end if
       end if

       !---- check that the directory exist, ----!
       !---- otherwise raise an error condition ----!
       Instrm_directory_set=directory_exists(trim(Instrm_directory))
       if (.not. Instrm_directory_set) then
          Err_CFML%Ierr=1
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The INSTRM directory: '"//trim(Instrm_directory)//"' doesn't exist"
          Instrm_directory = " "
       end if

    End Subroutine Set_Instrm_Directory
    !!----
    !!---- Subroutine Set_Instrm_Geometry_Directory(env_var)
    !!----    character(len=*),  intent(in), optional :: env_var
    !!----
    !!----    Assign the global public variable: Instrm_Geometry_Directory
    !!----    The optional intent 'in' variable env_var is the name of any
    !!----    directory set as the instrument geometry directory.
    !!----    If the directory doesn't exist the subroutine rises an error condition
    !!----    by putting Err_CFML%Ierr=1 and filling the error message
    !!----    variable Err_CFML%Msg. If the subroutine is invoked without argument
    !!----    the Instrm_Geometry_Directory="" corresponds to the current directory.
    !!----
    !!---- Update: July - 2010
    !!
    Subroutine Set_Instrm_Geometry_Directory(env_var)
       !---- Argument ----!
       character(len=*),  intent(in), optional :: env_var

       !---- Local Variables ----!
       integer:: i

       ! If an environment variable is given as an argument, then use it directly as the instrument
       ! geometry directory, otherwise take the current directory as the place where geometry instrument
       ! files are stored.
       if ( Present(env_var) ) then
           Instrm_Geometry_directory = trim(env_var)
           i=len_trim(Instrm_Geometry_Directory)
           if (Instrm_Geometry_Directory(i:i) /= ops_sep) Instrm_Geometry_Directory=trim(Instrm_Geometry_Directory)//ops_sep
           !---- check that the directory exist, ----!
           !---- otherwise raise an error condition ----!
           call clear_error()
           Err_CFML%Msg=" "
           Instrm_Geometry_directory_set=directory_exists(trim(Instrm_Geometry_Directory))
           if (.not. Instrm_Geometry_directory_set) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=&
              "The INSTRM Geometry directory doesn't exist, current directory assumed"
              Instrm_Geometry_Directory = " "
           end if
       else
           Instrm_Geometry_Directory = " "       !Current directory for searching .geom files
           Instrm_Geometry_directory_set=.true.
       end if

    End Subroutine Set_Instrm_Geometry_Directory

    !!--++
    !!--++ Subroutine Set_KeyTypes_on_File(filevar, nlines)
    !!--++    character(len=*),dimension(:), intent(in) :: filevar
    !!--++    integer,                       intent(in) :: nlines
    !!--++
    !---++ Set the information on NL_KEYTYPES
    !!--++
    !!--++ Update: April-2009
    !!
    Subroutine Set_KeyTypes_on_File(filevar, nlines)
       !---- Arguments ----!
       character(len=*),dimension(:), intent(in) :: filevar
       integer,                       intent(in) :: nlines

       !---- Local Variables ----!
       integer :: i,j,nl,ndim

       if (allocated(nl_keytypes)) deallocate(nl_keytypes)
       if (all(n_keytypes == 0)) return

       ndim=maxval(n_keytypes)
       allocate(nl_keytypes(7,ndim,2))
       nl_keytypes=0

       do i=1,7
          if (n_keytypes(i) == 0) cycle
          j=1
          do nl=1,nlines
             select case (i)
                case (1)
                   if (nl_keytypes(1,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'RRRRRRRRRR') cycle
                      nl_keytypes(1,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(1,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR') then
                         nl_keytypes(1,j,1)=nl
                      end if
                   end if

                case (2)
                   if (nl_keytypes(2,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'AAAAAAAAAA') cycle
                      nl_keytypes(2,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(2,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'AAAAAAAAAA') then
                         nl_keytypes(2,j,1)=nl
                      end if
                   end if

                case (3)
                   if (nl_keytypes(3,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'SSSSSSSSSS') cycle
                      nl_keytypes(3,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(3,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'SSSSSSSSSS') then
                         nl_keytypes(3,j,1)=nl
                      end if
                   end if

                case (4)
                   if (nl_keytypes(4,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'FFFFFFFFFF') cycle
                      nl_keytypes(4,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(4,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'FFFFFFFFFF') then
                         nl_keytypes(4,j,1)=nl
                      end if
                   end if

                case (5)
                   if (nl_keytypes(5,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'IIIIIIIIII') cycle
                      nl_keytypes(5,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(5,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'IIIIIIIIII') then
                         nl_keytypes(5,j,1)=nl
                      end if
                   end if

                case (6)
                   if (nl_keytypes(6,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'JJJJJJJJJJ') cycle
                      nl_keytypes(6,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(6,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'JJJJJJJJJJ') then
                         nl_keytypes(6,j,1)=nl
                      end if
                   end if

                case (7)
                   if (nl_keytypes(7,j,1) == 0) then
                      if (filevar(nl)(1:10) /= 'VVVVVVVVVV') cycle
                      nl_keytypes(7,j,1)=nl
                   else
                      if (filevar(nl)(1:10) == 'RRRRRRRRRR' .or. &
                          filevar(nl)(1:10) == 'AAAAAAAAAA' .or. &
                          filevar(nl)(1:10) == 'SSSSSSSSSS' .or. &
                          filevar(nl)(1:10) == 'FFFFFFFFFF' .or. &
                          filevar(nl)(1:10) == 'IIIIIIIIII' .or. &
                          filevar(nl)(1:10) == 'JJJJJJJJJJ' .or. &
                          filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(7,j,2)=nl-1
                         j=j+1
                         if (j > n_keytypes(i)) exit
                      end if
                      if (filevar(nl)(1:10) == 'VVVVVVVVVV') then
                         nl_keytypes(7,j,1)=nl
                      end if
                   end if
             end select
          end do
       end do

       do i=1,7
          do j=1,n_keytypes(i)
             if (nl_keytypes(i,j,1) /= 0 .and. nl_keytypes(i,j,2)==0) then
                nl_keytypes(i,j,2)=nlines
                exit
             end if
          end do
       end do

    End Subroutine Set_KeyTypes_on_File

    !!----
    !!---- Subroutine Update_Current_Instrm_UB(filenam,UB,wave)
    !!----    character(len=*),              intent(in) :: filenam
    !!----    real(kind=cp), dimension(3,3), intent(in) :: UB
    !!----    real(kind=cp),                 intent(in) :: wave
    !!----
    !!----    Subroutine updating the file 'filenam' where the characteristics
    !!----    of the current instrument are written. The global Current_Instrm
    !!----    variable is re-filled with new values of wavelength and UB-matrix.
    !!----    The file 'filenam' is re-written and the old version is saved with
    !!----    appended extension '.bak'.
    !!----    The Current_Orient global variable is also updated.
    !!----    In case of error the subroutine puts Err_CFML%Ierr=1
    !!----    and fils the error message variable Err_CFML%Msg.
    !!----
    !!---- Update: December - 2005
    !!
    Subroutine Update_Current_Instrm_UB(filenam,UB,wave)
       !---- Arguments ----!
       character(len=*),              intent(in) :: filenam
       real(kind=cp), dimension(3,3), intent(in) :: UB
       real(kind=cp),                 intent(in) :: wave

       !---- Local variables ----!
       character(len=120), dimension(:), allocatable :: file_lines
       character(len=120)            :: line
       character(len=10)             :: key
       integer                       :: i, j, lun_out, lun, ier, nlines,jw,iw,iub
       real(kind=cp), dimension(3,3) :: set
       logical                       :: read_wave, read_UB

       if(.not. Current_Instrm_set) then
         Err_CFML%Ierr=1
         Err_CFML%Flag=.true.
         Err_CFML%Msg=" Current Instrument not set! (call subroutine: Read_Current_Instrm) "
         return
       end if

       nlines = Number_Lines(filenam)
       if(nlines == 0) then
         Err_CFML%Ierr=1
         Err_CFML%Flag=.true.
         Err_CFML%Msg="Error opening the file: "//trim(filenam)//" => ZERO lines found!"
         return
       end if

       if(allocated(file_lines)) deallocate(file_lines)
       allocate(file_lines(nlines+10))

       open(newunit=lun,file=trim(filenam),status="old", action="read", position="rewind",iostat=ier)
       if(ier /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%Flag=.true.
         Err_CFML%Msg="Error opening the file: "//trim(filenam)
         return
       end if

       open(newunit=lun_out,file=trim(filenam)//".bak",status="replace", action="write",iostat=ier)
       if(ier /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%Flag=.true.
         Err_CFML%Msg="Error opening the backup file: "//trim(filenam)//".bak"
         return
       end if

       do i=1,nlines
          read(unit=lun,    fmt="(a)") line
         write(unit=lun_out,fmt="(a)") line
         file_lines(i)=line
       end do
       close(unit=lun)
       close(unit=lun_out)

       read_wave=.false.
       read_UB=.false.
       jw=0

       do i=1,nlines
          line=adjustl(file_lines(i))
          if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
          j=index(line," ")
          key=u_case(line(1:j-1))

          Select Case(key)
             Case("WAVE")
                read_wave=.true.
                jw = j
                iw = i
             Case("UBMAT")
                read_UB=.true.
                iub=i
          End Select

          if( read_wave .and. read_UB) exit
       End do
       if(iw == 0) then
          write(unit=file_lines(nlines+1),fmt="(a,f10.5)") "WAVE ",wave
          nlines=nlines+1
       else
          !Update the corresponding line
          write(unit=file_lines(iw)(jw+1:), fmt="(f10.5)") wave
       end if
       if(iub == 0) Then
         write(unit=file_lines(nlines+1),fmt="(a)") "UBMAT"
         nlines=nlines+1
         do i=1,3
            write(unit=file_lines(nlines+i),   fmt="(3f16.9)") ub(i,:)
         end do
         nlines=nlines+3
       else
         do i=1,3
            write(unit=file_lines(iub+i),   fmt="(3f16.9)") ub(i,:)
         end do
       end if

       set(:,1)=Current_Instrm%e1
       set(:,2)=Current_Instrm%e2
       set(:,3)=Current_Instrm%e3
       call Set_Current_Orient(wave,ub,set)

       open(newunit=lun,file=trim(filenam),status="replace", action="write",iostat=ier)
       if(ier /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%Flag=.true.
         Err_CFML%Msg="Error updating the file: "//trim(filenam)
         return
       end if

       do i=1,nlines
          write(unit=lun,fmt="(a)",iostat=ier) file_lines(i)
          if(ier /= 0) then
            Err_CFML%Ierr=1
            Err_CFML%Flag=.true.
            write(unit=Err_CFML%Msg,fmt="(a,i4)")"Error updating the file: "//trim(filenam)//" at line:",i
            exit
          end if
       end do
       close(unit=lun)

    End Subroutine Update_Current_Instrm_UB

    !!----
    !!---- Subroutine Write_Current_Instrm_data(lun,fil)
    !!----    integer,         optional, intent(in) :: lun
    !!----    character(len=*),optional, intent(in) :: fil
    !!----
    !!----    Writes the characteristics of the Current Instrument
    !!----    in the file of logical unit 'lun'
    !!----    If the subroutine is invoked without argument the subroutine
    !!----    outputs the information on the standard output (screen)
    !!----    If the second argument (fil) is provided
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Write_Current_Instrm_data(lun,fil)
       !---- Arguments ----!
       integer,         optional, intent(in) :: lun
       character(len=*),optional, intent(in) :: fil

       !--- Local variables ---!
       integer           :: ipr,i,j,n3
       character(len=16) :: forma


       ipr=6
       if (present(lun)) ipr=lun
       if (.not. present(fil)) then
          write(unit=ipr,fmt="(a)") " ---------------------------------------------------------------------------"
          write(unit=ipr,fmt="(a)") " ---   INFORMATION ABOUT THE CURRENT INSTRUMENT AND ORIENTATION MATRIX   ---"
          write(unit=ipr,fmt="(a)") " ---------------------------------------------------------------------------"
          write(unit=ipr,fmt="(a)") " "

          write(unit=ipr,fmt="(a)")          "        INFO: "//Current_Instrm%info
          write(unit=ipr,fmt="(a)")          "        NAME: "//Current_Instrm%name_inst
          write(unit=ipr,fmt="(a)")          "        GEOM: "//Current_Instrm%geom
          write(unit=ipr,fmt="(a)")          "    BL_FRAME: "//Current_Instrm%BL_frame
          write(unit=ipr,fmt="(a)")          "  DIST_UNITS: "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a)")          "  ANGL_UNITS: "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a)")          "    DET_TYPE: "//Current_Instrm%detector_type
          write(unit=ipr,fmt="(a,f8.3,a)")   "  DIST_SDETR: ", Current_Instrm%dist_samp_detector," "//Current_Instrm%dist_units
          if(index(Current_Instrm%geom,"Laue") /= 0) then
            write(unit=ipr,fmt="(a,f8.3,a)") "  LAMBDA_MIN: ", Current_Instrm%wave_min," Angstroms"
            write(unit=ipr,fmt="(a,f8.3,a)") "  LAMBDA_MAX: ", Current_Instrm%wave_max," Angstroms"
          end if
          write(unit=ipr,fmt="(a,f8.3,a)")   "  HORIZ_SIZE: ", Current_Instrm%horiz," "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a,f8.3,a)")   "   VERT_SIZE: ", Current_Instrm%vert ," "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a,f8.3,a)")   "   ANODE_GAP: ", Current_Instrm%agap," "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a,f8.3,a)")   " CATHODE_GAP: ", Current_Instrm%cgap ," "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a,i5    )")   "  HORIZ_PXEL: ", Current_Instrm%np_horiz
          write(unit=ipr,fmt="(a,i5    )")   "   VERT_PXEL: ", Current_Instrm%np_vert
          write(unit=ipr,fmt="(a,2f9.3)")    "   GN_CENTRE: ", Current_Instrm%ga_d, Current_Instrm%nu_d

          if (Current_Instrm%num_ang > 0) then
             write(unit=ipr,fmt="(a,i5    )")   "  NUMBER_ANG: ", Current_Instrm%num_ang
             write(unit=ipr,fmt="(a    )")      "  Names of Angular Motors, Limits (Min-Max), Offsets and velocities (deg/s):   "
             do i=1,Current_Instrm%num_ang
                write(unit=ipr,fmt="(a,4f9.3)")  "         "//Current_Instrm%ang_names(i), &
                Current_Instrm%ang_limits(i,1:2), Current_Instrm%ang_offsets(i),Current_Instrm%ang_velocity(i)
             end do
          end if

          if (Current_Instrm%num_disp > 0) then
             write(unit=ipr,fmt="(a,i5    )")   "  NUMBER_DISP: ", Current_Instrm%num_disp
             write(unit=ipr,fmt="(a    )")      "  Names of Displacement Motors, Limits (Min-Max) and Offsets:   "
             do i=1,Current_Instrm%num_disp
                write(unit=ipr,fmt="(a,3f9.3)")   "         "//Current_Instrm%disp_names(i), &
                Current_Instrm%ang_limits(i,1:2), Current_Instrm%disp_offsets(i)
             end do
          end if

          write(unit=ipr,fmt="(a,3f8.2 )")   "  DET_OFFSET: ", Current_Instrm%det_offsets(1:3)
          write(unit=ipr,fmt="(a,3f8.3 )")   "   e1_COMPNT: ", Current_Instrm%e1
          write(unit=ipr,fmt="(a,3f8.3 )")   "   e2_COMPNT: ", Current_Instrm%e2
          write(unit=ipr,fmt="(a,3f8.3 )")   "   e3_COMPNT: ", Current_Instrm%e3
          if(Current_Orient%orient_set) then
             write(unit=ipr,fmt="(a)") " "
             write(unit=ipr,fmt="(a)") "       ORIENTATION MATRIX  UB                       INVERSE OF UB-MATRIX"
             write(unit=ipr,fmt="(a)") " ---------------------------------------------------------------------------"
             write(unit=ipr,fmt="(a)") " "
             write(unit=ipr,fmt="(tr2,3f11.6,tr8,3f11.5)")  Current_Orient%ub(1,:), Current_Orient%ubinv(1,:)
             write(unit=ipr,fmt="(tr2,3f11.6,tr8,3f11.5)")  Current_Orient%ub(2,:), Current_Orient%ubinv(2,:)
             write(unit=ipr,fmt="(tr2,3f11.6,tr8,3f11.5)")  Current_Orient%ub(3,:), Current_Orient%ubinv(3,:)
             write(unit=ipr,fmt="(a)") " "
             if(index(Current_Instrm%geom,"Laue") == 0) write(unit=ipr,fmt="(a,f8.4,a)")  &
                                            "  WAVELENGTH:",Current_Orient%wave," angstroms"
          end if
          if (Current_Instrm%rangtim) then
             write(unit=ipr,fmt="(a)") "RANGE_TIME:"
             do i=1,Current_Instrm%num_ang
               n3=count(Current_Instrm%range_time(1,:,i) > 0.0001)
               write(unit=ipr,fmt="(tr12,a,i4)")  Current_Instrm%ang_names(i),n3
               do j=1,n3
                  write(unit=ipr,fmt="(tr12,2f10.4)") Current_Instrm%range_time(:,j,i)
               end do
             end do
          end if

          if (Current_Instrm%resol_given) then
             write(unit=ipr,fmt="(/,a)") "RESOLUTION SURFACE OF THE INSTRUMENT: "//trim(Current_Instrm%name_inst)
             n3=Current_Instrm%nGa
             do i=1,Current_Instrm%nNu
                write(unit=ipr,fmt="(tr12,20f10.4)") Current_Instrm%ReSurf(i,1:n3)
             end do
          else
             write(unit=ipr,fmt="(/,a)") " => No resolution surface has been provided"
          end if

       else  !Write a *.geom file, fil should be present!

          write(unit=ipr,fmt="(a)")       "INFO "//Current_Instrm%info
          write(unit=ipr,fmt="(a)")       "NAME "//Current_Instrm%name_inst
          write(unit=ipr,fmt="(a,i3,a)")  "GEOM ",Current_Instrm%igeom,"  "//Current_Instrm%geom
          write(unit=ipr,fmt="(a)")       "BLFR "//Current_Instrm%BL_frame
          write(unit=ipr,fmt="(a)")       "ROT_ORDER "//Current_Instrm%r_ord
          write(unit=ipr,fmt="(a)")       "DIST_UNITS "//Current_Instrm%dist_units
          write(unit=ipr,fmt="(a)")       "ANGL_UNITS "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a,i3)")    "DET_TYPE "//trim(Current_Instrm%detector_type)//" ipsd ", &
                                                 Current_Instrm%ipsd
          write(unit=ipr,fmt="(a,f8.3)")  "DIST_DET ",Current_Instrm%dist_samp_detector
          if(index(Current_Instrm%geom,"Laue") /= 0) then
            write(unit=ipr,fmt="(a,2f8.3)")  "WAVE_LIMITS ", Current_Instrm%wave_min,Current_Instrm%wave_max
          end if
          write(unit=ipr,fmt="(a,2f8.3,2i5)")"DIM_XZP     ", Current_Instrm%horiz,    Current_Instrm%vert, &
                                                              Current_Instrm%np_horiz, Current_Instrm%np_vert
          write(unit=ipr,fmt="(a,2f8.4)")    "GAPS_CA_AN  ",Current_Instrm%cgap, Current_Instrm%agap
          write(unit=ipr,fmt="(a,f8.5)")     "WAVE        ",Current_Orient%wave
          write(unit=ipr,fmt="(a,2f8.5)")    "GN_CENTRE   ",Current_Instrm%ga_d, Current_Instrm%nu_d
          write(unit=ipr,fmt="(a,3f8.5)")    "TILT_ANGLES ",Current_Instrm%Tiltx_d, Current_Instrm%Tilty_d, Current_Instrm%Tiltz_d
          write(unit=ipr,fmt="(a, f8.5)")    "RESOL       ",Current_Instrm%d_min
          if(Current_Instrm%gnxz_limited) then
            write(unit=ipr,fmt="(a,2f8.5)")  "GAM+_LIMITS ",Current_Instrm%gap_min, Current_Instrm%gap_max
            write(unit=ipr,fmt="(a,2f8.5)")  "GAM-_LIMITS ",Current_Instrm%gan_min, Current_Instrm%gan_max
            write(unit=ipr,fmt="(a,2f8.5)")  "NU_LIMITS   ",Current_Instrm%nu_min, Current_Instrm%nu_max
            write(unit=ipr,fmt="(a,2f8.5)")  "X_LIMITS    ",Current_Instrm%x_min, Current_Instrm%x_max
            write(unit=ipr,fmt="(a,2f8.5)")  "Z_LIMITS    ",Current_Instrm%z_min, Current_Instrm%z_max
          end if
          if(index(Current_Instrm%geom,"Laue") == 0) then
            write(unit=ipr,fmt="(a)") "UBMAT"
            do i=1,3
               write(unit=ipr,fmt="(3f14.9)") Current_Orient%ub(i,:)
            end do
          End If
          write(unit=ipr,fmt="(a,9f6.1)") "SETTING  ",Current_Instrm%e1, Current_Instrm%e2, Current_Instrm%e3
          write(unit=ipr,fmt="(a,i4)")    "NUM_ANG  ",Current_Instrm%num_ang
          write(unit=ipr,fmt="(a,i4)")    "NUM_DISP ",Current_Instrm%num_disp
          write(unit=ipr,fmt="(a)")       "ANG_LIMITS         Min      Max    Offset  Velocity(deg/s)"
          do i=1,Current_Instrm%num_ang
             write(unit=ipr,fmt="(a,2f8.2,2f10.4)") "      "//Current_Instrm%ang_names(i), &
                                                             Current_Instrm%ang_Limits(i,1:2), &
                                                             Current_Instrm%ang_offsets(i), &
                                                             Current_Instrm%ang_velocity(i)
          end do

          if (Current_Instrm%rangtim) then
             write(unit=ipr,fmt="(a)") "RANGE_TIME"
             do i=1,Current_Instrm%num_ang
               n3=count(Current_Instrm%range_time(1,:,i) > 0.0001)
               write(unit=ipr,fmt="(a,i4)")  Current_Instrm%ang_names(i),n3
               do j=1,n3
                  write(unit=ipr,fmt="(2f10.4)") Current_Instrm%range_time(:,j,i)
               end do
             end do
          end if

          if (Current_Instrm%num_disp > 0) then
             write(unit=ipr,fmt="(a)")    "DISP_LIMITS"
             do i=1,Current_Instrm%num_disp
                write(unit=ipr,fmt="(a,2f8.2,f10.4)") "      "//Current_Instrm%disp_names(i), &
                                                                Current_Instrm%disp_Limits(i,1:2), &
                                                                Current_Instrm%disp_offsets(i)
             end do
          end if

          if (Current_Instrm%resol_given) then
             write(unit=ipr,fmt="(/,a)") "RESOLUTION SURFACE OF THE INSTRUMENT: "//trim(Current_Instrm%name_inst)
             n3=Current_Instrm%nGa
             do i=1,Current_Instrm%nNu
                write(unit=ipr,fmt="(tr12,20f10.4)") Current_Instrm%ReSurf(i,1:n3)
             end do
          else
             write(unit=ipr,fmt="(/,a)") " => No resolution surface has been provided"
           end if

          write(unit=ipr,fmt="(a,3f10.4)")        "DET_OFF ", Current_Instrm%det_offsets

          forma="(16f8.4)"
          if(index(Current_Instrm%geom,"Laue") == 0 .and. Current_Instrm%np_vert < 513 .and. &
                   Current_Instrm%np_horiz < 513) then
            write(unit=ipr,fmt="(a)")             "DET_ALPHAS"
            do i=1,Current_Instrm%np_vert
               write(unit=ipr,fmt=forma) Current_Instrm%alphas(i,1:Current_Instrm%np_horiz)
            end do
            if(Current_Instrm%sigma_alpha_correct) then
              write(unit=ipr,fmt="(a)")           "DET_SIGMA_ALPHAS"
              do i=1,Current_Instrm%np_vert
                 write(unit=ipr,fmt=forma) Current_Instrm%sigma_alphas(i,1:Current_Instrm%np_horiz)
              end do
            end if
            if(Current_Instrm%shift_correct) then
               write(unit=ipr,fmt="(a)") "DET_SHIFTS" !horizontal shifts
               write(unit=ipr,fmt="(12f10.4)") Current_Instrm%shifts(1:Current_Instrm%np_horiz)
            end if
          end if
       end if

    End Subroutine Write_Current_Instrm_data

    !!----
    !!---- Subroutine Write_Generic_Numor(Num,lun)
    !!----    type(generic_numor_type), intent(in) :: N
    !!----    integer, optional,        intent(in) :: lun
    !!----
    !!---- Write the information of a Generic Numor
    !!----
    !!---- Update: April - 2009
    !!
    Subroutine Write_Generic_Numor(Num,lun)
       !---- Arguments ----!
       type(generic_numor_type), intent(in) :: Num
       integer, optional,        intent(in) :: lun

       !---- Local Variables ----!
       integer :: ilun,i

       ilun=6
       if (present(lun)) ilun=lun

       write(unit=ilun, fmt='(a)') '#### Numor Information ####'
       write(unit=ilun, fmt='(a,i6.6)') 'Numor: ',num%numor

       write(unit=ilun,fmt='(a,t50,a)') 'Instrument: '//trim(num%instr),'Date: '//trim(num%date)
       write(unit=ilun,fmt='(a)') 'Experimental Name: '//trim(num%expname)
       write(unit=ilun,fmt='(a)') ' '

       write(unit=ilun,fmt='(a)') '#---- Sample Information ----#'
       do i=1,num%sampleid%n
          write(unit=ilun,fmt='(a)') trim(num%sampleid%namevar(i))//': '//trim(num%sampleid%cvalues(i))
       end do
       write(unit=ilun,fmt='(a)') ' '

       select case (num%instr)
           case ('D20')
              write(unit=ilun,fmt='(a)') '#---- Diffractometer Optics and Reactor Parameters ----#'
              do i=1,num%diffopt%n
                 if (len_trim(num%diffopt%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%diffopt%namevar(i))//': ',num%diffopt%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Monochromator Motor Parameters ----#'
              do i=1,num%monmpar%n
                 if (len_trim(num%monmpar%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%monmpar%namevar(i))//': ',num%monmpar%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Diffractometer Motor Parameters ----#'
              do i=1,num%diffmpar%n
                 if (len_trim(num%diffmpar%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%diffmpar%namevar(i))//': ',num%diffmpar%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Detector and DAS Parameters ----#'
              do i=1,num%detpar%n
                 if (len_trim(num%detpar%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%detpar%namevar(i))//': ',num%detpar%rvalues(i)
              end do
             write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Data Acquisition Parameters ----#'
              do i=1,num%dacparam%n
                 if (len_trim(num%dacparam%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%dacparam%namevar(i))//': ',num%dacparam%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Sample Status ----#'
              do i=1,num%samplest%n
                 if (len_trim(num%samplest%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.8)') trim(num%samplest%namevar(i))//': ',num%samplest%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Counts Information ----#'
              write(unit=ilun,fmt='(a,i8)') trim(num%icounts%namevar(1))//': ',num%icounts%n
              write(unit=ilun,fmt='(a)') ' '

           case ('D1B')
              write(unit=ilun,fmt='(a)') '#---- Data Acquisition Flags ----#'
              do i=1,num%dacflags%n
                 if (len_trim(num%dacflags%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,i6)') trim(num%dacflags%namevar(i))//': ',num%dacflags%ivalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Data Acquisition Parameters ----#'
              do i=1,num%dacparam%n
                 if (len_trim(num%dacparam%namevar(i)) <= 0) cycle
                 write(unit=ilun,fmt='(a,g15.4)') trim(num%dacparam%namevar(i))//': ',num%dacparam%rvalues(i)
              end do
              write(unit=ilun,fmt='(a)') ' '

              write(unit=ilun,fmt='(a)') '#---- Counts Information ----#'
              write(unit=ilun,fmt='(a,g15.4)') trim(num%icounts%namevar(1))//': ',real(num%icounts%ivalues(1))
              write(unit=ilun,fmt='(a,g15.4)') trim(num%icounts%namevar(2))//': ',real(num%icounts%ivalues(2))/60000.0
              write(unit=ilun,fmt='(a,g15.4)') trim(num%icounts%namevar(3))//': ',real(num%icounts%ivalues(3))*0.001
              write(unit=ilun,fmt='(a)') ' '

       end select

    End Subroutine Write_Generic_Numor

    !!----
    !!---- Subroutine Write_HeaderInfo_POWDER_Numor(Num,lun)
    !!----    type(POWDER_Numor_type), intent(in) :: Num
    !!----    integer,       optional, intent(in) :: lun
    !!----
    !!----    Writes the characteristics of the numor objet 'Num'
    !!----    of type POWDER_Numor_type in the file of logical unit 'lun'.
    !!----    If the subroutine is invoked without the 'lun' argument the subroutine
    !!----    outputs the information on the standard output (screen)
    !!----
    !!---- Update: 15/03/2011
    !!
    Subroutine Write_HeaderInfo_POWDER_Numor(Num,lun)
       !---- Arguments ----!
       type(POWDER_Numor_type), intent(in) :: Num
       integer,       optional, intent(in) :: lun

       !--- Local variables ---!
       integer       :: ipr

       ipr=6
       if (present(lun)) ipr=lun

       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a,i6,a)") " ---  Header Information about the NUMOR: ",Num%numor," of instrument "//trim(num%instrm)
       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "         HEADER: "//trim(Num%header)
       write(unit=ipr,fmt="(a)")        "          TITLE: "//trim(Num%title )
       write(unit=ipr,fmt="(a)")        "      INTRUMENT: "//trim(Num%Instrm)
       write(unit=ipr,fmt="(a)")        "      SCAN_TYPE: "//trim(Num%Scantype)
       write(unit=ipr,fmt="(a,f14.3)") "     SCAN_START: ", Num%scans(1)
       write(unit=ipr,fmt="(a,f14.3)") "     SCAN_STEP : ", Num%scans(2)
       write(unit=ipr,fmt="(a,f14.3 )")  "      MONITOR: ", Num%monitor
       write(unit=ipr,fmt="(a,f14.3 )")  "    TIME(seg): ", Num%time
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SETPNT: ", Num%conditions(1)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-REGUL.: ", Num%conditions(2)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SAMPLE: ", Num%conditions(3)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3  )") " Magnetic Field: ", Num%conditions(5)
       write(unit=ipr,fmt="(a)") " "

    End Subroutine Write_HeaderInfo_POWDER_Numor

    !!----
    !!---- Subroutine Write_POWDER_Numor(Num,lun)
    !!----    type(POWDER_Numor_type), intent(in) :: Num
    !!----    integer,      optional, intent(in) :: lun
    !!----
    !!----    Writes the characteristics of the numor objet 'Num'
    !!----    of type POWDER_Numor_type in the file of logical unit 'lun'.
    !!----    If the subroutine is invoked without the 'lun' argument the subroutine
    !!----    outputs the information on the standard output (screen)
    !!----
    !!---- Update: March - 2005
    !!
    Subroutine Write_POWDER_Numor(Num,lun)
       !---- Arguments ----!
       type(POWDER_Numor_type), intent(in) :: Num
       integer,       optional, intent(in) :: lun

       !--- Local variables ---!
       integer       :: ipr, i, j, nlines,n
       integer, dimension(10) :: cou

       ipr=6
       if (present(lun)) ipr=lun

       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a,i6,a)") " ---  Information about the NUMOR: ",Num%numor," of instrument "//trim(num%instrm)
       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "         HEADER: "//Num%header
       write(unit=ipr,fmt="(a)")        "          TITLE: "//Num%title
       write(unit=ipr,fmt="(a)")        "      INTRUMENT: "//Num%Instrm
       write(unit=ipr,fmt="(a)")        "      SCAN_TYPE: "//Num%Scantype
       write(unit=ipr,fmt="(a,f14.3)") "      ANGLE_PHI: ", Num%angles(1)
       write(unit=ipr,fmt="(a,f14.3)") "      ANGLE_CHI: ", Num%angles(2)
       write(unit=ipr,fmt="(a,f14.3)") "    ANGLE_OMEGA: ", Num%angles(3)
       write(unit=ipr,fmt="(a,f14.3)") "    ANGLE_gamm: ", Num%angles(4)
       write(unit=ipr,fmt="(a,f14.3)") "      ANGLE_PSI: ", Num%angles(5)
       write(unit=ipr,fmt="(a,f14.3)") "     SCAN_START: ", Num%scans(1)
       write(unit=ipr,fmt="(a,f14.3)") "     SCAN_STEP : ", Num%scans(2)
       write(unit=ipr,fmt="(a,f14.3)") "     SCAN_WIDTH: ", Num%scans(3)
       write(unit=ipr,fmt="(a,f14.3 )")  "      MONITOR: ", Num%monitor
       write(unit=ipr,fmt="(a,f14.3 )")  "    TIME(min): ", Num%time
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SETPNT: ", Num%conditions(1)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-REGUL.: ", Num%conditions(2)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SAMPLE: ", Num%conditions(3)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3  )") "      VOLTMETER: ", Num%conditions(4)
       write(unit=ipr,fmt="(a,f14.3  )") " Magnetic Field: ", Num%conditions(5)
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a,i6)")     "  Calc. angl. type: ",Num%icalc
       write(unit=ipr,fmt="(a,i6)")     "  Principal  angle: ",Num%manip
       write(unit=ipr,fmt="(a,i6)")     "  Number of frames: ",Num%nframes
       write(unit=ipr,fmt="(a,i6)")     "  Number of angles: ",Num%nbang
       write(unit=ipr,fmt="(a,i6)")     "  Number of pixels: ",Num%nbdata
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "  Profile with all counts in detector: "


       do i=1, Num%nframes
          nlines=Num%nbdata/10+1
          n=1
          do j=1,nlines
             cou=nint(Num%counts(n:n+9,i))
             write(unit=ipr,fmt="(10i8)") cou
             n=n+10
             if (n > Num%nbdata) exit
          end do
       end do

    End Subroutine Write_POWDER_Numor

    !!----
    !!---- Subroutine Write_HeaderInfo_SXTAL_Numor(Num,lun)
    !!----    type(SXTAL_Numor_type), intent(in) :: Num
    !!----    integer,      optional, intent(in) :: lun
    !!----
    !!----    Writes the Header Information of the numor objet 'Num'
    !!----    of type SXTAL_Numor_type in the file of logical unit 'lun'.
    !!----    If the subroutine is invoked without the 'lun' argument the subroutine
    !!----    outputs the information on the standard output (screen)
    !!----
    !!---- Update: 15/03/2011
    !!
    Subroutine Write_HeaderInfo_SXTAL_Numor(Num,lun)
       !---- Arguments ----!
       type(SXTAL_Numor_type), intent(in) :: Num
       integer,      optional, intent(in) :: lun

       !--- Local variables ---!
       integer       :: ipr

       ipr=6
       if (present(lun)) ipr=lun

       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a,i6,a)") " ---  Information about the NUMOR: ",Num%numor," of instrument "//trim(Num%Instrm)
       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "         HEADER: "//trim(Num%header)
       write(unit=ipr,fmt="(a)")        "          TITLE: "//trim(Num%title )
       write(unit=ipr,fmt="(a)")        "      INTRUMENT: "//trim(Num%Instrm)
       write(unit=ipr,fmt="(a)")        "      SCAN_TYPE: "//trim(Num%Scantype)
       write(unit=ipr,fmt="(a,3f8.3)")  "      HKL(Min.): ", Num%hmin
       write(unit=ipr,fmt="(a,3f8.3)")  "      HKL(Max.): ", Num%hmax
       if (Num%Scantype == "phi") then
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_START: ", Num%scans(1) ," r.l.u."
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_STEP : ", Num%scans(2) ," r.l.u."
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_WIDTH: ", Num%scans(3) ," r.l.u."
       else
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_START: ", Num%scans(1) ," "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_STEP : ", Num%scans(2) ," "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_WIDTH: ", Num%scans(3) ," "//Current_Instrm%angl_units
       end if
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SETPNT: ", Num%conditions(1)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-REGUL.: ", Num%conditions(2)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SAMPLE: ", Num%conditions(3)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3  )") "      VOLTMETER: ", Num%conditions(4)
       write(unit=ipr,fmt="(a,f14.3  )") " Magnetic Field: ", Num%conditions(5)
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)") "       ORIENTATION MATRIX  UB"
       write(unit=ipr,fmt="(a)") " --------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(1,:)
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(2,:)
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(3,:)
       write(unit=ipr,fmt="(a)") " "

    End Subroutine Write_HeaderInfo_SXTAL_Numor

    !!----
    !!---- Subroutine Write_SXTAL_Numor(Num,lun)
    !!----    type(SXTAL_Numor_type), intent(in) :: Num
    !!----    integer,      optional, intent(in) :: lun
    !!----
    !!----    Writes the characteristics of the numor objet 'Num'
    !!----    of type SXTAL_Numor_type in the file of logical unit 'lun'.
    !!----    If the subroutine is invoked without the 'lun' argument the subroutine
    !!----    outputs the information on the standard output (screen)
    !!----
    !!---- Update: March - 2005, September -2018
    !!
    Subroutine Write_SXTAL_Numor(Num,lun)
       !---- Arguments ----!
       type(SXTAL_Numor_type), intent(in) :: Num
       integer,      optional, intent(in) :: lun

       !--- Local variables ---!
       integer       :: ipr, i, cou, ctot
       real(kind=cp) :: tim,mon,ang1,ang2

       ipr=6
       if (present(lun)) ipr=lun

       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a,i6,a)") " ---  Information about the NUMOR: ",Num%numor," of instrument "//trim(Num%Instrm)
       write(unit=ipr,fmt="(a)")      " ---------------------------------------------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "         HEADER: "//Num%header
       write(unit=ipr,fmt="(a)")        "          TITLE: "//trim(Num%title)
       write(unit=ipr,fmt="(a)")        "      INTRUMENT: "//Num%Instrm
       write(unit=ipr,fmt="(a)")        "      SCAN_TYPE: "//Num%Scantype
       write(unit=ipr,fmt="(a,f8.3)")   "  COUPLING_FACT: ", Num%cpl_fact
       write(unit=ipr,fmt="(a,3f8.3)")  "      HKL(Min.): ", Num%hmin
       write(unit=ipr,fmt="(a,3f8.3)")  "      HKL(Max.): ", Num%hmax
       write(unit=ipr,fmt="(a,3f8.3)")  "      DELTA-hkl: ", Num%dh
       write(unit=ipr,fmt="(a,f14.3,a)") "      ANGLE_PHI: ", Num%angles(1)," "//Current_Instrm%angl_units
       write(unit=ipr,fmt="(a,f14.3,a)") "      ANGLE_CHI: ", Num%angles(2)," "//Current_Instrm%angl_units
       write(unit=ipr,fmt="(a,f14.3,a)") "    ANGLE_OMEGA: ", Num%angles(3)," "//Current_Instrm%angl_units
       write(unit=ipr,fmt="(a,f14.3,a)") "    ANGLE_gamm: ", Num%angles(4)," "//Current_Instrm%angl_units
       write(unit=ipr,fmt="(a,f14.3,a)") "      ANGLE_PSI: ", Num%angles(5)," "//Current_Instrm%angl_units
       if (Num%Scantype == "phi") then
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_START: ", Num%scans(1) ," r.l.u."
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_STEP : ", Num%scans(2) ," r.l.u."
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_WIDTH: ", Num%scans(3) ," r.l.u."
       else
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_START: ", Num%scans(1) ," "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_STEP : ", Num%scans(2) ," "//Current_Instrm%angl_units
          write(unit=ipr,fmt="(a,f14.3,a)") "     SCAN_WIDTH: ", Num%scans(3) ," "//Current_Instrm%angl_units
       end if
       write(unit=ipr,fmt="(a,f14.3 )") "         PRESET: ", Num%preset
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SETPNT: ", Num%conditions(1)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-REGUL.: ", Num%conditions(2)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3,a)") "    Temp-SAMPLE: ", Num%conditions(3)," Kelvin"
       write(unit=ipr,fmt="(a,f14.3  )") "      VOLTMETER: ", Num%conditions(4)
       write(unit=ipr,fmt="(a,f14.3  )") " Magnetic Field: ", Num%conditions(5)
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)") "       ORIENTATION MATRIX  UB"
       write(unit=ipr,fmt="(a)") " --------------------------------------"
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(1,:)
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(2,:)
       write(unit=ipr,fmt="(tr2,3f12.7)")  Num%ub(3,:)
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a,i6)")     "  Calc. angl. type: ",Num%icalc
       write(unit=ipr,fmt="(a,i6)")     "  Principal  angle: ",Num%manip
       write(unit=ipr,fmt="(a,i6)")     "  Number of frames: ",Num%nframes
       write(unit=ipr,fmt="(a,i6)")     "  Number of angles: ",Num%nbang
       write(unit=ipr,fmt="(a,i6)")     "  Number of pixels: ",Num%nbdata
       write(unit=ipr,fmt="(a)") " "
       write(unit=ipr,fmt="(a)")        "  Profile with all counts in detector: "

       if(allocated(Num%counts)) then
         do i=1, Num%nframes
            cou=nint(sum(Num%counts(:,i)))
            !ang= Num%scans(1)+ real(i-1)*Num%scans(2)
            tim = Num%tmc_ang(1,i)
            mon = Num%tmc_ang(2,i)
            ctot = nint(Num%tmc_ang(3,i))
            ang1 = Num%tmc_ang(4,i)
            ang2 = Num%tmc_ang(5,i)
            write(unit=ipr,fmt="(4f12.2,2i8)") tim,mon,ang1,ang2, ctot, cou
         end do
       else
         do i=1, Num%nframes
            tim = Num%tmc_ang(1,i)
            mon = Num%tmc_ang(2,i)
            ctot = nint(Num%tmc_ang(3,i))
            ang1 = Num%tmc_ang(4,i)
            ang2 = Num%tmc_ang(5,i)
            write(unit=ipr,fmt="(4f12.2,i8)") tim,mon,ang1,ang2, ctot
         end do
       end if
    End Subroutine Write_SXTAL_Numor

    !!----
    !!---- Subroutine Read_Calibration_File(FileCal, Instrm, Cal )
    !!----    character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
    !!----    character(len=*), intent(in)                 :: Instrm     ! Instrument
    !!----    type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object
    !!----
    !!---- Read the Calibration File for Different Instruments at ILL.
    !!---- At the moment: D1A, D2B
    !!----
    !!---- 22/03/2011
    !!
    Subroutine Read_Calibration_File(FileCal, Instrm, Cal)
        !---- Arguments ----!
        character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
        character(len=*), intent(in)                 :: Instrm     ! Instrument
        type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object

        !---- Local Variables ----!

        call clear_error()

        select case (u_case(trim(Instrm)))
           case ('D1A')
              call Read_Calibration_File_D1A(FileCal,Cal)

           case ('D2B')
              call Read_Calibration_File_D2B(FileCal,Cal)

           case ('D4')
              call Read_Calibration_File_D4(FileCal,Cal)

           case ('D20')
              call Read_Calibration_File_D20(FileCal,Cal)

           case default
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading Calibration File for '//trim(Instrm)//' Instrument'
        end select

    End Subroutine Read_Calibration_File

    !!--++
    !!--++ Subroutine Read_Calibration_File_D1A(FileCal,Cal)
    !!--++    character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
    !!--++    type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object
    !!--++
    !!--++ Load the Calibration parameters for a D1A Instrument
    !!--++
    !!--++ 17/03/2011
    !!
    Subroutine Read_Calibration_File_D1A(FileCal,Cal)
        !---- Arguments ----!
        character(len=*), intent(in)                 :: FileCal
        type(calibration_detector_type), intent(out) :: Cal

        !---- Local Variables ----!
        character(len=512), dimension(:), allocatable :: filevar
        integer                                       :: i,iv,nlines
        integer, dimension(25)                        :: ivet
        real(kind=cp), dimension(25)                  :: vet

        ! Init
        call clear_error()
        Err_CFML%Msg=' '

        set_calibration_detector=.false.

        ! Detecting numor
        nlines = Number_Lines(FileCal)
        if (nlines <=0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Calibration File for D1A Instrument'
           return
        end if

        ! Allocating variables
        if (allocated(filevar)) deallocate(filevar)
        allocate(filevar(nlines))
        call Reading_Lines(FileCal,nlines,filevar)

        ! Load Information on Cal Object
        Cal%Name_Instrm='D1A'
        Cal%NDet=25
        Cal%NPointsDet=1
        if (allocated(Cal%Active)) deallocate(Cal%Active)
        if (allocated(Cal%PosX))    deallocate(Cal%PosX)
        if (allocated(Cal%Effic))  deallocate(Cal%Effic)

        allocate(Cal%PosX(Cal%NDet))
        allocate(Cal%Effic(Cal%NPointsDet,Cal%NDet))
        allocate(Cal%Active(Cal%NPointsDet,Cal%NDet))

        Cal%Active=.true.
        Cal%PosX=0.0
        Cal%Effic=1.0
        Cal%Pos_read=.false.
        ! First line is not considered

        ! Second line is the relative position: Last to First
        call get_num(filevar(2), vet,ivet,iv)
        if (iv /= 25) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Positions of Detectors for D1A Instrument'
           deallocate(filevar)
           return
        end if
        Cal%PosX=vet
        Cal%Pos_read=.true.
        ! Efficiency of each detector
        call get_num(filevar(3), vet,ivet,iv)
        if (iv /= 25) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Efficiency values for Detectors of D1A Instrument'
           deallocate(filevar)
           return
        end if
        Cal%Effic(1,:)=vet

        ! Mask for Points used or not
        do i=1,Cal%NDet
           if (Cal%Effic(1,i) < 0.0)  Cal%Active(1,i)=.false.
        end do

        set_calibration_detector =.true.
        deallocate(filevar)

    End Subroutine Read_Calibration_File_D1A

    !!--++
    !!--++ Subroutine Read_Calibration_File_D2B(FileCal,Cal)
    !!--++    character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
    !!--++    type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object
    !!--++
    !!--++ Load the Calibration parameters for a D2B Instrument. This corresponds to LAMP way of storing calibration
    !!--++
    !!--++ 17/03/2011 updated 21/02/2023
    !!
    Subroutine Read_Calibration_File_D2B(FileCal,Cal)
        !---- Arguments ----!
        character(len=*), intent(in)                 :: FileCal
        type(calibration_detector_type), intent(out) :: Cal

        !---- Local Variables ----!
        character(len=512), dimension(:), allocatable :: filevar
        character(len=512)                            :: line
        integer                                       :: i,iv,ini,j,nlines,k, k1,k2
        integer, dimension(6)                         :: ivet
        real(kind=cp), dimension(6)                   :: vet
        real(kind=cp), dimension(128)                 :: eff

        ! Init
        call clear_error()

        set_calibration_detector=.false.

        ! Detecting numor
        nlines = Number_Lines(FileCal)
        if (nlines <=0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Calibration File for D2B Instrument'
           return
        end if

        ! Allocating variables
        if (allocated(filevar)) deallocate(filevar)
        allocate(filevar(nlines))
        call Reading_Lines(FileCal,nlines,filevar)

        ! Load Information on Cal Object
        Cal%Name_Instrm='D2B'
        Cal%NDet=128
        Cal%NPointsDet=128
        if (allocated(Cal%Active)) deallocate(Cal%Active)
        if (allocated(Cal%PosX))    deallocate(Cal%PosX)
        if (allocated(Cal%Effic))  deallocate(Cal%Effic)

        allocate(Cal%PosX(Cal%NDet))
        allocate(Cal%Effic(Cal%NPointsDet,Cal%NDet))
        allocate(Cal%Active(Cal%NPointsDet,Cal%NDet))

        Cal%Active=.false.
        Cal%PosX=0.0           ! Load values is from 1 to 128 detector
        Cal%Effic=1.0

        !>
        !> Angle Positions (From 1 to 128)
        !> Corrections for angles for each tube detector. The reference is given on Numor%tmc_ang(4,nframes)
        !>
        ini=0
        do i=1,nlines
           if (index(filevar(i),'angles') > 0) then
              ini=i
              exit
           end if
        end do
        if (ini == 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Angles positions for Detectors in D2B Instrument'
           return
        end if

        ini=ini+1
        j=0
        do i=ini,nlines
           call get_num(filevar(i), vet,ivet,iv)
           if (iv <= 0) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading Angles positions for Detectors in D2B Instrument'
              return
           end if
           Cal%PosX(j+1:j+iv)=vet(1:iv)
           j=j+iv
           if (j == Cal%NDet) exit
        end do
        Cal%Pos_read=.true.

        !>                                                      T1      T2      T3      T4
        !> Active zone.                                        128      129    128     129
        !> Tube 1 (Up) + Tube 2 in reverse mode (down)          .        .      .       .
        !> Tube 1: 1-128 Points     Tube 2: 129 - 256           2       255     2      255
        !>                                                      1       256     1      256
        ini=0
        do i=1,nlines
           if (index(filevar(i),'zones') > 0) then
              ini=i
              exit
           end if
        end do
        if (ini == 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Zones values for Detectors in D2B Instrument'
           return
        end if

        ini=ini+1
        j=0   ! Number of Detector
        do i=ini,ini+64 !nlines
           line=filevar(i)
           k=index(line,"*")
           if(k /= 0) line=line(1:k-1)
           call get_num(line, vet,ivet,iv)
           if (iv /= 4) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading active zones for Detectors in D2B Instrument'
              exit
           end if
           ivet=ivet+1

           if (ivet(1) < 1 .or. ivet(2) > 128 .or. ivet(3) < 129 .or. ivet(4) > 256) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading active zones for Detectors in D2B Instrument'
              exit
           end if

           ! Tube 1
           j=j+1
           Cal%Active(ivet(1):ivet(2),j)=.true.

           ! Tube 2 in reverse mode
           j=j+1
           k1=256-ivet(4)+1
           k2=256-ivet(3)+1
           Cal%Active(k1:k2,j)=.true.
           if (j == Cal%NDet) exit
        end do

        if(Err_CFML%Ierr == 1) return

        ! Efficiency of each detector
        ini=0
        do i=1,nlines
           if (index(filevar(i),'efficiencies') > 0) then
              ini=i
              exit
           end if
        end do
        if (ini == 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D2B Instrument'
           return
        end if

        ini=ini+1
        j=0   ! Number of Detector
        k=0   ! Number of Points in the same Detector
        eff=1.0
        do i=ini,nlines
           call get_num(filevar(i), vet,ivet,iv)
           if (iv <= 0) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D2B Instrument'
              exit
           end if
           select case (iv)
              case (2)
                 eff(k+1:k+iv)=vet(1:iv)

                 j=j+1
                 if (mod(j,2) /= 0) then
                    Cal%Effic(:,j)=eff  !It was reversed !!! j and k
                 else
                    do k1=1,Cal%NPointsDet
                       Cal%Effic(k1,j)=eff(Cal%NPointsDet-k1+1) !instead of 'k1' it was written ':' and reversed !!!!!
                    end do
                 end if

                 k=0
                 eff=1.0
              case (6)
                 eff(k+1:k+iv)=vet(1:iv)
                 k=k+iv

              case default
                 Err_CFML%Ierr=1
                 Err_CFML%Flag=.true.
                 Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D2B Instrument'
                 exit
           end select

           if (j == Cal%NDet) exit
        end do

        if(Err_CFML%Ierr == 1) return

        ! Check Mask for Point Detectors from Effic values and put real efficiencies
        do j=1,Cal%NDet
           do i=1, Cal%NPointsDet
              if (Cal%Effic(i,j) <= 0.0) then
                 Cal%Active(i,j)=.false.
              else
                 Cal%Effic(i,j)=1.0/Cal%Effic(i,j)  !The read data are inverse of efficiencies
              end if
           end do
        end do
        where(.not. Cal%Active) Cal%Effic=-1.0

        set_calibration_detector =.true.

    End Subroutine Read_Calibration_File_D2B

    !!--++
    !!--++ Subroutine Read_Calibration_File_D20(FileCal,Cal)
    !!--++    character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
    !!--++    type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object
    !!--++
    !!--++ Load the Calibration parameters for D20 Instrument
    !!--++
    !!--++ 16/05/2011
    !!
    Subroutine Read_Calibration_File_D20(FileCal,Cal)
        !---- Arguments ----!
        character(len=*), intent(in)                 :: FileCal
        type(calibration_detector_type), intent(out) :: Cal

        !---- Local Variables ----!
        character(len=160)                     :: txt
        integer                                :: i,j,lun, ier

        ! Init
        call clear_error()
        set_calibration_detector=.false.
        Cal%Pos_read=.false.

        open(newunit=lun,file=trim(FileCal),status='old',action="read",position="rewind",iostat=ier)
        if (ier /= 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Calibration File: '//trim(FileCal)//', for D20 Instrument'
           return
        end if
        read(unit=lun,fmt="(a)") txt  !First line is a text indentifying the file containing the number of points
        i=index(txt,"(")+1
        j=index(txt,")")-1
        read(unit=txt(i:j),fmt=*,iostat=ier) Cal%NDet
        if(ier /= 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problem reading the number of points of the D20 detector in calibration file: '//trim(FileCal)
           return
        end if

        ! Load Information on Cal Object
        Cal%Name_Instrm='D20'
        !Cal%NDet=1600
        Cal%NPointsDet=1
        if (allocated(Cal%Active)) deallocate(Cal%Active)
        if (allocated(Cal%PosX))   deallocate(Cal%PosX)
        if (allocated(Cal%Effic))  deallocate(Cal%Effic)
        if (allocated(ipoint))     deallocate(ipoint)  !This pointer is globally accessed within the module (private)

        allocate(ipoint(Cal%NDet))
        allocate(Cal%PosX(Cal%NDet))
        allocate(Cal%Effic(Cal%NPointsDet,Cal%NDet))
        allocate(Cal%Active(Cal%NPointsDet,Cal%NDet))

        Cal%Active=.true.
        Cal%PosX=0.0
        Cal%Effic=1.0
        read(unit=lun,fmt=*,iostat=ier) Cal%PosX(1:Cal%NDet)
        if(ier /= 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problem reading cell positions of D20 detector in calibration file: '//trim(FileCal)
           return
        end if
        Cal%Pos_read=.true.
        read(unit=lun,fmt=*,iostat=ier) Cal%Effic(1,1:Cal%NDet)
        if(ier /= 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problem reading efficiencies of D20 detector in calibration file: '//trim(FileCal)
           return
        end if

        ! Mask for Points to be used or not and order the angles
        ipoint= sort(Cal%PosX,Cal%NDet)

        do i=1,Cal%NDet
           if (Cal%Effic(1,i) <= 0.0)  Cal%Active(1,i)=.false.
           if (Cal%Effic(1,i) > 0.0)   Cal%Effic(1,i) = 1.0/Cal%Effic(1,i)
        end do

        set_calibration_detector =.true.

    End Subroutine Read_Calibration_File_D20

    !!--++
    !!--++ Subroutine Read_Calibration_File_D4(FileCal,Cal)
    !!--++    character(len=*), intent(in)                 :: FileCal    ! Path+Filename of Calibration File
    !!--++    type(calibration_detector_type), intent(out) :: Cal        ! Calibration Detector Object
    !!--++
    !!--++ Load the Calibration parameters for a D4 Instrument
    !!--++
    !!--++ 17/03/2011
    !!
    Subroutine Read_Calibration_File_D4(FileCal,Cal)
        !---- Arguments ----!
        character(len=*), intent(in)                 :: FileCal
        type(calibration_detector_type), intent(out) :: Cal

        !---- Local Variables ----!
        character(len=512), dimension(:), allocatable :: filevar
        character(len=80)                             :: line
        integer                                       :: i,iv,ini,j,nlines,k
        integer, dimension(6)                         :: ivet
        real(kind=cp), dimension(6)                   :: vet

        ! Init
        call clear_error()

        set_calibration_detector=.false.

        ! Detecting numor
        nlines = Number_Lines(FileCal)
        if (nlines <=0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Calibration File for D4 Instrument'
           return
        end if

        ! Allocating variables
        if (allocated(filevar)) deallocate(filevar)
        allocate(filevar(nlines))
        call Reading_Lines(FileCal,nlines,filevar)

        ! Load Information on Cal Object
        Cal%Name_Instrm='D4'
        Cal%NDet=9
        Cal%NPointsDet=64
        if (allocated(Cal%Active)) deallocate(Cal%Active)
        if (allocated(Cal%PosX))    deallocate(Cal%PosX)
        if (allocated(Cal%Effic))  deallocate(Cal%Effic)

        allocate(Cal%PosX(Cal%NDet))
        allocate(Cal%Effic(Cal%NPointsDet,Cal%NDet))
        allocate(Cal%Active(Cal%NPointsDet,Cal%NDet))

        Cal%Active=.true.
        Cal%PosX=0.0
        Cal%Effic=1.0
        Cal%Pos_read=.false.

        ! Angle Position
        ini=0
        do i=1,nlines
           if (index(filevar(i),'angles') > 0) then
              ini=i
              exit
           end if
        end do
        if (ini == 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Angles positions for Detectors in D4 Instrument'
           deallocate(filevar)
           deallocate(Cal%Active)
           deallocate(Cal%PosX)
           deallocate(Cal%Effic)
           return
        end if

        ini=ini+1
        j=0
        do i=ini,nlines
           line=adjustl(filevar(i))
           if (len_trim(line) <=0) cycle
           if (line(1:1) =='#') cycle
           call get_num(line, vet,ivet,iv)
           select case (iv)
              case (2)
                 j=j+1
                 Cal%PosX(j)=vet(2)

              case default
                 Err_CFML%Ierr=1
                 Err_CFML%Flag=.true.
                 Err_CFML%Msg=' Problems reading Angles positions for Detectors in D4 Instrument'
                 deallocate(filevar)
                 deallocate(Cal%Active)
                 deallocate(Cal%PosX)
                 deallocate(Cal%Effic)
                 return
           end select
           if (j == Cal%NDet) exit
        end do
        Cal%Pos_read=.true.

        ! Efficiency of each detector
        ini=0
        do i=1,nlines
           if (index(filevar(i),'effic') > 0) then
              ini=i
              exit
           end if
        end do
        if (ini == 0) then
           Err_CFML%Ierr=1
           Err_CFML%Flag=.true.
           Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D4 Instrument'
           deallocate(filevar)
           deallocate(Cal%Active)
           deallocate(Cal%PosX)
           deallocate(Cal%Effic)
           return
        end if

        ini=ini+1
        do i=ini,nlines
           line=adjustl(filevar(i))
           if (len_trim(line) <=0) cycle
           if (line(1:1) =='#') cycle
           call get_num(line, vet,ivet,iv)
           if (iv <= 0) then
              Err_CFML%Ierr=1
              Err_CFML%Flag=.true.
              Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D4 Instrument'
              deallocate(filevar)
              deallocate(Cal%Active)
              deallocate(Cal%PosX)
              deallocate(Cal%Effic)
              return
           end if
           select case (iv)
              case (3)
                 j=ivet(1)
                 k=ivet(2)
                 Cal%Effic(k,j)=vet(3)

              case default
                 Err_CFML%Ierr=1
                 Err_CFML%Flag=.true.
                 Err_CFML%Msg=' Problems reading Efficiencies values for Detectors in D4 Instrument'
                 deallocate(filevar)
                 deallocate(Cal%Active)
                 deallocate(Cal%PosX)
                 deallocate(Cal%Effic)
                 return
           end select
        end do

        ! Mask for Point Detectors
        do i=1,Cal%NDet
           do j=1, Cal%NPointsDet
              if (Cal%Effic(j,i) < 0.0) Cal%Active(j,i)=.false.
           end do
        end do

        set_calibration_detector =.true.
        deallocate(filevar)

    End Subroutine Read_Calibration_File_D4

 End Module CFML_ILL_Instrm_Data
