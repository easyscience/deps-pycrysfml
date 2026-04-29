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
!!---- MODULE: CFML_Laue
!!----   INFO: Subroutines for treating Laue diffraction data
!!----
!!---- HISTORY
!!----    Update: 16/02/2026
!!----
!!
Module CFML_Laue

   !---- Use Modules ----!
   use CFML_Geom, only: Matrix_Rx,Matrix_Ry,Matrix_Rz
   use CFML_GlobalDeps, only: clear_error,cp,dp,err_cfml,ops_sep,pi,to_deg,to_rad
   use CFML_gSpacegroups, only: Set_SpaceGroup,Spg_Type
   use CFML_Maths, only: Sort
   use CFML_Metrics, only: Cell_G_Type
   use CFML_Reflections, only: Gener_Reflections,Get_MaxNumRef,RefList_Type
   use CFML_Strings, only: File_Type,Reading_File,U_Case

   !---- Variables ----!
   implicit none

   private

   !---- Public Subroutine ----!
   public :: Allocate_Laue_Ref_List,generate_laue_reflections
   public :: Get_Stereographic_Projection_From_ZV
   public :: Init_Header,Init_Laue_Instrm,Init_Laue_Ref
   public :: Calc_Laue_Spot_XZ_Angles,Calc_Visible_Reflections_List
   public :: Output_Excluded_Regions
   public :: Peak_Find_Threshold,Set_Mask_Excl
   public :: Read_HBin_Header_2D_Image,Read_Binary_2D_Image,Read_Laue_Image,Read_Laue_Instrm

   !---- Definitions ----!

   integer, public, parameter :: DATAKIND = 4 ! 16 bits integers only

   type, public :: Excluded_Regions_Type
      ! {Exc_rect(1,1,n),Exc_rect(2,1,n)} pixels (i1,j1) minimum i,j-corner of n-rectangle
      ! {Exc_rect(1,2,n),Exc_rect(2,2,n)} pixels (i2,j2) maximum i,j-corner of n-rectangle
      ! {Exc_circ(1,n),Exc_circ(2,n)} Centre of n-circle, of radius Exc_circ(3,n)
      ! The indices inv_circ and inv_rect indicate a circular or a rectangular area that
      ! acts as an inverse mask: all pixels outside this regions are excluded. If they are
      ! both zero it means that there is no inverse mask
      integer :: Nexc_rect   !Number of rectangular excluded regions
      integer :: Nexc_circ   !Number of circular excluded regions
      integer :: imax,jmax   !Maximum pixel indices (it is supposed that they start at 1)
      integer :: inv_circ,inv_rect !Inverse mask index for circular and rectangular regions
      integer, dimension(:,:,:), allocatable :: exc_rect  ! Rect. excl. regions (2,2,Nexc_rect) in pixels
      integer, dimension(:,:),   allocatable :: exc_circ  ! Circ. excl. regions (3,Nexc_circ) in pixels
   end type Excluded_Regions_Type

   type, public :: Header_Type
      ! This type encapsulates all the information corresponding to the header
      ! of the binary files generated and read by the present module.
      ! The header of the binary files is written as a character string of
      ! different lengths containing end-of-line characters in order to be seen
      ! easily with a text editor. The meaning of the different items should be
      ! defined by the program using the module.
      ! An example of header is written after the description of the type.
      !
      ! Created: December 2011   (JRC)
      ! Updated:  January 2012   (JRC)
      !
      ! Example of header written in a binary file, the first item is the total length of the header string
      ! ===================================================================================================
      ! Header_Length:   1264
      ! Title: sucrose at RTD19 SCC   SCC 01-Sep-11 16:39:15 Summed Omega-range: [ -33.000 -26.000]
      ! N_Frames:      1
      ! Nbits:   16
      ! Nrow-Ncol:    256   640
      ! Detector_Type: CYLINDRICAL
      ! ColRow_Order: Column_Order
      ! Pixel_size_h:      2.50000
      ! Pixel_size_v:      1.56350
      ! Scan_Type: Summed-Omega
      ! Scan Values:    -33.00000     0.07000   -26.00000
      ! N_vector_Items:      3
      ! Vector item #  1 (hkl-min):     5.37751   -12.52532    -0.18949
      ! Vector item #  2 (hkl-max):     0.00000     0.00000     0.00000
      ! Vector item #  3 (delta-hkl):   0.00000     0.00000     0.00000
      ! N_matrix_Items:      1
      ! Matrix item #  1 (UB-matrix):  -0.021241  -0.011520  -0.033343   0.001907   0.037429  -0.014147   0.020302  -0.005238  -0.011123
      ! Time   :   2997.00000
      ! Monitor:      6000.00
      ! Counts : 210666.00000
      ! N_environment:      4
      ! Environment item #  1 (Temp-set):    15.00000~Kelvin
      ! Environment item #  2 (Temp-regul):    15.00700~Kelvin
      ! Environment item #  3 (Temp-sample):  9999.99023~Kelvin
      ! Environment item #  4 (Magnetic Field):     0.00000~Tesla
      ! N_motors:      5
      ! Motor item #  1 (Gamma):    62.00300~Degrees
      ! Motor item #  2 (Omega):     8.00000~Degrees
      ! Motor item #  3 (Chi):   179.00999~Degrees
      ! Motor item #  4 (Phi):     0.01000~Degrees
      ! Motor item #  5 (Psi):     0.00000~Degrees
      ! ..... binary data follows the line above
      !
      integer           :: Header_Length               ! Numer of bytes of the header (length of the header string)
                                                       ! It is the first value writen/read in the file
      character(len=132):: Title
      character(len=80) :: User_LC_Date_Time           ! User, Local contac, Date and Time
      integer           :: Nframes                     ! Number of frames (blocks of data in which some environment or motor variable has changed)
      integer           :: Nbits                       ! Number of bits for integers representing the image: 16 or 32 bits
      integer           :: Nrow, Ncol                  ! Size of the image in pixels represented by the matrix Image(Nrow,Ncol)
      character(len=30) :: Instrument_name             ! Name of the instrument
      character(len=15) :: Detector_type               ! "Flat","Cylindrical","Curved", etc
      character(len=15) :: colrow_order                ! "row_order" (C-like),"column_order" (Fortran-like)
      integer,dimension(11) :: flags                   ! Corresponds to icdesc in numors
      real              :: Pix_Size_h, Pix_Size_v      ! Horizontal and vertical pixel size in mm
      real              :: sample_detector_dist        ! Sample detector distance in mm
      real              :: lambda_min,lambda_max       ! Range in wavelength in angstroms (if lambda_min=lambda_max, monochromatic)
      character(len=15) :: Scan_Type                   ! ACQ: static acquisition, MOTOR_NAME, ENVNT_NAME
      real              :: Init_Val,Step_Val,Final_Val ! Values of the scan items (given only if it is not ACQ)
      real              :: sigma_factor, normalization_time, normalization_monitor

      real, dimension(:),allocatable :: time, monitor, counts ! one per frame

      ! The comment,scalar, vector and matrix items are recorded only one time in the file
      integer           :: N_comment_Items                             ! Number of additional comment items (e.g. coupling factor between coupled motor motions)
      character(len=132),dimension(:),     allocatable :: comment      ! Comments for storing arbitrary information
      integer           :: N_scalar_Items                              ! Number of additional scalar items (e.g. coupling factor between coupled motor motions)
      character(len=15), dimension(:),     allocatable :: scalar_Name  ! e.g. "Coupling Factor"
      real,              dimension(:),     allocatable :: scalar_Value ! e.g.  2.0
      integer           :: N_vector_Items                              ! Number of additional 3D vectorial items (e.g. hkl indices)
      character(len=15), dimension(:),     allocatable :: vector_Name  ! e.g. "Miller indices"
      real,              dimension(:,:),   allocatable :: vector_Value ! e.g.  1 1 -3
      integer           :: N_matrix_Items                              ! Number of additional 3D matrix items  (e.g. UB matrix)
      character(len=15), dimension(:),     allocatable :: Matrix_Name  ! e.g. "Busing-Levy UB"
      real,              dimension(:,:,:), allocatable :: Matrix_Value ! e.g.  0.12345  0.1111 .....

      ! The environment and motor items are recorded one time per frame. Their values are recorded
      ! just before giving the intensity values of the image.
      integer           :: N_environment                             ! Number of environment items
      character(len=15), dimension(:),    allocatable :: Envnt_Name  ! e.g. Temperature, Magnetic Field, ...
      character(len=15), dimension(:),    allocatable :: Envnt_Unit  ! e.g. Kelvin, Tesla ...
      real,              dimension(:,:),  allocatable :: Envnt_Value ! e.g.  300.0,  10.0 ...

      integer           :: N_motors                                  ! Number of motors in the instrument
      character(len=15), dimension(:),    allocatable :: Motor_Name  ! e.g. Temperature, Magnetic Field, ...
      character(len=15), dimension(:),    allocatable :: Motor_Unit  ! e.g. degree, mm ...
      real,              dimension(:,:),  allocatable :: Motor_Value ! e.g.  47.11,  12.23 ...
   end type Header_Type

   type, public :: Image_Conditions
      character(Len=80)  :: file_name         ! 0
      character(Len=50)  :: host              ! 1
      character(Len=50)  :: datetime          ! 2
      character(Len=50)  :: user              ! 3
      character(Len=50)  :: sample            ! 4
      character(Len=1400):: comment           ! 5
      real               :: temp_begin        ! 6
      real               :: temp_end          ! 7
      real               :: expose_time       ! 8
      real               :: expose_phi        ! 9
      real               :: temp_min          !10
      real               :: temp_max          !11
      integer            :: xyres             !12
      integer            :: Numx              !13
      integer            :: Numy              !14
      integer            :: startx            !15
      integer            :: starty            !16
      integer            :: speed             !17
   end type Image_Conditions

   type, public            :: Laue_Instrument_Type
      character(len=80)    :: Info="Default Laue Diffractometer as VIVALDI" ! Information about the instrument
      character(len=80)    :: Name="LAUE_DIFF "  ! Name of the instrument
      character(len=3)     :: dtype="Cyl"        ! Rec or Cyl
      character(len=3)     :: r_ord='xzy'        ! xyz, yxz, ... Order of detector tilt calculations: xyz means Rx Ry Rz,
                                                 ! so the rotation around z is the first applied, then around y and finally
                                                 ! around x.
      character(len=3)     :: invert='no'
      integer              :: np_h,np_v          ! Number of horizontal and vertical pixels
      real(kind=cp)        :: D=159.155          ! Distance (or radius) of the detector centre to the crystal
      real(kind=cp)        :: ga_d=0.0,nu_d=0.0  ! Angles gamma and nu of the detector centre
      real(kind=cp)        :: tiltx_d=0.0        ! Tilt of the flat detector around x-axis (normally = nu_d)
      real(kind=cp)        :: tilty_d=0.0        ! Tilt of the flat detector around y-axis (normally = 0)
      real(kind=cp)        :: tiltz_d=0.0        ! Tilt of the flat detector around z-axis (normally = 0)
      real(kind=cp)        :: H=800.0,V=400.0    ! Horizontal and Vertical dimension of detector in mm
      real(kind=cp), dimension(3)   :: rOD       ! Vector position of the detector origin in the laboratory system
      real(kind=cp), dimension(3,3) :: RD=reshape((/1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0/),(/3,3/)) ! Rotation matrix giving the orientation of the D-frame w.r.t. the L-frame
      logical              :: tilted=.false.     ! True if RD /= I
      logical              :: displaced=.false.  ! True if rOD /= (0,0,0)
      logical              :: flip_hor=.false.   ! True -> flip the image horizontally as soon as read
      logical              :: flip_ver=.false.   ! True -> flip the image vertically as soon as read
      real(kind=cp)        :: L_min,L_max,L_central ! Lambda minimum , Lambda maximun, Central Lambda (used in quasi monochromatic case)
      real(kind=cp)        :: gap_min,gap_max    ! gamma minimum , gamma  maximun  (Positive values) | This change is needed due to odd
      real(kind=cp)        :: gan_min,gan_max    ! gamma minimum , gamma  maximun  (Negative values) | positions of flat detectors.
      real(kind=cp)        :: nu_min,nu_max      ! minimum admissible nu  and maximum admissible nu
      real(kind=cp)        :: d_min              ! Resolution limit ... if not given d_min= 2/L_min
      real(kind=cp)        :: x_min,x_max        ! in mm
      real(kind=cp)        :: z_min,z_max
      real(kind=cp)        :: xo,zo              !Origin of detector system in pixels. Normally (Np_h/2,Np_v/2)
      real(kind=cp), dimension(2) :: dism        !Displacement of the Origin of detector system w.r.t. (Np_h/2,Np_v/2) in mm
                                                 !This is applied in calculating the position of reflections in mm or other characteristics
                                                 !coming from input x,z coordinates in mm: dism=(/xo-Np_h/2,zo-Np_v/2)*conv to mm
   end type Laue_Instrument_Type

   type, public :: Laue_Ref_Type
      integer                      :: stats=-1    ! Status of the reflection (=0, observed)
      integer                      :: domain=1    ! Index for identifying the twin
      integer                      :: mult=0      ! Laue multiplicity
      integer                      :: nodal=0     ! Nodal index (1 for a nodal reflection: coprime integers)
      real(kind=cp)                :: ds=0.0      ! D-spacing in Angstroms of the reflection
      real(kind=cp)                :: gamma=0.0   ! Gamma angle in L-system
      real(kind=cp)                :: nu=0.0      ! Nu angle in L-system
      real(kind=cp)                :: Ttheta=0.0  ! 2Theta angle
      real(kind=cp)                :: Lambda=0.0  ! Wavelength
      real(kind=cp), dimension(3)  :: h=0.0       ! Reciprocal space coordinates
      real(kind=cp), dimension(3)  :: zv=0.0      ! Cartesian coordinates of h in the laboratory system
      real(kind=cp)                :: Obs_int=0.0 ! Observed intensity
      real(kind=cp)                :: Cal_int=0.0 ! Calculated intensity
      real(kind=cp)                :: Sigma=0.0   ! Standard deviation of the observed intensity
      real(kind=cp)                :: x=0.0,z=0.0 ! coordinates of the spot in mm
      real(kind=cp)                :: stheta=0.0  ! spherical angle Theta of the scattering vector in L-system (or sinTheta/Lambda)
      real(kind=cp)                :: sphi=0.0    ! spherical angle Phi   of the scattering vector in L-system (or auxiliary variable)
      integer,       dimension(3)  :: kindex=0    ! Indices for propagation vectors
                                                  ! i1: makes reference to the propagation vector numbered i1
                                                  ! i2: makes reference to the arm i2 of the star of vector i1
                                                  ! i3: harmonic number
                                                  ! h=(hx,hy,hz)= h0 + i3 * k(i1,i2)
      integer, dimension(:),allocatable :: hs     ! integer indices of reflection in superspace formulation 3+d
                                                  ! hs=(h,k,l,m,n,p,...). They are generated using q-vectors and harmonic indices
   end type Laue_Ref_Type

   type, public :: Laue_Ref_List_Type
      integer :: nref
      type(Laue_Ref_Type), dimension(:), allocatable :: LR
   end type  Laue_Ref_List_Type

   type, public:: PeakFind_Parameters_Type
      integer :: d_min   = 30   ! minimum distance between peaks
      integer :: m_pk    =  0   ! minimum slope of peaks
      integer :: pk_ar   = 30   ! minimum area of peaks
      integer :: l_siz   =  3   ! how many times pk_ar to consider maximum area of peaks
      integer :: cutoff  =  8   ! Consider peaks only when Intensity > Threshold = bck + cutoff * Sigma(bck)
      integer :: blocksz = 50   ! Size in pixels of the squares for searching local backgrounds in threshold algorithm
   end type PeakFind_Parameters_Type

   type, public :: Peak_Type
      integer                    :: npix=0               !Number of pixels contributing to peak
      integer                    :: pnt=-1               !Pointer to the position in the list of calculated peaks
      real(kind=cp), dimension(2):: cdm=(/0.0,0.0/)      !Baricentre of the peak
      real(kind=cp), dimension(2):: pos=(/0.0,0.0/)      !Position of the Peak (Maximum or Centroid - centre of mass- weighted with intensity)
      real(kind=cp), dimension(3):: Itens=(/0.0,0.0,0.0/)!2 eingenvalues of Inertia tensor + rotation angle (Lx,Lz,alpha)
      real(kind=cp)              :: Intensity=0.0        !Integrated intensity or intensity of maximum (depends on the algorithm using the type)
      real(kind=cp)              :: Sigma=0.0            !Estimated standard deviation
      logical                    :: is_sat=.false.
   end type Peak_Type

   type, public :: Peak_List_Type
      integer   :: N_Peaks
      type(Peak_Type), dimension(:), allocatable :: Peak
   end type Peak_List_Type

   !---- List of public procedures ----!


   !---- Interface Zone ----!
   interface Calc_Laue_XZ_Position
      module procedure Calc_Laue_XZ_Position_Ref
      !module procedure Calc_Laue_XZ_Position_gamnu
   end Interface Calc_Laue_XZ_Position

   interface

      Module Subroutine Allocate_Laue_Ref_List(L,n,d)
         class(Laue_Ref_List_Type), intent (in out) :: L
         integer,                   intent (in)     :: n
         integer, optional,         intent (in)     :: d
      End Subroutine Allocate_Laue_Ref_List

      Module Subroutine Calc_Laue_Spot_XZ_Angles(LaueDiff,R,RL,opt,dk_min)
         type(Laue_Instrument_Type),   intent(in)    :: LaueDiff
         type(Laue_Ref_Type),          intent(inout) :: R        ! Laue reflection
         real(kind=cp), dimension(3,3),intent(in)    :: RL       ! Matrix trasforming hkl to laboratory cartesian system
                                                                 ! Normally RL = Rx Ry Rz UB
         integer,       optional,      intent(in)    :: opt
         real(kind=cp), optional,      intent(in)    :: dk_min   ! Resolution for satellite reflections
      End Subroutine Calc_Laue_Spot_XZ_Angles

      Module Subroutine Calc_Laue_XZ_Position_Ref(LaueDiff,R,shift)
         type(Laue_Instrument_Type),          intent(in)     :: LaueDiff
         type(Laue_Ref_Type),                 intent(in out) :: R  !Laue reflection
         real(kind=cp), dimension(3),optional,intent(in)     :: shift
      End Subroutine Calc_Laue_XZ_Position_Ref

      Module Subroutine Calc_RD_rOD(LaueDiff)
         type(Laue_Instrument_Type), intent(in out)  :: LaueDiff
      End Subroutine Calc_RD_rOD

      Module Subroutine Calc_Visible_Reflections_List(LDiff,M,hkl,Ref_List)
        type(Laue_Instrument_Type),    intent(in)    :: LDiff
        real(kind=cp), dimension(3,3), intent(in)    :: M        ! Matrix transforming a reflection hkl
                                                                 ! to the Cartesian in L-system to be visible
        type(RefList_Type),            intent(in)    :: hkl      ! List of all accesible reflections computed
                                                                 ! by function Generate_Laue_Reflections
        type(Laue_Ref_List_Type),      intent(inout) :: Ref_List ! It must be allocated before calling this subroutine
      End Subroutine Calc_Visible_Reflections_List

      Module Subroutine Generate_Laue_Reflections(ldiff,cell,spg,hkl,n)
         !> Compute all acccesible reflections for the given Laue limits,
         !> cell and spacegroup
         type(Laue_Instrument_Type), intent(in)    :: ldiff  !> Laue instrument
         class(Cell_G_Type),         intent(in)    :: cell   !> Crystal cell
         class(Spg_Type),            intent(in)    :: spg    !> Space group
         type(RefList_Type),         intent(inout) :: hkl    !> List of reflections
         integer,                    intent(out)   :: n      !> Expected maximum number of Laue reflections
      End Subroutine Generate_Laue_Reflections
      
      Module Subroutine Get_Stereographic_Projection_From_ZV(r,x,z)
         !> Compute the stereographic projection from the
         !> scattering vector
         type(Laue_Ref_Type), intent(in)  :: r !> Reflection to be projected
         real(kind=4),        intent(out) :: x !> x coordinate of the projection
         real(kind=4),        intent(out) :: z !> z coordinate of the projection
      End Subroutine Get_Stereographic_Projection_From_ZV

      Module Subroutine Init_Header(Head)
         type(Header_Type), intent(in out) :: Head
      End Subroutine Init_Header

      Module Subroutine Init_Laue_Instrm(LaueDiff)
         Type(Laue_Instrument_Type),  intent(in out) :: LaueDiff
      End Subroutine Init_Laue_Instrm

      Elemental Module Subroutine Init_Laue_Ref(Ref,d)
         type(Laue_Ref_Type), intent(inout) :: Ref
         integer, optional,   intent(in)    :: d
      End Subroutine Init_Laue_Ref
      
      Module Subroutine Output_Excluded_Regions(lun,ExclR)
         integer,                     intent(in) :: lun
         type(Excluded_Regions_Type), intent(in) :: ExclR
      End Subroutine Output_Excluded_Regions

      Module Subroutine Peak_Find_Threshold(Fnd_par,Data2D,Mask_Excl,Mask2D,ExclR,Peaks)
         type(PeakFind_Parameters_Type),        intent(in)  :: Fnd_par
        integer(kind=DATAKIND), dimension(:,:), intent(in)  :: Data2D     ! Converted data
        integer(kind=1), dimension(:,:),        intent(in)  :: Mask_Excl  ! Mask for excluded regions, set by Set_Mask_Excl
        integer(kind=1), dimension(:,:),        intent(out) :: Mask2D     ! Mask Data (0 = Background, 1 = maximum, 2 = nodal maximum,
                                                                          !            3 = peak area but not maximum)
        type(Excluded_Regions_Type),            intent(in)  :: ExclR
        type(Peak_List_Type),                   intent(out) :: Peaks
      End Subroutine Peak_Find_Threshold

      Module Subroutine Pix_To_Mil(LaueDiff,xpc,ypc,xmm,ymm)
         type(Laue_Instrument_Type), intent(in)  :: LaueDiff
         real(kind=cp),              intent(in)  :: xpc,ypc
         real(kind=cp),              intent(out) :: xmm,ymm
      End Subroutine Pix_To_Mil

      Module Subroutine Quadratic_Eqn_Solver(a,b,c,x1,x2,ok)
         real(kind=cp), intent(in)  :: a,b,c
         real(kind=cp), intent(out) :: x1,x2
         logical,       intent(out) :: ok
      End Subroutine Quadratic_Eqn_Solver

      Module Subroutine Read_HBin_Header_2D_Image(Image_File,Max_Length,header,ok,mess,bin_lun,h_string)
         character(len=*),         intent(in) :: Image_File
         integer,                  intent(in) :: Max_Length
         type(Header_Type),        intent(out):: header
         logical,                  intent(out):: ok
         character(len=*),         intent(out):: mess
         integer,                  intent(out):: bin_lun
         character(len=*),optional,intent(out):: h_string
      End Subroutine Read_HBin_Header_2D_Image

      Module Subroutine Read_Binary_2D_Image(bin_lun,header,Image,nf)
         integer,                                 intent(in)    :: bin_lun  !Logical unit of binary file
         type(header_type),                       intent(inout) :: header
         integer(kind=DATAKIND),  dimension(:,:), intent(out)   :: Image
         integer ,                                intent(in)    :: nf       !number of the frame to be added
      End Subroutine Read_Binary_2D_Image

      Module Subroutine Read_Laue_Image(Image_File,Nrow,Ncol,Datam,ICd,header,extension,h_string,directory)
         character(len=*),                      intent(in)    :: Image_File
         integer,                               intent(in out):: Nrow, Ncol
         integer(kind=DATAKIND),dimension(:,:), intent(out)   :: Datam
         type(Image_Conditions), optional,      intent(out)   :: ICd
         type(Header_Type),      optional,      intent(out)   :: header
         character(len=5),       optional,      intent(out)   :: extension
         character(len=*),       optional,      intent(out)   :: h_string
         character(len=*),       optional,      intent(in)    :: directory
      End Subroutine Read_Laue_Image

      Module Subroutine Read_Laue_Instrm(filenam,LaueDiff,ExclR)
         character(len=*),                    intent(in)  :: filenam
         type(Laue_Instrument_Type),          intent(out) :: LaueDiff
         type(Excluded_Regions_Type),optional,intent(out) :: ExclR
      End Subroutine Read_Laue_Instrm

      Module Subroutine Set_Mask_Excl(ExclR,DataM,Mask_Excl)
         type(Excluded_Regions_type),                              intent (in)   :: ExclR
         integer(kind=DATAKIND),      dimension(:,:),              intent (in)   :: DataM
         integer(kind=1),             dimension(:,:), allocatable, intent(inout) :: Mask_Excl
      End Subroutine Set_Mask_Excl

   end interface

End Module CFML_Laue
