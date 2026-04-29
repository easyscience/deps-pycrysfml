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
!!---- MODULE: CFML_KVEC_SYMMETRY
!!----   INFO: Series of procedures handling operations with KVEC-Magnetic Symmetry
!!----         and Magnetic Structures. This module is maintained for compatibility
!!----         with the previous CrysFML library for handling magnetic structures
!!----
!!----
!!---- HISTORY
!!----
!!----    Update: 07/03/2011
!!----    Update: January 2022 (JRC: This module have been imported and adapted from
!!----                               the F95 CrysFML library. Many subroutines have been
!!----                               removed and some other have been added)
!!----
!!----
!!---- DEPENDENCIES
!!--++    Use CFML_GlobalDeps,         only: cp, dp, tpi, Err_CFML, Clear_Error, Set_Error
!!--++    Use CFML_Rational
!!--++    Use CFML_Maths,              only: Trace, Zbelong, Modulo_Lat, equal_matrix,             &
!!--++                                       Equal_Vector, Get_Cart_From_Spher,Determ3D
!!--++    Use CFML_Symmetry_Tables,    only: ltr_a,ltr_b,ltr_c,ltr_i,ltr_r,ltr_f,Sys_cry,LATT
!!--++    Use CFML_gSpaceGroups,       only: SPG_Type, Set_SpaceGroup, &
!!--++                                       symmetry_symbol, get_stabilizer
!!--++    Use CFML_Strings,            only: u_case, l_case, Frac_Trans_1Dig, Get_Separator_Pos,Pack_String, &
!!--++                                       Frac_Trans_2Dig, Get_Mat_From_Symb, Get_Words,       &
!!--++                                       Get_Transf, file_list_type, string_fraction_2Dig
!!--++    Use CFML_Atoms,              only: Allocate_mAtom_list, mAtom_List_Type
!!--++    Use CFML_Scattering_Tables,  only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form, &
!!--++                                       Magnetic_Form
!!--++    Use CFML_Propagation_Vectors,only: K_Equiv_Minus_K
!!--++    Use CFML_Metrics,            only: Cell_G_Type, Set_Crystal_Cell
!!----
!!---- VARIABLES
!!--..    Types
!!----    SYM_OPER_TYPE
!!----    MSYM_OPER_TYPE
!!----    MAGNETIC_DOMAIN_TYPE
!!----    MAGSYMM_K_TYPE
!!--..
!!----
!!---- PROCEDURES
!!----    Functions:
!!----       APPLYSO
!!----       APPLYMSO
!!----       IS_XYZ
!!----       LATTICE_TRANS
!!----       VECLENGTH
!!----
!!----    Subroutines:
!!----       CALC_INDUCED_SK
!!----       GET_ATOM_2ND_TENSOR
!!----       GET_SYMSYMB  (Overloaded)
!!--++       GET_SYMSYMB_I
!!--++       GET_SYMSYMB_R
!!----       LATSYM
!!----       READ_MSYMM
!!----       READ_XSYM
!!----       READN_SET_MAGNETIC_KV_STRUCTURE
!!----       WRITE_MAGNETIC_STRUCTURE
!!----
!!
 Module CFML_kvec_Symmetry

  !---- Use Modules ----!
  Use CFML_GlobalDeps,         only: cp, dp, tpi, Err_CFML, Clear_Error, Set_Error, CFML_debug
  Use CFML_Rational
  Use CFML_Maths,              only: Trace, Zbelong, Modulo_Lat, equal_matrix, Lat_Modulo,          &
                                     Equal_Vector, Get_Cart_From_Spher,Determ3D, Co_linear
  Use CFML_Symmetry_Tables,    only: ltr_a,ltr_b,ltr_c,ltr_i,ltr_r,ltr_f,Sys_cry,LATT
  Use CFML_gSpaceGroups,       only: SPG_Type, Set_SpaceGroup, Point_Orbit, Orbit_Type, Orbit_list, &
                                     symmetry_symbol, get_stabilizer, Kvect_Info_Type, Get_Orbit
  Use CFML_Strings,            only: u_case, l_case, Frac_Trans_1Dig, Get_Separator_Pos,Pack_String,&
                                     Frac_Trans_2Dig, Get_Mat_From_Symb, Get_Words, string_Numstd,  &
                                     Get_Transf, file_list_type, string_fraction_2Dig, Get_DateTime
  Use CFML_Atoms,              only: Allocate_mAtom_list, mAtom_List_Type, mAtom_Type, Atlist_Type
  Use CFML_Scattering_Tables,  only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form, &
                                     Magnetic_Form
  Use CFML_Propagation_Vectors,only: K_Equiv_Minus_K
  Use CFML_Metrics,            only: Cell_G_Type, Set_Crystal_Cell

  !---- Variables ----!
  implicit none

  private

  !---- List of public functions ----!
  public :: ApplyMSO, ApplySO,veclength, Lattice_Trans, get_kvec_info

  !---- List of public subroutines ----!
  public :: Readn_Set_Magnetic_Kv_Structure, Write_Magnetic_Structure, Get_kv_Orbit, &
            Init_MagSymm_k_Type,Calc_Induced_Sk,Latsym,Get_SymSymb,Read_Xsym,        &
            Calc_Magnetic_Moment_kvec, Get_kv_Orbit_ZeroCell, Write_CIF_P1_ssg

  !---- Definitions ----!

  !!----
  !!---- TYPE :: SYM_OPER_TYPE
  !!--..
  !!---- Type, public :: Sym_Oper_Type
  !!----    integer,       dimension(3,3) :: Rot     !  Rotational Part of Symmetry Operator
  !!----    real(kind=cp), dimension(3)   :: Tr      !  Traslational part of Symmetry Operator
  !!---- End Type  Sym_Oper_Type
  !!----
  !!----    Definition of Variable
  !!----
  !!---- Update: February - 2005
  !!
  Type, public :: Sym_Oper_Type
     integer,       dimension(3,3) :: Rot
     real(kind=cp), dimension(3)   :: Tr
  End Type Sym_Oper_Type

  !!----
  !!---- TYPE :: MSYM_OPER_TYPE
  !!--..
  !!---- Type, public :: MSym_Oper_Type
  !!----    integer, dimension(3,3) :: Rot     !  Rotational Part of Symmetry Operator
  !!----    real(kind=cp)           :: Phas    !  Phase in fraction of 2pi
  !!---- End Type  MSym_Oper_Type
  !!----
  !!----  Definition of Magnetic symmetry operator type
  !!----
  !!---- Update: April-2005
  !!
  Type, public :: MSym_Oper_Type
     integer, dimension(3,3) :: Rot
     real(kind=cp)           :: Phas
  End Type MSym_Oper_Type

  !!----
  !!---- TYPE :: MAGNETIC_DOMAIN_TYPE
  !!--..
  !!---- Type, public :: Magnetic_Domain_type
  !!----    integer                           :: nd=0          !Number of rotational domains (not counting chiral domains)
  !!----    logical                           :: Chir=.false.  !True if chirality domains exist
  !!----    logical                           :: trans=.false. !True if translations are associated to matrix domains
  !!----    logical                           :: Twin=.false.  !True if domains are to be interpreted as twins
  !!----    integer,dimension(3,3,24)         :: DMat=0        !Domain matrices to be applied to Fourier Coefficients
  !!----    real(kind=cp), dimension (2,24)   :: Dt=0.0        !Translations associated to rotation matrices
  !!----    real(kind=cp), dimension (2,24)   :: pop=0.0       !Populations of domains (sum=1,
  !!----                                                       !the second value is /=0 for chir=.true.)
  !!----    real(kind=cp), dimension (2,24)   :: pop_std=0.0   !Standard deviations of Populations of domains
  !!----    integer,dimension (2,24)          :: Lpop=0        !Number of the refined parameter
  !!----    real(kind=cp), dimension (2,24)   :: Mpop=0.0      !Refinement codes for populations
  !!----    character(len=10),dimension (2,24):: Lab           !Label of domain
  !!---- End type Magnetic_Domain_type
  !!----
  !!----
  !!--<<
  !!----  Magnetic S-domains corresponds to a different magnetic structure obtained from
  !!----  the domain 1 (actual model) by applying a rotational operator to the Fourier
  !!----  coefficients of magnetic moments. This rotational operator corresponds to a
  !!----  symmetry operator of the paramagnetic group that is lost in the ordered state.
  !!----  Chirality domains are simply obtained by changing the sign of the imaginary
  !!----  components of the Fourier coefficients. For each rotational domain two chiralities
  !!----  domains exist.
  !!-->>
  !!----
  !!---- Updated: October - 2006, July-2012 (JRC, more type of domains), November 2013 (standard deviations)
  !!
  Type, public :: Magnetic_Domain_type
     integer                           :: nd=0          !Number of rotational domains (not counting chiral domains)
     logical                           :: Chir=.false.  !True if chirality domains exist
     logical                           :: trans=.false. !True if translations are associated to matrix domains
     logical                           :: Twin=.false.  !True if domains are to be interpreted as twins
     integer,dimension(3,3,24)         :: DMat=0        !Domain matrices to be applied to Fourier Coefficients
     real(kind=cp), dimension (3,24)   :: Dt=0.0        !Translations associated to rotation matrices
     real(kind=cp), dimension (2,24)   :: pop=0.0       !Populations of domains (sum=1,
                                                        !the second value is /=0 for chir=.true.)
     integer      , dimension (2,24)   :: Lpop=0        !Number of the refined parameter
     real(kind=cp), dimension (2,24)   :: Mpop=0.0      !Multipliers for population
     real(kind=cp), dimension (2,24)   :: pop_std=0.0   !Standard deviations of Populations of domains
     character(len=10),dimension (2,24):: Lab           !Label of domain
  End type Magnetic_Domain_type

  !!----
  !!---- TYPE :: MAGSYMM_K_TYPE
  !!--..
  !!---- Type, Public :: MagSymm_k_Type
  !!----    character(len=31)                        :: MagModel   ! Name to characterize the magnetic symmetry
  !!----    character(len=15)                        :: Sk_type    ! If Sk_type="Spherical_Frame" the input Fourier coefficients are in spherical components
  !!----    character(len=1)                         :: Fcoef_type ! If Fcoef_type="S" (default) Fourier coefficient Sk,  If Fcoef_type="T" Fourier coefficient Tk=Sk exp(2pi i k r)
  !!----    character(len=15)                        :: BNS_number ! Added for keeping the same information
  !!----    character(len=15)                        :: OG_number  ! as in Magnetic_SPG_Type
  !!----    Character(len=34)                        :: BNS_symbol !             "
  !!----    Character(len=34)                        :: OG_symbol  !             "
  !!----    Integer                                  :: MagType    !             "
  !!----    Integer                                  :: Parent_num !             "
  !!----    Character(len=20)                        :: Parent_spg !             "
  !!----    character(len=1)                         :: Latt       ! Symbol of the crystallographic lattice
  !!----    integer                                  :: nirreps    ! Number of irreducible representations (max=4, if nirreps /= 0 => nmsym=0)
  !!----    Integer,             dimension(4)        :: irrep_dim       !Dimension of the irreps
  !!----    Integer,             dimension(4)        :: small_irrep_dim !Dimension of the small irrep
  !!----    Integer,             dimension(4)        :: irrep_modes_number !Number of the modes of the irrep
  !!----    Character(len=15),   dimension(4)        :: irrep_id        !Labels for the irreps
  !!----    Character(len=20),   dimension(4)        :: irrep_direction !Irrep direction in representation space
  !!----    Character(len=20),   dimension(4)        :: irrep_action    !Irrep character primary or secondary
  !!----    integer                                  :: nmsym      ! Number of magnetic operators per crystallographic operator (max=8)
  !!----    integer                                  :: centred    ! =0 centric centre not at origin, =1 acentric, =2 centric (-1 at origin)
  !!----    integer                                  :: mcentred   ! =1 Anti/a-centric Magnetic symmetry, = 2 centric magnetic symmetry
  !!----    integer                                  :: nkv        ! Number of independent propagation vectors
  !!----    real(kind=cp),       dimension(3,12)     :: kvec       ! Propagation vectors
  !!----    Character(len=4),    dimension(12)       :: kv_label
  !!----    logical,             dimension(12)       :: k_eqv_minusK
  !!----    integer                                  :: Num_Lat    ! Number of centring lattice vectors
  !!----    real(kind=cp), dimension(3,4)            :: Ltr        ! Centring translations
  !!----    integer                                  :: Numops     ! Reduced number of crystallographic Symm. Op.
  !!----    integer                                  :: Multip     ! General multiplicity of the space group
  !!----    integer,             dimension(4)        :: nbas       ! Number of basis functions per irrep (if nbas < 0, the corresponding basis is complex).
  !!----    integer,             dimension(12,4)     :: icomp      ! Indicator (0 pure real/ 1 pure imaginary) for coefficients of basis fucntions
  !!----    Complex(kind=cp),    dimension(3,12,48,4):: basf       ! Basis functions of the irreps of Gk
  !!----    character(len=80),   dimension(:),   allocatable :: SymopSymb  ! Alphanumeric Symbols for SYMM
  !!----    type(Sym_Oper_Type), dimension(:),   allocatable :: SymOp      ! Crystallographic symmetry operators (48)
  !!----    character(len=80),   dimension(:,:), allocatable :: MSymopSymb ! Alphanumeric Symbols for MSYMM (48,8)
  !!----    type(MSym_Oper_Type),dimension(:,:), allocatable :: MSymOp     ! Magnetic symmetry operators (48,8)
  !!---- End Type MagSymm_k_Type
  !!----
  !!----  Definition of the MagSymm_k_type derived type, encapsulating the information
  !!----  concerning the crystallographic symmetry, propagation vectors and magnetic matrices.
  !!----  Needed for calculating magnetic structure factors.
  !!----
  !!---- Created: April   - 2005
  !!---- Updated: January - 2014
  !!
  Type, Public :: MagSymm_k_Type
     character(len=31)                        :: MagModel   ! Name to characterize the magnetic symmetry
     character(len=15)                        :: Sk_type
     character(len=1)                         :: Fcoef_type
     character(len=15)                        :: BNS_number ! Added for keeping the same information
     character(len=15)                        :: OG_number  ! as in Magnetic_SPG_Type
     Character(len=34)                        :: BNS_symbol !             "
     Character(len=34)                        :: OG_symbol  !             "
     Integer                                  :: MagType    !             "
     Integer                                  :: Parent_num !             "
     Character(len=20)                        :: Parent_spg !             "
     character(len=1)                         :: Latt
     integer                                  :: nirreps
     Integer,             dimension(4)        :: irrep_dim          !Dimension of the irreps
     Integer,             dimension(4)        :: small_irrep_dim    !Dimension of the small irrep
     Integer,             dimension(4)        :: irrep_modes_number !Number of the mode of the irrep
     Character(len=15),   dimension(4)        :: irrep_id           !Labels for the irreps
     Character(len=20),   dimension(4)        :: irrep_direction    !Irrep direction in representation space
     Character(len=20),   dimension(4)        :: irrep_action       !Irrep character primary or secondary
     integer                                  :: nmsym
     integer                                  :: centred
     integer                                  :: mcentred
     integer                                  :: nkv
     real(kind=cp),       dimension(3,12)     :: kvec
     Character(len=4),    dimension(12)       :: kv_label
     logical,             dimension(12)       :: k_eqv_minusK
     integer                                  :: Num_Lat
     real(kind=cp), dimension(3,4)            :: Ltr
     integer                                  :: Numops
     integer                                  :: Multip
     integer,             dimension(4)        :: nbas
     integer,             dimension(12,4)     :: icomp
     Complex(kind=cp),    dimension(3,12,48,4):: basf
     character(len=80),   dimension(:),   allocatable :: SymopSymb  ! Alphanumeric Symbols for SYMM
     type(Sym_Oper_Type), dimension(:),   allocatable :: SymOp      ! Crystallographic symmetry operators (48)
     character(len=80),   dimension(:,:), allocatable :: MSymopSymb ! Alphanumeric Symbols for MSYMM (48,8)
     type(MSym_Oper_Type),dimension(:,:), allocatable :: MSymOp     ! Magnetic symmetry operators (48,8)
  End Type MagSymm_k_Type

  Type, public :: Point_Orbit_kv
     integer                                      :: Mult ! Multiplicity of the orbit
     character(len=15),allocatable,dimension(:)   :: Labs ! Label of each atom
     character(len=2), allocatable,dimension(:)   :: ChemSymb    ! Chemical symbol
     real(kind=cp),    allocatable,dimension(:,:) :: pos  ! (3,Mult) Positions of the points
     real(kind=cp),    allocatable,dimension(:,:) :: mom  ! (3,Mult) Associated moments
     integer,          allocatable,dimension(:,:) :: Latt ! (3,Mult) Lattice translation to put the atom with positive coordinates
     integer,          allocatable,dimension(:)   :: pts  ! (  Mult) Pointer to symmetry operator
     real(kind=cp),                dimension(3)   :: Latr ! Lattice translation corresponding to the origin of an arbitrary cell in the crystal
  End Type Point_Orbit_kv

  Type, extends(Point_Orbit_kv), public :: Mod_Orbit
     integer :: nd  !Number of incommensurate wave vectors
     Complex(kind=cp), allocatable, dimension(:,:,:) :: TFourier !(3,0:nd,Mult)
  End Type Mod_Orbit

  integer, public :: nlat,inlat
  real(kind=cp), dimension(3,24), public  :: Ltr    ! Centering Lattice Translations set with LatSym
  real(kind=cp), private, parameter :: eps_symm=0.001

  Interface Get_SymSymb
     module procedure Get_SymSymbI
     module procedure Get_SymSymbR
  end Interface Get_SymSymb

  Interface
    !-------------------!
    !---- Functions ----!
    !-------------------!
    Module Function Is_Xyz(A) Result(Iss_Xyz)
       !---- Argument ----!
       character(len=*), intent(in) :: A
       logical                      :: Iss_xyz
    End Function Is_Xyz
    Module Function ApplySO(Op,V) Result(Applysop)
       !---- Arguments ----!
       Type(Sym_Oper_Type),          intent(in) :: Op
       real(kind=cp), dimension(3),  intent(in) :: v
       real(kind=cp), dimension(3)              :: ApplySOp
    End Function ApplySO

    Module Function ApplyMSO(Op,Sk) Result(Skp)
       !---- Arguments ----!
       Type(MSym_Oper_Type), intent(in) :: Op
       Complex, dimension(3),intent(in) :: Sk
       Complex, dimension(3)            :: Skp
    End Function ApplyMSO

    Module Function Lattice_Trans(V,Lat) Result(Lattice_Transl)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: v
       character(len=*),            intent( in) :: Lat
       logical                                  :: Lattice_Transl
    End Function  Lattice_Trans

    Module Function Veclength(a,b) Result(c)
       !---- Arguments ----!
       real(kind=cp), intent(in)  , dimension(3,3)       :: a
       real(kind=cp), intent(in)  , dimension(3  )       :: b
       real(kind=cp)                                     :: c
    End Function Veclength

    Module Function get_kvec_info(MGp) result(kinfo)
      type(MagSymm_k_Type), intent(in) :: MGp
      type(Kvect_Info_Type)            :: kinfo
    End Function get_kvec_info
    !---------------------!
    !---- Subroutines ----!
    !---------------------!
    Module Subroutine Get_kv_Orbit_ZeroCell(Am,MGp,orbit,sig,modl)
       !---- Arguments ----!
       type(mAtom_List_Type),        intent(in)  :: Am
       Type(MagSymm_k_Type),         intent(in)  :: MGp
       type(Mod_Orbit),              intent(out) :: orbit
       real(kind=cp), optional,      intent(in)  :: sig
       logical,       optional,      intent(in)  :: modl
    End Subroutine Get_kv_Orbit_ZeroCell

    Module Subroutine Write_CIF_P1_ssg(fname,Cell,SpG,A,orbit,kinfo)
      character(len=*),     intent(in) :: fname
      class(Cell_G_Type),   intent(in) :: Cell
      class(SPG_type),      intent(in) :: SpG
      type (Atlist_Type),   intent(in) :: A
      type(Mod_Orbit),      intent(in) :: orbit
      type(Kvect_Info_Type),intent(in) :: kinfo
    End Subroutine Write_CIF_P1_ssg

    Module Subroutine Calc_Magnetic_Moment_kvec(Am,MGp,atm_lab,num_op,R_latt,vp,moment,sig,glb_phase,modl)
       !---- Arguments ----!
       type(mAtom_List_Type),      intent(in) :: Am
       type(MagSymm_k_Type),       intent(in) :: MGp
       character(len=*),           intent(in) :: atm_lab
       Integer,                    intent(in) :: num_op
       real(kind=cp), dimension(3),intent(in) :: R_latt
       real(kind=cp), dimension(3),intent(out):: vp
       real(kind=cp), dimension(3),intent(out):: moment
       real(kind=cp), optional,    intent(in) :: sig
       real(kind=cp), optional,    intent(in) :: glb_phase
       logical,       optional,    intent(in) :: modl
     End Subroutine Calc_Magnetic_Moment_kvec

    Module Subroutine LatSym(SYMB,numL,latc)
       !---- Argument ----!
       character(len=*),                        intent(in)  :: SYMB
       integer, optional,                       intent(in)  :: numL
       real(kind=cp),optional, dimension(:,:),  intent(in)  :: latc  !general vector (JRC, Jan2014)
    End Subroutine Latsym

    Module Subroutine Get_Atom_2nd_Tensor_Ctr(x,TensVal,Spgr,Codini,Icodes,Multip,Ord,Ss,Ipr)
       real(kind=cp), dimension(3),             intent(in    ) :: x
       real(kind=cp), dimension(6),             intent(in out) :: TensVal
       type(SPG_type),                          intent(in    ) :: Spgr
       Integer,                                 intent(in out) :: Codini
       Integer, dimension(6),                   intent(in out) :: Icodes
       real(kind=cp), dimension(6),             intent(in out) :: Multip
       integer,                       optional, intent(in    ) :: Ord
       integer, dimension(:),         optional, intent(in    ) :: Ss
       integer,                       optional, intent(in    ) :: Ipr
    End Subroutine Get_Atom_2nd_Tensor_Ctr

    Module Subroutine Get_kv_Orbit(At,MGp,R_Latt,orbit,sig,modl)
       !---- Arguments ----!
       Type(mAtom_Type),             intent(in)  :: At
       Type(MagSymm_k_Type),         intent(in)  :: MGp
       real(kind=cp), dimension(3),  intent(in)  :: R_latt
       type(Point_Orbit_kv),         intent(out) :: orbit
       real(kind=cp), optional,      intent(in)  :: sig
       logical,       optional,      intent(in)  :: modl
    End Subroutine Get_kv_Orbit

    Module Subroutine Read_Xsym(Info,Istart,Sim,Tt,ctrl)
       !---- Arguments ----!
       character (len=*),                     intent(in)     :: Info
       integer,                               intent(in)     :: istart
       integer, dimension(3,3),               intent(out)    :: sim
       real(kind=cp), optional, dimension(3), intent(out)    :: tt
       logical,       optional,               intent(in)     :: ctrl
    End Subroutine Read_Xsym

    Module Subroutine Read_Msymm(Info,Sim,P_Mag,ctrl)
       !---- Arguments ----!
       character (len=*),       intent( in) :: Info
       integer, dimension(3,3), intent(out) :: sim
       real(kind=cp),           intent(out) :: p_mag
       logical, optional,       intent(in)  :: ctrl
    End Subroutine Read_Msymm

    Module Subroutine Get_SymSymbI(X,T,Symb)
       !---- Arguments ----!
       integer,       dimension(3,3), intent( in) :: x
       real(kind=cp), dimension(3),   intent( in) :: t
       character (len=*),             intent(out) :: symb
    End Subroutine Get_SymSymbI

    Module Subroutine Get_SymSymbR(X,T,Symb)
       !---- Arguments ----!
       real(kind=cp),    dimension(3,3), intent( in) :: x
       real(kind=cp),    dimension(3),   intent( in) :: t
       character (len=*),                intent(out) :: symb
    End Subroutine Get_SymSymbR

    Module Subroutine Calc_Induced_Sk(cell,SpG,MField,dir_MField,Atm,ipr)
       !---- Arguments ----!
       type(Cell_G_Type),          intent(in)     :: Cell
       type(SPG_Type),             intent(in)     :: SpG
       real(kind=cp),              intent(in)     :: MField
       real(kind=cp),dimension(3), intent(in)     :: dir_MField
       type(Matom_list_type),    intent(in out)   :: Atm
       integer, optional,          intent(in)     :: ipr
    End Subroutine Calc_Induced_Sk

    Module Subroutine Init_MagSymm_k_Type(MGp)
       !---- Arguments ----!
       type(MagSymm_k_Type),  intent (in out) :: MGp
    End Subroutine Init_MagSymm_k_Type

    Module Subroutine Readn_Set_Magnetic_Kv_Structure(file_cfl,n_ini,n_end,MGp,Am,Mag_dom,Cell,database_path)
       !---- Arguments ----!
       type(file_list_type),                intent (in)     :: file_cfl
       integer,                             intent (in out) :: n_ini, n_end
       type(MagSymm_k_Type),                intent (out)    :: MGp
       type(mAtom_List_Type),               intent (out)    :: Am
       type(Magnetic_Domain_type),optional, intent (out)    :: Mag_dom
       type(Cell_G_Type),         optional, intent (in)     :: Cell
       character(len=*),          optional, intent (in)     :: database_path
    End Subroutine Readn_Set_Magnetic_Kv_Structure

    Module Subroutine Write_Magnetic_Structure(Ipr,MGp,Am,Mag_Dom,cell)
       !---- Arguments ----!
       Integer,                    intent(in)           :: Ipr
       type(MagSymm_k_Type),       intent(in)           :: MGp
       type(mAtom_List_Type),      intent(in)           :: Am
       type(Magnetic_Domain_Type), intent(in), optional :: Mag_Dom
       type(Cell_G_Type),          intent(in), optional :: cell
    End Subroutine Write_Magnetic_Structure

  End Interface

 End Module CFML_kvec_Symmetry

