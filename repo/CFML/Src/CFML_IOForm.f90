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
!!----               Ross Angel         (University of Padova)
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
!!---- MODULE: CFML_IO_Formats
!!----   INFO: Creation/Conversion for several formats
!!----
!!
Module CFML_IOForm
   !---- Use modules ----!
   Use CFML_GlobalDeps,        only: SP, CP, PI, EPS, TAB, Err_CFML, Clear_Error, CFML_debug
   Use CFML_Maths,             only: Get_Eps_Math
   Use CFML_Rational
   Use CFML_Strings,           only: l_case, u_case, get_num, cut_string, get_words, &
                                     get_numstd, Read_Key_Value, Read_Key_ValueSTD,  &
                                     string_numstd, Number_Lines, Reading_Lines,     &
                                     FindFMT, Init_FindFMT, String_Array_Type,       &
                                     File_type, Reading_File, Get_Transf,            &
                                     Get_Extension, Get_Datetime,Read_Fract,         &
                                     Frac_Trans_2Dig, Pack_String, File_List_type,   &
                                     String_Real, String_Count

   Use CFML_Atoms,             only: Atm_Type, Atm_Std_Type, ModAtm_std_type, Atm_Ref_Type, &
                                     AtList_Type, Allocate_Atom_List, Init_Atom_Type, mAtom_Type, &
                                     mAtom_List_Type, Allocate_mAtom_list, deAllocate_mAtom_list,&
                                     Write_Atom_List


   Use CFML_Metrics,           only: Cell_Type, Cell_G_Type, Cell_GLS_Type, Set_Crystal_Cell, U_equiv, &
                                     get_U_from_Betas, get_Betas_from_U, get_Betas_from_B,&
                                     Write_Crystal_Cell

   Use CFML_gSpaceGroups,      only: SpG_Type, SuperSpaceGroup_Type, Kvect_Info_Type,   &
                                     Change_Setting_SpaceG, Set_SpaceGroup, Get_Multip_Pos,&
                                     Get_Orbit, Allocate_Kvector, Write_SpaceGroup_Info, &
                                     Get_Mat_From_Symb_Int, Get_Symb_From_Mat, Get_Dimension_SymmOp, &
                                     Get_Symb_from_Rational_Mat, Allocate_SpaceGroup, P1_Constructor


   Use CFML_kvec_Symmetry,     only: MagSymm_k_Type, Readn_Set_Magnetic_Kv_Structure, Magnetic_Domain_type

   Use CFML_DiffPatt,          only: DiffPat_Type, DiffPat_E_Type, DiffPatt_Conditions_Type, PowPatt_CW_Conditions_Type, &
                                     PowPatt_TOF_Conditions_Type, Load_Pattern, Allocate_Pattern, Interval_Type, Pattern_Type, &
                                     Excl_reg_type, Bck_type

   Use CFML_Messages
   Use CFML_Scattering_Tables, only: Get_Z_Symb
   Use CFML_Molecules,         only: Molecule_type, Init_Molecule, Set_Euler_Matrix

   !---- Variables ----!
   implicit none

   private


   !---- Public subroutines ----!

   public :: Get_Block_Commands, Get_Block_KEY, Get_SubBlock_KEY, Get_Block_Phases, &
             Get_Block_Patterns, Read_Block_Instructions, Get_Blocks_Filetype, &
             Get_SubBlock_CommPatterns, Get_SubBlock_CommPhases, &
             Get_SubBlock_MolPhases, &
             Read_Block_ExcludeReg, Read_Block_Backgd, &
             Read_Xtal_Structure, Read_CFL_KVectors, Read_CFL_Cell, Read_CFL_SpG, &
             Read_CFL_Molecule, Read_CFL_Atoms, Read_CIF_AtomSite_Displace_Fourier, &
             Write_Cif_Template, Write_SHX_Template, Write_MCIF_Template, Write_CFL_File, &
             Write_InfoBlock_ExcludedRegions, Write_InfoBlock_Backgd, Write_CIF_SymTrans

   public :: Get_CFL_Block_Info,Get_CFL_NBlock,Read_CFL_Pattern,Read_CFL_Pat_Conditions,Read_CFL_Pat_Type,&
             Read_CFL_Phase,Write_CFL_Pattern,Write_CFL_Phase

   !--------------------!
   !---- PARAMETERS ----!
   !--------------------!
   character(len=*), parameter :: DIGCAR="0123456789+-"  ! Digit character and signs
   integer,          parameter :: MAX_PHASES=30          ! Maximum Number of Phases
   integer,          parameter :: MAX_PATTERNS=500       ! Maximum Number of Patterns
   real(kind=cp),    parameter :: EPSV=0.0001_cp         ! Small real value to be used for decisions


   !---------------!
   !---- TYPES ----!
   !---------------!

   !!----
   !!---- TYPE :: JOB_INFO_TYPE
   !!--..
   !!
   Type, public :: Job_Info_type
      character(len=120)                            :: Title       =" "       ! Title
      integer                                       :: Num_Phases  =0         ! Number of phases
      integer                                       :: Num_Patterns=0         ! Number of patterns
      integer                                       :: Num_cmd     =0         ! Number of command lines
      character(len=16),  dimension(:), allocatable :: Patt_typ               ! Type of Pattern
      character(len=128), dimension(:), allocatable :: Phas_nam               ! Name of phases
      character(len=128), dimension(:), allocatable :: cmd                    ! Command lines: text for actions
      type(interval_type),dimension(:), allocatable :: range_stl              ! Range in sinTheta/Lambda
      type(interval_type),dimension(:), allocatable :: range_q                ! Range in 4pi*sinTheta/Lambda
      type(interval_type),dimension(:), allocatable :: range_d                ! Range in d-spacing
      type(interval_type),dimension(:), allocatable :: range_2theta           ! Range in 2theta-spacing
      type(interval_type),dimension(:), allocatable :: range_Energy           ! Range in Energy
      type(interval_type),dimension(:), allocatable :: range_tof              ! Range in Time of Flight
      type(interval_type),dimension(:), allocatable :: Lambda                 ! Lambda
      real(kind=cp)      ,dimension(:), allocatable :: ratio                  ! ratio lambda2/lambda1
      real(kind=cp)      ,dimension(:), allocatable :: dtt1,dtt2              ! d-to-TOF coefficients
   End Type Job_Info_type

   !!----
   !!---- TYPE :: BLOCKINFO_TYPE
   !!--..
   !!----
   !!---- Update: May - 2023
   Type, public :: BlockInfo_Type
      character(len=60)             :: StrName=" " !> Name of the block
      character(len=10)             :: BlName=" "  !> Command, Phase, Pattern, Molec, Atoms....
      integer                       :: IBl=-1      !>     0      1        2      3      4  <-- Type of block
      integer, dimension(2)         :: Nl =0       !> Ini/End line
      integer, dimension(2)         :: iex=0       !> Extra values...for example number asignation
   End Type BlockInfo_Type

   !!----
   !!---- TYPE :: GENVEC_TYPE
   !!--..
   !!----
   !!---- Update: May - 2023
   Type, public :: GenVec_Type
      character(len=60)                 :: Str =" "     !> Generic string (Use for directives)
      integer                           :: NPar=0       !> Number of parameters
      integer,           dimension(15)  :: IV  =0       !> Integer values
      real(kind=cp),     dimension(15)  :: RV  =0.0_cp  !> Real values
      character(len=40), dimension(15)  :: CV  =' '     !> Words
   End Type GenVec_Type

   Type, public :: SXtal_attributes_Type
      logical                            :: sxtal=.false.   !> True if it will be used
      !Extinction
      integer                            :: ext_model=0     !> Extinction model (like in FullProf)
      real(kind=cp),      dimension(6)   :: Extinct=0.0     !> Extinction parameters
      real(kind=cp),      dimension(6)   :: mExtinct=1.0    !> Multipliers
      real(kind=cp),      dimension(6)   :: LExtinct=0      !> Parameter number
      !Domains
      integer                            :: Dom_type=0      !> Type of domains (like in FullProf)
      integer                            :: Num_Dom=0       !> Number of domains
      Character(len=60),  dimension(24)  :: Dom_oper=" "    !> Symbol of the domain operators
      real(kind=cp),  dimension(4,4,24)  :: Dom_mat=0.0     !> (4,4,Num_dom) Augmented matrices of domain operators
      real(kind=cp),      dimension(24)  :: dom_fract=0.0   !> Fraction
      real(kind=cp),      dimension(24)  :: mDom_fract=1.0  !> Multipliers
      real(kind=cp),      dimension(24)  :: LDom_fract=0    !> Parameter number
   End Type  SXtal_attributes_Type


   Type, public :: Powder_attributes_Type
      logical       :: powder = .false.                !> True if it will be used
      ! Primary Extinction
      real(kind=cp) :: Extinct    = 0.0                !> Isotropic primary extinction (for TOF)
      real(kind=cp) :: mExtinct   = 1.0                !> Multipliers
      integer       :: LExtinct   = 0                  !> Parameter number
      ! Isotropic size
      real(kind=cp) :: iso_size   = 0.0                !> Isotropic size in angstroms
      real(kind=cp) :: Gauss_iso_size_frac = 0.0       !> Fraction of the Gaussian size component
      real(kind=cp) :: miso_size  = 1.0                !> Multiplier
      real(kind=cp) :: mGauss_iso_size_frac = 1.0      !> Multiplier
      integer       :: Liso_size  = 0                  !> Parameter number of isotropic size
      integer       :: LGauss_iso_size_frac = 0        !> Parameter number of the Gaussian fraction
      ! Isotropic strain
      real(kind=cp) :: iso_strain  = 0.0               !> Isotropic strain in 10e-4
      real(kind=cp) :: Lorentz_iso_strain_frac = 0.0   !> Fraction of the Lorentzian strain component
      real(kind=cp) :: miso_strain   = 1.0             !> Multiplier
      real(kind=cp) :: mLorent_iso_strain_frac = 1.0   !> Multiplier
      integer       :: Liso_strain = 0                 !> Parameter number of isotropic strain
      integer       :: LLorent_iso_strain_frac = 0     !> Parameter number of Lorentzian strain component
      ! Anisotropic size and strain
      character(len=20) :: aniso_size_model=" "        !> Anisotropic size model
      character(len=20) :: aniso_strain_model=" "      !> Anisotropic strain model
      character(len=8)  :: Laue_class=" "              !> Specific Laue class needed for some anisotropic models
      integer       :: Nani_size=0                     !> Number of free anisotropic size parameters
      integer       :: Nani_strain=0                   !> Number of free anisotropic strain parameters
      real(kind=cp), dimension(4)    :: axis_size=0.0      !> Axis for needle or platelet anisotropic size
      real(kind=cp), dimension(4)    :: axis_strain=0.0    !> Axis for uniaxial anisotropic strain. The fourth component contains the square of the modulus
      real(kind=cp), dimension(15)   :: aniso_size=0.0     !> Anisotropic size parameters
      real(kind=cp), dimension(15)   :: aniso_strain=0.0   !> Anisotropic strain parameters
      real(kind=cp), dimension(15)   :: maniso_size=1.0    !> Multiplier anisotropic size parameters
      real(kind=cp), dimension(15)   :: maniso_strain=1.0  !> Multiplier anisotropic strain parameters
      integer,       dimension(15)   :: Laniso_size = 0    !> Parameter numbers of anisotropic size parameters
      integer,       dimension(15)   :: Laniso_strain = 0  !> Parameter numbers of anisotropic strain parameters
      !Preferred orientation
      integer                        :: n_pref  = 0     !> Number of preferred orientation axes for multiaxial March-Dollase model
      real(kind=cp), dimension(4,5)  :: axes_pref= 0.0  !> Axes for preferred orientation, the fourth component contains the square of the modulus
      real(kind=cp), dimension(2,5)  :: pref = 0.0      !> Preferred orientation parameters (value and fraction)
      real(kind=cp), dimension(2,5)  :: mpref= 1.0      !> Multipliers
      integer,       dimension(2,5)  :: Lpref= 0        !> Parameter numbers
      ! Absortion correction for TOF
      real(kind=cp), dimension(2)    :: abs_corr = 0.0  !> Absorption correction parameters
      real(kind=cp), dimension(2)    :: mabs_corr= 1.0  !> multipliers
      integer,       dimension(2)    :: Labs_corr= 0    !> Parameter numbers
   End Type Powder_attributes_Type


   !!----
   !!---- TYPE :: PHASE_TYPE
   !!----
   !!----
   !!---- Update: May - 2025
   Type, public :: Phase_Type
      integer                                        :: iph            !> Number of the phase
      logical                                        :: mag            !> If .true., magnetic contribution will be calculated
      logical                                        :: mag_only       !> If .true., pure magnetic phase (no nuclear contribution))
      character(len=:),                  allocatable :: name           !> Phase name
      class(Cell_G_Type),                allocatable :: cell           !> Crystal cell
      type(AtList_Type)                              :: atm_list       !> Atom list
      class(SpG_Type),                   allocatable :: spg            !> Space group
      type(kvect_info_type)                          :: kvec           !> k-vectors and Q_coefficients (only for Spg-> SuperSpace)
      real(kind=cp)                                  :: occ_fact       !> Factor used to normalize occupation factors
      integer                                        :: N_Species      !> Number of chemical species
      integer                                        :: Nmol           !> Number of molecules
      type(Molecule_type),dimension(:),  allocatable :: Mol            !> Molecules
      integer                                        :: Ncontr         !> Number of patterns to which the phase contributes
      integer,            dimension(:),  allocatable :: patterns       !> Patterns to which this phase contributes
      character(len=2),   dimension(:),  allocatable :: pat_mode       !> Type of pattern (CW,TF))
      character(len=2),   dimension(:),  allocatable :: pat_sample     !> Type of pattern (CW,TF))
      real(kind=cp),      dimension(:),  allocatable :: scale_factors  !> Scale factors for patterns
      real(kind=cp),      dimension(:),  allocatable :: mscale_factors !> Multipliers
      integer,            dimension(:),  allocatable :: Lscale_factors !> Parameter numbers
      integer                                        :: N_pow, N_sxt   !> Number of powder and single crystal patterns to which the phase contributes
      type(Powder_attributes_Type), dimension(:), allocatable :: pow   !> Attributes for powder patterns (microstructure, preferred orientation, ...)
      type( SXtal_attributes_Type), dimension(:), allocatable :: sxt   !> Attributes for single crystal patterns (extinction, domains, etc.)
   End Type Phase_Type



   !-------------------!
   !---- Variables ----!
   !-------------------!

   !---- Private ----!
   character(len=256), private                :: line
   character(len=60),  private, dimension(40) :: dire=" "
   integer,            private, dimension(15) :: ivet=0
   real(kind=cp),      private, dimension(15) :: vet=0.0_cp

   !---- Public ----!
   integer, public :: NP_Instr =0     !> Number of Directives
   integer, public :: NP_Backgd=0     !> Number of Background
   integer, public :: NP_ExReg =0     !> Number of Excluded Regions

   type(GenVec_Type), public, dimension(:), allocatable :: Vec_Instr    !> Vector of Instructions

   !---- Overloaded Zone ----!
   !Interface Readn_Set_Xtal_Structure
   !   Module Procedure Readn_Set_Xtal_Structure_Split  ! For Cell, Spg, A types
   !   !Module Procedure Readn_Set_Xtal_Structure_Molcr ! For Molecular Crystal Type
   !End Interface

   !---- Interface zone ----!
   Interface

      Module Function Get_CFL_Block_Info(cfl, name) Result(blok)
         !---- Arguments ----!
         type(File_Type),                    intent(in)  :: cfl  ! CFL content
         character(len=*),                   intent(in)  :: name ! Block name -PHASE, PATTERN...-
         type(BlockInfo_Type), dimension(:), allocatable :: blok ! Block information
      End Function Get_CFL_Block_Info

      Module Function Get_CFL_NBlock(cfl, name) Result(nblock)
         !---- Arguments ----!
         type(File_Type),  intent(in)  :: cfl    ! CFL content
         character(len=*), intent(in)  :: name   ! Block name -PHASE, PATTERN...-
         integer                       :: nblock ! Number of blocks
      End Function Get_CFL_NBlock

      Module Subroutine Get_Blocks_Filetype(ffile, NComm, Bl_Comm, NPatt, Bl_Patt, &
                                         NPhas, Bl_Phas, NCPatt, BlC_Patt, NCPhas, BlC_Phas)
         !---- Arguments ----!
         type(File_type),                               intent(in)  :: ffile
         integer,                            optional,  intent(out) :: NComm
         type(BlockInfo_Type),               optional,  intent(out) :: Bl_Comm
         integer,                            optional,  intent(out) :: NPatt
         type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Patt
         integer,                            optional,  intent(out) :: NPhas
         type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Phas
         integer,                            optional,  intent(out) :: NCPatt
         type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Patt
         integer,                            optional,  intent(out) :: NCPhas
         type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Phas

      End Subroutine Get_Blocks_Filetype

      Module Subroutine Get_Block_Commands(ffile, N_Ini, N_End)
         !---- Arguments ----!
         Type(file_type),    intent(in)  :: ffile
         integer,            intent(out) :: n_ini
         integer,            intent(out) :: n_end
      End Subroutine Get_Block_Commands

      Module Subroutine Get_Block_Patterns(ffile, N_Ini, N_End, NPatt, Patt, Ex_Ind)
         !---- Arguments ----!
         Type(file_type),                    intent(in)     :: ffile
         integer,                            intent(in)     :: n_ini
         integer,                            intent(in)     :: n_end
         integer,                            intent(out)    :: NPatt
         type(BlockInfo_Type), dimension(:), intent(in out) :: Patt
         integer, dimension(2), optional,    intent(in)     :: Ex_ind
      End Subroutine Get_Block_Patterns

      Module Subroutine Get_Block_Phases(ffile, N_Ini, N_End, NPhas, Phas, Ex_Ind)
         !---- Arguments ----!
         Type(file_type),                    intent(in)     :: ffile
         integer,                            intent(in)     :: n_ini
         integer,                            intent(in)     :: n_end
         integer,                            intent(out)    :: NPhas
         type(BlockInfo_Type), dimension(:), intent(in out) :: Phas
         integer, dimension(2), optional,    intent(in)     :: Ex_ind
      End Subroutine Get_Block_Phases

      Module Subroutine Get_Block_KEY(Key, ffile, N_Ini, N_End, Ind, StrName, N_Id)
         !---- Arguments ----!
         character(len=*),              intent(in)  :: Key
         Type(file_type),               intent(in)  :: ffile
         integer,                       intent(in)  :: n_ini
         integer,                       intent(in)  :: n_end
         integer, dimension(2),         intent(out) :: Ind
         character(len=*),              intent(out) :: StrName
         integer,                       intent(out) :: N_Id
      End Subroutine Get_Block_KEY

      Module Subroutine Get_SubBlock_KEY(Key, ffile, n_ini, n_end, Ind, StrName)
         !---- Arguments ----!
         character(len=*),                intent(in)  :: key
         Type(file_type),                 intent(in)  :: ffile
         integer,                         intent(in)  :: n_ini
         integer,                         intent(in)  :: n_end
         integer, dimension(2),           intent(out) :: Ind
         character(len=*),      optional, intent(out) :: StrName
      End Subroutine Get_SubBlock_KEY


      Module Subroutine Read_Block_ExcludeReg(ffile, n_ini, n_end, Exclr)
         !---- Arguments ----!
         Type(file_type),         intent(in)    :: ffile
         integer,                 intent(in)    :: n_ini
         integer,                 intent(in)    :: n_end
         Type(Excl_reg_type),     intent(out)   :: Exclr
      End Subroutine Read_Block_ExcludeReg

      Module Subroutine Read_Block_Instructions(ffile, N_ini, N_end, LSymm)
         !---- Arguments ----!
         type(File_type), intent(in)   :: Ffile
         integer,         intent(in)   :: N_ini
         integer,         intent(in)   :: N_end
         logical, optional, intent(in) :: LSymm
      End Subroutine Read_Block_Instructions

      Module Subroutine Read_Block_Backgd(ffile, n_ini, n_end, Bck, ipatt)
         !---- Arguments ----!
         Type(file_type),     intent(in)    :: ffile
         integer,             intent(in)    :: n_ini
         integer,             intent(in)    :: n_end
         Type(Bck_type),      intent(out)   :: Bck
         integer, optional,   intent(in)    :: ipatt
      End Subroutine Read_Block_Backgd


      Module Subroutine Write_InfoBlock_Backgd(Bck,Iunit,IPatt)
         !---- Arguments ----!
         type(Bck_type),      intent(in) :: Bck
         integer, optional,   intent(in) :: Iunit
         integer, optional,   intent(in) :: IPatt
      End Subroutine Write_InfoBlock_Backgd

      Module Subroutine Write_InfoBlock_ExcludedRegions(Excl,Iunit,IPatt)
         !---- Arguments ----!
         Type(Excl_reg_type), intent(in) :: Excl
         integer, optional,   intent(in) :: Iunit
         integer, optional,   intent(in) :: IPatt
      End Subroutine Write_InfoBlock_ExcludedRegions

      Module Function Get_NElem_Loop(cif, keyword, i_ini,i_end) Result(N)
         type(File_Type),   intent(in) :: cif
         character(len=*),  intent(in) :: keyword
         integer, optional, intent(in) :: i_ini, i_end
         integer                       :: n
      End Function Get_NElem_Loop

      Module Function Is_SSG_Struct(cif, i_ini,i_end) Result(ok)
         type(File_Type),   intent(in) :: cif
         integer, optional, intent(in) :: i_ini, i_end
         logical                       :: ok
      End Function Is_SSG_Struct

      Module Function Charge(label) result(q)
         !---- Arguments ----!
         character(len=*), intent(in):: label   ! Input string containing oxidation state
         integer                     :: q
      End Function Charge

      Module Subroutine Get_Job_Info(cfl,Job_info, i_ini,i_end)
         type(File_Type),      intent(in)  :: cfl
         type(job_info_type),  intent(out) :: Job_info
         integer,              intent(in)  :: i_ini, i_end
      End Subroutine Get_Job_Info

      Module Subroutine Read_Atom(Str, Atm)
         character(len=*), intent(in)   :: Str
         class (Atm_Type), intent(out)  :: Atm
      End Subroutine Read_Atom

      Module Subroutine Read_Cell(Str,Celda,Std,Cell,CFrame)
         character(len=*),                      intent(in)  :: Str
         real(kind=cp), dimension(6),           intent(out) :: Celda
         real(kind=cp), dimension(6), optional, intent(out) :: Std
         class(Cell_G_Type),          optional, intent(out) :: Cell
         character(len=*),            optional, intent(in)  :: CFrame
      End Subroutine Read_Cell

      Module Subroutine Read_Modulation_Amplitudes(Str, Atm, Ulabel, Nt)
         character(len=*),    intent(in )     :: str
         class(ModAtm_Std_Type),intent(in out)  :: Atm
         character(len=*),    intent(in)      :: ulabel
         integer,             intent(in)      :: nt
      End Subroutine Read_Modulation_Amplitudes

      Module Subroutine Read_Moment(Str, Atm)
         character(len=*), intent(in )     :: Str
         Class (Atm_Type), intent(in out)  :: Atm
      End Subroutine Read_Moment

      Module Subroutine Read_RngSintL(Str, v1,v2)
         character(len=*), intent(in)  :: str
         real(kind=cp),    intent(out) :: v1,v2
      End Subroutine Read_RngSintL

      Module Subroutine Read_SpaceGroup(Str,Spg)
         character(len=*),             intent(in)     :: Str
         class(SpG_Type), allocatable, intent(out)    :: SpG
      End Subroutine Read_SpaceGroup

      Module Subroutine Read_Transf(str, trans, orig)
         character(len=*),                intent(in)     :: str
         real(kind=cp),dimension(3,3),    intent(out)    :: trans
         real(kind=cp),dimension(3  ),    intent(out)    :: orig
      End Subroutine Read_Transf

      Module Subroutine Read_UTherms(Str, Atm)
         character(len=*),  intent(in )     :: Str
         Class (Atm_Type),  intent(in out)  :: Atm
      End Subroutine Read_UTherms

      Module Subroutine Read_Wavelength(Str,v1,v2,ratio)
         character(len=*), intent(in)     :: Str
         real(kind=cp),    intent(out)    :: v1,v2
         real(kind=cp),    intent(out)    :: ratio
      End Subroutine Read_Wavelength

      Module Subroutine Read_CFL_Molecule(cfl, N_ini, N_end, Mol)
         type(file_type),      intent(in)   :: cfl
         integer, optional,    intent(in)   :: N_Ini
         integer, optional,    intent(in)   :: N_End
         type (Molecule_type), intent(out)  :: Mol
      End Subroutine Read_CFL_Molecule

      Module Subroutine Read_CFL_Atoms(cfl, AtmList, Type_Atm, d, i_ini, i_end)
         type(File_Type),      intent(in)     :: cfl
         Type(AtList_Type),    intent(out)    :: AtmList
         character(len=*),     intent(in)     :: Type_Atm
         integer,              intent(in)     :: d
         integer, optional,    intent(in)     :: i_ini, i_end
      End Subroutine Read_CFL_Atoms

      Module Subroutine Read_CFL_Cell(cfl, Cell, CFrame, i_ini, i_end, cmd)
         !---- Arguments ----!
         type(File_Type),                intent(in)     :: cfl
         class(Cell_G_Type),allocatable, intent(out)    :: Cell
         character(len=*),     optional, intent( in)    :: CFrame
         integer,              optional, intent(in)     :: i_ini, i_end
         logical,              optional, intent(in)     :: cmd
      End Subroutine Read_CFL_Cell

      Module Subroutine Read_CFL_KVectors(cfl, Kvec, i_ini, i_end)
         type(File_Type),         intent(in)     :: cfl
         type(kvect_info_Type),   intent(out)    :: Kvec
         integer, optional,          intent(in)     :: i_ini, i_end
      End Subroutine Read_CFL_KVectors

      Module Subroutine Read_CFL_Pattern(cfl, pat, i_ini, i_end, genp)
         !---- Argumens ----!
         Type(File_Type),            intent(in)  :: cfl
         type(Pattern_Type),         intent(out) :: pat
         integer,          optional, intent(in)  :: i_ini
         integer,          optional, intent(in)  :: i_end
         logical,          optional, intent(out) :: genp
      End Subroutine Read_CFL_Pattern

      Module Subroutine Read_CFL_Pat_Conditions(cfl, cond, i_ini, i_end)
         !---- Arguments ----!
         type(File_Type),                  intent(in)     :: cfl   ! Containing information
         class(DiffPatt_Conditions_Type),  intent(inout)  :: cond  ! Conditions
         integer,              optional,   intent(in)     :: i_ini ! First lines to explore
         integer,              optional,   intent(in)     :: i_end ! Last line to explore
      End Subroutine Read_CFL_Pat_Conditions

      Module Subroutine Read_CFL_Pat_Type(cfl, pt, i_ini, i_end)
         !---- Arguments ----!
         type(File_Type),                intent(in)     :: cfl   ! Containing information
         type(Pattern_Type),             intent(inout)  :: pt    ! Pattern
         integer,              optional, intent(in)     :: i_ini ! First lines to explore
         integer,              optional, intent(in)     :: i_end ! Last line to explore
      End Subroutine Read_CFL_Pat_Type

      Module Subroutine Read_CFL_Phase(cfl, ph, atom_type, d,i_ini, i_end)
         !---- Arguments ----!
         Type(File_Type),                     intent(in)  :: cfl
         Type(Phase_Type),                    intent(out) :: ph
         character(len=*),                    intent(in)  :: atom_type
         integer,                             intent(in)  :: d
         integer,          optional,          intent(in)  :: i_ini
         integer,          optional,          intent(in)  :: i_end

      End Subroutine Read_CFL_Phase

      Module Subroutine Read_CFL_SpG(cfl, SpG, xyz_type, i_ini, i_end, database_path)
         Type(File_Type),                 intent(in)     :: cfl
         class(SpG_Type), allocatable,    intent(out)    :: SpG
         character(len=*), optional,      intent(in)     :: xyz_type
         integer, optional,               intent(in)     :: i_ini, i_end
         character(len=*), optional,      intent(in)     :: database_path
      End Subroutine Read_CFL_SpG

      Module Subroutine Write_CFL_Atoms(AtmList, Lun, Cell)
         Type(AtList_Type),            intent(in) :: AtmList
         integer,            optional, intent(in) :: Lun
         class(Cell_G_Type), optional, intent(in) :: Cell
      End Subroutine Write_CFL_Atoms

      Module Subroutine Write_CFL_File(Lun,Cell, SpG, Atm, Title,info_lines)
          integer,                               intent(in)    :: lun
          class(Cell_G_Type),                    intent(in)    :: Cell
          class(SpG_Type)  ,                     intent(in)    :: SpG
          Type(AtList_Type), optional,           intent(in)    :: Atm
          character(len=*),  optional,           intent(in)    :: Title
          character(len=*),dimension(:),optional,intent(in)    :: info_lines
      End Subroutine Write_CFL_File

      Module Subroutine Write_CFL_Pattern(Lun,Pat,iPat)
         integer,            intent(in) :: lun
         type(Pattern_Type), intent(in) :: Pat
         integer, optional,  intent(in) :: iPat
      End Subroutine Write_CFL_Pattern

      Module Subroutine Write_CFL_Phase(Lun,Ph,iPh)
         integer,           intent(in) :: lun
         type(Phase_Type),  intent(in) :: Ph
         integer, optional, intent(in) :: iPh
      End Subroutine Write_CFL_Phase

      Module Subroutine Get_CIF_NPhases(cif, NPhas, PhasesName, IPhas)
         type(File_Type),                  intent(in)  :: cif
         integer,                          intent(out) :: NPhas
         character(len=*), dimension(:),   intent(out) :: PhasesName
         integer,          dimension(:),   intent(out) :: IPhas
      End Subroutine Get_CIF_NPhases

      Module Subroutine Read_CIF_Atoms(cif, Atm, i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         type(atList_type),  intent(out) :: Atm
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Atoms

      Module Subroutine Read_CIF_AtomSite_Displace_Fourier(cif, AtmList,i_ini,i_end)
         !---- Arguments ----!
         Type(File_Type),   intent(in)    :: cif
         Type(AtList_Type), intent(in out):: AtmList
         integer, optional, intent(in)    :: i_ini,i_end   ! Index to Finish
      End Subroutine Read_CIF_AtomSite_Displace_Fourier

      Module Subroutine Read_CIF_Cell(cif, Cell, i_Ini, i_End)
         type(File_Type),    intent(in)  :: cif
         class(Cell_G_Type), intent(out) :: Cell
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Cell

      Module Subroutine Read_CIF_ChemName(cif, ChemName, i_ini,i_end)
         type(File_Type),    intent(in)  :: cif
         character(len=*),   intent(out) :: ChemName
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_Cif_ChemName

      Module Subroutine Read_CIF_Z(cif,Z, i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         real(kind=cp),      intent(out) :: Z
         integer, optional,  intent(in)  :: i_ini,i_end
      End Subroutine Read_CIF_Z

      Module Subroutine Read_CIF_Wave(cif, Wave,i_ini,i_end)
         type(File_Type),    intent(in)  :: cif
         real(kind=cp),      intent(out) :: Wave
         integer, optional,  intent(in)  :: i_ini,i_end
      End Subroutine Read_CIF_Wave

      Module Subroutine Read_CIF_Cont(cif,N_Elem_Type,Elem_Type,N_Elem,i_ini,i_end)
         type(File_Type),                      intent(in)  :: cif
         integer,                              intent(out) :: n_elem_type
         character(len=*), dimension(:),       intent(out) :: elem_type
         real(kind=cp), dimension(:),optional, intent(out) :: n_elem
         integer,                    optional, intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Cont

      Module Subroutine Read_CIF_IT(cif,IT,i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         integer,            intent(out) :: IT
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_IT

      Module Subroutine Read_CIF_Pressure(cif, P, SigP, i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         real(kind=cp),      intent(out) :: p
         real(kind=cp),      intent(out) :: sigp
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Pressure

      Module Subroutine Read_CIF_Title(cif,Title,i_Ini,i_End)
         type(File_Type),    intent(in)  :: cif
         character(len=*),   intent(out) :: title
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Title

      Module Subroutine Read_CIF_Temp(cif,T,SigT,i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         real(kind=cp),      intent(out) :: T
         real(kind=cp),      intent(out) :: SigT
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Temp

      Module Subroutine Read_CIF_Hall(cif, Hall, i_Ini, i_End)
         type(File_Type),    intent(in)  :: cif
         character(len=*),   intent(out) :: Hall
         integer, optional,  intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Hall

      Module Subroutine Read_CIF_HM(cif, Spgr_Hm, i_ini, i_end)
         type(File_Type),    intent(in)  :: cif
         character(len=*),   intent(out) :: spgr_hm
         integer, optional,  intent(in)  :: i_ini,i_end
      End Subroutine Read_CIF_HM

      Module Subroutine Read_CIF_Symm(cif, N_Oper, Oper_Symm,i_ini,i_end)
         type(File_Type),    intent(in)              :: cif
         integer,                        intent(out) :: n_oper
         character(len=*), dimension(:), intent(out) :: oper_symm
         integer, optional,              intent(in)  :: i_ini, i_end
      End Subroutine Read_CIF_Symm

      Module Subroutine Write_CIF_Atoms(Ipr, AtmList, SpG, Cell)
         integer,                      intent(in) :: Ipr
         Type(AtList_Type),            intent(in) :: AtmList
         class(SpG_Type),              intent(in) :: SpG
         class(Cell_G_Type), optional, intent(in) :: Cell
      End Subroutine Write_CIF_Atoms

      Module Subroutine Write_CIF_Cell(Ipr, Cell)
         integer,            intent(in) :: Ipr
         class(Cell_G_Type), intent(in) :: Cell
      End Subroutine Write_CIF_Cell

      Module Subroutine Write_CIF_ChemData(Ipr)
         integer, intent(in) :: Ipr
      End Subroutine Write_CIF_ChemData

      Module Subroutine Write_CIF_End(Ipr)
         integer, intent(in) :: Ipr
      End Subroutine Write_CIF_End

      Module Subroutine Write_CIF_Header(Ipr,str)
         integer,                    intent(in) :: Ipr
         character(len=*), optional, intent(in) :: Str
      End Subroutine Write_CIF_Header

      Module Subroutine Write_CIF_Powder_Profile(filename,Pat,r_facts)
         character(len=*),                      intent(in) :: filename
         class(DiffPat_Type),                   intent(in) :: Pat
         real(kind=cp), dimension(4), optional, intent(in) :: r_facts
      End Subroutine Write_CIF_Powder_Profile

      Module Subroutine Read_SHX_Atom(shx, n_fvar, fvar, elem_type, cell, AtmList)
         type(File_Type),                intent(in)      :: shx
         integer,                        intent(in)      :: n_fvar
         real(kind=cp), dimension(:),    intent(in)      :: fvar
         character(len=*), dimension(:), intent(in)      :: elem_type
         class(Cell_G_Type),             intent(in)      :: Cell
         type (AtList_type),             intent(out)     :: AtmList
      End Subroutine Read_SHX_Atom

      Module Subroutine Read_SHX_Cell(shx, Cell)
         type(File_Type),                 intent(in)     :: shx
         class(Cell_G_Type),allocatable,  intent(out)     :: Cell
      End Subroutine Read_SHX_Cell

      Module Subroutine Read_SHX_Wave(shx, Wave)
         type(File_Type),  intent(in)     :: shx
         real(kind=cp),    intent(out)    :: Wave
      End Subroutine Read_SHX_Wave

      Module Subroutine Read_SHX_Z(shx, Z)
         type(File_Type),  intent(in)     :: shx
         real(kind=cp),    intent(out)    :: Z
      End Subroutine Read_SHX_Z

      Module Subroutine Read_SHX_Fvar(shx, n_fvar, fvar)
         type(File_Type),                intent(in)    :: shx
         integer,                        intent(out)   :: n_fvar
         real(kind=cp), dimension(:),    intent(out)   :: fvar
      End Subroutine Read_SHX_Fvar

      Module Subroutine Read_SHX_Titl(shx,Title)
         type(File_Type),  intent(in)     :: shx
         character(len=*), intent(out)    :: title
      End Subroutine Read_SHX_Titl

      Module Subroutine Read_SHX_Latt(shx,latt)
         type(File_Type),  intent(in)    :: shx
         integer,          intent(out)   :: latt
      End Subroutine Read_SHX_Latt

      Module Subroutine Read_SHX_Cont(shx, n_elem_type, elem_type, n_elem)
         type(File_Type),                           intent(in)     :: shx
         integer,                                  intent(out)     :: n_elem_type
         character(len=*), dimension(:),           intent(out)     :: elem_type
         real(kind=cp),    dimension(:), optional, intent(out)     :: n_elem
      End Subroutine Read_SHX_Cont

      Module Subroutine Read_SHX_Symm(shx,n_oper,oper_symm)
         type(File_Type),  intent(in)               :: shx
         integer,          intent(out)              :: n_oper
         character(len=*), dimension(:),intent(out) :: oper_symm
      End Subroutine Read_SHX_Symm

      Module Subroutine Write_SHX_Template(filename,code,title,lambda,z,cell,spg,AtmList)
         !---- Arguments ----!
         character(len=*),        intent(in) :: filename
         integer,                 intent(in) :: code
         character(len=*),        intent(in) :: title
         real(kind=cp),           intent(in) :: lambda
         integer,                 intent(in) :: z
         class(Cell_G_Type),      intent(in) :: cell
         class(SpG_Type),         intent(in) :: SpG
         type(atlist_type),       intent(in) :: atmList
      End Subroutine Write_SHX_Template

      Module Subroutine Read_XTal_CFL(cfl, Cell, SpG, AtmList, Atm_typ, Nphase, CFrame, Job_Info, database_path)
         type(File_Type),               intent(in)  :: cfl
         class(Cell_G_Type),allocatable,intent(out) :: Cell
         class(SpG_Type), allocatable,  intent(out) :: SpG
         Type(AtList_Type),             intent(out) :: Atmlist
         character(len=*),    optional, intent(in)  :: Atm_typ
         Integer,             optional, intent(in)  :: Nphase
         character(len=*),    optional, intent(in)  :: CFrame
         Type(Job_Info_type), optional, intent(out) :: Job_Info
         character(len=*),    optional, intent(in)  :: database_path
      End Subroutine Read_XTal_CFL

      Module Subroutine Read_XTal_CIF(cif, Cell, Spg, AtmList, Nphase, database_path)
         type(File_Type),                 intent(in)  :: cif
         class(Cell_G_Type), allocatable, intent(out) :: Cell
         class(SpG_Type),    allocatable, intent(out) :: SpG
         Type(AtList_Type),               intent(out) :: Atmlist
         Integer,             optional,   intent(in)  :: Nphase
         character(len=*),    optional,   intent(in)  :: database_path
      End Subroutine Read_XTal_CIF

      Module Subroutine Read_XTal_FST(fst, Cell, Spg, Atm, MGp, mAtm, Mag_dom, database_path)
         Type(File_Type),                     intent(in)     :: FST
         class(Cell_G_Type),     allocatable, intent(out)    :: Cell
         Class(SpG_Type),        allocatable, intent(out)    :: SpG
         Type(AtList_Type),                   intent(out)    :: Atm
         Type(MagSymm_k_Type),      optional, intent(out)    :: MGp
         Type(mAtom_List_Type),     optional, intent(out)    :: mAtm
         Type(Magnetic_Domain_type),optional, intent(out)    :: Mag_dom
         character(len=*),          optional, intent(in)  :: database_path
      End Subroutine Read_XTal_FST

      Module Subroutine Read_XTal_MCIF(cif, Cell, Spg, AtmList, Kvec, Nphase, database_path)
         type(File_Type),                 intent(in)  :: cif
         class(Cell_G_Type),allocatable,  intent(out) :: Cell
         class(SpG_Type), allocatable,    intent(out) :: SpG
         Type(AtList_Type),               intent(out) :: Atmlist
         Type(Kvect_Info_Type), optional, intent(out) :: Kvec
         Integer,               optional, intent(in)  :: Nphase
         character(len=*),      optional, intent(in)  :: database_path
      End Subroutine Read_XTal_MCIF

      Module Subroutine Read_XTal_SHX(shx, Cell, SpG, Atm, database_path)
         type(File_Type),                 intent(in)  :: shx
         class (Cell_G_Type), allocatable,intent(out) :: Cell
         class (SpG_Type),    allocatable,intent(out) :: SpG
         type (AtList_type),              intent(out) :: Atm
         character(len=*),      optional, intent(in)  :: database_path
      End Subroutine Read_XTal_SHX

      Module Subroutine Write_CIF_Spg(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_CIF_Spg

      Pure Module Function Write_CIF_SymTrans(N,Tr) Result(Code)
         !---- Arguments ----!
         integer,                       intent(in)  :: N
         real(kind=cp),dimension(3),    intent(in)  :: Tr
         character (len=:), allocatable             :: Code
      End Function Write_CIF_SymTrans

      Module Subroutine Write_CIF_Template(filename, Cell, SpG, Atmlist, Type_data, Code)
         character(len=*),           intent(in) :: filename
         class(Cell_G_Type),         intent(in) :: Cell
         class(SpG_Type),            intent(in) :: SpG
         Type (AtList_Type),         intent(in) :: AtmList
         integer,                    intent(in) :: Type_data
         character(len=*),           intent(in) :: Code
      End Subroutine Write_CIF_Template

      Module Subroutine Read_MCIF_Parent_Propagation_Vector(cif, Kvec,i_ini,i_end)
         Type(File_Type),       intent(in)  :: cif
         Type(Kvect_Info_Type), intent(out) :: Kvec
         integer, optional,     intent(in)  :: i_ini,i_end
      End Subroutine Read_MCIF_Parent_Propagation_Vector

      Module Subroutine Read_MCIF_Cell_Wave_Vector(cif, SpG, Kvec, i_ini,i_end)
         Type(File_Type),                 intent(in)    :: cif
         class(SpG_Type),       optional, intent(inout) :: SpG
         Type(Kvect_Info_Type), optional, intent(out)   :: Kvec
         integer,               optional, intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_Cell_Wave_Vector

      Module Subroutine Read_MCIF_Parent_SpaceG(cif,Spg,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         class(SpG_Type),       intent(inout) :: SpG
         integer, optional,     intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_Parent_SpaceG

      Module Subroutine Read_MCIF_AtomSite_Moment(cif, AtmList,i_ini,i_end)
         Type(File_Type),   intent(in)    :: cif
         Type(AtList_Type), intent(inout) :: AtmList
         integer, optional, intent(in)  :: i_ini,i_end
      End Subroutine Read_MCIF_AtomSite_Moment

      Module Subroutine Read_MCIF_AtomSite_Moment_Fourier(cif, AtmList,i_ini,i_end)
         Type(File_Type),   intent(in)    :: cif
         Type(AtList_Type), intent(inout) :: AtmList
         integer, optional, intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_AtomSite_Moment_Fourier

      Module Subroutine Read_MCIF_SpaceG_Magn(cif,Spg,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         class(SpG_Type),       intent(inout) :: SpG
         integer, optional,     intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_Magn

      Module Subroutine Read_MCIF_SpaceG_Magn_SSG_Transf(cif,Spg,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         class(SpG_Type),       intent(inout) :: SpG
         integer, optional,     intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_Magn_SSG_Transf

      Module Subroutine Read_MCIF_SpaceG_Magn_Transf(cif, Spg,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         class(SpG_Type),       intent(inout) :: SpG
         integer, optional,     intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_Magn_Transf

      Module Subroutine Read_MCIF_SpaceG_SymOP_Magn_Centering(cif, nsym, symop,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         integer,                        intent(out)   :: nsym
         character(len=*), dimension(:), intent(out)   :: symop
         integer, optional,              intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_SymOP_Magn_Centering

      Module Subroutine Read_MCIF_SpaceG_SymOP_Magn_Ssg_Centering(cif, nsym, symop,i_ini,i_end)
         Type(File_Type),                intent(in)    :: cif
         integer,                        intent(out)   :: nsym
         character(len=*), dimension(:), intent(out)   :: symop
         integer, optional,              intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_SymOP_Magn_Ssg_Centering

      Module Subroutine Read_MCIF_SpaceG_SymOP_Magn_Operation(cif, nsym, symop,i_ini,i_end)
         Type(File_Type),       intent(in)    :: cif
         integer,                        intent(out)   :: nsym
         character(len=*), dimension(:), intent(out)   :: symop
         integer, optional,              intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_SpaceG_SymOP_Magn_Operation

      Module Subroutine Read_MCIF_SpaceG_SymOP_Magn_Ssg_Operation(cif, nsym, symop,i_ini,i_end,d)
         !---- Arguments ----!
         Type(File_Type),                intent(in)    :: cif
         integer,                        intent(out)   :: nsym
         character(len=*), dimension(:), intent(out)   :: symop
         integer, optional,              intent(in)    :: i_ini,i_end
         integer, optional,              intent(out)   :: d !dimension of matrix operators
      End Subroutine Read_MCIF_SpaceG_SymOP_Magn_Ssg_Operation

      Module Subroutine Write_MCIF_Template(filename,Cell,SpG,AtmList,Code)
         character(len=*),           intent(in) :: filename
         class(Cell_G_Type),         intent(in) :: Cell
         class(SpG_Type),            intent(in) :: SpG
         Type(AtList_Type),          intent(in) :: AtmList
         character(len=*), optional, intent(in) :: Code
      End Subroutine Write_MCIF_Template

      Module Subroutine Write_MCIF_SpaceG_SymOP_Magn_Operation(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_SpaceG_SymOP_Magn_Operation

      Module Subroutine Write_MCIF_SpaceG_SymOP_Magn_Centering(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_SpaceG_SymOP_Magn_Centering

      Module Subroutine Read_MCIF_AtomSite_Fourier_Wave_Vector(cif, SpG, Kvec, i_ini,i_end)
         Type(File_Type),                 intent(in)    :: cif
         class(SpG_Type),       optional, intent(inout) :: SpG
         Type(Kvect_Info_Type), optional, intent(inout) :: Kvec
         integer,               optional, intent(in)    :: i_ini,i_end
      End Subroutine Read_MCIF_AtomSite_Fourier_Wave_Vector

      Module Subroutine Write_MCIF_AtomSite_Moment(Ipr, AtmList)
         integer,           intent(in) :: Ipr
         Type(AtList_Type), intent(in) :: AtmList
      End Subroutine Write_MCIF_AtomSite_Moment

      Module Subroutine Write_MCIF_AtomSite_Moment_Fourier(Ipr,Atmlist)
         integer,            intent(in) :: Ipr
         Type(AtList_Type),  intent(in) :: AtmList
      End Subroutine Write_MCIF_AtomSite_Moment_Fourier

      Module Subroutine Write_MCIF_AtomSite_Fourier_Wave_Vector(Ipr,SpG,KVec)
         integer, intent(in)                         :: Ipr
         class(SpG_Type),       optional, intent(in) :: Spg
         Type(Kvect_Info_Type), optional, intent(in) :: Kvec
      End Subroutine Write_MCIF_AtomSite_Fourier_Wave_Vector

      Module Subroutine Write_MCIF_Parent_Propagation_Vector(Ipr,KVec)
         integer, intent(in)               :: Ipr
         Type(Kvect_Info_Type), intent(in) :: Kvec
      End Subroutine Write_MCIF_Parent_Propagation_Vector

      Module Subroutine Write_MCIF_Cell_Modulation_Dimension(Ipr,SpG)
         integer,         intent(in) :: Ipr
         class(SpG_Type), intent(in) :: Spg
      End Subroutine Write_MCIF_Cell_Modulation_Dimension

      Module Subroutine Write_MCIF_Cell_Wave_Vector(Ipr,SpG,KVec)
         integer, intent(in)                         :: Ipr
         class(SpG_Type),       optional, intent(in) :: Spg
         Type(Kvect_Info_Type), optional, intent(in) :: Kvec
      End Subroutine Write_MCIF_Cell_Wave_Vector

      Module Subroutine Write_MCIF_Parent_SpaceG(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_Parent_SpaceG

      Module Subroutine Write_MCIF_SpaceG_Magn(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_SpaceG_Magn

      Module Subroutine Write_MCIF_SpaceG_Magn_SSG_Transf(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_SpaceG_Magn_SSG_Transf

      Module Subroutine Write_MCIF_SpaceG_Magn_Transf(Ipr, SpG)
         integer,          intent(in) :: Ipr
         class(SpG_Type),  intent(in) :: Spg
      End Subroutine Write_MCIF_SpaceG_Magn_Transf

      Module Subroutine Write_MCIF_Spg(Ipr, Spg)
         integer,          intent(in) :: Ipr
         class (Spg_type), intent(in) :: Spg
      End Subroutine Write_MCIF_Spg

      Module Subroutine Get_SubBlock_CommPatterns(ffile, N_ini, N_end, Bl_Patt, NPatt, C_Patt)
         type(File_type),                    intent(in)     :: ffile
         integer,                            intent(in)     :: N_ini
         integer,                            intent(in)     :: N_end
         type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Patt
         integer,                            intent(out)    :: Npatt
         type(BlockInfo_Type), dimension(:), intent(in out) :: C_Patt
      End Subroutine Get_SubBlock_CommPatterns

      Module Subroutine Get_SubBlock_CommPhases(ffile, N_ini, N_end, Bl_Phas, NPhas, C_Phas)
         type(File_type),                    intent(in)     :: ffile
         integer,                            intent(in)     :: N_ini
         integer,                            intent(in)     :: N_end
         type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Phas
         integer,                            intent(out)    :: Nphas
         type(BlockInfo_Type), dimension(:), intent(in out) :: C_Phas
      End Subroutine Get_SubBlock_CommPhases

      Module Subroutine Get_SubBlock_MolPhases(ffile, N_Ini, N_End, NMol, BMol)
         !---- Arguments ----!
         Type(file_type),                    intent(in)     :: ffile
         integer,                            intent(in)     :: n_ini
         integer,                            intent(in)     :: n_end
         integer,                            intent(out)    :: NMol
         type(BlockInfo_Type), dimension(:), intent(in out) :: BMol
      End Subroutine Get_SubBlock_MolPhases


    End Interface

    Contains

    !!----
    !!---- READ_XTAL_STRUCTURE
    !!----
    !!----
    !!----
    !!----
    !!---- 09/05/2020
    !!
    Subroutine Read_Xtal_Structure(filenam, Cell, Spg, Atm, Atm_typ, MGp, mAtm, Mag_dom, IPhase, FType, FileList, database_path)
       !---- Arguments ----!
       character(len=*),                    intent( in)  :: filenam    ! Name of the file
       class(Cell_G_Type),     allocatable, intent(out)  :: Cell       ! Cell object
       class(SpG_Type),        allocatable, intent(out)  :: SpG        ! Space Group object
       type(Atlist_type),                   intent(out)  :: Atm        ! Atom List object
       character(len=*),          optional, intent(in)   :: Atm_typ    ! Type of atoms
       type(MagSymm_k_Type),      optional, intent(out)  :: MGp
       type(mAtom_List_Type),     optional, intent(out)  :: mAtm
       type(Magnetic_Domain_type),optional, intent(out)  :: Mag_dom
       integer,                   optional, intent(in)   :: IPhase     ! Number of phase
       type(File_Type),           optional, intent(out)  :: FType      ! File type
       type(File_list_Type),      optional, intent(out)  :: FileList   ! File list type
       character(len=*),          optional, intent(in)   :: database_path

       !---- Local Variables ----!
       integer :: i
       character(len=:), allocatable:: Ext
       type(File_Type) :: F

       !> Init
       call clear_error()

       !> Load filename
       F=Reading_File(trim(filenam))
       if (err_CFML%Ierr /= 0) return

       if (F%nlines ==0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="Zero lines in the file "//trim(filenam)
          return
       end if

       !> Extension
       Ext=get_extension(trim(filenam))

       select case (trim(u_case(ext)))
          case ('CFL')
             if (present(IPhase)) then
                if(present(Atm_typ)) then
                   if (present(database_path)) then
                     call Read_XTal_CFL(f, Cell, SpG, Atm, Atm_typ, NPhase=IPhase, database_path=database_path)
                   else
                     call Read_XTal_CFL(f, Cell, SpG, Atm, Atm_typ, NPhase=IPhase)
                   end if
                else
                  if (present(database_path)) then
                     call Read_XTal_CFL(f, Cell, SpG, Atm, NPhase=IPhase, database_path=database_path)
                  else
                     call Read_XTal_CFL(f, Cell, SpG, Atm, NPhase=IPhase)
                  end if
                end if
             else
                if(present(Atm_typ)) then
                  if (present(database_path)) then
                     call Read_XTal_CFL(f, Cell, SpG, Atm, Atm_typ, database_path=database_path)  !SpG is allocated inside the subroutine
                  else
                     call Read_XTal_CFL(f, Cell, SpG, Atm, Atm_typ)  !SpG is allocated inside the subroutine
                  end if
                else
                  if (present(database_path)) then
                     call Read_XTal_CFL(f, Cell, SpG, Atm, database_path=database_path)  !SpG is allocated inside the subroutine
                  else
                     call Read_XTal_CFL(f, Cell, SpG, Atm)  !SpG is allocated inside the subroutine
                  end if
                end if
             end if
          case ('CIF')
             if (present(database_path)) then
               call Read_XTal_CIF(f, Cell, SpG, Atm, database_path=database_path)
             else
               call Read_XTal_CIF(f, Cell, SpG, Atm)
             end if
          case ('INS','RES')
            if (present(database_path)) then
               call Read_XTal_SHX(f, Cell, SpG, Atm, database_path=database_path)
            else
               call Read_XTal_SHX(f, Cell, SpG, Atm)
            end if
          case ('FST')
             if(present(mAtm) .and. present(MGp)) then
                if(present(Mag_Dom)) then
                  if (present(database_path)) then
                     call Read_XTal_FST(f, Cell, SpG, Atm, MGp, mAtm, Mag_dom, database_path=database_path)
                  else
                     call Read_XTal_FST(f, Cell, SpG, Atm, MGp, mAtm, Mag_dom)
                  end if
                else
                  if (present(database_path)) then
                     call Read_XTal_FST(f, Cell, SpG, Atm, MGp, mAtm, database_path=database_path)
                  else
                     call Read_XTal_FST(f, Cell, SpG, Atm, MGp, mAtm)
                  end if
                end if
             else
                allocate(Cell_G_Type :: Cell)
                if (present(database_path)) then
                  call Read_XTal_FST(f, Cell, SpG, Atm, database_path=database_path)
                else
                  call Read_XTal_FST(f, Cell, SpG, Atm)
                end if
             end if
          case ('PCR')
          case ('MCIF')
            if (present(database_path)) then
               call Read_XTal_MCIF(f, Cell, Spg, ATM,database_path=database_path)
            else
               call Read_XTal_MCIF(f, Cell, Spg, ATM)
            end if
          case default
             Err_CFML%Ierr=1
             Err_CFML%Msg="The extension file is unknown. Pease, check it! Ext= "//trim(filenam)
             return
       end select

       !> End
       if (present(FType)) FType=F
       if(present(FileList)) then
         FileList%nlines=F%nlines
         allocate(FileList%line(F%nlines))
         do i=1,F%nlines
            FileList%line(i)=F%line(i)%Str
         end do
       end if

    End Subroutine Read_Xtal_Structure

End Module CFML_IOForm

