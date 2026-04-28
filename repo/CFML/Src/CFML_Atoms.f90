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
!!---- MODULE: CFML_Atoms
!!----   INFO: Subroutines related to Atoms definitions
!!----
!!---- HISTORY
!!----    Update: 06/03/2011
!!----
!!
Module CFML_Atoms

   !---- Use Modules ----!
   Use CFML_GlobalDeps
   Use CFML_Rational
   Use CFML_Maths, only: modulo_lat, equal_vector
   Use CFML_Metrics, only: Cell_G_Type
   Use CFML_Strings, only: u_case, l_case
   Use CFML_gSpaceGroups, only: spg_type, apply_op, SuperSpaceGroup_Type, Get_moment_ctr, Get_TFourier_ctr

   !---- Variables ----!
   implicit none

   private

   !---- List of public procedures ----!
   public :: Allocate_Atom_List, Extend_Atom_List, Init_Atom_Type, Read_Bin_Atom_List, &
             Write_Bin_atom_List, Write_Atom_List, Allocate_Atoms_Cell, Index_AtLab_on_AtList
   public :: Equiv_Atm, Wrt_Lab, Check_Symmetry_Constraints, Change_AtomList_Type, &
             AtList_To_Atm_Cell, Allocate_mAtom_list, deAllocate_mAtom_list, &
             Get_moment_and_displacement_SSG, Get_Transf_Tn

   !---- Parameters ----!
   integer, public, parameter :: MAX_MOD = 8          ! Maximum number of modulations per atom
   real(kind=cp), parameter :: R_ATOM = 1.1_cp      ! Average atomic radius

   !---- Types ----!

    !!----
    !!---- TYPE :: ATM_TYPE
    !!----
    !!----  Simple Atom type containing the minimum set of model parameters
    !!----  to describe the atom within a periodic crystal structure. The
    !!----  simplest atom is characterized by its labels (Lab,ChemSymb,SfacSymb, ThType),
    !!----  position X, isotropic displacement parameter U_iso, occupancy factor Occ,
    !!----  magnetic moment, anisotropic displacement parameters U, charge, atomic number,
    !!----  maximum module of magnetic moment and additional information.
    !!----
   Type, Public :: Atm_Type
      character(len=20)             :: Lab = " "          !> Label (identifier) of the atom
      character(len=2)              :: ChemSymb = " "     !> Chemical symbol
      character(len=4)              :: SfacSymb = " "     !> Scattering Factor Symbol
      integer                       :: Z = 0              !> Atomic number
      integer                       :: Mult = 0           !> Multiplicity of the Wyckoff position
      integer                       :: Charge = 0         !> Charge, ionic state
      real(kind=cp), dimension(3)   :: X = 0.0_cp         !> Fractional Coordinates
      real(kind=cp)                 :: U_iso = 0.0_cp     !> Biso, Uiso or Ueq (if ThType ="iso" normally U_iso = Biso)
      real(kind=cp)                 :: Occ = 1.0_cp       !> Occupancy factor
      character(len=4)              :: UType = "B_IJ"     !> Options: U_ij, B_ij, beta (U & beta are for anisotropic thermal factors)
      character(len=3)              :: ThType = "iso"     !> Options: iso, ani
      real(kind=cp), dimension(6)   :: U = 0.0_cp         !> Anisotropic thermal factors
      logical                       :: Magnetic = .false. !> Flag indication if the atom is magnetic or not.
      real(kind=cp)                 :: Mom = 0.0_cp       !> Maximum Module of Magnetic moment
      real(kind=cp), dimension(3)   :: Moment = 0.0_cp    !> Magnetic moment
      integer, dimension(3)         :: Ind_ff = 0         !> Pointer of form factors (1:Xray or species, 2:b, 3:Magff) to the number of the species in calculations
      character(len=40)             :: AtmInfo = " "      !> Information string for different purposes
      character(len=5)              :: wyck = " "         !> Wyckoff position label if known
      real(kind=cp), dimension(5)   :: VarF = 0.0_cp      !> Free variables used for different purposes (1,2,3 reserved for occupations, not refinable)
      logical                       :: active = .true.    !> Control for different purposes
   End Type Atm_Type

    !!----
    !!---- TYPE :: ATM_STD_TYPE
    !!----
    !!----  Simple Atom type + Standard deviation values
    !!----
   Type, Public, Extends(Atm_Type) :: Atm_Std_Type
      real(kind=cp), dimension(3)  :: X_Std = 0.0_cp     ! standard deviations
      real(kind=cp)                :: Occ_Std = 0.0_cp
      real(kind=cp)                :: U_iso_Std = 0.0_cp
      real(kind=cp), dimension(6)  :: U_Std = 0.0_cp
      real(kind=cp), dimension(3)  :: Moment_std = 0.0_cp
   End Type Atm_Std_Type

    !!----
    !!---- TYPE :: ModAtm_STD_TYPE
    !!----
    !!---- This type Modulated Atom type extends Atm_Std_Type by adding modulation
    !!---- Cosine(c) and Sine amplitudes(s) to each model parameter characterizing
    !!---- normal atoms. Up to max_mod harmonic numbers (Q_coeffs) are allowed
    !!---- Still to implement special modulation functions (crenel-type)
    !!---- Probably extending ModAtm_Std_Type or its descendents

    !!----
   Type, Public, Extends(Atm_Std_Type)      :: ModAtm_Std_Type
      integer                               :: n_oc = 0          !> Number of occupation amplitudes
      integer                               :: n_bc = 0          !> Number of B_iso amplitudes
      integer                               :: n_mc = 0          !> Number of moment amplitudes
      integer                               :: n_dc = 0          !> Number of static displacement amplitudes
      integer                               :: n_uc = 0          !> Number of thermal displacement amplitudes
      integer, dimension(max_mod)           :: poc_q = 0         !> Pointer to Q_coeffs of occupatiom amplitudes
      integer, dimension(max_mod)           :: pbc_q = 0         !> Pointer to Q_coeffs of B_iso amplitudes
      integer, dimension(max_mod)           :: pmc_q = 0         !> Pointer to Q_coeffs of moment amplitudes
      integer, dimension(max_mod)           :: pdc_q = 0         !> Pointer to Q_coeffs of displacement amplitudes
      integer, dimension(max_mod)           :: puc_q = 0         !> Pointer to Q_coeffs of thermal displacement amplitudes
      real(kind=cp), dimension(2, max_mod)  :: Ocs = 0.0_cp      !> Ocos,Osin up to 8  (Oc, Os)
      real(kind=cp), dimension(2, max_mod)  :: Ocs_std = 0.0_cp  !> (standard deviations of Ocs)
      real(kind=cp), dimension(2, max_mod)  :: Bcs = 0.0_cp      !> Bcos,Bsin up to 8  (Bc, Bs) B_Iso modulation amplitudes
      real(kind=cp), dimension(2, max_mod)  :: Bcs_std = 0.0_cp  !> (standard deviations of Bcs)
      real(kind=cp), dimension(6, max_mod)  :: Mcs = 0.0_cp      !> Mcos,Msin up to 8  (Mcx Mcy  Mcz , Msx  Msy  Msz)
      real(kind=cp), dimension(6, max_mod)  :: Mcs_std = 0.0_cp  !> (standard deviations of Mcs)
      real(kind=cp), dimension(6, max_mod)  :: Dcs = 0.0_cp      !> Dcos,Dsin up to 8  (Dcx Dcy  Dcz , Dsx  Dsy  Dsz)
      real(kind=cp), dimension(6, max_mod)  :: Dcs_std = 0.0_cp  !> (standard deviations of Dcs)
      real(kind=cp), dimension(12, max_mod) :: Ucs = 0.0_cp      !> Ucos,Usin up to 8  (Uc11 Uc22  Uc33, Uc12, Uc13, Uc23 , Us11 Us22  Us33, Us12, Us13, Us23)
      real(kind=cp), dimension(12, max_mod) :: Ucs_std = 0.0_cp  !> (standard deviations of Ucs)
      !> The items below are not yet used and they are not in CIFs
      real(kind=cp), dimension(:), allocatable :: Xs             !> Position in superspace
      real(kind=cp), dimension(:), allocatable :: Moms           !> Moment in superspace
      real(kind=cp), dimension(:, :), allocatable :: Us          !> Thermal factors in superspace
   End Type ModAtm_Std_Type

    !!----
    !!---- TYPE :: ATM_REF_TYPE
    !!----
    !!----   Refinement Atom type: This type extends Atm_Std_Type with the numbers
    !!----   of each model paramenter within the list of LSQ free parameters, as well
    !!----   as the corresponding multipliers for constraints.
    !!----
   Type, Public, Extends(Atm_Std_Type) :: Atm_Ref_Type
      integer, dimension(3)            :: L_X = 0      ! Code number of free parameters
      integer                          :: L_Occ = 0
      integer                          :: L_U_iso = 0
      integer, dimension(3)            :: L_moment = 0
      integer, dimension(6)            :: L_U = 0
      real(kind=cp), dimension(3)      :: M_X = 0.0_cp ! Multipliers
      real(kind=cp)                    :: M_Occ = 0.0_cp
      real(kind=cp)                    :: M_U_iso = 0.0_cp
      real(kind=cp), dimension(3)      :: M_moment = 0.0_cp
      real(kind=cp), dimension(6)      :: M_U = 0.0_cp
   End Type Atm_Ref_Type

    !!----
    !!---- TYPE :: ModAtm_REF_TYPE
    !!----
    !!----   Refinement Atom type: This type extends ModAtm_Std_Type with the numbers
    !!----   of each model paramenter within the list of LSQ free parameters, as well
    !!----   as the corresponding multipliers for constraints.
    !!----
   Type, Public, Extends(ModAtm_Std_Type)    :: ModAtm_Ref_Type
      integer, dimension(3)                  :: L_X = 0         ! Code Numbers of parameter
      integer                                :: L_Occ = 0
      integer                                :: L_U_iso = 0
      integer, dimension(6)                  :: L_U = 0
      real(kind=cp), dimension(3)            :: M_X = 0.0_cp    ! Multipliers of refinement
      real(kind=cp)                          :: M_Occ = 0.0_cp
      real(kind=cp)                          :: M_U_iso = 0.0_cp
      real(kind=cp), dimension(6)            :: M_U = 0.0_cp
      integer, dimension(2, max_mod)         :: L_Ocs = 0       ! Code Numbers of parameter
      integer, dimension(2, max_mod)         :: L_Bcs = 0       !
      integer, dimension(6, max_mod)         :: L_Mcs = 0       !
      integer, dimension(6, max_mod)         :: L_Dcs = 0       !
      integer, dimension(12, max_mod)        :: L_Ucs = 0       !
      real(kind=cp), dimension(2, max_mod)   :: M_Ocs = 0.0_cp  ! Multipliers
      real(kind=cp), dimension(2, max_mod)   :: M_Bcs = 0.0_cp  ! Multipliers
      real(kind=cp), dimension(6, max_mod)   :: M_Mcs = 0.0_cp  !
      real(kind=cp), dimension(6, max_mod)   :: M_Dcs = 0.0_cp  !
      real(kind=cp), dimension(12, max_mod)  :: M_Ucs = 0.0_cp  !
   End Type ModAtm_Ref_Type

    !!----
    !!---- TYPE ::ATM_CELL_TYPE
    !!--..
    !!---- This type is mostly used for distance-angle and Bond-valence calculations.
    !!---- It holds the position and coordination of all the atoms in the conventional
    !!---- unit cell as well as their distances to neighbours atoms.
    !!----
    !!---- 13/06/2019
    !!
   Type, Public :: Atm_Cell_Type
      integer                                            :: nat            ! Total number of atoms
      character(len=20), dimension(:),       allocatable :: Lab            ! Labels for atoms (dimension Nat)
      real(kind=cp),     dimension(:, :),    allocatable :: xyz            ! Fractional coordinates (3,nat)
      real(kind=cp),     dimension(:),       allocatable :: charge
      real(kind=cp),     dimension(:),       allocatable :: moment
      real(kind=cp),     dimension(:, :),    allocatable :: var_free       ! Free variables (10,nat)
      integer,           dimension(:),       allocatable :: neighb         ! Number of neighbours (nat)
      integer,           dimension(:, :),    allocatable :: neighb_atom    ! Ptr.->neighbour (# in list)(nat,idp)
      real(kind=cp),     dimension(:, :),    allocatable :: distance       ! Corresponding distances (nat,idp)
      real(kind=cp),     dimension(:, :, :), allocatable :: trans          ! Lattice translations   (3,nat,idp)
      integer                                            :: ndist          ! Number of distinct distances
      real(kind=cp),     dimension(:),       allocatable :: ddist          ! List of distinct distances(nat*idp)
      character(len=20), dimension(:),       allocatable :: ddlab          ! Labels of atoms at ddist (nat*idp)
   End Type Atm_Cell_Type

    !!---- Type, Public :: Atom_Equiv_Type
    !!----
    !!----  Updated: January 2014
    !!
   Type, Public :: Atom_Equiv_Type
      integer                                         :: mult = 0
      character(len=2)                                :: ChemSymb = " "
      character(len=20), allocatable, dimension(:)    :: Lab
      real(kind=cp),     allocatable, dimension(:, :) :: x
   End Type Atom_Equiv_Type

    !!---- Type, Public :: Atom_Equiv_List_Type
    !!----
    !!----  Updated: January 2014
    !!
   Type, Public :: Atom_Equiv_List_Type
      integer                                          :: nauas = 0
      type(Atom_Equiv_Type), allocatable, dimension(:) :: atm
   End Type Atom_Equiv_List_Type

    !!----
    !!---- Type, Public :: ALIST_TYPE
    !!----
    !!---- Updated: April - 2022
    !!
   Type, Public :: AtList_Type
      integer                                    :: natoms = 0        ! Number of atoms in the list
      integer                                    :: IPh               ! Index...for Phase (All the atoms of a list belong to a single phase)
      character(len=9)                           :: mcomp = "Crystal" ! For magnetic moments and modulation functions Mcs and Dcs It may be also "Cartesian" or "Spherical"
      logical                                    :: symm_checked = .false.
      logical,         dimension(:), allocatable :: Active            ! Flag for active or not
      class(Atm_Type), dimension(:), allocatable :: Atom              ! Atoms
   End Type AtList_Type

   !  INFO
   !   The following two types are not polymorphic they are directly imported from
   !   the old CrysFML. We maintain these two types because they are used in the modules
   !   containing the keyword "kvec". They are intended for working in legacy description
   !   of magnetic structures in which the formalism of k-vectors and irreducible representations
   !   are the explicitly used. No crystallographic groups are used.

    !!----
    !!---- TYPE :: MATOM_TYPE
    !!--..
    !!---- Type, public :: mAtom_Type
    !!----    character(len=10)                       :: Lab           ! Label
    !!----    character(len=2)                        :: ChemSymb      ! Chemical Symbol
    !!----    character(len=4)                        :: SfacSymb      ! Chemical Symbol for SF
    !!----    character(len=1)                        :: wyck          ! Wyckoff letter
    !!----    logical                                 :: active        ! Control for different purposes
    !!----    integer                                 :: Z             ! Atomic number
    !!----    integer                                 :: mult          ! multiplicity of the site
    !!----    real(kind=cp),dimension(3)              :: x             ! Fractional coordinates
    !!----    real(kind=cp),dimension(3)              :: x_std         ! Standar deviations
    !!----    real(kind=cp),dimension(3)              :: mx            ! Multiplier parameters of coordinates
    !!----    integer,      dimension(3)              :: lx            ! Numbers of LSQ parameters for coordinates
    !!----    real(kind=cp)                           :: occ           ! occupation factor
    !!----    real(kind=cp)                           :: occ_std       ! Standard deviation of occupation factor
    !!----    real(kind=cp)                           :: mOcc          !
    !!----    integer                                 :: lOcc          !
    !!----    real(kind=cp)                           :: Biso          ! Isotropic B-factor
    !!----    real(kind=cp)                           :: Biso_std      ! Standard deviation of Isotropic B-factor
    !!----    real(kind=cp)                           :: mBiso         !
    !!----    integer                                 :: lBiso         !
    !!----    character(len=4)                        :: utype         ! type of anisotropic thermal parameters: u_ij, b_ij, beta, none
    !!----    character(len=5)                        :: thtype        ! "isotr","aniso","other"
    !!----    real(kind=cp),dimension(6)              :: U             ! U11, U22, U33, U12, U13, U23
    !!----    real(kind=cp),dimension(6)              :: U_std         ! Standar_Deviations of U"s
    !!----    real(kind=cp)                           :: Ueq           ! Uequiv
    !!----    real(kind=cp),dimension(6)              :: mU            !
    !!----    real(kind=cp),dimension(6)              :: lU            !
    !!----    real(kind=cp)                           :: Charge        ! Charge
    !!----    real(kind=cp)                           :: Moment        ! Moment
    !!----    integer, dimension(5)                   :: Ind           ! Index for different purposes
    !!----    integer                                 :: Nvar          !
    !!----    real(kind=cp),dimension(25)             :: VarF          ! Free parameters to load
    !!----    real(kind=cp),dimension(25)             :: mVarF
    !!----    integer,      dimension(25)             :: LVarF
    !!----    character(len=40)                       :: AtmInfo       ! Information string
    !!----                           ===================
    !!----                           Magnetic parameters
    !!----                           ===================
    !!----    integer                                 :: nvk           ! Number of propagation vectors (excluding -k)
    !!----    integer,      dimension(12)             :: imat          ! Number of the magnetic matrices/irrep set to be applied
    !!----    real(kind=cp),dimension(3,12)           :: SkR           ! Real part of Fourier Coefficient
    !!----    real(kind=cp),dimension(3,12)           :: SkR_std       ! Standard deviations of the Real part of Fourier Coefficient
    !!----    real(kind=cp),dimension(3,12)           :: Spher_SkR     ! Real part of Fourier Coefficient in spherical components
    !!----    real(kind=cp),dimension(3,12)           :: Spher_SkR_std ! Standard deviations of Real part of Fourier Coefficient in spherical components
    !!----    real(kind=cp),dimension(3,12)           :: mSkR          ! Multipliers for the real part of Fourier coefficients
    !!----    integer,      dimension(3,12)           :: lskr          ! Numbers in the list of LSQ parameters
    !!----    real(kind=cp),dimension(3,12)           :: SkI           ! Imaginary part of Fourier Coefficient
    !!----    real(kind=cp),dimension(3,12)           :: SkI_std       ! Standard deviations of Imaginary part of Fourier Coefficient
    !!----    real(kind=cp),dimension(3,12)           :: Spher_SkI     ! Imaginary part of Fourier Coefficient in spherical components
    !!----    real(kind=cp),dimension(3,12)           :: Spher_SkI_std ! Standard deviations of Imaginary part of Fourier Coefficient in spherical components
    !!----    real(kind=cp),dimension(3,12)           :: mSki          ! Multipliers for the imaginary part of Fourier coefficients
    !!----    integer,      dimension(3,12)           :: lski          ! Numbers in the list of LSQ parameters
    !!----    real(kind=cp),dimension(12)             :: mphas         ! Magnetic Phase in fractions of 2pi
    !!----    real(kind=cp),dimension(12)             :: mphas_std     ! Standard deviations of Magnetic Phase in fractions of 2pi
    !!----    real(kind=cp),dimension(12)             :: mmphas        ! Multiplier for the magnetic phase
    !!----    integer,dimension(12)                   :: lmphas        ! Number in the list of LSQ parameters
    !!----    real(kind=cp),dimension(12,12)          :: cbas          ! Coefficients of the basis functions of irreps, the second index is 1:nvk
    !!----    real(kind=cp),dimension(12,12)          :: cbas_std      ! Standard deviations of Coefficients of the basis functions of irreps, the second index is 1:nvk
    !!----    real(kind=cp),dimension(12,12)          :: mbas          ! multiplier for the coefficients of the basis functions of irreps
    !!----    integer,dimension(12,12)                :: lbas          ! Numbers in the list of LSQ parameters
    !!----    character(len=5)                        :: chitype       ! "isotr","aniso"
    !!----    real(kind=cp),dimension(6)              :: chi           ! chi11, chi22, chi33, chi12, chi13, chi23
    !!----    real(kind=cp),dimension(6)              :: chi_std       ! Standar_Deviations of chi's
    !!----    real(kind=cp)                           :: Chieq         ! Chi equiv
    !!----    real(kind=cp),dimension(6)              :: mchi          !
    !!----    real(kind=cp),dimension(6)              :: lchi          !
    !!---- End Type mAtom_Type
    !!----
    !!---- Updated: April - 2005
    !!---- Updated: November 3, 2013 (include standard deviations of magnetic parameters,JRC)
    !!---- Updated: June 25, 2014 (include local magnetic susceptibility tensor,JRC)
    !!
   Type, public :: mAtom_Type
      character(len=10)                 :: Lab
      character(len=2)                  :: ChemSymb
      character(len=4)                  :: SfacSymb
      character(len=1)                  :: wyck
      logical                           :: Active
      integer                           :: Z
      integer                           :: Mult
      real(kind=cp), dimension(3)       :: X
      real(kind=cp), dimension(3)       :: X_Std
      real(kind=cp), dimension(3)       :: MX
      integer, dimension(3)             :: LX
      real(kind=cp)                     :: Occ
      real(kind=cp)                     :: Occ_Std
      real(kind=cp)                     :: MOcc
      integer                           :: LOcc
      real(kind=cp)                     :: Biso
      real(kind=cp)                     :: Biso_std
      real(kind=cp)                     :: MBiso
      integer                           :: LBiso
      character(len=4)                  :: Utype
      character(len=5)                  :: ThType
      real(kind=cp), dimension(6)       :: U
      real(kind=cp), dimension(6)       :: U_std
      real(kind=cp)                     :: Ueq
      real(kind=cp), dimension(6)       :: MU
      integer, dimension(6)             :: LU
      real(kind=cp)                     :: Charge
      real(kind=cp)                     :: Moment
      integer, dimension(5)             :: Ind
      integer                           :: NVar
      real(kind=cp), dimension(25)      :: VarF
      real(kind=cp), dimension(25)      :: mVarF
      integer, dimension(25)            :: LVarF
      character(len=40)                 :: AtmInfo

      integer                           :: nvk
      integer, dimension(12)            :: imat

      real(kind=cp), dimension(3, 12)   :: SkR
      real(kind=cp), dimension(3, 12)   :: SkR_std
      real(kind=cp), dimension(3, 12)   :: Spher_SkR
      real(kind=cp), dimension(3, 12)   :: Spher_SkR_std
      real(kind=cp), dimension(3, 12)   :: mSkR
      integer,       dimension(3, 12)   :: lskr

      real(kind=cp), dimension(3, 12)   :: SkI
      real(kind=cp), dimension(3, 12)   :: SkI_std
      real(kind=cp), dimension(3, 12)   :: Spher_SkI
      real(kind=cp), dimension(3, 12)   :: Spher_SkI_std
      real(kind=cp), dimension(3, 12)   :: mSki
      integer,       dimension(3, 12)   :: lski

      real(kind=cp), dimension(12)      :: mphas
      real(kind=cp), dimension(12)      :: mphas_std
      real(kind=cp), dimension(12)      :: mmphas
      integer, dimension(12)            :: lmphas

      real(kind=cp), dimension(12, 12)  :: cbas
      real(kind=cp), dimension(12, 12)  :: cbas_std
      real(kind=cp), dimension(12, 12)  :: mbas
      integer, dimension(12, 12)        :: lbas

      character(len=5)                  :: chitype
      real(kind=cp), dimension(6)       :: chi
      real(kind=cp), dimension(6)       :: chi_std
      real(kind=cp)                     :: Chieq
      real(kind=cp), dimension(6)       :: mchi
      real(kind=cp), dimension(6)       :: lchi

   End Type mAtom_Type

    !!----
    !!---- TYPE :: MATOM_LIST_TYPE
    !!--..
    !!---- Type, public :: mAtom_list_type
    !!----    integer                                   :: natoms     ! total number of atoms in the list
    !!----    logical                                   :: suscept    ! true if magnetic moments are calculated from local susceptibility
    !!----    real(kind=cp)                             :: MagField   ! Applied magnetic field strength in Tesla
    !!----    real(kind=cp), dimension(3)               :: dir_MField ! Direction of magnetic field in crystallographic system
    !!----    type(mAtom_Type),dimension(:),allocatable :: Atom       ! individual atoms
    !!---- End Type mAtom_list_type
    !!----
    !!---- Updated: April - 2005, June - 2014
    !!
   Type, public :: mAtom_List_Type
      integer                                     :: natoms
      logical                                     :: suscept    ! true if magnetic moments are calculated from local susceptibility
      real(kind=cp)                               :: MagField   ! Applied magnetic field strength in Tesla
      real(kind=cp),    dimension(3)              :: dir_MField ! Direction of magnetic field in crystallographic system
      type(mAtom_Type), dimension(:), allocatable :: Atom
   End type mAtom_List_Type

   !---- Overload Zone ----!

   Interface Extend_Atom_List
      Module Procedure Extend_List              !Creating a new AtList_Type with all atoms in unit cell
      Module Procedure Set_Atom_Equiv_List      !Creating a an Atom_Equiv_List_Type from AtList_Type in asymmetric unit
   End Interface Extend_Atom_List

   Interface Write_Bin_Atom_List
      Module Procedure Write_Bin_Atom_file
      Module Procedure Write_Bin_Atom_raw
   End Interface Write_Bin_Atom_List

   Interface Get_moment_and_displacement_SSG
      Module Procedure Get_moment_and_displacement_Lab
      Module Procedure Get_moment_and_displacement
   End Interface Get_moment_and_displacement_SSG

   !---- Interface Zone ----!
   Interface

      Pure Module Function Equiv_Atm(nam1, nam2, nameat) Result(equiv_atom)
         !> Determine whether the atoms of names "nam1" and "nam2" are included in
         !> the longer string "nameat" (constructed by function "wrt_lab").
         character(len=*), intent(in) :: nam1, nam2   !> Atom names
         character(len=*), intent(in) :: nameat      !> String containing atom names separated by "--"
         logical                      :: equiv_atom  !> True if nam1 and nam2 are in nameat
      End Function Equiv_Atm

      Pure Module Function Wrt_Lab(Nam1, Nam2) Result(Bilabel)
         !> Character function merging the main part of the labels
         !> (before underscore "_") of the atoms "nam1" and "nam2" into
         !> the string "bilabel" that is of the form: nam1--nam2
         character(len=*),  intent(in) :: nam1, nam2 !> Atoms name
         character(len=:), allocatable :: bilabel    !> Composed string without underscores
      End Function Wrt_Lab

      Pure Module Function Index_AtLab_on_AtList(AtLab, IPhase, AtList) Result(Indx)
         !---- Arguments ----!
         character(len=*),  intent(in) :: AtLab
         integer,           intent(in) :: IPhase
         type(AtList_Type), intent(in) :: AtList
         integer                       :: Indx
      End Function Index_AtLab_on_AtList

      !> Transformation of modulation amplitudes attached to an atom in complex form
      !> Mode="M" or "m" for magnetic modulation amplitudes
      !> Mode="D" or "d" for atomic displacement modulation amplitudes
      !>
      !>   Tmu[n]=1/2 {At%Mcs(1:3,j)+ i At%Mcs(4:6,j)}  (j=1,..At%n_mc)
      !>                        or
      !>   Tmu[n]=1/2 {At%Dcs(1:3,j)+ i At%Dcs(4:6,j)}  (j=1,..At%n_dc)
      !>
      !>   Tns[n]= g_op (Tmu) = magm Tmu[n]Eg exp{2pi.i (Hg rmu + tgI)}  (magm = t_inv det(g) g)
      !>                      or
      !>   Tns[n]= g_op (Tmu) =  g Tmu[n]Eg exp{2pi.i (Hg rmu + tgI)}
      !>
      !>   The action of the operator g_op on modulation amplitudes permute, conjugate and
      !>   multiply by a phase factor the original set. A real space lattice translation
      !>   does not modify the amplitudes that are
      !>
      Module Function Get_Transf_Tn(At, SpG, s, mode) result(Tns)
         type(ModAtm_Std_Type),      intent(in) :: At     !> Modulated atom type
         type(SuperSpaceGroup_type), intent(in) :: SpG    !> Superspace group
         integer,                    intent(in) :: s      !> Number of the symmetry operator
         character(len=*),           intent(in) :: mode   !> M or D
         complex(kind=cp), dimension(3, SpG%nq) :: Tns    !> Transformed set of modulation amplitudes
      End Function Get_Transf_Tn

      Module Subroutine Init_Atom_Type(Atm, d)
         !---- Arguments ----!
         class(Atm_Type), intent(in out)   :: Atm
         integer, intent(in)       :: d
      End Subroutine Init_Atom_Type

      Module Subroutine Init_mAtom_Type(A)
         !---- Arguments ----!
         type(mAtom_Type), intent(in out)   :: A
      End Subroutine Init_mAtom_Type

      Module Subroutine Allocate_Matom_List(N, A, MField, dirF)
         !---- Arguments ----!
         integer,                               intent(in)     :: n
         type(mAtom_list_type),                 intent(in out) :: A
         real(kind=cp), optional,               intent(in)     :: MField
         real(kind=cp), optional, dimension(3), intent(in)     :: dirF
      End Subroutine Allocate_Matom_List

      Module Subroutine Deallocate_mAtom_list(A)
         !---- Arguments ----!
         type(mAtom_list_type), intent(in out)   :: A
      End Subroutine Deallocate_mAtom_list

      Module Subroutine Allocate_Atoms_Cell(Nasu, Mul, Dmax, Ac)
         !---- Arguments ----!
         integer,             intent(in)     :: nasu
         integer,             intent(in)     :: mul
         real(kind=cp),       intent(in)     :: dmax
         type(Atm_cell_type), intent(in out) :: Ac
      End Subroutine Allocate_Atoms_Cell

      Module Subroutine Allocate_Atom_List(N, A, Type_Atm, d, iph)
         !---- Arguments ----!
         integer,           intent(in)       :: n
         type(Atlist_type), intent(in out)   :: A
         character(len=*),  intent(in)       :: Type_Atm
         integer,           intent(in)       :: d
         integer, optional, intent(in)       :: iph
      End Subroutine Allocate_Atom_List

      Module Subroutine AtList_To_Atm_Cell(A, Ac)
         !---- Arguments ----!
         type(Atlist_type),   intent(in)      :: A
         type(Atm_Cell_Type), intent(in out)  :: Ac
      End Subroutine AtList_To_Atm_Cell

      Module Subroutine Change_AtomList_Type(AtList, TypeAtm, Nv)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         character(len=*),  intent(in)     :: TypeAtm
         integer, optional, intent(in)     :: Nv
      End Subroutine Change_AtomList_Type

      Module Subroutine Check_Symmetry_Constraints(SpG, Atm)
         class(SpG_Type),   intent(in)     :: SpG
         type(AtList_Type), intent(in out) :: Atm
      End Subroutine Check_Symmetry_Constraints

      Module Subroutine CopyInfo_Atm_Type(At1, At2)
         !---- Arguments ----T
         class(Atm_Type), intent(in out):: At1
         class(Atm_Type), intent(in)    :: At2
      End Subroutine CopyInfo_Atm_Type

      Module Subroutine CopyInfo_Std_Type(At1, At2)
         !---- Arguments ----T
         class(Atm_Std_Type), intent(in out):: At1
         class(Atm_Std_Type), intent(in)    :: At2
      End Subroutine CopyInfo_Std_Type

      Module Subroutine CopyInfo_MStd_Type(At1, At2)
         !---- Arguments ----T
         class(ModAtm_Std_Type), intent(in out):: At1
         class(ModAtm_Std_Type), intent(in)    :: At2
      End Subroutine CopyInfo_MStd_Type

      !> Module Subroutine Get_moment_and_displacement_Lab(AtL,SpG,atm_lab,num_op,R_latt,pos,Disp,Moment,phase_shift)
      !>
      !>    Subroutine to obtain the position, magnetic moment and displacement of an atom for a structure
      !>    described using a superspace group.
      !>    The atom list of the structure 'AtL' and superspace group 'Spg' are the first two
      !>    intent(in) arguments of the subroutine, the rest of arguments are:
      !>     character(len=*),             intent(in) :: atm_lab  -> Atomic identification (label of the atom among AtL%Atom(:)%Lab)
      !>     Integer,                      intent(in) :: num_op   -> Number of the symmetry operator of the SpG to be applied
      !>     real(kind=cp), dimension(3),  intent(in) :: R_latt   -> Lattice translation of the average structure
      !>     real(kind=cp), dimension(3),  intent(out):: pos      -> Average position of the resulting atom
      !>     real(kind=cp), dimension(3),  intent(out):: Disp     -> Displacement vector with respect to the average position
      !>     real(kind=cp), dimension(3),  intent(out):: moment   -> Magnetic moment of the atom
      !>     real(kind=cp), dimension(:),  intent(in),optional :: phase_shift  -> arbitrary phases (one per modulation vector)
      !>
      !>   This subroutine calls the subroutine Get_moment_and_displacement once the Label of the input is
      !>   resolved and a well defined atom is obtained. Both subroutines are overloaded under
      !>   the name "Get_moment_and_displacement_SSG"
      !>
      Module Subroutine Get_moment_and_displacement_Lab(AtL, SpG, atm_lab, num_op, R_latt, pos, Disp, Moment, phase_shift)
         !---- Arguments ----!
         type(AtList_Type),           intent(in) :: AtL
         type(SuperSpaceGroup_type),  intent(in) :: SpG
         character(len=*),            intent(in) :: atm_lab
         Integer,                     intent(in) :: num_op
         real(kind=cp), dimension(3), intent(in) :: R_latt
         real(kind=cp), dimension(3), intent(out):: pos
         real(kind=cp), dimension(3), intent(out):: Disp
         real(kind=cp), dimension(3), intent(out):: moment
         real(kind=cp), dimension(:), intent(in), optional :: phase_shift
      End Subroutine Get_moment_and_displacement_Lab

      Module Subroutine Get_moment_and_displacement(At, SpG, s, Lat, disp, moment, pos, phase_shift, prn)
         type(ModAtm_Std_Type),       intent(in)  :: At     !Modulated atom type
         type(SuperSpaceGroup_type),  intent(in)  :: SpG
         integer,                     intent(in)  :: s      !Pointer to the symmetry operator relating x and x0: x= g(s) x0 + t(s) + Lat
         real(kind=cp), dimension(3), intent(in)  :: Lat    !Additional lattice translation
         real(kind=cp), dimension(3), intent(out) :: disp   !Amplitude of displacement modulation at position x= g(s) At%x + ts + Lat
         real(kind=cp), dimension(3), intent(out) :: moment !Amplitude of magnetic moment at position x= g(s) At%x + ts + Lat
         real(kind=cp), dimension(3), optional, intent(out) :: pos    !Final position x= g(s) At%x + ts + Lat
         real(kind=cp), dimension(:), optional, intent(in)  :: phase_shift      !phase_shift   xI= phase_shift + (mE, rI)
         logical, optional, intent(in) :: prn
      End Subroutine Get_moment_and_displacement

      Module Subroutine Read_Bin_Atom_List(filename, A, Type_Atm)
         !---- Arguments ----!
         character(len=*),  intent(in)     :: filename
         type(atlist_type), intent(in out) :: A
         character(len=*),  intent(in)     :: Type_Atm
      End Subroutine Read_Bin_Atom_List

      Module Subroutine Write_Bin_Atom_file(filename, A)
         !---- Arguments ----!
         character(len=*),  intent(in) :: filename
         type(atlist_type), intent(in) :: A
      End Subroutine Write_Bin_Atom_file

      Module Subroutine Write_Bin_Atom_raw(Ats, Lun)
         !---- Arguments ----!
         type(atlist_type), intent(in) :: Ats
         integer,           intent(in) :: Lun
      End Subroutine Write_Bin_Atom_raw

      Module Subroutine Write_Atom_List(A, Iphas, Iunit, SpG)
         !---- Arguments ----!
         type(atlist_type), intent(in) :: A
         integer, optional, intent(in) :: Iphas
         integer, optional, intent(in) :: IUnit
         type(SuperSpaceGroup_type), optional, intent(in) :: SpG
      End Subroutine Write_Atom_List

      Module Subroutine Extend_List(A, B, Spg, Type_Atm, Conven, lun)
         !---- Arguments ----!
         type(atlist_type), intent(in)     :: A
         type(atlist_type), intent(in out) :: B
         class(SpG_Type),   intent(in)     :: SpG
         character(len=*),  intent(in)     :: Type_Atm
         logical, optional, intent(in)     :: Conven
         integer, optional, intent(in)     :: lun
      End Subroutine Extend_List

      Module Subroutine Set_Atom_Equiv_List(SpG, cell, A, Ate, lun)
         class(SpG_Type),            intent(in) :: SpG
         type(Cell_G_Type),          intent(in) :: Cell
         type(Atlist_Type),          intent(in) :: A
         type(Atom_Equiv_List_Type), intent(out):: Ate
         integer, optional,          intent(in) :: lun
      End Subroutine Set_Atom_Equiv_List

   End Interface

End Module CFML_Atoms
