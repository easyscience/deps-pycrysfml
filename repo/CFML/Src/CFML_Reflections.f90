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
!!---- MODULE: CFML_Reflections_Utilities
!!----   INFO: Series of procedures handling operation with
!!----         Bragg reflections
!!----
!!
Module CFML_Reflections
   !---- Use Modules ----!
   Use CFML_GlobalDeps,                only: CP, PI, TPI, Err_CFML, Clear_Error, Set_Error, CFML_debug
   Use CFML_gSpaceGroups,              only: Spg_Type,kvect_info_type, SuperSpaceGroup_type, Allocate_KVector, &
                                             Get_Rotation_Order
   Use CFML_Maths,                     only: Trace, Sort, Equal_vector
   Use CFML_Metrics,                   only: Cell_G_Type
   Use CFML_Strings,                   only: l_case
   Use CFML_Rational

   !---- Variables ----!
   implicit none

   private

   !---- List of public functions ----!
   public :: H_Absent, mH_Absent, H_Equal, H_Latt_Absent, H_Equiv, H_Mult, H_S, &
             Get_MaxNumRef, Get_Asymm_Unit_H, Get_h_info

   !---- List of public subroutines ----!
   public :: H_Equiv_List, H_Uni, Initialize_RefList, Gener_Reflections, Gener_Reflections_Shub, &
             Search_Extinctions, Hkl_Gen_Sxtal, Write_Asu, &
             Write_Info_RefList, Init_Refl_Conditions
   public :: Init_Reflist, Generate_Reflections

   !---- Parameters ----!
   real(kind=cp), parameter :: EPS_REF  = 0.0002_cp   ! Epsilon for comparisons within this module

   !---- Types ----!

   !!----
   !!---- TYPE :: REFL_TYPE
   !!--..
   !!
   Type, public :: Refl_Type
      integer,dimension(:), allocatable :: H             ! H
      integer                           :: Mult  = 0     ! Mutiplicity
      real(kind=cp)                     :: S     = 0.0   ! Sin(Theta)/lambda=1/2d
      integer                           :: Imag  = 0     ! 0: nuclear reflection, 1:magnetic, 2=both
      integer                           :: Pcoeff= 0     ! Pointer to the harmonic q_coeff
      real(kind=cp), dimension(3)       :: Hr            !> Real 3D indices of the reflection (Non-integer for incommensurate structures).
   End Type Refl_Type

   !!----
   !!---- TYPE :: SREFL_TYPE
   !!--.. The three indices of the components below correspond to X-rays, Neutron and Electrons
   !!
   Type, public, extends(Refl_Type) :: SRefl_Type
      real(kind=cp), dimension(3)  :: Fo    = 0.0_cp  !> Observed Structure Factor
      real(kind=cp), dimension(3)  :: Fc    = 0.0_cp  !> Calculated Structure Factor
      real(kind=cp), dimension(3)  :: SFo   = 0.0_cp  !> Sigma of  Fo
      real(kind=cp), dimension(3)  :: Phase = 0.0_cp  !> Phase in degrees
      real(kind=cp), dimension(3)  :: A     = 0.0_cp  !> real part of the Structure Factor
      real(kind=cp), dimension(3)  :: B     = 0.0_cp  !> Imaginary part of the Structure Factor
      real(kind=cp), dimension(3)  :: W     = 1.0_cp  !> Weight factor
   End Type SRefl_Type

   !!----
   !!---- TYPE :: MREFL_TYPE
   !!--.. Here, only Neutrons are considered
   !!
   Type, Public, extends(SRefl_Type) :: MRefl_Type
      real(kind=cp)                  :: mIvo  =0.0_cp               ! Observed modulus of the Magnetic Interaction vector
      real(kind=cp)                  :: smIvo =0.0_cp               ! Sigma of observed modulus of the Magnetic Interaction vector
      complex(kind=cp), dimension(3) :: msF   =cmplx(0.0_cp,0.0_cp) ! Magnetic structure factor w.r.t. unitary Crystal Frame
      complex(kind=cp), dimension(3) :: mIv   =cmplx(0.0_cp,0.0_cp) ! Magnetic interaction vector w.r.t. unitary Crystal Frame
      complex(kind=cp), dimension(3) :: MiVC  =0.0                  ! Magnetic interaction vector in Cartesian components w.r.t. Crystal Frame
      real(kind=cp)                  :: sqMiV =0.0                  ! Square of the Magnetic Interaction vector
   End Type MRefl_Type

   !>
   !>  TYPE :: REFLIST_TYPE
   !>
   !>
   Type, public :: RefList_Type
      integer                                     :: NRef=0 !> Number of Reflections
      integer                                     :: i_ph=1 !> Number of the phase for wich the RefList has been generated
                                                            !> Indicator for the number of the crystallographic phase
      class(refl_type), dimension(:), allocatable :: Ref    !> Reflection List
   End Type RefList_Type

   !>
   !>  TYPE :: RefP_type
   !>
   !>   This type extends RefList_Type for powder diffraction patterns
   !>
   Type, extends(RefList_Type), public :: RefP_type         !> This is a property of a phase and the patterns to which it contributes
       integer                                    :: N_patt !> Number of patterns to which the reflection list contributes
       integer,       dimension(:),   allocatable :: patts  !> Patterns to which the reflection list contributes (same as component 'patterns' in Phase_type)
       integer, dimension(:,:,:),     allocatable :: ptr    !> (2,Nref,N_pat), Contributions are in the phase structure, calculated using wdt*FWHM for each pattern
                                                            !> index 1(2) is the minimum(maximum) points in the scattering variable of pattern n_pat
       real(kind=cp), dimension(:,:), allocatable :: pos    !> (Nref,N_patt) !Position of the reflection
       real(kind=cp), dimension(:,:), allocatable :: HG     !> (Nref,N_patt) ! Gaussian FWHM of the reflection or Sigma for TOF
       real(kind=cp), dimension(:,:), allocatable :: HL     !> (Nref,N_patt) ! Lorentzian FWHM of the reflection or Gamma for TOF
       real(kind=cp), dimension(:,:), allocatable :: FWHM   !> (Nref,N_patt) ! FWHM of the reflection
       real(kind=cp), dimension(:,:), allocatable :: ETA    !> (Nref,N_patt) ! Mixing coefficient of the reflection (pseudo-Voigt)
       real(kind=cp), dimension(:,:), allocatable :: Alpha  !> (Nref,N_patt) ! TOF alpha of the reflection
       real(kind=cp), dimension(:,:), allocatable :: Beta   !> (Nref,N_patt) ! TOF beta of the reflection
       real(kind=cp), dimension(:,:), allocatable :: Corr   !> (Nref,N_patt) ! Lorentz-factor * Preferred orientation * absorption *....
   End Type RefP_type


   !---- Private Variables ----!
   logical :: hkl_ref_cond_ini=.false.                ! reflection conditions array has been initialized

   !---- Public Variables ----!
   character(len=80), dimension(58),  public :: Hkl_Ref_Conditions=" "  ! Reflection conditions



   !---- Overload  Zone ----!
   Interface Search_Extinctions
       Module Procedure Search_Extinctions_Iunit
       Module Procedure Search_Extinctions_File
   End Interface Search_Extinctions

   Interface H_S
       Module Procedure H_S_int
       Module Procedure H_S_real
   End Interface H_S

   Interface H_Equal
       Module Procedure H_Equal_Int
       Module Procedure H_Equal_real
   End Interface H_Equal


   !---- Interface Zone ----!
   Interface
      Module Function H_Equal_Int(H,K) Result (Info)
         !---- Arguments ----!
         integer, dimension(:), intent(in) :: H
         integer, dimension(:), intent(in) :: K
         logical                           :: info
      End Function H_Equal_Int

      Module Function H_Equal_Real(H,K) Result (Info)
         !---- Arguments ----!
         real(kind=cp), dimension(:), intent(in) :: H
         real(kind=cp), dimension(:), intent(in) :: K
         logical                                 :: info
      End Function H_Equal_Real

      Module Function H_Absent(H, SpG) Result(Info)
         !---- Arguments ----!
         integer, dimension(:), intent (in) :: H
         class(SpG_Type),       intent (in) :: SpG
         logical                            :: info
      End Function H_Absent

      Module Function mH_Absent(H,SpG) Result(Info)
         !---- Arguments ----!
         integer, dimension(:), intent (in) :: H
         class(SpG_Type),       intent (in) :: SpG
         logical                            :: info
      End Function mH_Absent

      Module Function H_Latt_Absent(H, Latt, n) Result(info)
         !---- Arguments ----!
         integer,        dimension(:),  intent (in) :: h
         type(rational), dimension(:,:),intent (in) :: Latt
         integer,                       intent (in) :: n
         logical                                    :: info
      End Function H_Latt_Absent

      Module Function H_Equiv(H, K, SpG, Friedel) Result (Info)
         !---- Arguments ----!
         integer, dimension(:),        intent(in)  :: H
         integer, dimension(:),        intent(in)  :: K
         class(Spg_Type),              intent(in)  :: SpG
         logical, optional,            intent(in)  :: Friedel
         logical                                   :: info
      End Function H_Equiv

      Module Subroutine H_Equiv_List(H, SpG, Friedel, Mult, H_List,ipos)
         !---- Arguments ----!
         integer, dimension(:),    intent (in) :: H
         class(SpG_Type),          intent (in) :: SpG
         Logical,                  intent (in) :: Friedel
         integer,                  intent(out) :: mult
         integer, dimension(:,:),  intent(out) :: h_list
         integer, optional,        intent(out) :: ipos
      End Subroutine H_Equiv_List

      Module Function H_Mult(H, SpG, Friedel) Result(N)
         !---- Arguments ----!
         integer, dimension(:),  intent (in)  :: H
         class(SpG_Type),        intent (in)  :: SpG
         Logical,                intent (in)  :: Friedel
         integer                              :: N
      End Function H_Mult

      Module Function H_S_real(H, Cell)  Result(S)
         !---- Arguments ----!
         real(kind=cp), dimension(3),            intent(in) :: h
         class(Cell_G_Type),                     intent(in) :: Cell
         real(kind=cp)                                      :: S
      End Function H_S_real

      Module Function H_S_int(H, Cell, Nk, Kv) Result(S)
         !---- Arguments ----!
         integer, dimension(:),                  intent(in) :: h
         class(Cell_G_Type),                     intent(in) :: Cell
         integer, optional,                      intent(in) :: Nk
         real(kind=cp),dimension(:,:), optional, intent(in) :: Kv
         real(kind=cp)                                      :: S
      End Function H_S_int

      Module Function Get_MaxNumRef(SinTLMax, VolCell, SinTLMin, Mult, Multip) Result(numref)
         !> Provides un upper limit of the expected maximum number of
         !> reflections up to SinTLMax for a volume VolCell of the
         !> primitive cell. If SinTLMin is given, the result is the
         !> number of reflections in the interval [SinTLMin,SinTLMax].
         !> We obtain an estimation of the expected mumber of unique reflections.
         !> If Mult is provided, it increases the number of reflections for safety: Numref=nint(Mult*Numref)
         !> If Multip is provided the result is divided by half this multiplicity,
         !> so we obtain an estimation of the expected mumber of unique reflections.
         real(kind=cp),           intent(in) :: SinTLMax !> Maximum sinTheta/Lambda
         real(kind=cp),           intent(in) :: VolCell  !> Direct Cell Volume
         real(kind=cp), optional, intent(in) :: SinTLMin !> Minimum sinTheta/Lambda
         real(kind=cp), optional, intent(in) :: Mult     !> Factor for controlling Numref= nint(Mult*numref)
         integer,       optional, intent(in) :: Multip   !> Multiplicity: Numref=2*Numref/(max(1,Multip))
         integer                             :: numref   !> Number of reflections
      End Function Get_MaxNumRef

      Module Function Unitary_Vector_H(H, Cell) Result (U)
         !---- Arguments ----!
         integer, dimension(3), intent(in) :: H
         class(Cell_G_Type),    intent(in) :: Cell
         real(kind=cp), dimension(3)       :: U
      End Function Unitary_Vector_H

      Module Function Asu_H(H, SpG) Result(K)
         !---- Arguments ----!
         integer, dimension (3),  intent(in) :: h
         class(SpG_Type),         intent(in) :: SpG
         integer, dimension(3)               :: k
      End Function Asu_H

      Module Function Asu_H_Cubic(H, Laue) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: H
         character(len=*),      intent(in) :: Laue
         integer, dimension(3)             :: K
      End Function Asu_H_Cubic

      Module Function Asu_H_Hexagonal(H, Laue) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: h
         character(len=*),      intent(in) :: Laue
         integer, dimension(3)             :: k
      End Function Asu_H_Hexagonal

      Module Function Asu_H_Monoclinic(H, Axis) Result(K)
         !---- Argument ----!
         integer, dimension(3),      intent(in) :: h
         character(len=*), optional, intent(in) :: Axis
         integer, dimension(3)                  :: k
      End Function Asu_H_Monoclinic

      Module Function Asu_H_Orthorhombic(H) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: h
         integer, dimension(3)             :: k
      End Function Asu_H_Orthorhombic

      Module Function Asu_H_Tetragonal(H,Laue) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: h
         character(len=*),      intent(in) :: Laue
         integer, dimension(3)             :: k
      End Function Asu_H_Tetragonal

      Module Function Asu_H_Triclinic(H) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: h
         integer, dimension(3)             :: k
      End Function Asu_H_Triclinic

      Module Function Asu_H_Trigonal(H, Laue) Result(K)
         !---- Argument ----!
         integer, dimension(3), intent(in) :: h
         character(len=*),      intent(in) :: Laue
         integer, dimension(3)             :: k
      End Function Asu_H_Trigonal

      Module Function Get_Asymm_Unit_H(H,SpG) Result(k)
         !---- Arguments ----!
         integer, dimension (3),  intent(in) :: h
         class(SpG_Type),         intent(in) :: SpG
         integer, dimension(3)               :: k
      End Function Get_Asymm_Unit_H

      Module Function Generate_Reflections(Cell,Slmin,Slmax,SpG,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ) Result(Reflex)
         !> Calculate reflections between the sin_theta/lambda shells defined by (Slmin,Slmax)
         !> Valid for all type of space groups.
         class(Cell_G_Type),                intent(in)     :: Cell     !> Unit cell object
         real(kind=cp),                     intent(in)     :: Slmin    !> Minimum SinTheta/Lambda
         real(kind=cp),                     intent(in)     :: Slmax    !> Maximum SinTheta/Lambda
         class(Spg_Type),                   intent(in)     :: SpG      !> General Space Group
         logical,                 optional, intent(in)     :: MagExt   !> Magnetic extinctions if MagExt = True
         type(kvect_info_type),   optional, intent(in)     :: Kinfo    !> Modulation vector information
         logical,                 optional, intent(in)     :: Order    !> If True, reflections ordered by increasing sinTheta/Lambda
         logical,                 optional, intent(in)     :: Unique   !> Ordered unique reflections are generated
         integer, dimension(3),   optional, intent(in)     :: seqindx  !> Sequence of indices change
         integer, dimension(3,2), optional, intent(in)     :: hlim     !> Index limits
         logical,                 optional, intent(in)     :: Mag_only !> Only magnetic reflections are generated
         logical,                 optional, intent(in)     :: Friedel  !> Apply Friedel law if True
         character(len=*),        optional, intent(in)     :: Ref_typ  !> Reflection type: Refl | SRefl | MRefl
         type(RefList_Type)                                :: Reflex   !> Reflection list
      End Function Generate_Reflections

      Module Subroutine Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,iphase,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ,kout)
         !---- Arguments ----!
         class(Cell_G_Type),                intent(in)     :: Cell
         real(kind=cp),                     intent(in)     :: Slmin
         real(kind=cp),                     intent(in)     :: Slmax
         class(RefList_Type),               intent(in out) :: Reflex
         class(Spg_Type) ,        optional, intent(in)     :: SpG
         integer,                 optional, intent(in)     :: iphase
         logical,                 optional, intent(in)     :: MagExt
         type(kvect_info_type),   optional, intent(in)     :: Kinfo
         logical,                 optional, intent(in)     :: Order
         logical,                 optional, intent(in)     :: Unique
         integer, dimension(3),   optional, intent(in)     :: seqindx
         integer, dimension(3,2), optional, intent(in)     :: hlim
         logical,                 optional, intent(in)     :: Mag_only
         logical,                 optional, intent(in)     :: Friedel
         character(len=*),        optional, intent(in)     :: Ref_typ
         type(kvect_info_type),   optional, intent(out)    :: kout
      End Subroutine Gener_Reflections

      Module Function Get_h_info(h,SpG,mag)  Result(info)
         integer, dimension(:), intent (in) :: h
         class(SpG_Type),       intent (in) :: SpG
         logical,               intent (in) :: mag
         integer, dimension(4)              :: info
      End Function Get_h_info

      Module Subroutine Init_Refl_Conditions()
         !---- Arguments ----!
      End Subroutine Init_Refl_Conditions

      Module Subroutine Write_Integral_Conditions(SpG,iunit)
         !---- Arguments ----!
         class(SpG_Type),    intent(in)  :: SpG
         integer, optional,  intent(in)  :: iunit
      End Subroutine Write_Integral_Conditions

      Module Subroutine Write_Glide_Planes_Conditions(SpG,Iunit)
         !---- Arguments ----!
         class(SpG_Type),    intent(in) :: SpG
         integer, optional,  intent(in) :: Iunit
      End Subroutine Write_Glide_Planes_Conditions

      Module Subroutine Write_Screw_Axis_Conditions(SpG ,Iunit)
         !---- Arguments ----!
         class(SpG_Type),    intent(in) :: SpG
         integer, optional,  intent(in) :: Iunit
      End Subroutine Write_Screw_Axis_Conditions

      Module Subroutine Search_Extinctions_Iunit(SpG, Iunit)
         !---- Arguments ----!
         class(SpG_Type),   intent(in) :: SpG
         integer, optional, intent(in) :: Iunit
      End Subroutine Search_Extinctions_Iunit

      Module Subroutine Search_Extinctions_File(SpG, nlines, filevar)
         !---- Arguments ----!
         class(SpG_Type),                intent(in)   :: SpG
         integer,                        intent(out)  :: nlines
         character(len=*), dimension(:), intent(out)  :: filevar
      End Subroutine Search_Extinctions_File

      Module Function Init_Reflist(N,Ctype,D) Result(Reflex)
         !> Returns a reflection list object
         integer,             intent(in)     :: N       !> Number of reflections in the list
         character(len=*),    intent(in)     :: Ctype   !> Reflection type: Refl | SRefl | MRefl
         integer, optional,   intent(in)     :: D       !> Dimension of the magnetic hkl
         class(RefList_Type), allocatable    :: Reflex  !> Reflection list
      End Function Init_Reflist

      Module Subroutine Initialize_RefList(N, Reflex, Ctype, D)
         !---- Arguments ----!
         integer,             intent(in)     :: N
         class(RefList_Type),  intent(in out) :: Reflex
         character(len=*),    intent(in)     :: Ctype
         integer, optional,   intent(in)     :: D
      End Subroutine Initialize_RefList

      Module Subroutine Write_Info_RefList(Reflex, Iunit, Mode,kinfo)
         !---- Arguments ----!
         class(RefList_Type),             intent(in) :: Reflex
         integer,               optional, intent(in) :: Iunit
         character(len=*),      optional, intent(in) :: Mode
         type(kvect_info_type), optional, intent(in) :: kinfo
      End Subroutine Write_Info_RefList

      Module Subroutine Write_Asu(Spacegroup, iunit)
         !---- Arguments ----!
         class (SPG_type),        intent(in) :: Spacegroup
         integer,optional,        intent(in) :: iunit
      End Subroutine Write_Asu

      Module Subroutine H_Uni(Cell, Spg, Friedel, Vmin, Vmax, Code, MaxRef, Reflex, No_order, Check_ok, Hlim, Ref_Typ)
         !---- Arguments ----!
         type (Cell_G_Type),               intent(in)     :: Cell
         class (SpG_Type) ,                intent(in)     :: Spg
         logical,                          intent(in)     :: Friedel
         real(kind=cp),                    intent(in)     :: vmin,vmax
         character(len=1),                 intent(in)     :: code
         integer,                          intent(in)     :: MaxRef
         class (RefList_Type),             intent(out)    :: reflex
         logical,                optional, intent(in)     :: no_order
         logical,                optional, intent(out)    :: check_ok
         integer, dimension(3,2),optional, intent(in)     :: hlim
         character(len=*),       optional, intent(in)     :: Ref_typ
      End Subroutine H_Uni

      Module Subroutine Hkl_Gen_Sxtal(Crystalcell,Spacegroup,stlmin,stlmax,Reflex,ord,hlim)
         !---- Arguments ----!
         type(Cell_G_Type),                 intent(in)  :: crystalcell
         type (SPG_Type) ,                  intent(in)  :: spacegroup
         real(kind=cp),                     intent(in)  :: stlmin,stlmax
         class(RefList_Type),               intent(out) :: reflex
         Integer, dimension(3),   optional, intent(in)  :: ord
         Integer, dimension(3,2), optional, intent(in)  :: hlim
      End Subroutine Hkl_Gen_Sxtal

      Module Subroutine Gener_Reflections_Shub(Cell,SpG, Smax, Reflex,Friedel)
         !---- Arguments ----!
         type (Cell_G_Type),    intent(in)     :: Cell
         type (SpG_Type) ,      intent(in)     :: SpG
         real(kind=cp),         intent(in)     :: Smax
         type (RefList_Type),   intent(in out) :: Reflex
         logical,  optional,    intent(in)     :: Friedel
      End Subroutine Gener_Reflections_Shub

   End Interface



End Module CFML_Reflections

