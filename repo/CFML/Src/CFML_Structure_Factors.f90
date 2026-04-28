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
!!---- MODULE: CFML_Structure_Factors
!!----   INFO: Main module for Structure Factors Calculations
!!----
!!----
!!
Module CFML_Structure_Factors

    !---- Use Modules ----!
    Use CFML_GlobalDeps,                  only: CP, DP, TPI, err_cfml, clear_error, CFML_debug
    Use CFML_Strings,                     only: L_Case, U_Case, File_Type, File_list_Type
    Use CFML_Atoms,                       only: AtList_type, Atm_Ref_Type, ModAtm_Ref_Type
    Use CFML_gSpaceGroups,                only: Spg_Type
    Use CFML_Metrics,                     only: Cell_G_Type
    Use CFML_Reflections,                 only: RefList_Type, Refl_Type, SRefl_Type, MRefl_Type, &
                                                Gener_Reflections_Shub, Gener_Reflections
    Use CFML_Scattering_Tables
    Use CFML_Rational


    !---- Variables ----!
    implicit none

    private

    !---- List of public procedures ----!
    public :: Additional_Scattering_Factors, Allocate_Scattering_Species, &
              Calc_General_StrFactor, Calc_hkl_StrFactor, Calc_StrFactor, &
              Init_Structure_Factors, Init_Calc_hkl_StrFactors, Init_Calc_StrFactors, &
              Modify_SF, Magnetic_Structure_Factors, Set_Form_Factors, Structure_Factors, &
              Write_Structure_Factors, Write_Structure_Factors_Mag, &
              SF_init_opMatTr, SF_Clear_init_symOp
    public :: Calc_Mag_Structure_Factor ! added by Nebil 14/03/2025

    !---- Definitions ----!

    !----
    !--++ F(h)=Sum_j[Fj(h){Aj(h)+i Bj(h)}]
    !----

    !!--++
    !!--++    TYPE :: HR_Type
    !!--++       Define a H vector
    !!--++
    !!--..
    !!
    Type, Private :: HR_Type
       real, dimension(3) :: H=0
    End Type HR_Type

    !!----
    !!---- TYPE, PUBLIC :: Scattering_Species_Type
    !!----
    !!----     Type encapsulating scattering factors for neutrons and X-rays.
    !!----     Constructed by calling Set_Form_Factors
    !!----
    Type, Public :: Scattering_Species_Type
       integer                                            :: Num_Species=0        ! Number of species
       integer                                            :: Num_Magspc =0        ! number of magnetic species
       character(len=6),        dimension(:), allocatable :: Symb
       character(len=6),        dimension(:), allocatable :: Symb_Mag
       real(kind=cp),           dimension(:), allocatable :: br
       real(kind=cp),           dimension(:), allocatable :: bi
       real(kind=cp),           dimension(:), allocatable :: delta_fp
       real(kind=cp),           dimension(:), allocatable :: delta_fpp
       type(Xray_Form_Type),    dimension(:), allocatable :: Xcoef
       type(Magnetic_Form_Type),dimension(:), allocatable :: Mcoef
    End Type Scattering_Species_Type

    !!----
    !!---- TYPE, PUBLIC :: STRF_TYPE
    !!--..
    !!
    Type, Public  :: Strf_Type
       real(kind=cp)                    :: sqNuc=0.0      ! Square of the nuclear structure factor
       real(kind=cp)                    :: sqMiV=0.0      ! Square of the Magnetic Interaction vector
       complex(kind=cp)                 :: NsF  =0.0      ! Nuclear structure factor
       complex(kind=cp), dimension(3)   :: MsF  =0.0      ! Magnetic structure factor w.r.t. unitary Crystal Frame
       complex(kind=cp), dimension(3)   :: MiV  =0.0      ! Magnetic interaction vector w.r.t. unitary Crystal Frame
       complex(kind=cp), dimension(3)   :: MiVC =0.0      ! Magnetic interaction vector in Cartesian components w.r.t. Crystal Frame
    End Type  Strf_Type

    !!----
    !!---- TYPE, PUBLIC :: STRF_LIST_TYPE
    !!--..
    !!
    Type, Public  :: StrfList_Type
       integer                                   :: Nref=0 ! Number of Reflections
       Type(Strf_Type), dimension(:),allocatable :: Strf
    End Type  StrfList_Type

    logical, private :: SF_Initialized=.false.   ! Variable indicating if the module has been initialized

    integer,                                    private :: NSpecies=0 ! Number of chemical species for X-rays scattering form factors
    integer, dimension(:),         allocatable, private :: P_A    ! Integer pointer from atoms to species. Dim=N_atoms
    real(kind=cp), dimension(:,:), allocatable, private :: AF0    ! Array for Atomic Factor. Dim=(Natoms,NRef)
    real(kind=cp), dimension(:),   allocatable, private :: AFP    ! Array for real part of anomalous scattering form factor. Dim=(Natoms)
    real(kind=cp), dimension(:),   allocatable, private :: AFPP   ! Array for imaginary part of anomalous scattering form factor. Dim=(Natoms)
    real(kind=cp), dimension(:,:), allocatable, private :: AJH    ! Array for Aj(h). Dim=(Natoms,NRef)
    real(kind=cp), dimension(:,:), allocatable, private :: BJH    ! Array for Bj(h). Dim=(Natoms,NRef)
    real(kind=cp), dimension(:,:), allocatable, private :: FF_a   ! Array for X-rays scattering form factors. Dim=(4,NSpecies)
    real(kind=cp), dimension(:,:), allocatable, private :: FF_b   ! Array for X-rays scattering form factors. Dim=(4,NSpecies)
    real(kind=cp), dimension(  :), allocatable, private :: FF_c   ! Array for X-rays scattering form factors. Dim=(NSpecies)
    real(kind=cp), dimension(  :), allocatable, private :: FF_Z   ! Array for X-rays scattering form factors. Dim=(NSpecies)
    real(kind=cp), dimension(:,:), allocatable, private :: HT     ! Array for HT Calculations. Dim(Natoms, NRef)
    real(kind=cp), dimension(:,:), allocatable, private :: TH     ! Array for TH Calculations. Dim(Natoms, NRef)
    ! Private symmetry operators
    real(kind=cp), dimension(:,:), allocatable, private :: optr   ! Associated translations of symmetry operators
    integer, dimension(:,:,:),     allocatable, private :: opMat  ! Matrices of the symmetry operators
    logical, private :: init_symOp=.false.            !Should be initialized for different phases
    logical, private :: diff_electrons=.false.        !If electron diffraction is being calculated it becomes .true. to select the third %Fc %Fo ... component


    type(HR_Type), dimension(:,:), allocatable, private :: HR     ! Array for HR Calculations. Dim=(Natoms, Nref)


    !---- Overload zone ----!
    Interface Write_Structure_Factors
       Module Procedure Write_Structure_Factors_Crys
       Module Procedure Write_Structure_Factors_Mag
    End Interface Write_Structure_Factors

    Interface Additional_Scattering_Factors
       Module Procedure Additional_Scattering_Factors_FT
       Module Procedure Additional_Scattering_Factors_FLT
    End Interface Additional_Scattering_Factors

    !---- Interface Zone ----!
    Interface

       Module Function Fj(S,A,B,C) Result(Res)
          !---- Arguments ----!
          real(kind=cp),             intent(in) :: s
          real(kind=cp),dimension(4),intent(in) :: a
          real(kind=cp),dimension(4),intent(in) :: b
          real(kind=cp),             intent(in) :: c
          real(kind=cp)                         :: res
       End Function Fj

       Module Subroutine Create_Table_HR_HT(Reflex, Grp)
          !---- Arguments ----!
          class(RefList_Type),intent(in) :: Reflex
          type(Spg_type),     intent(in) :: Grp
       End Subroutine Create_Table_HR_HT

       Module Subroutine Calc_Table_AB(Nref, Atm, Grp)
          !---- Arguments ----!
          integer,            intent(in) :: Nref
          type(AtList_type),  intent(in) :: Atm
          type(SpG_type),     intent(in) :: Grp
       End Subroutine Calc_Table_AB

       Module Subroutine Calc_Table_TH(Reflex, Atm)
          !---- Argument ----!
          class(RefList_Type),intent(in) :: Reflex
          type(AtList_type),  intent(in) :: Atm
       End Subroutine Calc_Table_TH

       Module Subroutine Create_Table_AF0_Electrons(Reflex, Atm, lun)
          !---- Arguments ----!
          class(RefList_Type),intent(in) :: Reflex
          type(AtList_type),  intent(in) :: Atm
          integer, optional,  intent(in) :: lun
       End Subroutine Create_Table_AF0_Electrons

       Module Subroutine Create_Table_AF0_Xray(Reflex, Atm, lambda, lun)
          !---- Arguments ----!
          class(RefList_Type),     intent(in) :: Reflex
          type(AtList_type),       intent(in) :: Atm
          real(kind=cp), optional, intent(in) :: lambda
          integer,       optional, intent(in) :: lun
       End Subroutine Create_Table_AF0_Xray

       Module Subroutine Create_Table_AFP_NeutNuc(Atm, lun)
          !---- Arguments ----!
          type(AtList_type), intent(in) :: Atm
          integer, optional, intent(in) :: lun
       End Subroutine Create_Table_AFP_NeutNuc

       Module Subroutine Create_Table_fabc_Xray(Atm, Lambda, Elect, lun)
          !---- Arguments ----!
          type(AtList_type),           intent(in) :: Atm
          real(kind=cp),     optional, intent(in) :: lambda
          integer,           optional, intent(in) :: elect
          integer,           optional, intent(in) :: lun
       End Subroutine Create_Table_fabc_Xray

       Module Subroutine Write_Structure_Factors_Crys(Reflex, Lun, Mode,iph)
          !---- Argument ----!
          class(RefList_Type),        intent(in) :: Reflex
          integer,                    intent(in) :: lun
          character(len=*), optional, intent(in) :: Mode
          integer,          optional, intent(in) :: iph
       End Subroutine Write_Structure_Factors_Crys

       Module Subroutine Write_Structure_Factors_Mag(Reflex, Stf, Lun, Full,iph)
          !---- Argument ----!
          class(RefList_Type),     intent(in) :: Reflex
          type(StrfList_Type),     intent(in) :: stf
          integer,                 intent(in) :: lun
          logical, optional,       intent(in) :: full
          integer, optional,       intent(in) :: iph
       End Subroutine Write_Structure_Factors_Mag

       Module Subroutine Allocate_Scattering_Species(N, Scf)
          !---- Arguments ----!
          integer,                       intent(in)  :: n
          type(Scattering_Species_Type), intent(out) :: Scf
       End Subroutine Allocate_Scattering_Species

       Module Subroutine Additional_Scattering_Factors_FT(Fil, Add_Scatt)
          !---- Arguments ----!
          type(File_Type),               intent(in)  :: fil
          Type(Scattering_Species_Type), intent(out) :: add_Scatt
       End Subroutine Additional_Scattering_Factors_FT

       Module Subroutine Additional_Scattering_Factors_FLT(Fil, Add_Scatt)
          !---- Arguments ----!
          type(File_list_Type),          intent(in)  :: fil
          Type(Scattering_Species_Type), intent(out) :: add_Scatt
       End Subroutine Additional_Scattering_Factors_FLT

       Module Subroutine Init_Structure_Factors(Reflex, Atm, Grp, Mode, Lambda, Lun)
          !---Arguments ---!
          class(RefList_Type),         intent(in) :: Reflex
          type(AtList_type),           intent(in) :: Atm
          type(SpG_type),              intent(in) :: Grp
          character(len=*),  optional, intent(in) :: Mode
          real(kind=cp),     optional, intent(in) :: lambda
          integer,           optional, intent(in) :: lun
       End Subroutine Init_Structure_Factors

       Module Subroutine Init_Calc_StrFactors(Reflex, Atm, Grp, Mode, Lambda, Lun)
          !---Arguments ---!
          class(RefList_Type),        intent(in) :: Reflex
          type(AtList_type),          intent(in) :: Atm
          type(SpG_type),             intent(in) :: Grp
          character(len=*), optional, intent(in) :: Mode
          real(kind=cp),    optional, intent(in) :: lambda
          integer,          optional, intent(in) :: lun
       End Subroutine Init_Calc_StrFactors

       Module Subroutine Init_Calc_hkl_StrFactors(Atm, Grp, Mode, Lambda, Lun)
          !---Arguments ---!
          type(AtList_type),           intent(in) :: Atm
          type(SpG_type),              intent(in) :: Grp
          character(len=*),  optional, intent(in) :: Mode
          real(kind=cp),     optional, intent(in) :: lambda
          integer,           optional, intent(in) :: lun
       End Subroutine Init_Calc_hkl_StrFactors

       Module Subroutine Set_Fixed_Tables(Reflex, Atm, Grp, Mode, Lambda, Lun)
          !---- Arguments ----!
          class(RefList_Type),        intent(in) :: Reflex
          type(AtList_type),          intent(in) :: Atm
          type(SpG_type),             intent(in) :: Grp
          character(len=*), optional, intent(in) :: Mode
          real(kind=cp),    optional, intent(in) :: lambda
          integer,          optional, intent(in) :: lun
       End Subroutine Set_Fixed_Tables

       Module Subroutine Set_Form_Factors(Atm, Scf, Lambda, Add_Scatt, Mag, Lun)
          !---- Arguments ----!
          type(AtList_type),                      intent(in out):: Atm
          type(Scattering_Species_Type),          intent(out)   :: Scf
          real(kind=cp),                optional, intent(in)    :: lambda
          type(Scattering_Species_Type),optional, intent(in)    :: Add_Scatt
          logical,                      optional, intent(in)    :: mag
          integer,                      optional, intent(in)    :: lun
       End Subroutine Set_Form_Factors

       Module Subroutine Modify_SF(Reflex, Atm, Grp, List, Nlist, Partyp, Mode)
          !---- Arguments ----!
          class(RefList_Type),        intent(in out) :: Reflex
          type(AtList_type),          intent(in)     :: Atm
          class(SpG_type),            intent(in)     :: Grp
          integer,dimension(:),       intent(in)     :: List
          integer,                    intent(in)     :: NList
          character(len=*), optional, intent(in)     :: partyp
          character(len=*), optional, intent(in)     :: Mode
       End Subroutine Modify_SF

       Module Subroutine Sum_AB(Reflex, Natm, Icent)
          !---- Arguments ----!
          class(RefList_Type),intent(in out)  :: Reflex
          integer,            intent(in)      :: Natm
          integer,            intent(in)      :: icent
       End Subroutine Sum_AB

       Module Subroutine Sum_AB_NeutNuc(Reflex, Natm, Icent)
          !---- Arguments ----!
          class(RefList_Type),  intent(in out) :: Reflex
          integer,              intent(in)     :: Natm
          integer,              intent(in)     :: icent
       End Subroutine Sum_AB_NeutNuc

       Module Subroutine Structure_Factors(Reflex, Atm, Grp, Mode, Lambda)
          !---- Arguments ----!
          class(RefList_Type),          intent(in out) :: Reflex
          type(AtList_type),            intent(in)     :: Atm
          class(SpG_type),              intent(in)     :: Grp
          character(len=*),   optional, intent(in)     :: Mode
          real(kind=cp),      optional, intent(in)     :: lambda
       End Subroutine Structure_Factors

       Module Subroutine Calc_General_StrFactor(Hn, Sn, Atm, Grp, Scf, fn, fx, fe)
          !---- Arguments ----!
          real(kind=cp),dimension(3),    intent(in) :: Hn
          real(kind=cp),                 intent(in) :: Sn
          type(AtList_type),             intent(in) :: Atm
          class(SpG_type),               intent(in) :: Grp
          type(Scattering_Species_Type), intent(in) :: Scf
          complex, optional,             intent(out):: fn,fx,fe
       End Subroutine Calc_General_StrFactor

       Module Subroutine Calc_hkl_StrFactor(Hn, Sn, Atm, Grp, Mode, Rad, Sf2, Deriv, fc)
          !---- Arguments ----!
          integer,dimension(3),                  intent(in) :: Hn
          real(kind=cp),                         intent(in) :: Sn
          type(AtList_type),                     intent(in) :: Atm
          class(SpG_type),                       intent(in) :: Grp
          character(len=*),                      intent(in) :: Mode
          character(len=*),                      intent(in) :: Rad
          real(kind=cp),                         intent(out):: sf2
          real(kind=cp), dimension(:), optional, intent(out):: deriv
          complex,                     optional, intent(out):: fc
       End Subroutine Calc_hkl_StrFactor

       Module Subroutine Calc_StrFactor(Nn, Sn, Atm, Grp, Mode, Rad, Sf2, Deriv, fc)
          !---- Arguments ----!
          integer,                            intent(in) :: nn
          real(kind=cp),                      intent(in) :: sn
          type(AtList_type),                  intent(in) :: Atm
          class(SpG_type),                    intent(in) :: Grp
          character(len=*),                   intent(in) :: mode
          character(len=*),                   intent(in) :: rad
          real(kind=cp),                      intent(out):: sf2
          real(kind=cp),dimension(:),optional,intent(out):: deriv
          complex, optional,                  intent(out):: fc
       End Subroutine Calc_StrFactor

       Module Subroutine Magnetic_Structure_Factors(Reflex, Cell, Atm, Grp, Smax, Stf, mode, lun)
          !---- Arguments ----!
          class(RefList_Type),       intent(in out) :: Reflex
          type(Cell_G_Type),         intent(in)     :: Cell
          type(AtList_type),         intent(in out) :: Atm
          class(SpG_type),           intent(in)     :: Grp
          real(kind=cp),             intent(in)     :: Smax
          type(StrfList_Type),       intent(out)    :: Stf
          character(len=*),optional, intent(in)     :: mode
          integer,         optional, intent(in)     :: lun
       End Subroutine Magnetic_Structure_Factors

       Module Subroutine Calc_Mag_Structure_Factor(Hm, Cell, Grp, Atm, Scf, Mode, Strf, Magonly, Mdom, Tdom, Twin)
          !---- Arguments ----!
          class(Refl_Type),                      intent(in)  :: Hm
          type(Cell_G_type),                     intent(in)  :: Cell
          class(SpG_type),                       intent(in)  :: Grp
          type(AtList_type),                     intent(in)  :: Atm
          type(Scattering_Species_Type),         intent(in)  :: Scf
          character(len=*),                      intent(in)  :: Mode
          type(Strf_Type),                       intent(out) :: Strf
          logical,                     optional, intent(in)  :: Magonly
          integer, dimension(3,3),     optional, intent(In)  :: Mdom
          real(kind=cp), dimension(3), optional, intent(In)  :: Tdom
          character(len=*),            optional, intent(In)  :: Twin
       End Subroutine Calc_Mag_Structure_Factor

   End Interface

   contains

      Subroutine SF_init_opMatTr(SpG)
        class(SPG_Type), intent(in) :: SpG
        integer :: i
        if(.not. init_symOp) then
           ! Private symetry operators to accelerate calculations
           if(allocated(opMat)) deallocate(opMat)
           if(allocated(opTr))  deallocate(opTr)
           allocate(opMat(3,3,SpG%Multip),opTr(3,SpG%Multip))
           do i=1,SpG%Multip
             opMat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
              opTr(:,i)  = SpG%Op(i)%Mat(1:3,4)
           End do
           init_symOp=.true.
        end if
      End Subroutine SF_init_opMatTr

      Subroutine SF_Clear_init_symOp()
         !> Set public variable init_symOp to False.
        init_symOp=.false.
      End Subroutine SF_Clear_init_symOp


End Module CFML_Structure_Factors

