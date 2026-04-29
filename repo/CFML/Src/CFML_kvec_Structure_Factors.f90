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
!!---- MODULE: CFML_Magnetic_Structure_Factors
!!----   INFO: Main module for Structure Factors Calculations
!!----
!!---- HISTORY
!!----    Update: 07/03/2011
!!----
!!----
!!---- DEPENDENCIES
!!--++    Use CFML_GlobalDeps,                  only: cp, sp, dp, tpi, Err_CFML, Clear_Error
!!--++    Use CFML_Maths,                       only: sort
!!--++    Use CFML_Strings,                     only: L_Case,U_Case
!!--++    Use CFML_Scattering_Tables,           only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form, &
!!--++                                                Magnetic_Form
!!--++    Use CFML_Metrics,                     only: Cell_G_Type
!!--++    Use CFML_gSpaceGroups,                only: SPG_Type, Set_SpaceGroup
!!--++    Use CFML_kvec_Symmetry,               only: ApplyMSO, MagSymm_k_type, Write_Magnetic_Structure, &
!!--++                                                Magnetic_Domain_type
!!--++    Use CFML_Reflections,                 only: Hkl_Gen_Sxtal,Gener_Reflections_Shub, Get_MaxNumRef,&
!!--++                                                Refl_Type, RefList_Type, h_s, Initialize_RefList
!!--++    Use CFML_Atoms,                       only: Matom_list_type, Allocate_mAtom_list
!!--++    Use CFML_Propagation_Vectors,         only: K_Equiv_Minus_K
!!--++    Use CFML_Rational
!!----
!!---- VARIABLES
!!--..    Types
!!--++    HR_TYPE                      [Private]
!!----    MAGH_TYPE
!!----    MAGH_LIST_TYPE
!!----    MAGHD_TYPE
!!----    MAGHD_LIST_TYPE
!!--++    AJH                          [Private]
!!--++    BJH                          [Private]
!!--++    HR                           [Private]
!!--++    HT                           [Private]
!!--++    MFI                          [Private]
!!--++    MFR                          [Private]
!!--++    MSF_INITIALIZED              [Private]
!!----    PN
!!--++    TH                           [Private]
!!----
!!---- PROCEDURES
!!----    Functions:
!!--++       MFJ                         [Private]
!!----
!!----    Subroutines:
!!----       CALC_MAG_INTERACTION_VECTOR
!!----       CALC_MAGNETIC_STRF_MIV
!!----       CALC_MAGNETIC_STRF_MIV_DOM
!!----       CALC_MAGNETIC_STRF_TENSOR
!!--++       CALC_TABLE_MAB              [Private]
!!--++       CALC_TABLE_TH               [Private]
!!--++       CREATE_TABLE_HR_HT          [Private]
!!--++       CREATE_TABLE_MFR            [Private]
!!----       GEN_SATELLITES
!!----       INIT_MAG_STRUCTURE_FACTORS
!!----       MAG_STRUCTURE_FACTORS
!!----       MODIFY_MSF
!!--++       SET_FIXED_TABLES            [Private]
!!--++       SUM_MAB                     [Private]
!!----       WRITE_MAG_STRUCTURE_FACTORS
!!----
!!
 Module CFML_kvec_Structure_Factors
    !---- Use Modules ----!
    Use CFML_GlobalDeps,                  only: cp, sp, dp, tpi, Err_CFML, Clear_Error, CFML_debug
    Use CFML_Maths,                       only: sort
    Use CFML_Strings,                     only: L_Case,U_Case
    Use CFML_Scattering_Tables,           only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form, &
                                                Magnetic_Form
    Use CFML_Metrics,                     only: Cell_G_Type
    Use CFML_gSpaceGroups,                only: SPG_Type, Set_SpaceGroup
    Use CFML_kvec_Symmetry,               only: ApplyMSO, MagSymm_k_type, Write_Magnetic_Structure, &
                                                Magnetic_Domain_type
    Use CFML_Reflections,                 only: Hkl_Gen_Sxtal, Get_MaxNumRef,&
                                                Refl_Type, RefList_Type, h_s, Initialize_RefList
    Use CFML_Atoms,                       only: Matom_list_type, Allocate_mAtom_list
    Use CFML_Propagation_Vectors,         only: K_Equiv_Minus_K
    Use CFML_Rational

    !---- Variables ----!
    implicit none

    private

    !---- List of public functions ----!

    !---- List of public subroutines ----!
    public :: Calc_Mag_Interaction_Vector, Gen_satellites, Init_Mag_Structure_Factors, &
              Mag_Structure_Factors, Modify_MSF, Write_Mag_Structure_Factors,          &
              Calc_Magnetic_StrF_MiV, Calc_Magnetic_StrF_MiV_dom,       &
              Calc_Magnetic_Strf_Tensor

    !---- List of private functions ----!
    private :: mFj

    !---- List of private subroutines ----!
    private :: Calc_Table_MAB, Create_Table_mFR,   &
               Create_Table_HR_HT, Set_Fixed_Tables, Calc_Table_TH, Sum_MAB

    !---- Definitions ----!

    !!--++
    !!--++ TYPE :: HR_TYPE
    !!--..
    !!--++    Type, Private :: HR_Type
    !!--++       real(kind=cp), dimension(3) :: H
    !!--++    End Type HR_Type
    !!--++
    !!--++    (Private)
    !!--++    Define the scatering vector vector  H+k
    !!--++
    !!--++ Update: April - 2005
    !!
    Type, Private :: HR_Type
       real(kind=cp), dimension(3) :: H
    End Type HR_Type

    !!----
    !!---- TYPE :: MAGH_TYPE
    !!--..
    !!----    Type, Public  :: MagH_Type
    !!----       logical                         :: keqv_minus  !True if k equivalent to -k
    !!----       integer                         :: mult        !Multiplicity of the reflection (useful for powder calculations)
    !!----       integer                         :: num_k       !number of the propagation vector vk
    !!----       real(kind=cp)                   :: signp       !+1 for -vk   and -1 for +vk
    !!----       real(kind=cp)                   :: s           !sinTheta/Lambda
    !!----       real(kind=cp)                   :: sqMiV       !Square of the Magnetic Interaction vector
    !!----       real(kind=cp),    dimension(3)  :: H           ! H +/- k
    !!----       complex(kind=cp), dimension(3)  :: MsF         !magnetic structure factor
    !!----       complex(kind=cp), dimension(3,3):: TMsF        !tensorial magnetic structure factor
    !!----       complex(kind=cp), dimension(3)  :: MiV         !magnetic interaction vector
    !!----       complex(kind=cp), dimension(3)  :: MiVC        !magnetic interaction vector in Cartesian components
    !!----    End Type  MagH_Type
    !!----
    !!----    Define the scatering vector vector  H+k and the sign -1 for H+k and +1 for H-k.
    !!----    Includes the magnetic interaction vector MiV = Mper = M
    !!----
    !!---- Updated: April-2005, June - 2012, June -2014
    !!
    Type, Public  :: MagH_Type
       logical                          :: keqv_minus  !True if k equivalent to -k
       integer                          :: mult        !Multiplicity of the reflection (useful for powder calculations)
       integer                          :: num_k       !number of the propagation vector vk
       real(kind=cp)                    :: signp       !+1 for -vk   and -1 for +vk
       real(kind=cp)                    :: s           !sinTheta/Lambda
       real(kind=cp)                    :: sqMiV       !Square of the Magnetic Interaction vector
       real(kind=cp),    dimension(3)   :: H           ! H +/- k
       complex(kind=cp), dimension(3)   :: MsF         !Magnetic structure factor w.r.t. unitary Crystal Frame
       complex(kind=cp), dimension(3,3) :: TMsF        !tensorial magnetic structure factor
       complex(kind=cp), dimension(3)   :: MiV         !Magnetic interaction vector w.r.t. unitary Crystal Frame
       complex(kind=cp), dimension(3)   :: MiVC        !Magnetic interaction vector in Cartesian components w.r.t. Crystal Frame
    End Type  MagH_Type


    !!----
    !!----  TYPE :: MAGH_LIST_TYPE
    !!--..
    !!----     Type, Public  :: MagH_List_Type
    !!----        integer                                   :: Nref
    !!----        Type(MagH_Type),allocatable, dimension(:) :: Mh
    !!----     End Type MagH_List_Type
    !!----
    !!----     Define a list of magnetic reflections containing the
    !!----     scatering vector, the magnetic structure factor and
    !!----     the magnetic interaction vector.
    !!----
    !!---- Update: April - 2005
    !!
    Type, Public  :: MagH_List_Type
       integer                                   :: Nref
       Type(MagH_Type),allocatable, dimension(:) :: Mh
    End Type MagH_List_Type

    !!----
    !!----  TYPE :: MAGHD_TYPE
    !!--..
    !!----     Type, Public  :: MagHD_Type
    !!----        logical                            :: keqv_minus  !True if k equivalent to -k
    !!----        integer                            :: num_k       !number of the propagation vector vk
    !!----        real(kind=cp)                      :: signp       !+1 for -vk   and -1 for +vk
    !!----        real(kind=cp)                      :: s           !sinTheta/Lambda
    !!----        real(kind=cp)                      :: sqAMiV      !Square of the Average Magnetic Interaction vector
    !!----        real(kind=cp)                      :: sqMiV       !Average of the Square of Magnetic Interaction vectors
    !!----        real(kind=cp),   dimension(3)      :: H           ! H +/- k
    !!----        complex(kind=cp),dimension(3,2,24) :: MsF         !Magnetic structure factors of each domain (second dimension for chirality domains)
    !!----        complex(kind=cp),dimension(3,2,24) :: MiV         !Magnetic interaction vector of each domain
    !!----        complex(kind=cp),dimension(3,2,24) :: MiVC        !Magnetic interaction vector of each domain w.r.t. to Cartesian Crystal Frame
    !!----        complex(kind=cp),dimension(3)      :: AMiV        !Average Magnetic interaction vector = 1/nd Sum{ pop(i) Miv(:,i)} in Cartesian Frame
    !!----     End Type  MagHD_Type
    !!----
    !!----    Define the scatering vector vector  H+k and the sign -1 for H+k and +1 for H-k.
    !!----    Includes the average magnetic interaction vector AMiV(:) = 1/nd Sum[i]{ pop(i) MiVC(:,i)}
    !!----    This type should be used whenever magnetic domains are present (single crystal work)
    !!----
    !!---- Updated: November - 2006, June 2012 (JRC)
    !!
    Type, Public  :: MagHD_Type
       logical                            :: keqv_minus
       integer                            :: num_k     !number of the propagation vector vk
       real(kind=cp)                      :: signp     !+1 for -vk   and -1 for +vk
       real(kind=cp)                      :: s         !sinTheta/Lambda
       real(kind=cp)                      :: sqAMiV    !Square of the Average Magnetic Interaction vector
       real(kind=cp)                      :: sqMiV     !Average of the Square of Magnetic Interaction vectors
       real(kind=cp),   dimension(3)      :: H         ! H +/- k
       complex(kind=cp),dimension(3,2,24) :: MsF       !Magnetic structure factors of each domain (second dimension for chirality domains)
       complex(kind=cp),dimension(3,2,24) :: MiV       !Magnetic interaction vector of each domain
       complex(kind=cp),dimension(3,2,24) :: MiVC      !Magnetic interaction vector of each domain in Cartesian Crystal space
       complex(kind=cp),dimension(3)      :: AMiV      !Average Magnetic interaction vector = 1/nd Sum{ pop(i) Miv(:,i)}
    End Type  MagHD_Type

    !!----
    !!----  MAGHD_LIST_TYPE
    !!----    Type, Public  :: MagHD_List_Type
    !!----       integer                                    :: Nref
    !!----       Type(MagHD_Type),allocatable, dimension(:) :: Mh
    !!----    End Type MagHD_List_Type
    !!----
    !!----    Define a list of magnetic reflections containing the
    !!----    scatering vector, the magnetic structure factor and
    !!----    the magnetic interaction vector for each of the domains.
    !!----
    !!---- Update: February - 2009 Oksana Zaharko
    !!
    Type, Public  :: MagHD_List_Type
       integer                                    :: Nref
       Type(MagHD_Type),allocatable, dimension(:) :: Mh
    End Type MagHD_List_Type

    !!--++
    !!--++ AJH
    !!--++     real(kind=cp), dimension(:,:,:), allocatable, private :: Ajh
    !!--++
    !!--++     (Private)
    !!--++     Array for Aj(h). The dimensions are Ajh(3,Natoms,Nref)
    !!--++     where the magnetic structure factor vector is
    !!--++           M(h)=p Sum_j [Fj(h){Aj(h)+i Bj(h)}]
    !!--++
    !!--++ Update: April - 2005
    !!
    real(kind=cp), dimension(:,:,:), allocatable, private :: AJH

    !!--++
    !!--++ BJH
    !!--++     real(kind=cp), dimension(:,:,:), allocatable, private :: Bjh
    !!--++
    !!--++     (Private)
    !!--++     Array for Bj(h). The dimensions are Bjh(3,Natoms,Nref)
    !!--++     where the magnetic structure factor vector is
    !!--++           M(h)=pSum_j[Fj(h){Aj(h)+i Bj(h)}]
    !!--++
    !!--++ Update: April - 2005
    !!
    real(kind=cp), dimension(:,:,:), allocatable, private :: BJH

    !!--++
    !!--++ HR
    !!--++     Type(HR_Type), dimension(:,:), allocatable, private :: Hr
    !!--++
    !!--++     (Private)
    !!--++     Array for HR Calculations. The dimension are HR(Nsymop,NRef)
    !!--++     Transformed H by the rotational part of the crystallographic
    !!--++     symmetry operators:  HR = matmul (H, Rs)
    !!--++
    !!--++ Update: April - 2005
    !!
    type(HR_Type), dimension(:,:), allocatable, private :: HR

    !!--++
    !!--++ HT
    !!--++     real(kind=cp), dimension(:,:), allocatable, private :: Ht
    !!--++
    !!--++     (Private)
    !!--++     Array for HT Calculations. The dimension are HT(Nsymop,Nref)
    !!--++     Scalar products of H.t, real sccatering vector by the translational
    !!--++     parts of the crystallographic symmetry operators: HT= dot_product(H,Ts)
    !!--++
    !!--++ Update: april - 2005
    !!
    real(kind=cp), dimension(:,:), allocatable, private :: HT

    !!--++
    !!--++ MFI
    !!--++     real(kind=cp), dimension(:), allocatable, private :: AFPP
    !!--++
    !!--++     (Private)
    !!--++     Array for imaginary part of magnetic form factor.
    !!--++     The dimension is: mFI(Natoms,NRef)
    !!--++
    !!--++ Update: April - 2005
    !!
    real(kind=cp), dimension(:,:), allocatable, private :: mFI

    !!--++
    !!--++ MFR
    !!--++     real(kind=cp), dimension(:,:), allocatable, private :: mFR
    !!--++
    !!--++     (Private)
    !!--++     Array for Magnetic form Factors. The dimensions are mFR(Natoms,NRef)
    !!--++
    !!--++ Update: April - 2005
    !!
    real(kind=cp), dimension(:,:), allocatable, private :: mFR

    !!--++
    !!--++ MSF_INITIALIZED
    !!--++    logical, private :: MSF_Initialized
    !!--++
    !!--++    (Private)
    !!--++    Logical Variable indicating if the module has been initialized.
    !!--++
    !!--++ Update: april - 2005
    !!
    logical, private :: MSF_Initialized=.false.

    !!----
    !!---- PN
    !!----    real(kind=dp), parameter, public :: pn=0.2695420113693928312
    !!----
    !!----    pn=Constant=  1/2 * gamma (muN) * r0 (units of 10^-12 cm)
    !!----    gamma: magnetic moment of neutrons in nuclear magnetons = 1.91304272(45) muN
    !!----    r0   : Classical radius of the electron = 0.28179403267(27) × 10-12 cm
    !!----
    !!---- Update: May - 2015
    !!
    real(kind=dp), parameter, public :: pn=0.2695420113693928312

    !!--++
    !!--++ TH
    !!--++    real(kind=cp), dimension(:,:), allocatable, private :: Th
    !!--++
    !!--++    (Private)
    !!--++    Array for TH Calculations. The dimension are TH(Natoms,Nref)
    !!--++
    !!--++ Update: April - 2005
    !!
    real(kind=cp), dimension(:,:), allocatable, private :: TH

  Interface

    !-------------------!
    !---- Functions ----!
    !-------------------!

    Module Function Mfj(S,Coeff) Result(Res)
       !---- Arguments ----!
       real(kind=cp),             intent(in) :: s  !sin Theta/Lambda
       real(kind=cp),dimension(7),intent(in) :: coeff
       real(kind=cp)                         :: res
    End Function Mfj

    !---------------------!
    !---- Subroutines ----!
    !---------------------!

    Module Subroutine Calc_Mag_Interaction_Vector(Reflex,Cell)
       !---- Argument ----!
       type(MagH_List_Type),intent(in out) :: Reflex
       type(Cell_G_Type),   intent(in)     :: Cell
    End Subroutine Calc_Mag_Interaction_Vector

    Module Subroutine Calc_Magnetic_Strf_Miv(Cell,Mgp,Atm,Mh)
       !---- Arguments ----!
       type(Cell_G_Type),    intent(in)     :: Cell
       type(MagSymm_k_Type), intent(in)     :: MGp
       type(Matom_list_type),intent(in)     :: Atm
       type(MagH_Type),      intent(in out) :: Mh
    End Subroutine Calc_Magnetic_StrF_MiV

    Module Subroutine Calc_Magnetic_Strf_Miv_Dom(Cell,Mgp,Atm,Mag_Dom,Mh)
       !---- Arguments ----!
       type(Cell_G_Type),         intent(in)      :: Cell
       type(MagSymm_k_Type),      intent(in)      :: MGp
       type(Matom_list_type),     intent(in)      :: Atm
       type(Magnetic_Domain_type),intent(in)      :: Mag_Dom
       type(MagHD_Type),          intent(in out)  :: Mh
    End Subroutine Calc_Magnetic_Strf_Miv_Dom

    Module Subroutine Calc_Magnetic_Strf_Tensor(SpG,Atm,Mh)
       !---- Arguments ----!
       type(SPG_Type),           intent(in)     :: SpG
       type(Matom_list_type),    intent(in)     :: Atm
       type(MagH_Type),          intent(in out) :: Mh
    End Subroutine Calc_Magnetic_StrF_Tensor

    Module Subroutine Calc_Table_Mab(Cell,Mlist,Atm,Mgp)
       !---- Arguments ----!
       type(Cell_G_Type),      intent(in) :: Cell
       type(MagH_List_Type),   intent(in) :: MList
       type(Matom_list_type),  intent(in) :: Atm
       type(MagSymm_k_type),   intent(in) :: MGp
    End Subroutine Calc_Table_Mab

    Module Subroutine Calc_Table_Th(Reflex,Atm)
       !---- Arguments ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(Matom_list_type),  intent(in) :: Atm
    End Subroutine Calc_Table_Th

    Module Subroutine Create_Table_Hr_Ht(Reflex,Grp)
       !---- Arguments ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(MagSymm_k_type),   intent(in) :: Grp
    End Subroutine Create_Table_HR_HT

    Module Subroutine Create_Table_mFR(Reflex,Atm,lun)
       !---- Arguments ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(Matom_list_type),  intent(in) :: Atm
       integer, optional,      intent(in) :: lun
    End Subroutine Create_Table_mFR


    Module Subroutine Gen_Satellites(Cell,Grp,Smax,H,Ord,Powder,hkl)
       !---- Arguments ----!
       type(Cell_G_Type),                intent(in)     :: cell
       type(MagSymm_k_Type),             intent(in)     :: Grp
       real(kind=cp),                    intent(in)     :: smax
       type(MagH_List_Type),             intent(in out) :: H
       logical, optional,                intent(in)     :: ord
       logical, optional,                intent(in)     :: powder
       class (RefList_Type), optional,   intent(in)     :: hkl
    End Subroutine Gen_Satellites


    Module Subroutine Init_Mag_Structure_Factors(Reflex,Atm,Grp,lun)
       !---Arguments ---!
       type(MagH_List_Type),           intent(in) :: Reflex
       type(Matom_list_type),          intent(in) :: Atm
       type(MagSymm_k_Type),           intent(in) :: Grp
       integer,              optional, intent(in) :: lun
    End Subroutine Init_Mag_Structure_Factors

    Module Subroutine Mag_Structure_Factors(Cell,Atm,Grp,Reflex)
       !---- Arguments ----!
       type(Cell_G_Type),  intent(in)     :: Cell
       type(Matom_list_type),    intent(in)     :: Atm
       type(MagSymm_k_Type),     intent(in)     :: Grp
       type(MagH_List_Type),     intent(in out) :: Reflex
    End Subroutine Mag_Structure_Factors

    Module Subroutine Modify_MSF(Reflex,Atm,Grp,List,Nlist)
       !---- Arguments ----!
       type(MagH_List_Type),         intent(in out) :: Reflex
       type(Matom_list_type),        intent(in)     :: Atm
       type(MagSymm_k_Type),         intent(in)     :: Grp
       integer,dimension(:),         intent(in)     :: List
       integer,                      intent(in)     :: NList
    End Subroutine Modify_MSF

    Module Subroutine Set_Fixed_Tables(Reflex,Atm,Grp,lun)
       !---- Arguments ----!
       type(MagH_List_Type),         intent(in) :: Reflex
       type(Matom_list_type),        intent(in) :: Atm
       type(MagSymm_k_Type),         intent(in) :: Grp
       integer, optional,            intent(in) :: lun
    End Subroutine Set_Fixed_Tables

    Module Subroutine Sum_MAB(Reflex,Natm,icent)
       !---- Arguments ----!
       type(MagH_List_Type), intent(in out)  :: Reflex
       integer,              intent(in)      :: Natm
       integer,              intent(in)      :: icent
    End Subroutine Sum_MAB

    Module Subroutine Write_Mag_Structure_Factors(Lun,Reflex,Grp)
       !---- Arguments ----!
       integer,               intent(in) :: lun
       type(MagH_List_Type),  intent(in) :: Reflex
       type(MagSymm_k_Type),  intent(in) :: Grp
    End Subroutine Write_Mag_Structure_Factors

  End Interface

 End Module CFML_kvec_Structure_Factors
