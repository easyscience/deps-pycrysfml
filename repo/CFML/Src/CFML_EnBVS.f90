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
!!---- MODULE: CFML_BVS_Energy_Calc
!!----   INFO: Subroutines related to calculations of energy or
!!----         configuration properties depending on the crystal structure: BVS, Energy,....
!!----
!!---- HISTORY
!!----    Updated: 16/12/2014, 20/02/2018
!!----
!!----
!!---- DEPENDENCIES
!!----
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!----       ALLOCATE_ATOMS_CONF_LIST
!!--++       BOND_VALENCE                  [Private]
!!----       CALC_BVS
!!----       CALC_MAP_BVEL
!!----       CALC_MAP_BVS
!!----       CALC_SITE_ENE
!!--++       COMPLETE_TABLE                [Private]
!!----       COST_BVS
!!----       COST_BVS_COULOMBREP
!!----       DEALLOCATE_ATOMS_CONF_LIST
!!----       EWALD
!!----       SET_TABLE_D0_B
!!----       SPECIES_ON_LIST
!!----       SET_FORMAL_CHARGES
!!----
!!
 Module CFML_EnBVS
    !---- Use Files ----!
    Use CFML_GlobalDeps,        only: Sp,Cp,dp,pi,tpi,clear_error,Err_CFML, CFML_debug
    Use CFML_Rational
    Use CFML_Maths,             only: cross_product, Sort, Set_Eps_Math, epss
    use CFML_Strings,           only: Sort_Strings,Get_words, U_Case,pack_string
    Use CFML_Scattering_Tables, only: Get_Ionic_Radius, Get_Chem_Symb, Get_Z_Symb, Get_Covalent_Radius, Set_Chem_Info
    use CFML_Metrics,           only: Cell_G_Type
    use CFML_gSpaceGroups,      only: SPG_Type, Get_Orbit, Point_Orbit
    use CFML_Atoms,             only: Atm_Type, Init_Atom_type, Write_Atom_List, AtList_Type, Allocate_Atom_List, &
                                      Extend_Atom_List, Atm_Cell_Type, Allocate_Atoms_Cell
    use CFML_Geom,              only: Coord_Info, Distance, calc_dist_angle_sigma, calc_dist_angle
    use CFML_Export_VTK,        only: write_grid_VESTA
    use CFML_BVS_Tables

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public :: Allocate_Atoms_Conf_List, Calc_BVS, Deallocate_Atoms_Conf_List,           &
              Set_Table_d0_b, Species_on_List, Set_Table_BVEL_Params,    &
              Calc_Map_BVS, Cost_BVS, Cost_BVS_CoulombRep, Calc_Map_BVEL, Ewald,        &
              Set_Formal_Charges, Calc_Site_Ene

    !---- List of public private ----!
    private :: Bond_Valence, Complete_Table, Complete_Table_BVEL, get_soft_covalent_radius

    !---- Definitions ----!

    !!----
    !!---- TYPE :: ATOMS_CONF_LIST_TYPE
    !!--..
    !!---- Type, public :: Atoms_Conf_List_Type
    !!----    integer                                     :: natoms    ! Total number of atoms in the list
    !!----    integer                                     :: N_Spec    ! Number of different species in the list
    !!----    integer                                     :: N_Anions  ! Number of anions in the list
    !!----    integer                                     :: N_Cations ! Number of cations in the list
    !!----    real(kind=cp)                               :: Tol       ! Tolerance(%) for sum of radii conditions
    !!----    real(kind=cp)                               :: totatoms  ! Total number of atoms in the unit cell
    !!----    character(len=4), dimension(:), allocatable :: Species   ! Symbol + valence
    !!----    real(kind=cp),    dimension(:), allocatable :: Radius    !ionic/atomic radius of species
    !!----    type(Atm_Type),  dimension(:),  allocatable :: atom
    !!---- End Type Atoms_Conf_List_Type
    !!----
    !!---- Update: March - 2005
    !!
    Type, public :: Atoms_Conf_List_Type
       integer                                     :: natoms    ! Total number of atoms in the list
       integer                                     :: N_Spec    ! Number of different species in the list
       integer                                     :: N_Anions  ! Number of anions in the list
       integer                                     :: N_Cations ! Number of cations in the list
       real(kind=cp)                               :: Tol       ! Tolerance(%) for sum of radii conditions
       real(kind=cp)                               :: totatoms  ! Total number of atoms in the unit cell
       character(len=4), dimension(:), allocatable :: Species
       real(kind=cp),    dimension(:), allocatable :: Radius    !ionic/atomic radius of species
       type(Atm_Type),   dimension(:), allocatable :: Atom
    End type Atoms_Conf_List_Type

    Interface

      Module Function get_soft_covalent_radius(nam) result(radius)
        character(len=*), intent(in) :: nam
        real(kind=cp)                :: radius
      End Function get_soft_covalent_radius

      Module Subroutine Allocate_Atoms_Conf_List(n,A)
         !---- Arguments ----!
         integer,                     intent(in)       :: N  !N. atoms in asymmetric unit
         type (Atoms_Conf_List_Type), intent(in out)   :: A  !Objet to be allocated
      End Subroutine Allocate_Atoms_Conf_List

      Module Subroutine Bond_Valence(D0,B0,D,Sd,Bv,Sbv)
         !---- Arguments ----!
         real(kind=cp),  intent(in)  :: d0,b0  !Bond-valence parameters
         real(kind=cp),  intent(in)  :: d,sd   !Bond distance and sigma
         real(kind=cp),  intent(out) :: bv,sbv !Bond-valence and sigma
      End Subroutine Bond_Valence

      Module Subroutine Calc_BVS(A, Ipr, N_Bvsm, Bvs_M, Filecod, info_string)
         !---- Arguments ----!
         type (Atoms_Conf_List_type),            intent(in)  :: A      !  In -> Object of Atoms_Conf_List_type
         integer,                      optional, intent(in)  :: Ipr
         integer,                      optional, intent(in)  :: N_bvsm
         character(len=*),dimension(:),optional, intent(in)  :: bvs_m
         character(len=*),             optional, intent(in)  :: Filecod
         character(len=*),             optional, intent(out) :: info_string
      End Subroutine Calc_BVS

      Module Subroutine Calc_Map_BVEL(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol,emin,npix,outp,bvel_map)
         !---- Arguments ----!
         type (Atoms_Conf_List_type), intent(in) :: A
         class (SpG_Type),            intent(in) :: SpG
         Type (Cell_G_Type),          intent(in) :: Cell
         character(len=*),            intent(in) :: Filecod
         integer,                     intent(in) :: ndimx
         integer,                     intent(in) :: ndimy
         integer,                     intent(in) :: ndimz
         character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
         real(kind=cp),               intent(in) :: drmax
         real(kind=cp), optional,     intent(in) :: delta
         real(kind=cp), optional,     intent(out):: vol
         real(kind=cp), optional,     intent(out):: emin
         integer,       optional,     intent(out):: npix
         logical,       optional,     intent(in) :: outp
         real(kind=cp), optional,  dimension(:,:,:), allocatable, intent(out) :: bvel_map
      End Subroutine Calc_Map_BVEL

      Module Subroutine Calc_Map_BVS(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol)
         !---- Arguments ----!
         type (Atoms_Conf_List_type), intent(in) :: A
         class (SpG_Type),            intent(in) :: SpG
         Type (Cell_G_Type),          intent(in) :: Cell
         character(len=*),            intent(in) :: Filecod
         integer,                     intent(in) :: ndimx
         integer,                     intent(in) :: ndimy
         integer,                     intent(in) :: ndimz
         character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
         real(kind=cp),               intent(in) :: drmax
         real(kind=cp), optional,     intent(in) :: delta
         real(kind=cp), optional,     intent(out):: vol
      End Subroutine Calc_Map_BVS

      Module Subroutine Complete_Table(A,N_bvsm,bvs_m)
         type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
         Integer,                       intent(in) :: N_bvsm
         Character(len=*),dimension(:), intent(in) :: bvs_m
      End Subroutine Complete_Table

      Module Subroutine Complete_Table_BVEL(A,N_bvel,bvel)
        type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
        Integer,                       intent(in) :: N_bvel
        Character(len=*),dimension(:), intent(in) :: bvel
      End Subroutine Complete_Table_BVEL

      Module Subroutine Cost_BVS(A, GII, ERep,gic)
         !---- Arguments ----!
         type (Atoms_Conf_List_type),  intent(in out)  :: A    !  In -> Object of Atoms_Conf_List_type
         real(kind=cp),                intent(out)     :: GII  !  GII_a
         real(kind=cp),      optional, intent(out)     :: ERep !  Repulsion term due to shoft spheres
         character(len=*),   optional, intent(in)      :: gic  !  If present GII_c is put in GII
      End Subroutine Cost_BVS

      Module Subroutine Cost_BVS_CoulombRep(A, GII, ERep)
         !---- Arguments ----!
         type (Atoms_Conf_List_type),  intent(in out):: A      !  In -> Object of Atoms_Conf_List_type
         real(kind=cp),                intent(out)   :: GII    !  GII_a
         real(kind=cp),                intent(out)   :: ERep   !  Pseudo Repulsion Coulomb "energy"
      End Subroutine Cost_BVS_CoulombRep

      Module Subroutine Deallocate_Atoms_Conf_List(A)
         !---- Arguments ----!
         type (Atoms_Conf_List_Type), intent(in out)   :: A  !Objet to be deallocated
      End Subroutine Deallocate_Atoms_Conf_List

      Module Subroutine Calc_Site_Ene(A,Spg,Cell,x,y,z,atname,drmax,emin)
        !---- Arguments ----!
        Type (Atoms_Conf_List_Type), Intent(in out):: A
        class (SpG_Type),            Intent(in)    :: SpG
        Type (Cell_G_Type),          Intent(in)    :: Cell
        real(kind=cp),               intent(in)    :: x
        real(kind=cp),               intent(in)    :: y
        real(kind=cp),               intent(in)    :: z
        character(len=*),            intent(in)    :: atname  !Species of the probe atom e.g. LI+1
        real(kind=cp),               intent(in)    :: drmax
        real(kind=cp),               intent(out)   :: emin
      End Subroutine Calc_Site_Ene

      Module Subroutine Ewald(Lattvec,Vol,Ac,e)
        Real(kind=cp), Dimension(3,3),  Intent(in)   :: Lattvec
        Real(kind=cp),                  Intent(in)   :: Vol
        Type (Atm_Cell_Type),           Intent(in)   :: Ac
        Real(kind=dp),                  Intent(out)  :: e
      End Subroutine Ewald

      Module Subroutine Set_Formal_Charges(SpGr,Cell,A,eps_val,iwrt)
        !---- Arguments ----!
        class (SpG_Type),      Intent(in)    :: SpGr
        Type (Cell_G_Type),    Intent(in)    :: Cell
        Type (AtList_Type),    Intent(in out):: A
        Real(kind=cp),Optional,Intent(in)    :: eps_val
        Integer,      Optional,Intent(in)    :: iwrt
      End Subroutine Set_Formal_Charges

      Module Subroutine Set_Table_BVEL_Params(A,N_bvel,bvel,soft,nread)
         type (Atoms_Conf_List_type),            intent(in)  :: A
         integer,                      optional, intent(in)  :: N_bvel
         character(len=*),dimension(:),optional, intent(in)  :: bvel
         logical,                      optional, intent(in)  :: soft
         integer,                      optional, intent(in)  :: nread
      End Subroutine Set_Table_BVEL_Params

      Module Subroutine Set_Table_d0_b(A,N_bvsm,bvs_m,soft)
         !---- Arguments ----!
         type (Atoms_Conf_List_type),            intent(in)  :: A
         integer,                      optional, intent(in)  :: N_bvsm
         character(len=*),dimension(:),optional, intent(in)  :: bvs_m
         logical,                      optional, intent(in)  :: soft
      End Subroutine Set_Table_d0_b


      Module Subroutine Species_on_List(A,MulG, tol, covalent,softbvs)
         !---- Arguments ----!
         type (Atoms_Conf_List_Type), intent(in out) :: A
         Integer, optional,           intent(in)     :: MulG
         real(kind=cp), optional,     intent(in)     :: tol
         logical,       optional,     intent(in)     :: covalent
         logical,       optional,     intent(in)     :: softbvs
      End Subroutine Species_on_List

   End Interface

 End Module CFML_EnBVS
