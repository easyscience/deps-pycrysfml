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
!!---- MODULE: CFML_Utilities
!!----   INFO: This module contains utilities built from CrysFML08 library in order to
!!----         perform tasks that imply more than a single call to a CrysFML08 procedure.
!!----
!!---- HISTORY
!!----    Update: 10/07/2024
!!----
!!---- DEPENDENCIES
!!----
!!---- VARIABLES
!!----    Types
!!----    JSON_TYPE
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!----       ATOMS_FROM_DICT
!!----       CELL_FROM_DICT
!!----       COMPUTE_POWDER_PATTERN
!!----       EXPERIMENTAL_CONDITIONS_FROM_DICT
!!----       POWDER_PATTERN
!!----       POWDER_PATTERN_FROM_DICT
!!----       READ_JSON
!!----       SPG_FROM_DICT
!!----       TCH
!!----       XTAL_STRUCTURE_FROM_DICT
!!----
!!

module CFML_Py_Utilities
    !---- Use Modules ----!
    use cfml_atoms, only: allocate_atom_list,atlist_type,get_moment_and_displacement_ssg,matom_list_type
    use cfml_bonds_tables, only: bond_length_table,remove_bonds_table,set_bonds_table
    use cfml_diffpatt, only: diffpat_e_type,powpatt_cw_conditions_type,powpatt_tof_conditions_type
    use cfml_geom, only: calc_dist_angle
    use cfml_globaldeps, only: clear_error,cp,dp,err_cfml,to_deg,to_rad
    use cfml_gspacegroups, only: get_inv_op,get_multip_pos,get_orbit,point_orbit,spg_type,set_spacegroup,superspacegroup_type,write_spacegroup_info
    use cfml_ioform, only: blockinfo_type,get_cfl_block_info,read_cfl_spg,read_xtal_structure
    use cfml_kvec_symmetry, only: applyso,calc_magnetic_moment_kvec,get_kv_orbit,get_symsymb,init_magsymm_k_type,lattice_trans,&
        magsymm_k_type,point_orbit_kv,readn_set_magnetic_kv_structure,sym_oper_type
    use cfml_laue, only: allocate_laue_ref_list,calc_visible_reflections_list,generate_laue_reflections,&
        get_stereographic_projection_from_zv,laue_instrument_type,laue_ref_list_type
    use cfml_maths, only: get_eps_math,lat_modulo,locate,set_eps_math,zbelong
    use cfml_metrics, only: cell_g_type,set_crystal_cell
    use cfml_profiles, only: calc_pseudo_voigt,get_fwhm_eta,lorcomp,pseudovoigt,tof_Jorgensen_VonDreele,prof_val,init_prof_val
    use cfml_rational
    use cfml_reflections, only: get_maxnumref,gener_reflections,h_uni,initialize_reflist,reflist_type,srefl_type,mrefl_type,refl_type
    use cfml_scattering_tables, only: chem_info,get_chem_symb,get_z_symb,remove_chem_info,set_chem_info
    use cfml_strings, only: file_list_type,file_type,l_case,reading_file,u_case
    use cfml_structure_factors, only: calc_mag_structure_factor,scattering_species_type,set_form_factors,strflist_type,structure_factors,init_structure_factors,sf_init_opmattr,sf_clear_init_symop,Strf_Type
    use cfml_sxtal_geom, only: cell_fr_ub
    use cfml_utilities, only: cw_powder_pattern,tof_powder_pattern
    use cfml_wraps_utils, only: ndarray_to_pointer,pointer_to_alloc_array
    use forpy_mod

    implicit none

    private

    public :: calculate_laue_image,calculate_laue_zone,set_instrument_esmeralda,set_spg_esmeralda,set_ub_esmeralda

    public :: compute_envelope,cw_powder_pattern_from_dict,magnetic_structure_factors_from_mcif,&
              read_crystal_structure,read_faults_structure,set_boundary,set_crystal_coordination,&
              set_mask_atoms,structure_factors_from_cif,tof_powder_pattern_from_dict,&
              update_global_phase

    real(kind=dp), parameter :: INV_8LN2=0.18033688011112042591999058512524_dp
    real,          parameter :: EPSILON = 0.001

    ! ---------------------------------------------------------------------------
    ! Esmeralda variables and types
    !
    logical,                      public :: is_instrument_es_configured = .false.
    logical,                      public :: is_spg_es_configured        = .false.
    logical,                      public :: is_ub_es_configured         = .false.
    real(kind=4), dimension(3,3), public :: ub_es
    type(cell_g_type),            public :: cell_es
    type(laue_instrument_type),   public :: instrument_es
    type(reflist_type),           public :: hkl_es
    class(spg_type), allocatable, public :: spg_es

    type, public :: esmeralda_laue_image_type
        integer                                   :: nref !> number of reflections in the image
        real(kind=4), dimension(:,:), allocatable :: hkl  !> Miller index
        real(kind=4), dimension(:,:), allocatable :: xz   !> x,z coordinates in pixels
        real(kind=4), dimension(:,:), allocatable :: xz_stereo !> stereographic projection
    end type esmeralda_laue_image_type
    !
    ! ---------------------------------------------------------------------------

    type, public :: coordination_asu_type
        !> Describes coordination for an atom in the asymmetric unit of a crystal
        type(coordination_orbit_type), dimension(:), allocatable :: orbit !> coordination for each atom in the orbit (mult)
    end type coordination_asu_type

    type, public :: coordination_crystal_type
        !> Describes coordination in a crystal cell
        integer                                                  :: nextra !> Number of atoms outside the unit cell needed for representing all bonds in the unit cell
        real(kind=cp),               dimension(:,:), allocatable :: dmin   !> minimum distance between species i,j of the asymmetric unit (nasu,nasu)
        real(kind=cp),               dimension(:,:), allocatable :: dmax   !> maximum distance between species i,j of the asymmetric unit (nasu,nasu)
        type(coordination_asu_type), dimension(:),   allocatable :: asu    !> coordination for every atom in the asymmetric unit          (nasu)
    end type coordination_crystal_type

    type, public :: coordination_orbit_type
        !> Describes coordination for one atom in the orbit of one atom of the asymmetric unit
        integer :: nbonds                                          !> number of bonds
        type(crystal_bond_type), dimension(:), allocatable :: bond
    end type coordination_orbit_type

    type, public :: crystal_bond_type
        integer               :: asu   !> asymmetric unit index
        integer               :: orbit !> orbit index
        integer               :: id    !> bond type
        integer, dimension(3) :: tr    !> translation vector
        real, dimension(3)    :: rc    !> bond in cartesian coordinates
        real                  :: d     !> bond distance
    end type crystal_bond_type

    type, public :: crystal_type
        character(len=:),   allocatable              :: file     !> file with the crystal data
        integer                                      :: maxmul   !> maximum multiplicity
        integer                                      :: nat_cell !> number of atoms / unit cell
        integer                                      :: nat_masu !> Number of atoms in the magnetic asymmetric unit (nat_asu would be asu%natoms)
        integer                                      :: nelem    !> number of elements
        logical                                      :: fourier  !> True if a description in terms of Fourier coefficients was given
        logical                                      :: nuclear  !> True if the nuclear structure has been given
        logical                                      :: super    !> True if spg is a superspace group
        class(cell_g_type), allocatable              :: cell     !> crystal cell
        class(spg_type),    allocatable              :: spg      !> space group object
        type(atlist_type)                            :: asu      !> asymmetric unit
        type(point_orbit), dimension(:), allocatable :: orbit    !> orbit for atoms in the asymmetric unit (nasu)
        type(magsymm_k_type)                         :: mgp      !> magnetic symmetry
        type(matom_list_type)                        :: am       !> magnetic atoms
        character(len=2), dimension(:), allocatable  :: element  !> chemical symbols of the elements (nelem)
        integer, dimension(:), allocatable           :: asu2elem !> maps each atom of the asymmetric unit to element
        integer, dimension(:), allocatable           :: magnetic !> specifies if the atom is magnetic (nat_masu)
        integer, dimension(:), allocatable           :: split    !> Number of nuclear sites / number of magnetic sites for each asymmetric unit atom (nasu)
        integer, dimension(:,:), allocatable         :: id       !> Maps each atom of the unit cell to its equivalent atom in the magnetic asymmetric unit (maxmul,nasu)
        integer, dimension(:,:,:), allocatable       :: magat    !> map magnetic atoms to crystal atoms (maxmul,nasu,2)
        real, dimension(:,:,:), allocatable          :: magtr    !> centring translations of magnetic atoms (maxmul,nasu,3)
    end type

    type, public :: envelope_type
        integer              :: np !> number of points
        real, dimension(3)   :: x0,t
        real, dimension(:,:), allocatable :: x  !> points (3,np)
        real, dimension(:,:), allocatable :: v  !> vector associated to points (3,np)
    end type

    type, public :: envelope_list_type
        integer                                        :: nenv !> number of envelopes
        type(envelope_type), dimension(:), allocatable :: env  !> envelopes
    end type

    type, public :: graphical_crystal_type
        !> Atoms, magnetic moments and bonds stored in a convenient way to be graphically displayed.
        integer :: natoms                                  ! number of atoms
        integer :: nbonds                                  ! number of bonds
        integer, dimension(:), allocatable   :: magnetic   !> 0: non-magnetic, 1 : magnetic (nasu)
        integer, dimension(:),   allocatable :: z          ! atomic number (natoms)
        integer, dimension(:),   allocatable :: ncoor      ! coordination of atom(i) considering only atoms in this type
        integer, dimension(:,:), allocatable :: id_atom    ! asymmetric unit index, orbit index, translation vector (5,natoms)
        integer, dimension(:),   allocatable :: bond_asu   ! asymmetric unit index used to assign color to the associated half bond (2*nbonds)
        integer, dimension(:),   allocatable :: id         ! id given by crystal_type
        integer, dimension(:),   allocatable :: bond_id    ! bond type (2*nbonds)
        integer, dimension(:),   allocatable :: bond_z     ! atomic number used to assign color to the associated half bond (2*nbonds)
        real, dimension(:,:),    allocatable :: pos_c      ! atomic positions in cartesian coordinates (3,natoms)
        real, dimension(:,:),    allocatable :: mom_c      ! magnetic moment in cartesian coordinates (3,natoms)
        real, dimension(:,:),    allocatable :: bond_orig  ! origin in cartesian coordinates of the half bond (from i to j) (3,2*nbonds)
        real, dimension(:,:),    allocatable :: bond_c     ! cartesian coordinates of the half bond (from i to j) (3,2*nbonds)
    end type graphical_crystal_type

    interface

        module function compute_envelope(g,at_id,axis,step) result(env_list)
            !> Compute the envelope for the specified atom along the specified direction
            type(graphical_crystal_type), intent(in) :: g        !> atoms, magnetic moments and bonds inside limits
            integer,                      intent(in) :: at_id    !> index of the atom for which the envelope is calculated
            real, dimension(3),           intent(in) :: axis     !> direction along the envelope is calculated
            real,                         intent(in) :: step     !> step used in the calculation of envelope points
            type(envelope_list_type)                 :: env_list !> list of computed envelopes
        end function compute_envelope

        module subroutine cw_powder_pattern_from_dict(json,xc,yc)
            !> Computes a powder pattern from information provided by a
            !> dictionary. This subroutine was introduced mainly for
            !> Python applications. Python can easily transform a json file
            !> into a dictionary.
            type(dict),                               intent(inout) :: json !> json file content
            real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
            real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
        end subroutine cw_powder_pattern_from_dict

        module function inside_limits(x,limits) result(inside)
            !> Check if an atom with coordinates x is inside limits
            real, dimension(3),   intent(in) :: x
            real, dimension(2,3), intent(in) :: limits
            logical                          :: inside
        end function inside_limits

        module function read_crystal_structure(filename,database_path) result(crystal)
            !> Build the object crystal from data given in a cfl, cif or mcif file
            character(len=*),           intent(in) :: filename      !> file containing the crystal data
            character(len=*), optional, intent(in) :: database_path !> full path of the database file
            type(crystal_type)                     :: crystal       !> crystal object
        end function read_crystal_structure

        module function read_faults_structure(filename) result(crystal)
            !> Build the object crystal from data given in faults file
            character(len=*),        intent(in) :: filename !> file containing the crystal data
            type(crystal_type)                  :: crystal  !> crystal object
        end function read_faults_structure

        module function magnetic_structure_factors_from_mcif(mcif_file,sfacsymb,experiment,sintlmin,sintlmax,unique,mag_only,mag_ext,friedel) Result(hkl)
            !> Computes nuclear and magnetic structure factors from a mcif file
            character(len=*),               intent(in) :: mcif_file   !> mcif file
            character(len=4), dimension(:), intent(in) :: sfacsymb    !> scattering factor symbols for atoms in the mcif
            character(len=*), optional,     intent(in) :: experiment  !> powder: "P" | single-crystal: "S"
            real(kind=cp),    optional,     intent(in) :: sintlmin    !> minimum value of sin(theta) / lambda
            real(kind=cp),    optional,     intent(in) :: sintlmax    !> maximum value of sin(theta) / lambda
            logical,          optional,     intent(in) :: unique      !> generate only unique reflections
            logical,          optional,     intent(in) :: mag_only    !> if True, generate only magnetic reflections
            logical,          optional,     intent(in) :: mag_ext     !> if True, consider magnetic extinctions
            logical,          optional,     intent(in) :: friedel     !> if True, apply Friedel law
            type(reflist_type)                         :: hkl         !> reflection list
        end function magnetic_structure_factors_from_mcif

        module function set_boundary(crystal,coor,limits,boundary_coordination) result(c)
            !> Returns atoms, magnetic moments and bonds inside limits
            type(crystal_type),              intent(inout) :: crystal               !> crystal object
            type(coordination_crystal_type), intent(in)    :: coor                  !> crystal coordination
            real, dimension(2,3),            intent(in)    :: limits                !> cell limits in crystal coordinates (2,3)
            logical,                         intent(in)    :: boundary_coordination !> if True, includes atoms bonded to boundary atoms
            type(graphical_crystal_type)                   :: c                     !> atoms, magnetic moments and bonds inside limits
        end function set_boundary

        module function set_crystal_coordination(crystal,dmin,dmax) result(coor)
            !> Computes the coordination of atoms in a crystal
            type(crystal_type),                      intent(inout) :: crystal  !> crystal object
            real(kind=cp), dimension(:,:), optional, intent(in)    :: dmin     !> minimum distance between species i,j of the asymmetric unit (nasu,nasu)
            real(kind=cp), dimension(:,:), optional, intent(in)    :: dmax     !> maximum distance between species i,j of the asymmetric unit (nasu,nasu)
            type(coordination_crystal_type)                        :: coor     !> crystal coordination
        end function set_crystal_coordination

        module function set_mask_atoms(id_atom,id_to_hide) result(mask_array)
            !> Set the mask array for CrystalView
            integer, dimension(:), intent(in)            :: id_atom     !> index referring to an atom in the asymmetric unit
            integer, dimension(:), intent(in)            :: id_to_hide  !> index referring to an atom in the asymmetric unit
            integer, dimension(:), allocatable           :: mask_array  !> mask array: 0: invisible, 1: visible
        end function set_mask_atoms

        module function structure_factors_from_cif(cif_file,radiation,lambda,sintlmin,sintlmax,unique,friedel) Result(hkl)
            !> Computes structure factors from a cif file
            character(len=*),           intent(in) :: cif_file  !> cif or mcif file
            character(len=*), optional, intent(in) :: radiation !> x-rays: "XRA" | neutrons: "NEU" | electrons: "ELE"
            real(kind=cp),    optional, intent(in) :: lambda    !> wavelength
            real(kind=cp),    optional, intent(in) :: sintlmin  !> minimum value of sin(theta) / lambda
            real(kind=cp),    optional, intent(in) :: sintlmax  !> maximum value of sin(theta) / lambda
            logical,          optional, intent(in) :: unique    !> generate only unique reflections
            logical,          optional, intent(in) :: friedel   !> if True, apply Friedel law
            type(reflist_type)                     :: hkl       !> reflection list
        end function structure_factors_from_cif

        module subroutine tof_powder_pattern_from_dict(json,xc,yc)
            !> Computes a powder pattern from information provided by a
            !> dictionary. This subroutine was introduced mainly for
            !> Python applications. Python can easily transform a json file
            !> into a dictionary.
            type(dict),                               intent(inout) :: json !> json file content
            real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
            real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
        end subroutine tof_powder_pattern_from_dict

        module function U_from_cr_orth_cell(cr_orth_cel) result(U)
            real(kind=cp), dimension(3,3), intent(in) :: cr_orth_cel
            real(kind=cp), dimension(3,3)             :: U
        end function U_from_cr_orth_cell

        module subroutine set_asu_orbits(crystal)
            !> Set orbits for atoms of the asymmetric unit
            type(crystal_type),              intent(inout) :: crystal       !> crystal object
        end subroutine set_asu_orbits

        module subroutine set_asu_orbits_from_magnetic(crystal)
            !> Set orbits for atoms of the asymmetric unit from the magnetic symmetry group
            type(crystal_type), intent(inout) :: crystal       !> crystal object
        end subroutine set_asu_orbits_from_magnetic

        module subroutine set_elements(crystal)
            !> Set elements in crystal
            type(crystal_type),              intent(inout) :: crystal       !> crystal object
        end subroutine set_elements

        module subroutine set_nuclear_from_magnetic(crystal)
        !> Build the nuclear part from the magnetic one when the nuclear
        !> was not specified in the crystal file
        type(crystal_type),                  intent(inout) :: crystal       !> crystal object
        end subroutine set_nuclear_from_magnetic

        module subroutine update_global_phase(global_phase,crystal,c)
            !> Update global phase of an incommensurate structure
            real,                            intent(in)    :: global_phase  !> global_phase
            type(crystal_type),              intent(in)    :: crystal       !> crystal object
            type(graphical_crystal_type),    intent(inout) :: c             !> atoms, magnetic moments and bonds inside limits
        end subroutine update_global_phase

    end interface

    interface ! Esmeralda procedures

        module function calculate_laue_image() result(laue_img)
            !> Calculate visible reflections
            type(esmeralda_laue_image_type) :: laue_img !> Laue img
        end function calculate_laue_image

        module function calculate_laue_zone(x1,z1,x2,z2) result(zone)
            !> Calculate the zone that contains two points on the detector
            real, intent(in) :: x1                    !> x coordinate of the first point  in pixels
            real, intent(in) :: z1                    !> z coordinate of the first point  in pixels
            real, intent(in) :: x2                    !> x coordinate of the second point in pixels
            real, intent(in) :: z2                    !> z coordinate of the second point in pixels
            real, dimension(:,:), allocatable :: zone !> x,z sampling of the zone
        end function calculate_laue_zone

        module subroutine set_instrument_esmeralda(name,dtype,d,h,v,np_h,np_v,xo,zo,ga_d,nu_d,l_min,l_max,&
        x_min,x_max,z_min,z_max,gan_min,gan_max,gap_min,gap_max,nu_min,nu_max)
            !> Set the Laue instrument for Esmeralda
            character(len=*), intent(in) :: name    !> Instrument name
            character(len=*), intent(in) :: dtype   !> Detector type: 'Rec' | 'Cyl'
            real(kind=4),     intent(in) :: d       !> Sample-detector distance (mm)
            real(kind=4),     intent(in) :: h       !> Horizontal detector size (mm)
            real(kind=4),     intent(in) :: v       !> Vertical   detector size (mm)
            integer,          intent(in) :: np_h    !> Number of horizontal pixels
            integer,          intent(in) :: np_v    !> Number of vertical   pixels
            real(kind=4),     intent(in) :: xo      !> Coordinate x in pixels for detector origin
            real(kind=4),     intent(in) :: zo      !> Coordinate z in pixels for detector origin
            real(kind=4),     intent(in) :: ga_d    !> Angle gamma in degrees for detector origin
            real(kind=4),     intent(in) :: nu_d    !> Angle nu    in degrees for detector origin
            real(kind=4),     intent(in) :: l_min   !> Lambda min
            real(kind=4),     intent(in) :: l_max   !> Lambda max
            real(kind=4),     intent(in) :: x_min   !> x min (mm)
            real(kind=4),     intent(in) :: x_max   !> x max (mm)
            real(kind=4),     intent(in) :: z_min   !> z min (mm)
            real(kind=4),     intent(in) :: z_max   !> z max (mm)
            real(kind=4),     intent(in) :: gan_min !> Negative gamma min (degrees)
            real(kind=4),     intent(in) :: gan_max !> Negative gamma max (degrees)
            real(kind=4),     intent(in) :: gap_min !> Positive gamma min (degrees)
            real(kind=4),     intent(in) :: gap_max !> Positive gamma max (degrees)
            real(kind=4),     intent(in) :: nu_min  !> Nu min (degrees)
            real(kind=4),     intent(in) :: nu_max  !> Nu max (degrees)
        end subroutine set_instrument_esmeralda

        module subroutine set_spg_esmeralda(spg_id)
            !> Set the space group for Esmeralda.
            character(len=*), intent(in) :: spg_id !> string specifying the space group to be built
        end subroutine set_spg_esmeralda

        module subroutine set_ub_esmeralda(ub)
            !> Set the UB matrix for Esmeralda
            real(kind=4), dimension(3,3), intent(in) :: ub
        end subroutine set_ub_esmeralda

    end interface

end module