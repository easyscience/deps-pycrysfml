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
!!----       ATOMS_FROM_JSON
!!----       CELL_FROM_JSON
!!----       COMPUTE_POWDER_PATTERN
!!----       EXPERIMENTAL_CONDITIONS_FROM_JSON
!!----       POWDER_PATTERN
!!----       POWDER_PATTERN_FROM_JSON
!!----       READ_JSON
!!----       SPG_FROM_JSON
!!----       TCH
!!----       XTAL_STRUCTURE_FROM_JSON
!!----
!!

module CFML_Utilities
    !---- Use Modules ----!
    use cfml_atoms, only: allocate_atom_list,atlist_type
    use cfml_diffpatt, only: diffpat_e_type,powpatt_cw_conditions_type,powpatt_tof_conditions_type
    use cfml_diffraction, only: pat,ph,rl,lorentz_abs_cw,n_patterns,n_phases,powder_lorentz_integint_cw
    use cfml_globaldeps, only: clear_error,cp,dp,err_cfml,pi,set_error,to_deg,to_rad
    use cfml_gspacegroups, only: get_multip_pos,spg_type,set_spacegroup,superspacegroup_type,write_spacegroup_info
    use cfml_ioform, only: blockinfo_type,get_blocks_filetype,get_cfl_block_info,phase_type,read_cfl_pattern,&
        read_xtal_cfl,read_xtal_structure
    use cfml_maths, only: locate
    use cfml_metrics, only: cell_g_type,set_crystal_cell
    use cfml_profiles, only: calc_pseudo_voigt,get_fwhm_eta,pseudovoigt,tof_Jorgensen_VonDreele,prof_val,init_prof_val
    use cfml_reflections, only: gener_reflections,get_maxnumref,h_uni,initialize_reflist,reflist_type,refp_type,srefl_type
    use cfml_strings, only: file_type,get_num,l_case,reading_file
    use cfml_structure_factors, only: structure_factors,init_structure_factors,sf_clear_init_symop
    use forpy_mod

    implicit none

    private

    public :: allocate_profile_contributions_for_phases,compute_patterns,cw_powder_pattern,tof_powder_pattern
    public :: check_phases_allocation,check_patterns_allocation,read_cfl
    public :: generate_reflections_for_patterns

    real(kind=dp), parameter :: INV_8LN2=0.18033688011112042591999058512524_dp

    type, public :: profile_contrib
        integer                                  :: n_pts
        real(kind=cp), dimension(:), allocatable :: yp    ! (n_pts)
    end type profile_contrib

    type, public :: profile_contrib_phase
        integer                                          :: n_patts
        type(profile_contrib), dimension(:), allocatable :: prc     ! (n_patts)
    end type profile_contrib_phase

    interface

        ! Utilities_CFL
        module subroutine check_patterns_allocation()
            !> Checks if the array of patterns is allocated. If not, sets an error in err_cfml.
        end subroutine check_patterns_allocation

        module subroutine check_phases_allocation()
            !> Checks if the array of phases is allocated. If not, sets an error in err_cfml.
        end subroutine check_phases_allocation

        module subroutine read_cfl(cfl)
            !> Computes patterns from instructions given in a cfl file
            type(file_type), intent(in) :: cfl !> cfl file type
        end subroutine read_cfl

        ! Utilities Patterns
        module subroutine allocate_profile_contributions_for_phases(prph)
            !> Sets the profile contributions for each phase
            type(profile_contrib_phase), dimension(:), allocatable, intent(inout) :: prph !> profile contributions for each phase
        end subroutine allocate_profile_contributions_for_phases

        module subroutine compute_patterns()
            !> Computes patterns stored in Pat
        end subroutine compute_patterns

        module subroutine cw_powder_pattern(cell,spg,a,ppc,xc,yc,tth)
            !> Computes a powder pattern from cell, space group,
            !> atom list and experimental conditions
            class(cell_g_type),                       intent(in)    :: cell !> unit cell
            class(spg_type),                          intent(in)    :: spg  !> space group
            type(atlist_type),                        intent(in)    :: a    !> list of atoms
            type(powpatt_cw_conditions_type),         intent(inout) :: ppc  !> experimental conditions
            real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
            real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
            real(kind=cp), dimension(:), optional,    intent(in)    :: tth  !> two theta axis provided by the user
        end subroutine cw_powder_pattern

        module subroutine tof_powder_pattern(cell,spg,a,ppc,xc,yc,tof)
            !> Computes a powder pattern from cell, space group,
            !> atom list and experimental conditions
            class(cell_g_type),                       intent(in)    :: cell !> unit cell
            class(spg_type),                          intent(in)    :: spg  !> space group
            type(atlist_type),                        intent(in)    :: a    !> list of atoms
            type(powpatt_tof_conditions_type),        intent(inout) :: ppc  !> experimental conditions
            real(kind=cp), dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
            real(kind=cp), dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity
            real(kind=cp), dimension(:), optional,    intent(in)    :: tof  !> tof axis provided by the user
        end subroutine tof_powder_pattern

        ! Utilities Reflections
        module subroutine generate_reflections_for_patterns()
            !> Generates the list of reflections for each phase
        end subroutine generate_reflections_for_patterns

    end interface

end module