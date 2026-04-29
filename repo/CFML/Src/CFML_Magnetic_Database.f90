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
!!---- MODULE: CFML_Groups
!!----         Space Groups and their algebra
!!----
!!--.. Rational matrix of special type dimension (3+d+1,3+d+1). The matrix of the
!!--.. symmetry operator is extended with a column containing the translation in
!!--.. the 3+d space plus a zero 3+d+1 row and +1 at position (3+d+1,3+d+1).
!!--..
!!--.. In order to limit the operators to the factor group w.r.t. traslations, a
!!--.. modulo-1 is applied in the multiplication of two operators.
!!----
!!
Module CFML_Magnetic_Database
    !---- Use Modules ----!
    Use CFML_GlobalDeps
    Use CFML_Rational

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public :: Read_Magnetic_Data, Read_Magnetic_Binary, Allocate_Magnetic_DBase, DeAllocate_Magnetic_DBase

    !---- Parameters ----!
    integer, public, parameter :: MAGCOUNT=1651      ! Magnetic Groups

    !---- Variables ----!
    logical, public :: Magnetic_DBase_allocated=.false.
    logical         :: mcif=.false.

    !> For the ith nonhexagonal point operator:
    Character(Len=8),  dimension(:), public, allocatable :: point_op_label   ! point_op_label(i): point operator symbol (from Litvin)
    Character(Len=10), dimension(:), public, allocatable :: point_op_xyz
    Integer,       dimension(:,:,:), public, allocatable :: point_op_matrix  ! point_op_matrix(i): point operator matrix

    !> For the ith hexagonal point operator:
    Character(Len=8),  dimension(:), public, allocatable :: point_op_hex_label  ! point_op_hex_label(i): point operator symbol (from Litvin)
    Character(Len=10), dimension(:), public, allocatable :: point_op_hex_xyz    ! point_op_hex_xyz(i): point operator in x,y,z notation
    Integer,       dimension(:,:,:), public, allocatable :: point_op_hex_matrix ! point_op_hex_matrix(i): point operator matrix

    !> For the ith magnetic space group
    Character(Len=12), dimension(  :), public, allocatable :: nlabel_bns           ! nlabel_bns(i): numerical label in BNS setting
    Integer,           dimension(:,:), public, allocatable :: nlabelparts_bns      ! nlabel_parts_bns(j,i): jth part of nlabel_bns
    Character(Len=14), dimension(  :), public, allocatable :: spacegroup_label_bns ! label_bns(i): group symbol
    Character(Len=12), dimension(  :), public, allocatable :: nlabel_og            ! nlabel_og(i): numerical label in OG setting
    Integer,           dimension(:,:), public, allocatable :: nlabelparts_og       ! nlabel_parts_og(j,i): jth part of nlabel_og
    Character(Len=14), dimension(  :), public, allocatable :: spacegroup_label_og  ! label_og(i): group symbol
    Character(Len=27), dimension(  :), public, allocatable :: spacegroup_label_unified  ! UNIFIED group symbol
    Integer,           dimension(  :), public, allocatable :: magtype              ! magtype(i): type of magnetic space group (1-4)

    !> BNS-OG transformation (if type-4)
    Integer,         dimension(:,:,:), public, allocatable :: bnsog_point_op     ! bnsog_point_op(j,k,i): 3x3 point operator part of transformation
    Integer,         dimension(:,  :), public, allocatable :: bnsog_origin       ! bnsog_origin(j,i): translation part of transformation
    Integer,         dimension(    :), public, allocatable :: bnsog_origin_denom ! bnsog_point_origin(i): common denominator
    Integer,         dimension(    :), public, allocatable :: ops_count          ! iops_count(i): number of point operators
    Integer,         dimension(    :), public, allocatable :: wyckoff_site_count ! wyckoff_count(i): number of wyckoff sites
    Integer,         dimension(: , :), public, allocatable :: wyckoff_pos_count  ! wyckoff_pos_count(j,i): number of positions in jth wyckoff site
    Integer,         dimension(: , :), public, allocatable :: wyckoff_mult       ! wyckoff_mult(j,i): multiplicity for jth wyckoff site
    Character(Len=1),dimension(: , :), public, allocatable :: wyckoff_label      ! wyckoff_label(j,i): symbol (a,b,c,...,z,alpha) for jth wyckoff site

    !> For BNS setting
    Integer, dimension(    :), public, allocatable :: lattice_bns_vectors_count  ! number of lattice vectors defining the lattice
    Integer, dimension(:,:,:), public, allocatable :: lattice_bns_vectors        ! (k,j,i): kth component of the jth lattice vector
    Integer, dimension(:,  :), public, allocatable :: lattice_bns_vectors_denom  !(j,i): common denominator

    !> For jth operator
    Integer, dimension(  :,:), public, allocatable :: ops_bns_point_op    ! ops_bns_point_op(j,i): point operator part
    Integer, dimension(:,:,:), public, allocatable :: ops_bns_trans       ! ops_bns_trans(k,j,i): kth component of translation part
    Integer, dimension(  :,:), public, allocatable :: ops_bns_trans_denom ! ops_bns_trans_denom(j,i): common denominator
    Integer, dimension(  :,:), public, allocatable :: ops_bns_timeinv     ! ops_bns_timeinv(j,i): 1=no time inversion, -1=time inversion

    !> For jth wyckoff site
    Integer, dimension(:,  :,:,:), public, allocatable :: wyckoff_bns_fract       ! wyckoff_bns_fract(k,j,i): kth component of fractional part of wyckoff position
    Integer, dimension(    :,:,:), public, allocatable :: wyckoff_bns_fract_denom ! wyckoff_bns_fract_denom(j,i): common denominator
    Integer, dimension(:,:,:,:,:), public, allocatable :: wyckoff_bns_xyz         ! wyckoff_bns_xyz(m,k,j,i): mth component to coeffcient of kth parameter (x,y,z)
    Integer, dimension(:,:,:,:,:), public, allocatable :: wyckoff_bns_mag  ! wyckoff_bns_mag(m,k,j,i): mth component to coeffcient of kth magnetic parameter (mx,my,mz)

    !> For OG setting (for type-4 groups)
    Integer, dimension(    :), public, allocatable :: lattice_og_vectors_count  ! lattice_og_vectors_count(i): number of lattice vectors defining the lattice
    Integer, dimension(:,:,:), public, allocatable :: lattice_og_vectors   ! lattice_og_vectors(k,j,i): kth component of the jth lattice vector
    Integer, dimension(:,  :), public, allocatable :: lattice_og_vectors_denom  ! lattice_og_vectors_denom(j,i): common denominator

    !> For jth operator
    Integer, dimension(  :,:), public, allocatable :: ops_og_point_op    ! ops_og_point_op(j,i): point operator part
    Integer, dimension(:,:,:), public, allocatable :: ops_og_trans       ! ops_og_trans(k,j,i): kth component of translation part
    Integer, dimension(  :,:), public, allocatable :: ops_og_trans_denom ! ops_og_trans_denom(j,i): common denominator
    Integer, dimension(  :,:), public, allocatable :: ops_og_timeinv     ! ops_og_timeinv(j,i): 1=no time inversion, -1=time inversion

    !> For jth wyckoff site
    Integer, dimension(:,  :,:,:), public, allocatable :: wyckoff_og_fract        ! wyckoff_og_fract(k,j,i): kth component of fractional part of wyckoff position
    Integer, dimension(    :,:,:), public, allocatable :: wyckoff_og_fract_denom  ! wyckoff_og_fract_denom(j,i): common denominator
    Integer, dimension(:,:,:,:,:), public, allocatable :: wyckoff_og_xyz          ! wyckoff_og_xyz(m,k,j,i): mth component to coefficient of kth parameter (x,y,z)
    Integer, dimension(:,:,:,:,:), public, allocatable :: wyckoff_og_mag          ! wyckoff_og_mag(m,k,j,i): mth component to coefficient of kth magnetic parameter (mx,my,mz)

    !------------------------!
    !---- Interface Zone ----!
    !------------------------!
    Interface
       Module Subroutine Allocate_Magnetic_DBase()
       End Subroutine Allocate_Magnetic_DBase

       Module Subroutine Deallocate_Magnetic_DBase()
       End Subroutine Deallocate_Magnetic_DBase

       Module Subroutine Read_Magnetic_Binary(DB_Path, EnvDB)
          character(len=*), optional, intent(in) :: DB_Path
          character(len=*), optional, intent(in) :: EnvDB
       End Subroutine Read_Magnetic_Binary

       Module Subroutine Read_Magnetic_Data(DB_Path, EnvDB)
          character(len=*), optional, intent(in) :: DB_Path
          character(len=*), optional, intent(in) :: EnvDB
       End Subroutine Read_Magnetic_Data

    End Interface

End Module CFML_Magnetic_Database