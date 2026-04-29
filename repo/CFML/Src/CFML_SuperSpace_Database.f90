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
Module CFML_SuperSpace_Database
   !---- Use Modules ----!
   Use CFML_GlobalDeps
   Use CFML_Rational

   implicit none

   private

   public :: Read_SSG_DBase, Read_single_SSG, Allocate_SSG_DBase, Deallocate_SSG_DBase

   !> Parameters
   integer, parameter, public :: M_CEN =    16    !
   integer, parameter, public :: M_NCL =   322    !
   integer, parameter, public :: M_NGS = 16697    !
   integer, parameter, public :: M_OPS =    48    !
   integer, parameter, public :: M_DIM =     6    !
   integer, parameter, public :: M_COND=    50    !
   integer, parameter, public :: M_QV  =     3    !

   !> Variables
   logical, public :: SSG_DBase_allocated=.false.

   integer,                              public :: nclasses=0          ! number of Bravais classes
   integer,   dimension(:), allocatable, public :: iclass_nmod         ! for each Bravais class: number of modulation q vectors
   integer,   dimension(:), allocatable, public :: iclass_number       ! class number
   integer,   dimension(:), allocatable, public :: iclass_spacegroup   ! basic space group of lattice
   integer,   dimension(:), allocatable, public :: iclass_nstars       ! number of different stars of q
   integer, dimension(:,:), allocatable, public :: iclass_nmodstar     ! number of modulation q vectors for each star

   character(len=5),  dimension(:)       , allocatable, public :: class_nlabel       ! class number label: 1.1, 1.2, 1.3, etc.
   character(len=50), dimension(:)       , allocatable, public :: class_label        !   class label
   integer,           dimension(:,:,:,:) , allocatable, public :: iclass_qvec        !   q vectors
   integer,           dimension(:)       , allocatable, public :: iclass_ncentering  ! number of centering translations
   integer,           dimension(:,:,:)   , allocatable, public :: iclass_centering   ! centering translations: D=3+d integers followed by a common denominator
   integer,                                             public :: ngroups=0          ! number of superspace groups
   integer,           dimension(:)       , allocatable, public :: igroup_number      ! for each superspace group,  group number
   integer,           dimension(:)       , allocatable, public :: igroup_class       ! Bravais class
   integer,           dimension(:)       , allocatable, public :: igroup_spacegroup  ! Basic space group
   character(len=13), dimension(:)       , allocatable, public :: group_nlabel       !   group number label: 1.1.1.1, 2,1,1,1, etc.
   character(len=60), dimension(:)       , allocatable, public :: group_label        !   group label
   integer,           dimension(:)       , allocatable, public :: igroup_nops        !   number of operators

   !> (D+1)x(D+1) augmented matrix for each operator in supercentered setting
   !> common denominator in element (D+1,D+1)
   integer, dimension(:,:,:,:), allocatable, public :: igroup_ops
   integer, dimension(:)      , allocatable, public :: igroup_nconditions  ! number of reflection conditions
   integer, dimension(:,:,:,:), allocatable, public :: igroup_condition1   ! matrix representation of righthand side
   integer, dimension(:,:,:)  , allocatable, public :: igroup_condition2   ! vector representation of lefthand side
   integer, dimension(:)      , allocatable, public :: pos_group           !position in the file of the groups
   integer, dimension(:)      , allocatable, public :: pos_class           !position in the file of the Bravais classes
   integer, dimension(:)      , allocatable, public :: pos_all             !position in the file of all


   !------------------------!
   !---- Interface Zone ----!
   !------------------------!
   Interface
      Module Subroutine Allocate_SSG_DBase()
         !---- Arguments ----!
      End Subroutine Allocate_SSG_DBase

      Module Subroutine Deallocate_SSG_DBase()
         !---- Arguments ----!
      End Subroutine Deallocate_SSG_DBase

      Module Subroutine Read_SSG_DBase(DB_Path, EnvDB)
         character(len=*), optional, intent(in)  :: DB_Path
         character(len=*), optional, intent(in)  :: EnvDB
      End Subroutine Read_SSG_DBase

      Module Subroutine Read_single_SSG(str,num,DB_Path, EnvDB)
         character(len=*),           intent(in)  :: str
         integer,                    intent(out) :: num
         character(len=*), optional, intent(in)  :: DB_Path
         character(len=*), optional, intent(in)  :: EnvDB
      End Subroutine Read_single_SSG

   End Interface

End Module CFML_SuperSpace_Database