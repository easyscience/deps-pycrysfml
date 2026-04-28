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
Module CFML_magSuperSpace_Database
   !---- Use Modules ----!
   Use CFML_GlobalDeps
   Use CFML_Rational

   implicit none

   private

!   public :: Read_magSSG_DBase, Read_single_magSSG, Allocate_magSSG_DBase, Deallocate_magSSG_DBase
   public :: Read_magSSG_DBase, Allocate_magSSG_DBase, Deallocate_magSSG_DBase

   !> Parameters
   integer, parameter, public :: Mag_NGS = 325127    !
   integer, parameter, public :: Mag_OPS =     96    !
   integer, parameter, public :: Mag_DIM =      6    !

   !> Variables
   logical, public :: magSSG_DBase_allocated=.false.
!
   integer, dimension(:), allocatable :: mgroup_ssg ! (Mag_NGS) for each magnetic superspace group
                                                    ! associated nonmagnetic superspace group, same numbering as in
                                                    ! ssg_datafile.txt

   integer, dimension(:), allocatable :: mgroup_spacegroup  ! (Mag_NGS) basic nonmagnetic space group

   integer, dimension(:), allocatable :: mgroup_nmod  ! (Mag_NGS)  number of modulations


   integer, dimension(:), allocatable :: mgroup_mag ! (Mag_NGS)  basic magnetic space group number
                                                    ! number is given by mgroup_spacegroup(i).mgroup_mag(i):
                                                    ! 1.1, 2.1, 2.2, etc.

   integer, dimension(:),   allocatable ::  mgroup_nops  ! (Mag_NGS)  number of operators
   integer, dimension(:,:), allocatable ::  mgroup_ops_r ! (Mag_OPS,Mag_NGS)  time reversal (1=includes time reversal, -1=does not include time reversal)



   character(len=21), dimension(:), allocatable :: mgroup_nlabel ! (Mag_NGS) group number label: 1.1.1.1.m1.1, 2,1,1,1.m2.1, etc.
   character(len=63), dimension(:), allocatable :: mgroup_label  ! (Mag_NGS) group label


   integer(kind=1), dimension(:,:,:,:), allocatable :: mgroup_ops ! (7,7,Mag_OPS,Mag_NGS)  (d+1)x(d+1) augmented matrix for each operator
                                                                     !   common denominator in element (d+1,d+1)
!   Augmented transformation matrix from the standard setting of the
!   magnetic basic space group to the standard setting of the magnetic
!   superspace group.  This matrix is used to transform Wyckoff positions
!   in the magnetic basic space group to the setting of the magnetic
!   superspace group. Common denominator in element (4,4)
!
      integer(kind=1), dimension(:,:,:), allocatable :: mgroup_transmag !(4,4,Mag_NGS)


   !------------------------!
   !---- Interface Zone ----!
   !------------------------!
   Interface
      Module Subroutine Allocate_magSSG_DBase()
         !---- Arguments ----!
      End Subroutine Allocate_magSSG_DBase

      Module Subroutine Deallocate_magSSG_DBase()
         !---- Arguments ----!
      End Subroutine Deallocate_magSSG_DBase

      Module Subroutine Read_magSSG_DBase(DB_Path, EnvDB)
         character(len=*), optional, intent(in)  :: DB_Path
         character(len=*), optional, intent(in)  :: EnvDB
      End Subroutine Read_magSSG_DBase

      !Module Subroutine Read_single_magSSG(str,num,DB_Path, EnvDB)
      !   character(len=*),           intent(in)  :: str
      !   integer,                    intent(out) :: num
      !   character(len=*), optional, intent(in)  :: DB_Path
      !   character(len=*), optional, intent(in)  :: EnvDB
      !End Subroutine Read_single_magSSG

   End Interface

End Module CFML_magSuperSpace_Database