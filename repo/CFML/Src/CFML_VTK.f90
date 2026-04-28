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
!!---- MODULE: CFML_VTK
!!----   INFO: Module to manipulate / build data for graphical representation with VTK
!!----

module CFML_VTK

    use cfml_globaldeps

    implicit none

    private

    !---- Public Subroutine ----!
    public :: scan_arrays, scan_limits

    interface

        module subroutine scan_arrays(counts,th,vtk_points,vtk_cells,vtk_counts)
            !---- Arguments ----!
            integer,         dimension(:,:,:), intent(in)    :: counts   ! Scan counts
            integer,                           intent(in)    :: th       ! Threshold
            integer,         dimension(:,:),   intent(inout) :: vtk_points
            integer(kind=8), dimension(:,:),   intent(inout) :: vtk_cells
            integer,         dimension(:),     intent(inout) :: vtk_counts
        end subroutine scan_arrays

        module subroutine scan_limits(counts,th,max_cnt,npoints)
            !---- Arguments ----!
            integer, dimension(:,:,:), intent(in)  :: counts   ! Scan counts
            integer,                   intent(out) :: th       ! Threshold
            integer,                   intent(out) :: max_cnt  ! Maximum counts / pixel
            integer,                   intent(out) :: npoints  ! Number of points in the range [th,max_cnt]
        end subroutine scan_limits

    end interface

end module CFML_VTK