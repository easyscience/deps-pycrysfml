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
!!---- MODULE: CFML_Maps
!!----   INFO: Subroutines related to operations on the array's map
!!----
!!
 Module CFML_Maps
    !---- Use Modules ----!
    Use CFML_GlobalDeps,   only: CP, DP, EPS, Err_CFML, Clear_Error
    Use CFML_Strings,      only: l_case
    Use CFML_gSpaceGroups, only: SpG_Type, Apply_OP
    Use CFML_Metrics,      only: Cell_Type, Cell_G_Type
    Use CFML_Geom,         only: Distance

    !---- Variables ----!
    implicit none

    private

    !---- List of public functions ----!
    public :: Index_Cube, Vertice_Point, Vertices_Cube, Vpoint_in_Cube, &
              Vpoint_in_Square, Vpoint_in_Line, Load_Section


    !---- List of public subroutines ----!
    public :: Load_ExtendedMap, Search_Peaks, Statistic_Map, &
              Calculate_Contour2D, Calculate_Mesh, Set_Cube_Info, &
              Percol_Analysis, Read_BVEL, Peak_List, DFS_C, DFS_N

    !---- Definitions ----!

    !---- Parameters ----!
    integer, public, parameter ::  MAX_POINTS    = 500000   ! Number of maximum points permitted

    !!----
    !!----  TYPE :: CUBE_INFO_TYPE
    !!--..
    !!----  Date: 31/05/2020
    !!
    Type, public :: Cube_Info_Type
       integer                :: NElem =0 ! Number of Elemens
       integer                :: Code  =0 ! 1:   2:   3:   4:   5:   6:
       integer, dimension(12) :: Edges =0 ! Code for Edge connections
    End Type Cube_Info_Type

    !!----
    !!----  TYPE :: NODE
    !!----
    !!----
    !!----  31/05/2020
    !!
    Type, Private :: Node
       Integer                     :: nbonds    =0    ! number of bonds of the node
       Integer                     :: nbonds_pbc=0    ! number of bonds of the node with periodic boundary conditions (pbc)
       Integer                     :: Type      =0    ! 0: inside unit cell 1: on a face 2: on an edge  3: on a coner
       Integer                     :: state     =0    ! 0: non-visited, 1: visited
       Integer                     :: cc        =0    ! connected component to which the node belongs before pbc
       Integer                     :: cc_pbc    =0    ! connected component to which the node belongs after pbc
       Integer, Dimension(3)       :: face      =0    !
       Integer, Dimension(12)      :: bond      =0    ! bonds of the node
       Integer, Dimension(12)      :: bond_pbc  =0    ! bonds of the node
       Real(kind=cp), Dimension(3) :: xyz    =0.0_cp  ! coordinates of the node
       Real(kind=cp), Dimension(3) :: xyz_aux=0.0_cp  ! coordinates of the node
    End Type node

    !!----
    !!----  TYPE :: COMPONENT
    !!----
    !!----  Type :: Component
    !!----    Integer                            :: nbonds  ! number of bonds
    !!----    Integer                            :: state   ! 0: non-visited
    !!----                                                  ! 1: visited
    !!----    Integer                            :: cc      ! component index
    !!----    Integer                            :: pbc     ! bonds of the node
    !!----    Integer, Dimension(:), Allocatable :: bond    ! component-component bonds
    !!----  End Type Component
    !!----
    !!----  Update: July - 2016
    !!
    Type, Private :: Component
       Integer                            :: nbonds=0   ! number of bonds
       Integer                            :: state =0   ! 0: non-visited 1: visited
       Integer                            :: cc    =0   ! component index
       Integer                            :: pbc   =0   ! bonds of the node
       Integer, Dimension(:), Allocatable :: bond       ! component-component bonds
    End Type component

    !---- Variables ----!
    Type(Cube_Info_Type), dimension(0:255), public   :: Cube_Info
    Type(Node),           Dimension(:), Allocatable  :: nodes        ! Nodes of the graph
    Type(Component),      Dimension(:), Allocatable  :: cts          ! Connected components of the graph


    !---- Overlapp zone ----!
    Interface  Vertice_Point
       Module Procedure Vertice_Point_Cal
       Module Procedure Vertice_Point_Fix
    End Interface

    !------------------------!
    !---- Interface Zone ----!
    !------------------------!
    Interface
        Pure Module Function Index_Cube(iv, mc) Result(Ind)
           integer, dimension(8), intent(in) :: iv
           logical,               intent(in) :: mc
           integer                           :: Ind
        End Function Index_Cube

        Pure Module Function Vertices_Cube(Index_Cube) Result(Iv)
           integer, intent(in)   :: index_cube
           integer, dimension(8) :: iv
        End Function Vertices_Cube

        Pure Module Function Vertice_Point_Cal(Code_Edge,D0,D1,D2,D3,D4,D5,D6,D7,D8) Result(V)
           integer,          intent(in) :: code_edge
           real(kind=cp),    intent(in) :: d0,d1,d2,d3,d4,d5,d6,d7,d8
           real(kind=cp), dimension(3)  :: v
        End Function Vertice_Point_Cal

        Pure Module Function Vertice_Point_Fix(Code_Edge) Result(V)
           integer,          intent(in) :: code_edge
           real(kind=cp), dimension(3)  :: v
        End Function Vertice_Point_Fix

        Pure Module Function VPoint_in_Line(R, X0, X1) Result (X)
           real(kind=cp), intent(in) :: R
           real(kind=cp), intent(in) :: X0, X1
           real(kind=cp)             :: X
        End Function VPoint_in_Line

        Pure Module Function VPoint_in_Square(R, S, X00, X01, X10, X11) Result(x)
           real(kind=cp), intent(in) :: r, s
           real(kind=cp), intent(in) :: x00, x01, x10, x11
           real(kind=cp)             :: x
        End Function VPoint_in_Square

        Pure Module Function VPoint_in_Cube(R,S,T,X000,X001,X010,X011,X100,X101,X110,X111) Result(X)
           real(kind=cp), intent(in) :: R, S, T
           real(kind=cp), intent(in) :: X000,X001,X010,X011,X100,X101,X110,X111
           real(kind=cp)             :: X
        End Function VPoint_in_Cube

        Pure Module Function xy_sect(p1,p2,h,xy) result(sect)
           integer,                       intent(in) :: p1,p2
           real(kind=cp), dimension(0:4), intent(in) :: h,xy
           real(kind=cp)                             :: sect
        End Function xy_sect

        Pure Module Function Peak_Position(nr3d,i,j,k) Result(Pto)
           integer, dimension(:,:,:),intent(in) :: nr3d
           integer,                  intent(in) :: i
           integer,                  intent(in) :: j
           integer,                  intent(in) :: k
           real(kind=cp), dimension(3)          :: pto
        End Function Peak_Position

        Module Subroutine Set_Cube_Info()
        End Subroutine Set_Cube_Info

        Module Subroutine Statistic_Map(Rho,MaxV,MinV,AveV,SigV)
           real(kind=cp), dimension(:,:,:), intent(in) :: Rho
           real(kind=cp), optional,         intent(out):: MaxV
           real(kind=cp), optional,         intent(out):: MinV
           real(kind=cp), optional,         intent(out):: AveV
           real(kind=cp), optional,         intent(out):: SigV
        End Subroutine Statistic_Map

        Pure Module Function Load_Section(Rho,ngrid,imap,section,limits,ngrid2) Result(dmap)
           real(kind=cp), dimension(:,:,:), intent(in)   :: rho
           integer,       dimension(3),     intent(in)   :: ngrid
           integer,                         intent(in)   :: imap
           integer,                         intent(in)   :: section
           real(kind=cp), dimension(2,2),   intent(in)   :: limits
           integer,       dimension(2),     intent(in)   :: ngrid2
           real(kind=cp), dimension(ngrid2(1),ngrid2(2)) :: dmap
        End Function Load_Section

        Pure Module Function Load_ExtendedMap(Rho,Ngrid,Limits) Result(Rhonew)
           real(kind=cp), dimension(:,:,:), intent(in) :: rho
           integer,       dimension(3),     intent(in) :: ngrid
           real(kind=cp), dimension(2,3),   intent(in) :: limits
           real(kind=cp), dimension(ngrid(1)+1,ngrid(2)+1,ngrid(3)+1):: rhonew
        End Function Load_ExtendedMap

        Module Subroutine Calculate_Contour2D(d,ilb,iub,jlb,jub,x,y,z,nlv,npt,xyz)
           integer,                                    intent(in)     :: ilb,iub
           integer,                                    intent(in)     :: jlb,jub
           real(kind=cp), dimension (ilb:iub,jlb:jub), intent(in)     :: d
           real(kind=cp), dimension (ilb:iub),         intent(in)     :: x
           real(kind=cp), dimension (jlb:jub),         intent(in)     :: y
           real(kind=cp), dimension (:),               intent(in)     :: z
           integer,                                    intent(in)     :: nlv
           integer,                                    intent(in out) :: npt
           real(kind=cp), dimension (:,:),             intent(out)    :: xyz
        End Subroutine Calculate_Contour2D

        Module Subroutine Calculate_Mesh(Rho,Ngrid,Nlevel,Levels,MC_Method,NPoints,Xyz,Limits,Step)
           real(kind=cp),    dimension(:,:,:),           intent(in)  :: Rho
           integer,          dimension(3),               intent(in)  :: ngrid
           integer,                                      intent(in)  :: nlevel
           real(kind=cp),    dimension(:),               intent(in)  :: Levels
           character(len=*),                             intent(in)  :: MC_Method
           integer,          dimension(:),               intent(out) :: NPoints
           real(kind=cp),    dimension(:,:),             intent(out) :: Xyz
           real(kind=cp),    dimension(2,3),   optional, intent(in)  :: Limits
           integer,          dimension(3),     optional, intent(in)  :: Step
        End Subroutine Calculate_Mesh

        Module Subroutine Peak_List(Pto,Grp,Cell,Npks,Pks)
           real(kind=cp), dimension(4),   intent(in)     :: Pto
           class(SpG_Type),               intent(in)     :: Grp
           class(Cell_G_Type),            intent(in)     :: Cell
           integer,                       intent(in out) :: NPks
           real(kind=cp), dimension(:,:), intent(in out) :: Pks
        End Subroutine Peak_List

        Module Subroutine Search_Peaks(Rho,Grp,Cell,NPFound,Pks,Abs_Code)
           real(kind=cp), dimension(:,:,:),    intent(in)      :: Rho         ! Density
           class(SpG_Type),                    intent(in)      :: Grp         ! Space Group
           class(cell_G_type),                 intent(in)      :: Cell        ! Cell
           integer,                            intent(in out)  :: NPFound     ! Number of peaks to found
           real(kind=cp), dimension(:,:),      intent(out)     :: Pks         ! Peak List  dimension(4, NPfound)
           logical, optional,                  intent(in)      :: Abs_Code    ! logical to use absolute value on Rho
        End Subroutine Search_Peaks

        Module Subroutine Read_BVEL(Filename,rho)
           character(len=*),                             intent(in)  :: filename
           real(kind=cp), dimension(:,:,:), allocatable, intent(out) :: rho
        End Subroutine Read_BVEL

        Module Subroutine Percol_Analysis(rho, Emin, Eini, Eend, dE, E_percol, axis, Lun)
           Real(kind=cp), Dimension(:,:,:), Intent(in)    :: rho
           Real(kind=cp),                   Intent(in)    :: Emin
           Real(kind=cp),                   Intent(in)    :: Eini
           Real(kind=cp),                   Intent(in)    :: Eend
           Real(kind=cp),                   Intent(in)    :: dE
           Real(kind=cp), Dimension(3),     Intent(inout) :: E_percol
           Integer, Optional,               Intent(in)    :: axis
           Integer, Optional,               Intent(in)    :: lun
        End Subroutine Percol_Analysis

    End Interface

 Contains

    !!----
    !!---- DFS_C
    !!----
    !!----
    !!---- 31/05/2020
    !!
    Recursive Subroutine DFS_C(id,ncc)
       !---- Arguments ----!
       Integer, Intent(in) :: id, ncc

       !---- Local variables ----!
       Integer             :: j, k

       cts(id)%state  = 1
       cts(id)%cc     = ncc

       Do j = 1, cts(id)%nbonds
          k = cts(id)%bond(j)
          if (cts(k)%state == 0) Call DFS_C(k,ncc)
       End Do

    End Subroutine DFS_C

    !!----
    !!---- DFS_N
    !!----
    !!----
    !!---- 31/05/2020
    !!
    Recursive Subroutine DFS_N(id, ncc)
       !---- Arguments ----!
       integer, Intent(in) :: id, ncc

       !---- Local variables ----!
       Integer             :: j, k

       nodes(id)%state  = 1
       nodes(id)%cc     = ncc
       Do j = 1, nodes(id)%nbonds
          k = nodes(id)%bond(j)
          if (nodes(k)%state == 0) Call DFS_N(k,ncc)
       End Do

    End Subroutine DFS_N



 End Module CFML_Maps
