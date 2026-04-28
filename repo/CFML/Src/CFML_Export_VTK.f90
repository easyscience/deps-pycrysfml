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
!!---- MODULE: CFML_Export_Vtk
!!----   INFO: Module to export volumetric data format to VTK format or VESTA format
!!----
!!---- HISTORY
!!----    Update: 23/12/2011  Laurent Chapon
!!----    Update: 20/06/2020  JRC
!!----
!!----
!!----
Module CFML_Export_VTK

  use CFML_Globaldeps,   only: cp, dp,clear_error,Err_CFML
  use CFML_Metrics,      only: Cell_G_Type
  use CFML_Atoms,        only: AtList_Type
  use CFML_gSpaceGroups, only: SPG_Type, Apply_OP
  use CFML_Strings,      only: Pack_String
  use CFML_Maths,        only: modulo_lat

  implicit none
  private
  public :: array3D_pointdata_2_vts, unitCell_to_PDBfile, write_grid_VESTA

  character(len=1), parameter:: end_line=char(10)

  interface write_grid_VESTA
       Module Procedure write_grid_VESTA_cp
       Module Procedure write_grid_VESTA_dp
  end interface

  contains

  !!----
  !!---- Subroutine array3D_pointdata_2_vts(Z,cell,xmin,xmax,ymin,ymax,zmin,zmax,filename)
  !!----  real(kind=cp), dimension(:,:,:), intent(in) :: Z           ! 3D Array to export
  !!----  type(Cell_G_Type),               intent(in) :: cell        ! The unit-cell
  !!----  real(kind=cp),                   intent(in) :: xmin,xmax   ! Limits of x
  !!----  real(kind=cp),                   intent(in) :: ymin,ymax   ! Limits of y
  !!----  real(kind=cp),                   intent(in) :: zmin,zmax   ! Limits of z
  !!----  Character(len=*),                intent(in) :: filename    ! Name of the vtk file
  !!----
  !!----  Export a 3D array representing for example a density
  !!----  into a vts file (vtk file format). This format correponds to the new xml format of vtk
  !!----  as opposed to the legacy ascii format. vts corresponds to a structured grid
  !!----  and the data is compressed using raw compression.
  !!----  This routine should be used for pointData, i.e. data corresponding to a distribution:
  !!----  Each point is defined by a
  !!----  Update: December - 2011
  !!
  Subroutine array3D_pointdata_2_vts(Z,cell,xmin,xmax,ymin,ymax,zmin,zmax,filename)
    real(kind=cp), dimension(:,:,:), intent(in) :: Z           ! 3D Array to export
    type(Cell_G_Type),               intent(in) :: cell        ! The unit-cell
    real(kind=cp),                   intent(in) :: xmin,xmax   ! Limits of x
    real(kind=cp),                   intent(in) :: ymin,ymax   ! Limits of y
    real(kind=cp),                   intent(in) :: zmin,zmax   ! Limits of z
    Character(len=*),                intent(in) :: filename    ! Name of the vtk file

    ! Local variables
    integer :: nx, ny, nz
    integer :: vtk_id, ier
    integer :: i,j,k
    integer :: nbytes, nbits
    character(len=48)   :: nbit_c, offset_c, nx_c, ny_c, nz_c
    character(len=1000) :: header
    integer :: offset
    real(kind=cp), dimension(3) :: pos
    integer :: ntot ! Total number of points
    integer :: ntot_bytes


    ! Open the file and check that the file is valid

    open(newunit=vtk_id,file=trim(filename)//'.vts', &
           form       = 'UNFORMATTED',  &
           access     = 'STREAM',       &
           action     = 'WRITE',        &
           status     = 'REPLACE',      &
  !        convert    = 'LITTLE_ENDIAN',&
  !        recordtype = 'STREAM',       &
  !        buffered   = 'YES',          &
           iostat=ier)
    if (ier/=0) then
      Err_CFML%Ierr=1
      Err_CFML%flag=.true.
      Err_CFML%Msg="Error opening the file: "//trim(filename)//'.vts'
      return
    end if

    ! Get the extent in each direction.
    nx=size(Z,1)
    ny=size(Z,2)
    nz=size(Z,3)

    ! Total number of points
    ntot= nx * ny * nz

    ! Determine the number of bits used
    nbytes=sizeof(z(1,1,1))
    !nbytes=kind(z(1,1,1))
    nbits=8*nbytes
    !
    ntot_bytes=ntot*nbytes*3

    ! Calculate the offset between points and Z
    offset=nbytes*ntot*3+4
    !
    ! ----------- Here starts the header
    write(unit=vtk_id) '<?xml version="1.0"?>'//end_line
    write(unit=vtk_id) '<VTKFile byte_order="LittleEndian" version="0.1" type="StructuredGrid">'//end_line
    write(unit=nx_c,fmt=*) nx-1
    write(unit=ny_c,fmt=*) ny-1
    write(unit=nz_c,fmt=*) nz-1
    write(unit=header,fmt='(A)') '<StructuredGrid WholeExtent="0 '//&
         trim(Pack_String(nx_c))//' 0 '//trim(Pack_String(ny_c))//' 0 '//trim(Pack_String(nz_c))//'">'
    write(unit=vtk_id) trim(header)//end_line
    write(unit=header,fmt='(A)') '<Piece Extent="0 '//trim(Pack_String(nx_c))//' 0 '//&
         trim(Pack_String(ny_c))//' 0 '//trim(Pack_String(nz_c))//'">'
    write(unit=vtk_id) trim(header)//end_line
    write(unit=vtk_id) '<Points>'//end_line
    write(unit=nbit_c,fmt=*) nbits
    write(unit=header,fmt='(A)') '<DataArray NumberOfComponents="3" offset="0" type="Float'//&
         trim(Pack_String(nbit_c))//'" Name="points" format="appended"/>'
    write(unit=vtk_id) trim(header)//end_line
    write(unit=vtk_id) '</Points>'//end_line
    write(unit=vtk_id) '<PointData scalars="density">'//end_line
    write(unit=offset_c,fmt=*) offset
    write(unit=header,fmt='(A)') '<DataArray NumberOfComponents="1" offset="'// &
         trim(Pack_string(offset_c))//'" type="Float'//trim(Pack_String(nbit_c))//'" Name="density" format="appended"/>'
    write(unit=vtk_id) trim(header)//end_line
    write(unit=vtk_id) '</PointData>'//end_line
    write(unit=vtk_id) '</Piece>'//end_line
    write(unit=vtk_id) '</StructuredGrid>'//end_line
    write(unit=vtk_id) '<AppendedData encoding="raw">'//end_line
    write(unit=vtk_id) '_'

    !------------- Here finishes the header
    ! This is where the data is written

    ! First we write the number of bytes for the points
    write(unit=vtk_id) ntot*nbytes*3
    ! Then write the coordinates of the points
    do k=1,nz
       do j=1,ny
            do i=1,nx
                pos=[xmin+(xmax-xmin)*(i-1.0)/nx,ymin+(ymax-ymin)*(j-1.0)/ny,zmin+(zmax-zmin)*(k-1.0)/nz]
                pos=Matmul(cell%Cr_Orth_cel,pos)
                write(vtk_id) pos
            end do
       end do
    end do
    ! write the number of bytes for the data points
    write(unit=vtk_id) ntot*nbytes
    ! write the data points
    do k=1,nz
       do j=1,ny
           do i=1,nx
               write(vtk_id) Z(i,j,k)
           end do
       end do
    end do
    ! Finished to write the data, close the xml
    write(vtk_id) end_line
    write(vtk_id) '</AppendedData>'//end_line
    write(vtk_id) '</VTKFile>'//end_line
    ! Close the file
    close(unit=vtk_id)

  End Subroutine array3D_pointdata_2_vts

  Subroutine unitCell_to_PDBfile(cell,spaceg,atom_list,filename)
    type(Cell_G_Type), intent(in) :: cell
    type(SPG_Type),    intent(in) :: spaceg
    type(AtList_type), intent(in) :: atom_list
    Character(len=*),  intent(in) :: filename ! Name of the pdb file
    !
    integer :: pdb_id, ier, i, j, g, L, nt
    !real(kind=cp), dimension(3,3) :: ort_mat
    real(kind=cp), dimension(3)   :: xx, v
    real(kind=cp), dimension(:,:), allocatable :: xtemp


    open(newunit=pdb_id,file=trim(filename)//'.pdb', status='REPLACE',  iostat=ier)
    if(ier /= 0) then
      Err_CFML%Ierr=1
      Err_CFML%flag=.true.
      Err_CFML%Msg="Error opening the file: "//trim(filename)//'.pdb'
      return
    end if

    ! Write the title
    write(unit=pdb_id,fmt='(A)') "TITLE     pdb (protein data bank) file generated by CRYSFML"
    ! Cell dimensions, space group name and Z.
    write(unit=pdb_id,fmt='(A6,3F9.3,3F7.2,A11,I4)') "CRYST1", cell%cell, cell%ang,trim(spaceg%SPG_Symb),spaceg%multip
    ! Now write the origin which is the same as the unit cell:
    ! ---------------------------------------------------------------------------------------------
    ! From pdb manual: the transformation from the orthogonal coordinates contained
    !in the database entry to the submitted coordinates
    !---------------------------------------------------------------------------------------------
    write(unit=pdb_id,fmt='(A)') "ORIGX1      1.000000  0.000000  0.000000        0.00000"
    write(unit=pdb_id,fmt='(A)') "ORIGX2      0.000000  1.000000  0.000000        0.00000"
    write(unit=pdb_id,fmt='(A)') "ORIGX3      0.000000  0.000000  1.000000        0.00000"
    ! Now write the scale which is the same as the unit cell:
    ! ---------------------------------------------------------------------------------------------
    ! From pdb manual: the transformation from the orthogonal coordinates contained in the entry to fractional
    ! crystallographic coordinates
    !---------------------------------------------------------------------------------------------
    write(unit=pdb_id,fmt='(A6,4X,3F10.6,5X,F10.5)') "SCALE1",cell% Orth_Cr_cel(:,1), 0.0
    write(unit=pdb_id,fmt='(A6,4X,3F10.6,5X,F10.5)') "SCALE2",cell% Orth_Cr_cel(:,2), 0.0
    write(unit=pdb_id,fmt='(A6,4X,3F10.6,5X,F10.5)') "SCALE3",cell% Orth_Cr_cel(:,3), 0.0
    ! Loop on atoms
    g=0
    allocate(xtemp(3,spaceg%multip))

    do i=1, atom_list%natoms
        L=1
        xtemp(:,1)=modulo_lat(atom_list%atom(i)%x)
       do_eq:do j=2, spaceg%multip
          xx=Apply_OP(spaceG%op(j),xtemp(:,1))
          xx=modulo_lat(xx)
          do nt=1,L
             v=xtemp(:,nt)-xx(:)
              if (sum(abs((v))) < 1e-6 ) cycle do_eq
          end do
          L=L+1
          xtemp(:,L)=xx(:)
       end do do_eq
       do j=1,L
         g=g+1
         write(unit=pdb_id,fmt='(A6,I5,A5,14X,3F8.3,2F6.2,10X,A2)') "ATOM  ", g ,trim(atom_list%atom(i)%ChemSymb), &
                                        matmul(cell%Cr_Orth_cel,xtemp(:,j)), 1.0, 0.0,atom_list%atom(i)%ChemSymb
       end do
    end do ! loop on atoms
    close(unit=pdb_id)
  End Subroutine unitCell_to_PDBfile

  !!----
  !!---- Subroutine write_grid_VESTA(rho,cell,title,filename,code)
  !!----  real(kind=cp or dp), dimension(:,:,:), intent(in) :: rho         ! 3D Array to export
  !!----  type(Cell_G_Type),                     intent(in) :: cell        ! The unit-cell
  !!----  Character(len=*),                      intent(in) :: title       ! Title
  !!----  Character(len=*),                      intent(in) :: filename    ! Name of the vtk file
  !!----  Character(len=*),                      intent(in) :: code        ! 'P' -> pgrid, 'G' -> ggrid
  !!----
  !!----  Export a 3D array representing for example a density
  !!----  into a pgrid/ggrid file (VESTA file format).
  !!----  Updated: October - 2014 (JRC)
  !!
  Subroutine write_grid_VESTA_cp(rho,cell,title,filename,code)
    real(kind=cp), dimension(:,:,:), intent(in) :: rho         ! 3D Array to export
    type(Cell_G_Type),               intent(in) :: cell        ! The unit-cell
    Character(len=*),                intent(in) :: title       ! title
    Character(len=*),                intent(in) :: filename    ! Name of the VESTA file
    Character(len=*),                intent(in) :: code        ! 'P' -> pgrid, 'G' -> ggrid

    ! Local variables
    integer :: vesta_id, ier
    character(len=len(filename)+10) :: filegrid
    integer(kind=4)              :: gType, fType=0, nVal=1, ndim=3, nASYM != ngrid(1)*ngrid(2)*ngrid(3)
    integer(kind=4), dimension(4):: version=[3,0,0,0]
    integer(kind=4), dimension(3):: ngrid
    character(len=79)            :: tit

    if(code == "P" .or. code == "p") then
      filegrid=trim(filename)//".pgrid"
      gtype=1
    else
      filegrid=trim(filename)//".ggrid"
      gtype=0
    end if
    tit=trim(title)
    ! Open the file and check that the file is valid

    open(newunit=vesta_id,file=trim(filegrid), &
           form       = 'UNFORMATTED',  &
           access     = 'STREAM',       &
           action     = 'WRITE',        &
           status     = 'REPLACE',      &
           iostat=ier)
    if (ier/=0) then
      Err_CFML%Ierr=1
      Err_CFML%flag=.true.
      Err_CFML%Msg="Error opening the file: "//trim(filegrid)
      return
    end if

    ! Get the extent in each direction.
    ngrid=[size(rho,1),size(rho,2),size(rho,3)]
    nASYM=ngrid(1)*ngrid(2)*ngrid(3)

    ! ----------- Here starts the header
    write(vesta_id) version
    write(vesta_id) tit(1:79)//end_line
    write(vesta_id) gType
    write(vesta_id) fType !=0
    write(vesta_id) nval  !=1
    write(vesta_id) ndim  !=3
    write(vesta_id) ngrid
    write(vesta_id) nASYM !
    write(vesta_id) Cell%cell,Cell%ang
    ! ----------- Here starts the densitydata
    write(vesta_id) rho

    close(unit=vesta_id)

  End Subroutine write_grid_VESTA_cp

  Subroutine write_grid_VESTA_dp(rho,cell,title,filename,code)
    real(kind=dp), dimension(:,:,:), intent(in) :: rho         ! 3D Array to export
    type(Cell_G_Type),               intent(in) :: cell        ! The unit-cell
    Character(len=*),                intent(in) :: title       ! title
    Character(len=*),                intent(in) :: filename    ! Name of the VESTA file
    Character(len=*),                intent(in) :: code        ! 'P' -> pgrid, 'G' -> ggrid

    ! Local variables
    integer :: vesta_id, ier
    character(len=len(filename)+10) :: filegrid
    integer(kind=4)              :: gType, fType=0, nVal=1, ndim=3, nASYM != ngrid(1)*ngrid(2)*ngrid(3)
    integer(kind=4), dimension(4):: version=[3,0,0,0]
    integer(kind=4), dimension(3):: ngrid


    if(code == "P" .or. code == "p") then
      filegrid=trim(filename)//".pgrid"
      gtype=1
    else
      filegrid=trim(filename)//".ggrid"
      gtype=0
    end if
    ! Open the file and check that the file is valid

    open(newunit=vesta_id,file=trim(filegrid), &
           form       = 'UNFORMATTED',  &
           access     = 'STREAM',       &
           action     = 'WRITE',        &
           status     = 'REPLACE',      &
           iostat=ier)
    if (ier/=0) then
      Err_CFML%Ierr=1
      Err_CFML%flag=.true.
      Err_CFML%Msg="Error opening the file: "//trim(filegrid)
      return
    end if

    ! Get the extent in each direction.
    ngrid=[size(rho,1),size(rho,2),size(rho,3)]
    nASYM=ngrid(1)*ngrid(2)*ngrid(3)

    ! ----------- Here starts the header
    write(vesta_id) version
    write(vesta_id) title(1:79)//end_line
    write(vesta_id) gType
    write(vesta_id) fType !=0
    write(vesta_id) nval  !=1
    write(vesta_id) ndim  !=3
    write(vesta_id) ngrid
    write(vesta_id) nASYM !
    write(vesta_id) Cell%cell,Cell%ang
    ! ----------- Here starts the densitydata
    write(vesta_id) rho

    close(unit=vesta_id)

  End Subroutine write_grid_VESTA_dp


End Module CFML_Export_VTK


