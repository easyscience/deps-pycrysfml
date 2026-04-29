!-------------------------------------------------------------
! Int3D08
! -------------------------------------------------------------
! This file is part of Int3d08
!
! The Int3D project is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!
! Authors: Nebil A. Katcho (ILL)
!          Juan Rodriguez-Carvajal (ILL)
!
!
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! -------------------------------------------------------------

module nexus_mod

    ! ------
    ! Public
    ! ------
    ! function nxs_to_powder_numor(nexus) Result(pnum)
    ! subroutine display_nexus(lun,nexus)
    ! subroutine initialize_nexus(nexus,dims)
    ! subroutine partial_nexus_copy(nexus1,nexus2)
    ! subroutine read_calibration(filename,path,machine,calibration)
    ! subroutine read_nexus(filename,nexus)
    ! subroutine write_simple_nexus(namef,nexus,novirtual,raw)
    ! subroutine write_vnexus(namef,ga_Dv,nu_Dv,avcounts,nexus)
    !
    ! -------
    ! Private
    ! -------
    ! subroutine get_scan_type(nexus,motors)
    ! subroutine get_source(lsource)
    ! subroutine read_nexus_ansto_wombat(nexus)
    ! subroutine read_nexus_ill(nexus)
    ! subroutine read_nexus_psi_dmc(nexus)
    ! subroutine read_nexus_psi_zebra(nexus)
    ! subroutine set_data_ordering(data_ordering_,data_ordering)

    use hdf5
    use CFML_GlobalDeps, only: ops_sep
    use CFML_ILL_Instrm_Data, only: Powder_Numor_Type,Initialize_Numor, Calibration_Detector_Type, &
                                    Current_Instrm
    use CFML_Strings, only: L_Case,Get_Filename

    implicit none

    private

    ! List of public subroutines
    public :: nxs_to_powder_numor, initialize_nexus, read_calibration, read_nexus, write_vnexus, &
              display_nexus, partial_nexus_copy, write_simple_nexus
    ! List of public variables
    logical, public :: err_nexus, war_nexus
    character(len=:), allocatable, public :: err_nexus_mess, war_nexus_mess

    type, public :: nexus_type

        integer                                  :: actual_step
        integer                                  :: manip
        integer                                  :: nbang  ! Total number of angles moved during the scan
        integer                                  :: nbdata ! nz * nx
        integer                                  :: nz     ! number of vertical pixels
        integer                                  :: nx     ! number of horizontal pixels
        integer                                  :: nf     ! number of frames
        integer                                  :: n4     ! forth dimension for 4D nexus, can be different things
        integer                                  :: run_number
        real                                     :: fcoupling
        real                                     :: magnetic_field
        real                                     :: reg_temperature
        real                                     :: scan_start
        real                                     :: scan_step
        real                                     :: scan_width
        real                                     :: setp_temperature
        real                                     :: temperature
        real                                     :: virtual_cgap
        real                                     :: wave
        real, dimension(3)                       :: reflection
        real, dimension(3,3)                     :: ub
        real, dimension(:), allocatable          :: monitor
        real, dimension(:), allocatable          :: timef
        real, dimension(:), allocatable          :: total_counts
        real, dimension(:,:), allocatable        :: angles     ! phi,chi,omega,gamma,psi,canne,nu,2theta
        integer, dimension(:,:,:), allocatable   :: counts     ! (nz,nx,nf)
        integer, dimension(:,:,:,:), allocatable :: counts4D   ! (nz,nx,nf,n4)
        character(len=:), allocatable            :: data_ordering
        character(len=:), allocatable            :: end_time
        character(len=:), allocatable            :: experiment_id
        character(len=:), allocatable            :: filcod
        character(len=:), allocatable            :: filename
        character(len=:), allocatable            :: geometry
        character(len=:), allocatable            :: instrument_name
        character(len=:), allocatable            :: scan_type
        character(len=:), allocatable            :: source
        character(len=:), allocatable            :: user
        character(len=:), allocatable            :: local_contact
        logical                                  :: is_actual_step
        logical                                  :: is_canne
        logical                                  :: is_chi
        logical                                  :: is_gamma
        logical                                  :: is_mode
        logical                                  :: is_monitor
        logical                                  :: is_nu
        logical                                  :: is_omega
        logical                                  :: is_phi
        logical                                  :: is_psi
        logical                                  :: is_timef
        logical                                  :: is_total_counts
        logical                                  :: is_tth
        logical                                  :: is_ub
        logical                                :: is_virtual
        logical                                :: is_4D

    end type nexus_type

    ! Parameters
    real, parameter :: MIN_SCAN_ANGLE = 0.01

    ! Local variables with hdf5 types
    integer :: hdferr
    integer(HID_T) :: file_id,dset,filetype,dset2,dset3,space,space2,space3
    integer(SIZE_T), parameter :: STR_MAX_LEN = 20 ! maximum string length
    integer(HSIZE_T), dimension(1) :: scalar
    integer(HSIZE_T), dimension(2) :: maxdims,maxdims3,data_dims
    integer(HSIZE_T), dimension(3) :: dims,dims3
    integer(HSIZE_T), dimension(4) :: dims4

    contains

    ! Public procedures

    function nxs_to_powder_numor(nexus) Result(pnum)

        ! Arguments
        type(nexus_type), intent(in) :: nexus
        type(Powder_Numor_type)      :: pnum

        ! Local variables
        integer :: i,n,ia,ic


        call Initialize_Numor(pnum,max(nexus%nbang,1),nexus%nbdata,nexus%nf)
        pnum%title    = trim(nexus%experiment_id)
        pnum%header   = trim(nexus%user)//trim(nexus%local_contact)//trim(nexus%end_time)
        pnum%instrm   = trim(adjustl(nexus%instrument_name))
        pnum%numor    = nexus%run_number
        pnum%manip    = nexus%manip
        pnum%scantype = nexus%scan_type
        pnum%wave     = nexus%wave
        pnum%conditions(1) = nexus%setp_temperature
        pnum%conditions(2) = nexus%reg_temperature
        pnum%conditions(3) = nexus%temperature
        pnum%conditions(5) = nexus%magnetic_field
        pnum%scans(1) = nexus%scan_start
        pnum%scans(2) = nexus%scan_step
        pnum%scans(3) = nexus%scan_width

        Select Case(trim(pnum%instrm))
          Case("d1b")
            pnum%scans(1)=nexus%angles(8,1)
            pnum%scans(2)=128.0/pnum%nbdata
          Case("d20")
            pnum%scans(2)=160.0/pnum%nbdata
        End Select
        pnum%scans(3)=pnum%scans(1)+(nexus%nbdata-1)*pnum%scans(2)

        if (nexus%is_timef)        pnum%tmc_ang(1,:) = nexus%timef(:)
        if (nexus%is_monitor)      pnum%tmc_ang(2,:) = nexus%monitor(:)
        if (nexus%is_total_counts) pnum%tmc_ang(3,:) = nexus%total_counts(:)
        pnum%time =    sum(pnum%tmc_ang(1,:))
        pnum%monitor = sum(pnum%tmc_ang(2,:)) / pnum%nframes
        if (nexus%is_phi) pnum%angles(1) = 0.5*(nexus%angles(1,1) + nexus%angles(1,nexus%nf))
        if (nexus%is_chi) pnum%angles(2) = 0.5*(nexus%angles(2,1) + nexus%angles(2,nexus%nf))
        if (nexus%is_omega) then
            pnum%angles(3) = 0.5*(nexus%angles(3,1) + nexus%angles(3,nexus%nf))
        else if (nexus%is_canne) then
            pnum%angles(3) = 0.5*(nexus%angles(6,1) + nexus%angles(6,nexus%nf))
        end if
        if (nexus%is_tth) then
            pnum%angles(4) = 0.5*(nexus%angles(8,1) + nexus%angles(8,nexus%nf))
        else if (nexus%is_gamma) then
            pnum%angles(4) = 0.5*(nexus%angles(4,1) + nexus%angles(4,nexus%nf))
        end if
        if (nexus%is_phi) pnum%angles(5) = 0.5*(nexus%angles(5,1) + nexus%angles(5,nexus%nf))
        do n = 1 , nexus%nf
            i = 0
            do ic = 1 , nexus%nx
                do ia = 1 , nexus%nz
                    i = i + 1
                    if (.not. nexus%is_4D) then
                        pnum%counts(i,n) = nexus%counts(ia,ic,n) ! (nz,nx,nf)
                    else
                        pnum%counts(i,n) = nexus%counts4D(ia,ic,n,1) ! (nz,nx,nf,n4)
                    end if
                end do
            end do
        end do
        if (nexus%nbang <= 1) then
            if (nexus%scan_type == 'phi') then
                pnum%tmc_ang(4,:) = nexus%angles(1,:)
            else if (nexus%scan_type == 'chi') then
                pnum%tmc_ang(4,:) = nexus%angles(2,:)
            else if (nexus%scan_type == 'omega') then
                pnum%tmc_ang(4,:) = nexus%angles(3,:)
            else if (nexus%scan_type == 'gamma') then
                pnum%tmc_ang(4,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'psi') then
                pnum%tmc_ang(4,:) = nexus%angles(5,:)
            else if (nexus%scan_type == 'canne') then
                pnum%tmc_ang(4,:) = nexus%angles(6,:)
            else if (nexus%scan_type == 'nu') then
                pnum%tmc_ang(4,:) = nexus%angles(7,:)
            else if (nexus%scan_type == '2theta') then
                pnum%tmc_ang(4,:) = nexus%angles(8,:)
            end if
        else if (nexus%nbang == 2) then
            if (nexus%scan_type == 'omega') then
                pnum%tmc_ang(4,:) = nexus%angles(3,:)
                pnum%tmc_ang(5,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'canne') then
                pnum%tmc_ang(4,:) = nexus%angles(6,:)
                pnum%tmc_ang(5,:) = nexus%angles(4,:)
            end if
        end if
        if (nexus%geometry == 'NB') pnum%icalc = 1

    end function nxs_to_powder_numor

    subroutine display_nexus(lun,nexus)
        integer,          intent(in) :: lun
        type(nexus_type), intent(in) :: nexus

        integer :: i,j
        character(len=:), allocatable :: ffirst

        write(lun,"(/,a)")       "   --------------------------------- "
        write(lun,"(a)")         "   Content of the current NEXUS type "
        write(lun,"(a,/)")       "   --------------------------------- "
        write(lun,"(a,i6)")      "           Numor: ", nexus%run_number
        write(lun,"(a)")         " Instrument_Name: "//trim(nexus%instrument_name)
        write(lun,"(a)")         "   Local_Contact: "//trim(nexus%local_contact  )
        write(lun,"(a)")         "            User: "//trim(nexus%user           )
        write(lun,"(a)")         "   Experiment_id: "//trim(nexus%experiment_id  )
        write(lun,"(a)")         "          Filcod: "//trim(nexus%filcod         )
        write(lun,"(a)")         "        Filename: "//trim(nexus%filename       )
        write(lun,"(a)")         "   Data_Ordering: "//trim(nexus%data_ordering  )
        write(lun,"(a)")         "        End_Time: "//trim(nexus%end_time       )
        write(lun,"(a)")         "        Geometry: "//trim(nexus%geometry       )
        write(lun,"(a)")         "       Scan_Type: "//trim(nexus%scan_type      )
        write(lun,"(a)")         "          Source: "//trim(nexus%source         )
        write(lun,"(a,i8)")      "           Manip: ", nexus%manip
        write(lun,"(a,i8)")      "              Nz: ", nexus%nz
        write(lun,"(a,i8)")      "              Nx: ", nexus%nx
        write(lun,"(a,i8)")      "         Nframes: ", nexus%nf
        write(lun,"(a,f8.5)")    "      Wavelength: ", nexus%wave
        write(lun,"(a,f8.3)")    "      f-coupling: ", nexus%fcoupling
        write(lun,"(a,f8.3)")    "       Mag.Field: ", nexus%magnetic_field
        write(lun,"(a,3f5.2,a)") "             hkl: (", nexus%reflection(:),")"
        write(lun,"(a,f8.3)")    "       Reg.Temp.: ", nexus%reg_temperature
        write(lun,"(a,f8.3)")    "       Set.Temp.: ", nexus%setp_temperature
        write(lun,"(a,f8.3)")    "     Temperature: ", nexus%temperature
        write(lun,"(a,f8.3)")    "      Scan_start: ", nexus%scan_start
        write(lun,"(a,f8.3)")    "       Scan_step: ", nexus%scan_step
        write(lun,"(a,f8.3)")    "      Scan_width: ", nexus%scan_width
        write(lun,"(a,f8.3)")    "    Virtual-cgap: ", nexus%virtual_cgap
        write(lun,"(/,a)")       "        LOGICALS:"
        write(lun,"(a)") &
        "     is_canne  is_chi   is_gamma is_mode is_monitor is_nu  is_omega  is_phi  is_psi   is_timef  is_tth is_virtual is_ub  is_total_counts"
        write(lun,"(14L9/)") nexus%is_canne,nexus%is_chi,nexus%is_gamma,nexus%is_mode,nexus%is_monitor,nexus%is_nu,nexus%is_omega,nexus%is_phi, &
        nexus%is_psi,nexus%is_timef,nexus%is_tth,nexus%is_virtual,nexus%is_ub,nexus%is_total_counts
        if(nexus%is_ub) then
           write(lun,"(a)")     "       UB-matrix: "
           do i=1,3
             write(lun,"(tr10,3f14.6)") nexus%ub(i,:)
           end do
        end if
        j=min(5,nexus%nf)
        if( j == 1) then
          ffirst="  First value"
        else
          ffirst="  First 5 values"
        end if
        if (allocated(nexus%monitor)) then
          write(lun,"(a)")     ffirst//" of Monitor:"
          write(lun,"(5f12.2)")  nexus%monitor(1:j)
        else
            write(lun,"(a)")  "  => Monitor not allocated!"
        end if
        if (allocated(nexus%timef))  then
          write(lun,"(a)")  ffirst//" of Time:"
          write(lun,"(5f12.4)")  nexus%timef(1:j)
        else
            write(lun,"(a)")  "  => Timef not allocated!"
        end if
        if (allocated(nexus%total_counts)) then
          write(lun,"(a)")  ffirst//" of Total_Counts:"
          write(lun,"(5f12.4)")  nexus%total_counts(1:j)
        else
            write(lun,"(a)")  "  => Total_Counts not allocated!"
        end if
        if (allocated(nexus%angles))  then
           write(lun,"(a)")  ffirst//" List of values of Angles:"
           do i=1,j
             write(lun,"(8f12.4)")  nexus%angles(:,i)
           end do
        else
            write(lun,"(a)")  "  => Angles not allocated!"
        end if
        if (allocated(nexus%counts))  then
           write(lun,"(a,i1,a)")  "  Max-values of counts of the first ",j," frames"
           do i=1,j
               write(lun,"(2(a,i5))") "   Frame #",i,"  MaxVal (counts(:,:)) =", maxval(nexus%counts(:,:,i))
           end do
        else
            write(lun,"(a)")  "  => Counts not allocated!"
        end if
        write(lun,"(a,/)")    "   ------------------------------------------ "

     end subroutine display_nexus

    subroutine initialize_nexus(nexus,dims)

        !---- Arguments ----!
        type(nexus_type),                intent(in out)  :: nexus
        integer, dimension(:), optional, intent(in)      :: dims ! [nz,nx,nf]

        nexus%manip = 0
        nexus%is_4D = .false.
        if(present(dims)) then
            if (size(dims) == 4) then
                nexus%is_4D = .true.
                nexus%nz = dims(1)
                nexus%nx = dims(2)
                nexus%nf = dims(3)
                nexus%n4 = dims(4)
            else
                nexus%nz = dims(1)
                nexus%nx = dims(2)
                nexus%nf = dims(3)
                nexus%n4 = 0
            end if
        end if
        nexus%run_number        = 0
        nexus%fcoupling         = 0.0
        nexus%magnetic_field    = 0.0
        nexus%reflection(:)     = 0.0
        nexus%reg_temperature   = 0.0
        nexus%scan_start        = 0.0
        nexus%scan_step         = 0.0
        nexus%scan_width        = 0.0
        nexus%setp_temperature  = 0.0
        nexus%temperature       = 0.0
        nexus%ub(:,:)           = 0.0
        nexus%virtual_cgap      = 0.0
        nexus%wave              = 0.0
        nexus%data_ordering     = ''
        nexus%end_time          = ''
        nexus%experiment_id     = ''
        nexus%filcod            = ''
        nexus%filename          = ''
        nexus%geometry          = '4C'
        nexus%instrument_name   = ''
        nexus%local_contact     = ''
        nexus%scan_type         = ''
        nexus%source            = ''
        nexus%user              = ''
        nexus%is_actual_step    = .false.
        nexus%is_canne          = .false.
        nexus%is_chi            = .false.
        nexus%is_gamma          = .false.
        nexus%is_mode           = .false.
        nexus%is_monitor        = .false.
        nexus%is_nu             = .false.
        nexus%is_omega          = .false.
        nexus%is_phi            = .false.
        nexus%is_psi            = .false.
        nexus%is_timef          = .false.
        nexus%is_tth            = .false.
        nexus%is_total_counts   = .false.
        nexus%is_ub             = .false.
        nexus%is_virtual        = .false.

        if (allocated(nexus%monitor))      deallocate(nexus%monitor)
        if (allocated(nexus%timef))        deallocate(nexus%timef)
        if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
        if (allocated(nexus%counts4D))     deallocate(nexus%counts4D)
        if (allocated(nexus%angles))       deallocate(nexus%angles)

        if(present(dims)) then
            allocate(nexus%monitor(dims(3)))
            nexus%monitor=0.0
            allocate(nexus%timef(dims(3)))
            nexus%timef=0.0
            allocate(nexus%total_counts(dims(3)))
            nexus%total_counts=0.0
            allocate(nexus%angles(8,dims(3)))
            nexus%angles=0.0
            if (.not. nexus%is_4D) then
                if (allocated(nexus%counts)) deallocate(nexus%counts)
                allocate(nexus%counts(dims(1),dims(2),dims(3)))
                nexus%counts=0
            else
                if (allocated(nexus%counts4D)) deallocate(nexus%counts4D)
                allocate(nexus%counts4D(dims(1),dims(2),dims(3),dims(4)))
                nexus%counts4D=0
            end if
           nexus%counts=0
        end if
    end subroutine initialize_nexus

    ! This subrotuine make a copy of the parts of nexus type
    ! not depending on dimensions of data
    subroutine partial_nexus_copy(nexus1,nexus2)
        type(nexus_type), intent(in)      :: nexus1
        type(nexus_type), intent(in out)  :: nexus2

        nexus2%manip             =  nexus1%manip
        nexus2%run_number        =  nexus1%run_number
        nexus2%fcoupling         =  nexus1%fcoupling
        nexus2%magnetic_field    =  nexus1%magnetic_field
        nexus2%reflection(:)     =  nexus1%reflection(:)
        nexus2%reg_temperature   =  nexus1%reg_temperature
        nexus2%scan_start        =  nexus1%scan_start
        nexus2%scan_step         =  nexus1%scan_step
        nexus2%scan_width        =  nexus1%scan_width
        nexus2%setp_temperature  =  nexus1%setp_temperature
        nexus2%temperature       =  nexus1%temperature
        nexus2%ub(:,:)           =  nexus1%ub(:,:)
        nexus2%virtual_cgap      =  nexus1%virtual_cgap
        nexus2%wave              =  nexus1%wave
        nexus2%data_ordering     =  nexus1%data_ordering
        nexus2%end_time          =  nexus1%end_time
        nexus2%experiment_id     =  nexus1%experiment_id
        nexus2%filcod            =  nexus1%filcod
        nexus2%filename          =  nexus1%filename
        nexus2%geometry          =  nexus1%geometry
        nexus2%instrument_name   =  nexus1%instrument_name
        nexus2%local_contact     =  nexus1%local_contact
        nexus2%scan_type         =  nexus1%scan_type
        nexus2%source            =  nexus1%source
        nexus2%user              =  nexus1%user
        nexus2%is_canne          =  nexus1%is_canne
        nexus2%is_chi            =  nexus1%is_chi
        nexus2%is_gamma          =  nexus1%is_gamma
        nexus2%is_mode           =  nexus1%is_mode
        nexus2%is_monitor        =  nexus1%is_monitor
        nexus2%is_nu             =  nexus1%is_nu
        nexus2%is_omega          =  nexus1%is_omega
        nexus2%is_phi            =  nexus1%is_phi
        nexus2%is_psi            =  nexus1%is_psi
        nexus2%is_timef          =  nexus1%is_timef
        nexus2%is_tth            =  nexus1%is_tth
        nexus2%is_total_counts   =  nexus1%is_total_counts
        nexus2%is_ub             =  nexus1%is_ub
        nexus2%is_virtual        =  nexus1%is_virtual

    end subroutine partial_nexus_copy

    subroutine read_calibration(filename,path,machine,calibration)

        ! Arguments
        character(len=*),                  intent(in)  :: filename
        character(len=*),                  intent(in)  :: path
        character(len=*),                  intent(in)  :: machine
        type(Calibration_Detector_Type),  intent(out)  :: calibration


        ! Local variables
        integer :: i,j,hdferr,nx,nz
        integer(HID_T) :: file_id,dset,space
        integer(HSIZE_T), dimension(3) :: dims,dims_
        real, parameter :: epsilon = 0.001
        logical :: exist
        real, dimension(:,:), allocatable :: calib

        ! Check that nexus file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            err_nexus = .true.
            war_nexus_mess = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = "read_calibration: error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        if (hdferr /= -1) call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        if (hdferr /= -1) then
            call h5fopen_f(trim(filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = "read_calibration: error opening nexus file"
                return
            end if
        end if

        ! Get calibration
        call h5dopen_f(file_id,path,dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_calibration: wrong path.'
            return
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims_,hdferr)
        !   Assign memory to arrays and read data
        if (hdferr /= -1) then
            nx = dims(1)
            nz = dims(2)
            allocate(calib(nx,nz))
            call h5dread_f(dset,H5T_NATIVE_REAL,calib,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        else
            err_nexus = .true.
            err_nexus_mess = 'read_calibration: calibration cannot be applied, only implemented for d2b.'
            return
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_calibration: error reading calibration data.'
            return
        end if

        ! Close NEXUS file.
        call h5fclose_f(file_id,hdferr)

        ! Close FORTRAN interface.
        call h5close_f(hdferr)

        !Setting up the calibration type
        !Type, public :: Calibration_Detector_Type
        !   character(len=12)                            :: Name_Instrm      ! Instrument Name
        !   integer                                      :: NDet             ! Number of Detectors
        !   integer                                      :: NPointsDet       ! Number of Points per Detector
        !   real(kind=cp), dimension(:),   allocatable   :: PosX             ! Relative Positions of each Detector
        !   real(kind=cp), dimension(:,:), allocatable   :: Effic            ! Efficiency of each point detector (NpointsDetect,NDect)
        !   logical,       dimension(:,:), allocatable   :: Active           ! Flag for active points on detector
        !End Type Calibration_Detector_Type
        calibration%Name_Instrm=machine
        calibration%NDet=size(calib,2)
        calibration%NPointsDet=size(calib,1)
        if(allocated(calibration%PosX)) deallocate(calibration%PosX)
        if(allocated(calibration%Effic)) deallocate(calibration%Effic)
        if(allocated(calibration%Active)) deallocate(calibration%Active)
        allocate(calibration%PosX(calibration%NDet))
        allocate(calibration%Effic(calibration%NPointsDet,calibration%NDet))
        allocate(calibration%Active(calibration%NPointsDet,calibration%NDet))
        calibration%Active=.true.; calibration%PosX=0.0
        ! Replace zeros by epsilon and make Active false
        do i = 1 , size(calib,1)
            do j = 1 , size(calib,2)
                if (abs(calib(i,j)) < epsilon) then
                    calib(i,j) = epsilon
                    calibration%Active=.false.
                end if
            end do
        end do

        Select Case(trim(machine))
          case("D1B")
               calibration%PosX(:)=[((i-1)*0.1, i=1,calibration%NDet)]
          case("D20")
               calibration%PosX(:)=[((i-1)*0.05, i=1,calibration%NDet)]
          case("D2B")
               calibration%PosX(:)=[((i-1)*1.25, i=1,calibration%NDet)]
        End Select

    end subroutine read_calibration

    subroutine read_nexus(filename,nexus,source,raw)

        ! Arguments
        character(len=*), intent(in)  :: filename
        type(nexus_type), intent(out) :: nexus
        character(len=*), intent(in), optional :: source
        logical,          intent(in), optional :: raw

        ! Local variables
        integer :: i
        logical :: exist
        character(len=:), allocatable :: basename
        character(len=:), allocatable :: lsource

        ! Initialize variables
        err_nexus  = .false.
        call initialize_nexus(nexus)

        ! Check that nexus file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus: file '//trim(filename)//' not found'
            return
        end if

        ! Extract the file code
        nexus%filename = filename
        basename = Get_Filename(nexus%filename)
        i = index(basename,'.')
        if (i == 0) then
            nexus%filcod = basename
        else
            nexus%filcod = basename(1:i-1)
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = "read_nexus: error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        if (hdferr /= -1) call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        if (hdferr /= -1) then
            call h5fopen_f(trim(nexus%filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = "read_nexus: error opening nexus file: "//trim(filename)
                call h5close_f(hdferr)
                return
            end if
        end if

        ! Get source
        if (present(source)) then
            nexus%source = lsource
        else
            call get_source(nexus%source)
        end if
        if (nexus%source == 'ill') then
            if(present(raw)) then
              call read_nexus_ILL(nexus,raw)
            else
              call read_nexus_ILL(nexus)
            end if
        else if (nexus%source == 'psi-zebra') then
            call read_nexus_PSI_Zebra(nexus)
        else if (nexus%source == 'psi-dmc') then
            call read_nexus_PSI_DMC(nexus)
        else if (nexus%source == 'ansto-wombat') then
            call read_nexus_ANSTO_Wombat(nexus)
        else
            err_nexus = .true.
            err_nexus_mess = "read_nexus: unknown source"
            call h5fclose_f(file_id,hdferr)
            call h5close_f(hdferr)
            return
        end if
        nexus%nbdata = nexus%nx * nexus%nz

        ! Close NEXUS file anf FORTRAN interface
        call h5fclose_f(file_id,hdferr)
        call h5close_f(hdferr)

    end subroutine read_nexus

    subroutine write_simple_nexus(namef,nexus,novirtual,raw)
        ! Arguments
        character(len=*), intent(in)  :: namef
        type(nexus_type), intent(in)  :: nexus
        logical, optional, intent(in) :: novirtual
        logical, optional, intent(in) :: raw

        ! Local variables
        integer, parameter             :: NVARS = 6
        character(len=15), target      :: MODE='FOURCIRCLE_MODE'
        integer                        :: i,hdferr,filter_info,filter_info_both,nv,nh,nf
        integer(HID_T)                 :: filen,filetype,group,subgroup,space,dset,dcpl ! handles
        integer(SIZE_T)                :: length
        integer(HSIZE_T), dimension(1) :: dims_1D
        integer(HSIZE_T), dimension(2) :: dims_2D,chunk_2D
        integer(HSIZE_T), dimension(3) :: dims_3D,chunk_3D
        integer(HSIZE_T), dimension(2) :: var_dims = (/14,6/)
        integer(SIZE_T), dimension(NVARS)  :: str_len = (/5,5,3,3,8,14/)
        !!integer, dimension(NVARS)      :: scanned
        real, dimension(:), allocatable :: val
        real, dimension(:,:), allocatable :: scanned_data
        character(len=14), dimension(NVARS), target :: &
            var_names = (/"omega         ", "gamma         ", "chi           ", "phi           ","Monitor1      ","AcquisitionSpy"/)
        character(len=100), dimension(:), allocatable :: paths
        character(len=:), allocatable, target :: inst_name,data_ordering
        logical :: avail
        type(C_PTR) :: f_ptr

        ! Initialize Fortran interface
        call h5open_f(hdferr)

        !  Check if gzip compression is available and can be used for both
        !  compression and decompression.  Normally we do not perform error
        !  checking in these examples for the sake of clarity, but in this
        !  case we will make an exception because this filter is an
        !  optional part of the hdf5 library.
        !
        call h5zfilter_avail_f(H5Z_FILTER_DEFLATE_F, avail, hdferr)
        if (.not. avail) then
            write(*,'("gzip filter not available.",/)')
            stop
        end if
        call h5zget_filter_info_f(H5Z_FILTER_DEFLATE_F, filter_info, hdferr)
        filter_info_both = IOR(H5Z_FILTER_ENCODE_ENABLED_F,H5Z_FILTER_DECODE_ENABLED_F)
        if (filter_info .ne. filter_info_both) then
            write(*,'("gzip filter not available for encoding and decoding.",/)')
            stop
        end if

        if (trim(nexus%instrument_name) == '') then
            inst_name = current_instrm%name_inst
        else
            inst_name = nexus%instrument_name
        end if
        if (trim(nexus%data_ordering) == '') then
            data_ordering = current_instrm%data_ordering
        else
            data_ordering = nexus%data_ordering
        end if

        ! Create the hdf5 file
        call h5fcreate_f(namef,H5F_ACC_TRUNC_F,filen,hdferr)

        ! Create Nexus structure
        !   /entry0
        !   /entry0/instrument
        !   /entry0/instrument/Det1
        !   /entry0/instrument/gamma
        !   /entry0/instrument/nu
        !   /entry0/data_scan
        !   /entry0/data_scan/detector_data
        call h5gcreate_f(filen,'entry0',group,hdferr)
        call h5gcreate_f(group,'instrument',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'data_scan',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)
        call h5gopen_f(filen,'entry0/instrument',group,hdferr)
        i = index(inst_name,'19')
        if (i < 1) then
            call h5gcreate_f(group,'Detector',subgroup,hdferr)
        else
            call h5gcreate_f(group,'Det1',subgroup,hdferr)
            call h5gcreate_f(group,'SingleCrystalSettings',subgroup,hdferr)
            call h5gclose_f(subgroup,hdferr)
        end if
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'gamma',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'nu',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)
        call h5gopen_f(filen,'entry0/data_scan',group,hdferr)
        call h5gcreate_f(group,'detector_data',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'scanned_variables',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)
        call h5gopen_f(filen,'entry0/data_scan/scanned_variables',group,hdferr)
        call h5gcreate_f(group,'variables_names',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)

        ! Write wavelength
        dims_1D = 1
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        call h5gopen_f(filen,'entry0/',group,hdferr)
        call h5dcreate_f(group,'wavelength',H5T_NATIVE_REAL,space,dset,hdferr)
        !   Write the data to the dataset
        call h5dwrite_f(dset,H5T_NATIVE_REAL,nexus%wave,dims_1D,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)

        ! Write instrument name
        dims_1D = 1
        !   Create file datatype
        length = len(trim(inst_name))
        call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
        call h5tset_size_f(filetype,length,hdferr)
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        !   Create the dataset and write the string data to it
        call h5gopen_f(filen,'entry0/instrument',group,hdferr)
        call h5dcreate_f(group,'name',filetype,space,dset,hdferr)
        f_ptr = C_LOC(inst_name(1:1))
        call h5dwrite_f(dset,filetype,f_ptr,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)
        call h5tclose_f(filetype,hdferr)

        ! Write data ordering
        ! Write instrument name
        dims_1D = 1
        !   Create file datatype
        length = len(trim(data_ordering))
        call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
        call h5tset_size_f(filetype,length,hdferr)
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        !   Create the dataset and write the string data to it
        i = index(inst_name,'19')
        if (i < 1) then
            call h5gopen_f(filen,'entry0/instrument/Detector',group,hdferr)
        else
            call h5gopen_f(filen,'entry0/instrument/Det1',group,hdferr)
        end if
        call h5dcreate_f(group,'data_ordering',filetype,space,dset,hdferr)
        f_ptr = C_LOC(data_ordering(1:1))
        call h5dwrite_f(dset,filetype,f_ptr,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)
        call h5tclose_f(filetype,hdferr)

        ! Write virtual cgap
        if (.not. present(novirtual)) then
            dims_1D = 1
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            i = index(inst_name,'19')
            if (i < 1) then
                call h5gopen_f(filen,'entry0/instrument/Detector',group,hdferr)
            else
                call h5gopen_f(filen,'entry0/instrument/Det1',group,hdferr)
            end if
            call h5dcreate_f(group,'virtual_cgap',H5T_NATIVE_REAL,space,dset,hdferr)
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_REAL,nexus%virtual_cgap,dims_1D,hdferr)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
        else if (index(inst_name,'19') > -1) then
            ! Write some additional information for D19
            dims_1D = NVARS
            !!scanned(1:4) = 0
            !!scanned(5) = 1
            !!scanned(6) = 1
            !!if (nexus%manip == 1) then
            !!    scanned(2) = 1
            !!else if (nexus%manip == 2) then
            !!    scanned(1) = 1
            !!    if (nexus%nbang == 2) scanned(2) = 1
            !!else if (nexus%manip == 3) then
            !!    scanned(3) = 1
            !!else if (nexus%manip == 4) then
            !!    scanned(4) = 1
            !!end if
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            call h5gopen_f(filen,'entry0/data_scan/scanned_variables/variables_names',group,hdferr)
            !!call h5dcreate_f(group,'scanned',H5T_NATIVE_INTEGER,space,dset,hdferr)
            !   Write the data to the dataset
            !!call h5dwrite_f(dset,H5T_NATIVE_INTEGER,scanned,dims_1D,hdferr)
            !   Close and release resources
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
            !   Create file datatype
            !call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
            !call h5tset_size_f(filetype,VARLEN,hdferr)
            call h5tcopy_f(H5T_STRING,filetype,hdferr)
            call H5Tset_strpad_f(filetype,H5T_STR_NULLPAD_F,hdferr)
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            !   Create the dataset and write the string data to it
            call h5dcreate_f(group,'name',filetype,space,dset,hdferr)
            !f_ptr = C_LOC(varnames(1)(1:1))
            !call h5dwrite_f(dset,filetype,f_ptr,hdferr)
            call h5dwrite_vl_f(dset,filetype,var_names,var_dims,str_len,hdferr,space)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
            call h5tclose_f(filetype,hdferr)
            allocate(scanned_data(nexus%nf,NVARS))
            do i = 1 , nexus%nf
                scanned_data(i,1) = nexus%angles(3,i) ! Omega
                scanned_data(i,2) = nexus%angles(4,i) ! Gamma
                scanned_data(i,3) = nexus%angles(2,i) ! Chi
                scanned_data(i,4) = nexus%angles(1,i) ! Phi
                scanned_data(i,5) = nexus%monitor(i)
                scanned_data(i,6) = nexus%timef(i)
            end do
            dims_2D = (/ nexus%nf,NVARS /)
            chunk_2D = (/ NVARS, 1 /)
            !   Create the dataset creation property list, add the gzip
            !   compression filter and set the chunk size.
            call h5pcreate_f(H5P_DATASET_CREATE_F,dcpl,hdferr)
            call h5pset_deflate_f(dcpl,6,hdferr)
            call h5pset_chunk_f(dcpl,2,chunk_2D,hdferr)
            !   Create dataspace
            call h5screate_simple_f(2,dims_2D,space,hdferr)
            call h5gopen_f(filen,'entry0/data_scan/scanned_variables',group,hdferr)
            call h5dcreate_f(group,'data',H5T_NATIVE_REAL,space,dset,hdferr,dcpl)
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_REAL,scanned_data,dims_2D,hdferr)
            !   Close and release resources
            call h5pclose_f(dcpl,hdferr)
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
            ! Write mode: we assume four circle
            dims_1D = 1
            !   Create file datatype
            length = len(trim(MODE))
            call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
            call h5tset_size_f(filetype,length,hdferr)
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            !   Create the dataset and write the string data to it
            call h5gopen_f(filen,'entry0/instrument/SingleCrystalSettings',group,hdferr)
            call h5dcreate_f(group,'mode_label',filetype,space,dset,hdferr)
            f_ptr = C_LOC(MODE)
            call h5dwrite_f(dset,filetype,f_ptr,hdferr)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
            call h5tclose_f(filetype,hdferr)
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            call h5gopen_f(filen,'entry0/instrument/SingleCrystalSettings',group,hdferr)
            call h5dcreate_f(group,'mode',H5T_NATIVE_INTEGER,space,dset,hdferr)
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_INTEGER,1,dims_1D,hdferr)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
        end if

        ! Write gamma and nu of the detector
        allocate(paths(2),val(2))
        paths(1) = 'entry0/instrument/gamma'
        paths(2) = 'entry0/instrument/nu'
        val(1) = nexus%angles(4,1) !ga_Dv
        val(2) = nexus%angles(7,1) !nu_Dv
        do i = 1 , 2
            dims_1D = 1
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            call h5gopen_f(filen,trim(paths(i)),group,hdferr)
            call h5dcreate_f(group,'value',H5T_NATIVE_REAL,space,dset,hdferr)
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_REAL,val(i),dims_1D,hdferr)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
        end do

        ! Write counts
        if (.not. nexus%is_4D) then
            nv = size(nexus%counts,1)
            nh = size(nexus%counts,2)
            nf = size(nexus%counts,3)
            dims_3D = [nv,nh,nf]
            chunk_3D = [nv,nh,1]
            !   Create dataspace
            call h5screate_simple_f(3,dims_3D,space,hdferr)
            !   Create the dataset creation property list, add the gzip
            !   compression filter and set the chunk size.  !
            call h5pcreate_f(H5P_DATASET_CREATE_F,dcpl,hdferr)
            call h5pset_deflate_f(dcpl,6,hdferr)
            call h5pset_chunk_f(dcpl,3,chunk_3D,hdferr)
            !
            call h5gopen_f(filen,'entry0/data_scan/detector_data',group,hdferr)
            if (present(raw)) then
                i = index(inst_name,'19')
                if (i < 1) then
                    call h5dcreate_f(group,'raw_data',H5T_NATIVE_INTEGER,space,dset,hdferr,dcpl)
                else
                    call h5dcreate_f(group,'data_raw',H5T_NATIVE_INTEGER,space,dset,hdferr,dcpl)
                end if
            else
                call h5dcreate_f(group,'data',H5T_NATIVE_INTEGER,space,dset,hdferr,dcpl)
            end if
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims_3D,hdferr)
            !   Close and release resources
            call h5pclose_f(dcpl,hdferr)
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
        end if

        ! Close the hdf5 file
        call h5fclose_f(filen,hdferr)

    end subroutine write_simple_nexus

    subroutine write_vnexus(namef,ga_Dv,nu_Dv,avcounts,nexus)
        ! Arguments
        character(len=*),          intent(in) :: namef
        real,                      intent(in) :: ga_Dv,nu_Dv
        integer, dimension(:,:,:), intent(in) :: avcounts
        type(nexus_type),          intent(in) :: nexus

        ! Local variables
        integer :: i,hdferr,filter_info,filter_info_both,nv,nh
        integer(HID_T) :: filen,filetype,group,subgroup,space,dset,dcpl ! handles
        integer(SIZE_T) :: length
        integer(HSIZE_T), dimension(1) :: dims_1D
        integer(HSIZE_T), dimension(3) :: dims_3D,chunk_3D
        real, dimension(:), allocatable :: val
        character(len=100), dimension(:), allocatable :: paths
        character(len=:), allocatable, target :: inst_name,data_ordering
        logical :: avail
        type(C_PTR) :: f_ptr

        ! Initialize Fortran interface
        call h5open_f(hdferr)

        !  Check if gzip compression is available and can be used for both
        !  compression and decompression.  Normally we do not perform error
        !  checking in these examples for the sake of clarity, but in this
        !  case we will make an exception because this filter is an
        !  optional part of the hdf5 library.
        !
        call h5zfilter_avail_f(H5Z_FILTER_DEFLATE_F, avail, hdferr)
        if (.not. avail) then
            write(*,'("gzip filter not available.",/)')
            stop
        end if
        call h5zget_filter_info_f(H5Z_FILTER_DEFLATE_F, filter_info, hdferr)
        filter_info_both = IOR(H5Z_FILTER_ENCODE_ENABLED_F,H5Z_FILTER_DECODE_ENABLED_F)
        if (filter_info .ne. filter_info_both) then
            write(*,'("gzip filter not available for encoding and decoding.",/)')
            stop
        end if

        inst_name = current_instrm%name_inst

        ! Create the hdf5 file
        call h5fcreate_f(namef,H5F_ACC_TRUNC_F,filen,hdferr)

        ! Create Nexus structure
        !   /entry0
        !   /entry0/instrument
        !   /entry0/instrument/Det1
        !   /entry0/instrument/gamma
        !   /entry0/instrument/nu
        !   /entry0/data_scan
        !   /entry0/data_scan/detector_data
        call h5gcreate_f(filen,'entry0',group,hdferr)
        call h5gcreate_f(group,'instrument',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'data_scan',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)
        call h5gopen_f(filen,'entry0/instrument',group,hdferr)
        i = index(inst_name,'19')
        if (i < 1) then
            call h5gcreate_f(group,'Detector',subgroup,hdferr)
        else
            call h5gcreate_f(group,'Det1',subgroup,hdferr)
        end if
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'gamma',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gcreate_f(group,'nu',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)
        call h5gclose_f(group,hdferr)
        call h5gopen_f(filen,'entry0/data_scan',group,hdferr)
        call h5gcreate_f(group,'detector_data',subgroup,hdferr)
        call h5gclose_f(subgroup,hdferr)

        ! Write wavelength
        dims_1D = 1
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        call h5gopen_f(filen,'entry0/wavelength',group,hdferr)
        call h5dcreate_f(group,'value',H5T_NATIVE_REAL,space,dset,hdferr)
        !   Write the data to the dataset
        call h5dwrite_f(dset,H5T_NATIVE_REAL,nexus%wave,dims_1D,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)

        ! Write instrument name
        dims_1D = 1
        !   Create file datatype
        length = len(trim(current_instrm%name_inst))
        call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
        call h5tset_size_f(filetype,length,hdferr)
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        !   Create the dataset and write the string data to it
        call h5gopen_f(filen,'entry0/instrument',group,hdferr)
        call h5dcreate_f(group,'name',filetype,space,dset,hdferr)
        f_ptr = C_LOC(inst_name(1:1))
        call h5dwrite_f(dset,filetype,f_ptr,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)
        call h5tclose_f(filetype,hdferr)

        ! Write data ordering
        ! Write instrument name
        dims_1D = 1
        !   Create file datatype
        length = len(trim(current_instrm%data_ordering))
        call h5tcopy_f(h5t_FORTRAN_S1,filetype,hdferr)
        call h5tset_size_f(filetype,length,hdferr)
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        !   Create the dataset and write the string data to it
        i = index(inst_name,'19')
        if (i < 1) then
            call h5gopen_f(filen,'entry0/instrument/Detector',group,hdferr)
        else
            call h5gopen_f(filen,'entry0/instrument/Det1',group,hdferr)
        end if
        call h5dcreate_f(group,'data_ordering',filetype,space,dset,hdferr)
        data_ordering = current_instrm%data_ordering
        f_ptr = C_LOC(data_ordering(1:1))
        call h5dwrite_f(dset,filetype,f_ptr,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)
        call h5tclose_f(filetype,hdferr)

        ! Write virtual cgap
        dims_1D = 1
        !   Create dataspace
        call h5screate_simple_f(1,dims_1D,space,hdferr)
        i = index(inst_name,'19')
        if (i < 1) then
            call h5gopen_f(filen,'entry0/instrument/Detector',group,hdferr)
        else
            call h5gopen_f(filen,'entry0/instrument/Det1',group,hdferr)
        end if
        call h5dcreate_f(group,'virtual_cgap',H5T_NATIVE_REAL,space,dset,hdferr)
        !   Write the data to the dataset
        call h5dwrite_f(dset,H5T_NATIVE_REAL,nexus%virtual_cgap,dims_1D,hdferr)
        !   Close and release resources
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)

        ! Write gamma and nu of the detector
        allocate(paths(2),val(2))
        paths(1) = 'entry0/instrument/gamma'
        paths(2) = 'entry0/instrument/nu'
        val(1) = ga_Dv
        val(2) = nu_Dv
        do i = 1 , 2
            dims_1D = 1
            !   Create dataspace
            call h5screate_simple_f(1,dims_1D,space,hdferr)
            call h5gopen_f(filen,trim(paths(i)),group,hdferr)
            call h5dcreate_f(group,'value',H5T_NATIVE_REAL,space,dset,hdferr)
            !   Write the data to the dataset
            call h5dwrite_f(dset,H5T_NATIVE_REAL,val(i),dims_1D,hdferr)
            !   Close and release resources
            call h5gclose_f(group,hdferr)
            call h5dclose_f(dset,hdferr)
            call h5sclose_f(space,hdferr)
        end do

        ! Write counts
        nv = size(avcounts,1)
        nh = size(avcounts,2)
        dims_3D = [nv,nh,1]
        chunk_3D = [nv,nh,1]
        !   Create dataspace
        call h5screate_simple_f(3,dims_3D,space,hdferr)
        !   Create the dataset creation property list, add the gzip
        !   compression filter and set the chunk size.  !
        call h5pcreate_f(H5P_DATASET_CREATE_F,dcpl,hdferr)
        call h5pset_deflate_f(dcpl,6,hdferr)
        call h5pset_chunk_f(dcpl,3,chunk_3D,hdferr)
        !
        call h5gopen_f(filen,'entry0/data_scan/detector_data',group,hdferr)
        call h5dcreate_f(group,'data',H5T_NATIVE_INTEGER,space,dset,hdferr,dcpl)
        !   Write the data to the dataset
        call h5dwrite_f(dset,H5T_NATIVE_INTEGER,avcounts,dims_3D,hdferr)
        !   Close and release resources
        call h5pclose_f(dcpl,hdferr)
        call h5gclose_f(group,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5sclose_f(space,hdferr)

        ! Close the hdf5 file
        call h5fclose_f(filen,hdferr)

    end subroutine write_vnexus

    ! Private procedures

    subroutine get_scan_type(nexus,motors)

        ! Arguments
        type(nexus_type),      intent(inout) :: nexus
        integer, dimension(8), intent(in)    :: motors

        ! Local variables
        integer :: i

        nexus%scan_type = 'unknown'
        nexus%nbang = 0
        do i = 1 , 8
            if (motors(i) == 1) nexus%nbang = nexus%nbang + 1
        end do
        if (nexus%nbang == 1) then
            if (motors(1) == 1) then
                nexus%manip = 4
                nexus%scan_type  = 'phi'
                nexus%scan_start = nexus%angles(1,1)
                nexus%scan_width = nexus%angles(1,nexus%nf) - nexus%angles(1,1)
            else if (motors(2) == 1) then
                nexus%manip = 3
                nexus%scan_type = 'chi'
                nexus%scan_start = nexus%angles(2,1)
                nexus%scan_width = nexus%angles(2,nexus%nf) - nexus%angles(2,1)
            else if (motors(3) == 1) then
                nexus%manip = 2
                nexus%scan_type = 'omega'
                nexus%scan_start = nexus%angles(3,1)
                nexus%scan_width = nexus%angles(3,nexus%nf) - nexus%angles(3,1)
            else if (motors(4) == 1) then
                nexus%manip = 1
                nexus%scan_type = 'gamma'
                nexus%scan_start = nexus%angles(4,1)
                nexus%scan_width = nexus%angles(4,nexus%nf) - nexus%angles(4,1)
            else if (motors(5) == 1) then
                nexus%scan_type = 'psi'
                nexus%scan_start = nexus%angles(5,1)
                nexus%scan_width = nexus%angles(5,nexus%nf) - nexus%angles(5,1)
            else if (motors(6) == 1) then
                nexus%scan_type = 'canne'
                nexus%scan_start = nexus%angles(6,1)
                nexus%scan_width = nexus%angles(6,nexus%nf) - nexus%angles(6,1)
            else if (motors(7) == 1) then
                nexus%scan_type = 'nu'
                nexus%scan_start = nexus%angles(7,1)
                nexus%scan_width = nexus%angles(7,nexus%nf) - nexus%angles(7,1)
            else if (motors(8) == 1) then
                nexus%manip = 1
                nexus%scan_type = '2theta'
                nexus%scan_start = nexus%angles(8,1)
                nexus%scan_width = nexus%angles(8,nexus%nf) - nexus%angles(8,1)
            end if
            if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
        else if (nexus%nbang == 2) then
            if (motors(3) == 1 .and. motors(4) == 1 .or. motors(3) == 1 .and. motors(8) == 1) then
                nexus%manip = 2
                nexus%scan_type = 'omega'
                nexus%scan_start = nexus%angles(3,1)
                nexus%scan_width = nexus%angles(3,nexus%nf) - nexus%angles(3,1)
                if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                if (nexus%instrument_name == 'd10') then
                    nexus%fcoupling = (nexus%angles(8,nexus%nf)-nexus%angles(8,1)) / (nexus%angles(3,nexus%nf)-nexus%angles(3,1))
                else
                    nexus%fcoupling = (nexus%angles(4,nexus%nf)-nexus%angles(4,1)) / (nexus%angles(3,nexus%nf)-nexus%angles(3,1))
                end if
            else if (motors(3) == 1 .and. motors(4) == 1 .or. motors(4) == 1 .and. motors(8) == 1) then
                nexus%manip = 2
                nexus%scan_type = 'canne'
                nexus%scan_start = nexus%angles(6,1)
                nexus%scan_width = nexus%angles(6,nexus%nf) - nexus%angles(6,1)
                if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                if (nexus%instrument_name == 'd10') then
                    nexus%fcoupling = (nexus%angles(8,nexus%nf)-nexus%angles(8,1)) / (nexus%angles(6,nexus%nf)-nexus%angles(6,1))
                else
                    nexus%fcoupling = (nexus%angles(4,nexus%nf)-nexus%angles(4,1)) / (nexus%angles(6,nexus%nf)-nexus%angles(6,1))
                end if
            end if
        end if

    end subroutine get_scan_type

    subroutine get_source(lsource)

        ! Arguments
        character(len=:), allocatable, intent(out)   :: lsource

        ! Local variables
        integer(hid_t) :: grp_id

        call h5gopen_f(file_id,'entry0',grp_id,hdferr)
        if (hdferr /= -1) then
            lsource = 'ill'
            call h5gclose_f(grp_id,hdferr)
            return
        end if
        call h5gclose_f(grp_id,hdferr)
        call h5gopen_f(file_id,'entry/DMC',grp_id,hdferr)
        if (hdferr /= -1) then
            lsource = 'psi-dmc'
            call h5gclose_f(grp_id,hdferr)
            return
        end if
        call h5gclose_f(grp_id,hdferr)
        call h5gopen_f(file_id,'entry1',grp_id,hdferr)
        if (hdferr /= -1) then
            call h5gclose_f(grp_id,hdferr)
            call h5gopen_f(file_id,'entry1/ZEBRA',grp_id,hdferr)
            if (hdferr /= -1) then
                lsource = 'psi-zebra'
                call h5gclose_f(grp_id,hdferr)
                return
            end if
            call h5gclose_f(grp_id,hdferr)
            call h5gopen_f(file_id,'entry1/data',grp_id,hdferr)
            if (hdferr /= -1) then
                lsource = 'ansto-wombat'
                call h5gclose_f(grp_id,hdferr)
                return
            end if
        end if
        call h5gclose_f(grp_id,hdferr)
        lsource = 'unknown'

    end subroutine get_source

    subroutine read_nexus_ansto_wombat(nexus)

        ! Arguments
        type(nexus_type),  intent(inout) :: nexus

        ! Local variables
        integer :: i
        integer(hid_t) :: grp_id
        integer, dimension(8) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: monitor
        integer, dimension(:,:,:), allocatable :: counts
        real :: chi,gamma,omega,phi
        character(len=80) :: end_time,user
        character(len=:), allocatable :: omega_address

        ! Instrument name
        nexus%instrument_name = 'wombat'

        ! Wavelength: not specified in wombat hdf

        ! User name
        call h5dopen_f(file_id,'entry1/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user,dims,hdferr)
        if (hdferr /= -1) then
            i=index(user,char(0))
            nexus%user = user(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Local contact: not specified in wombat hdf

        ! End time:
        call h5dopen_f(file_id,'entry1/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        if (hdferr /= -1) then
            i=index(end_time,char(0))
            nexus%end_time = end_time(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Experiment identifier: not specified.

        ! Set-point temperature: not specified

        ! Regulation temperature: not specified

        ! Sample temperature: is an array, but in nexus is considered as an scalar.

        ! Magnetic field: is an array, but in nexus is considered as an scalar.

        ! Geometry: no keyword specifying the geometry. It must be guessed from
        ! hdf structure
        call h5dopen_f(file_id,'entry1/sample/euler_omega',dset,hdferr)
        if (hdferr /= -1) then
            nexus%geometry = '4C' ! 4-circle
        else
            nexus%geometry = 'NB' ! Normal Beam
        end if

        ! Data ordering: not specified

        ! Reflection : not specified

        ! UB-matrix: not specified

        ! Counts
        call h5dopen_f(file_id,'entry1/data/hmm_xy',dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ansto_wombat: data not found in nexus.'
            return
        end if
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        if (hdferr /= -1) then
            nexus%nz = dims(2)
            nexus%nx = dims(1)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(counts(nexus%nx,nexus%nz,nexus%nf))
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,counts,dims,hdferr)
            do i = 1 , nexus%nf
                nexus%counts(:,:,i) = transpose(counts(:,:,i))
            end do
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ansto_wombat: error reading counts.'
            return
        end if

        ! Scan variables
        if (allocated(nexus%angles)) deallocate(nexus%angles)
        allocate(nexus%angles(8,nexus%nf))
        motors(:) = 0
        ! Monitor
        call h5dopen_f(file_id,'entry1/monitor/bm1_counts',dset,hdferr)
        if (hdferr == 0) then
            if (allocated(nexus%monitor)) deallocate(nexus%monitor)
            if (allocated(monitor)) deallocate(monitor)
            allocate(nexus%monitor(nexus%nf),monitor(nexus%nf))
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_INTEGER,monitor(:),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            nexus%monitor(:) = monitor(:)
            nexus%is_monitor = .true.
        end if
        ! Phi
        call h5dopen_f(file_id,'entry1/sample/euler_phi',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,phi,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(1,:) = phi
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(1,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_phi = .true.
                if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(1) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Chi
        call h5dopen_f(file_id,'entry1/sample/euler_chi',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,chi,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(2,:) = chi
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(2,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_chi = .true.
                if (abs(nexus%angles(2,nexus%nf)-nexus%angles(2,1)) > MIN_SCAN_ANGLE) motors(2) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Omega
        hdferr = 0
        if (nexus%geometry == '4C') then
            omega_address = 'entry1/sample/euler_omega'
        else if (nexus%geometry == 'NB') then
            call h5gopen_f(file_id,'entry1/sample/msom',grp_id,hdferr)
            if (hdferr /= -1 ) then
                omega_address = 'entry1/sample/msom'
            else
                call h5gopen_f(file_id,'entry1/sample/rotate',grp_id,hdferr)
                if (hdferr /= -1 ) omega_address = 'entry1/sample/rotate'
            end if
        end if
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            call h5dopen_f(file_id,omega_address,dset,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) then
                if (dims(1) == 1) then
                    call h5dread_f(dset,H5T_NATIVE_REAL,omega,scalar,hdferr)
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(3,:),dims,hdferr)
                end if
                if (hdferr /= -1) then
                    nexus%is_omega = .true.
                    if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) > MIN_SCAN_ANGLE) motors(3) = 1
                end if
                call h5dclose_f(dset,hdferr)
            end if
        end if
        ! Gamma
        call h5dopen_f(file_id,'entry1/sample/azimuthal_angle',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,gamma,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(4,:) = gamma + 60.0
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(4,:),dims,hdferr)
                if (hdferr /= -1) nexus%angles(4,:) = nexus%angles(4,:) + 60.0
            end if
            if (hdferr /= -1) then
                nexus%is_gamma = .true.
                if (abs(nexus%angles(4,nexus%nf)-nexus%angles(4,1)) > MIN_SCAN_ANGLE) motors(4) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Nu
        nexus%angles(7,:) = 0.0
        call get_scan_type(nexus,motors)

    end subroutine read_nexus_ansto_wombat

    subroutine read_nexus_ill(nexus,raw)

        ! Arguments
        type(nexus_type),  intent(in out) :: nexus
        logical, optional, intent(in)     :: raw

        ! Local variables
        integer :: i,mode,nvar
        integer, dimension(8) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        !!integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real :: phi_val,chi_val,omega_val,gamma_val,psi_val,canne_val,nu_val,tth_val,monitor
        real, dimension(9) :: ub
        real, dimension(:,:), allocatable :: datos
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names
        character(len=STR_MAX_LEN) :: namef
        character(len=30) :: data_ordering
        character(len=80) :: user,local_contact,end_time,experiment_id
        character(len=:), allocatable :: instrument_address

        ! Instrument name
        call h5dopen_f(file_id,'entry0/instrument/name',dset,hdferr)
        if (hdferr /= -1) then
            instrument_address = 'entry0/instrument'
            call h5dget_type_f(dset,filetype,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,filetype,namef,dims,hdferr)
            call h5dclose_f(dset,hdferr)
            if (hdferr /= -1) then
                i=index(namef,char(0))
                nexus%instrument_name = L_Case(namef(1:i-1))
            end if
        else
            call h5dopen_f(file_id,'entry0/XTREMD/name',dset,hdferr)
            if (hdferr /= -1) then
                nexus%instrument_name = 'xtremed'
                instrument_address = 'entry0/XTREMD'
                call h5dclose_f(dset,hdferr)
            else
                call h5dopen_f(file_id,'entry0/XTREMED/name',dset,hdferr)
                if (hdferr /= -1) then
                    nexus%instrument_name = 'xtremed'
                    instrument_address = 'entry0/XTREMED'
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            if (hdferr == -1) then
                call h5dopen_f(file_id,'entry0/D10/name',dset,hdferr)
                if (hdferr /= -1) then
                    nexus%instrument_name = 'd10'
                    instrument_address = 'entry0/D10'
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            if (hdferr == -1) then
                call h5dopen_f(file_id,'entry0/D16/name',dset,hdferr)
                if (hdferr /= -1) then
                    nexus%instrument_name = 'd16'
                    nexus%is_4D = .true. 
                    instrument_address = 'entry0/D16'
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            if (hdferr == -1) then
                call h5dopen_f(file_id,'entry0/D20/name',dset,hdferr)
                if (hdferr /= -1) then
                    nexus%instrument_name = 'd20'
                    nexus%is_4D = .true. 
                    instrument_address = 'entry0/D20'
                    call h5dclose_f(dset,hdferr)
                end if
            end if
        end if
        if (hdferr == -1) then
            call h5dclose_f(dset,hdferr)
            err_nexus = .true.
            err_nexus_mess = "read_nexus_ill: error getting instrument name"
            return
        end if

        ! Run number
        call h5dopen_f(file_id,'entry0/run_number',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%run_number,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        else ! D16
            call h5dopen_f(file_id,instrument_address//'/Beam/wavelength',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        end if
        call h5dclose_f(dset,hdferr)

        ! User name
        call h5dopen_f(file_id,'entry0/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user,dims,hdferr)
        if (hdferr /= -1) then
            i=index(user,char(0))
            nexus%user = user(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Local contact
        call h5dopen_f(file_id,'entry0/user/namelocalcontact',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,local_contact,dims,hdferr)
        if (hdferr /= -1) then
            i=index(local_contact,char(0))
            nexus%local_contact = local_contact(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! End time
        call h5dopen_f(file_id,'entry0/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        if (hdferr /= -1) then
            i=index(end_time,char(0))
            nexus%end_time = end_time(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Experiment identifier
        call h5dopen_f(file_id,'entry0/experiment_identifier',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,experiment_id,dims,hdferr)
        if (hdferr /= -1) then
            i=index(experiment_id,char(0))
            nexus%experiment_id = experiment_id(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Set-point temperature
        call h5dopen_f(file_id,'entry0/sample/setpoint_temperature',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%setp_temperature,scalar,hdferr)
        else
            call h5dopen_f(file_id,'entry0/sample/setp_temperature',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%setp_temperature,scalar,hdferr)
        end if
        call h5dclose_f(dset,hdferr)

        ! Regulation temperature
        call h5dopen_f(file_id,'entry0/sample/regulation_temperature',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reg_temperature,scalar,hdferr)
        else
            call h5dopen_f(file_id,'entry0/sample/reg_temperature',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reg_temperature,scalar,hdferr)
        end if
        call h5dclose_f(dset,hdferr)

        ! Sample temperature
        call h5dopen_f(file_id,'entry0/sample/temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%temperature,scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        ! If sample_temperature exists (new nexus), overwrite temperature
        call h5dopen_f(file_id,'entry0/sample/sample_temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%temperature,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Magnetic field
        call h5dopen_f(file_id,'entry0/sample/field',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%magnetic_field,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Geometry
        call h5dopen_f(file_id,instrument_address//'/SingleCrystalSettings/mode',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_INTEGER,mode,scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            if (mode == 0) then
                nexus%geometry = 'NB' ! Normal Beam
            else if (mode == 1) then
                nexus%geometry = '4C' ! Four Circle
            else if (mode == 2) then
                nexus%geometry = 'MT' ! Minimum Tilt
            else
                nexus%geometry = '??'
            end if
        else ! D16
            call h5dopen_f(file_id,instrument_address//'/mode',dset,hdferr)
            if (hdferr /= -1) then
                call h5dread_f(dset,H5T_NATIVE_INTEGER,mode,scalar,hdferr)
                call h5dclose_f(dset,hdferr)
                if (mode == 0) then
                    nexus%geometry = 'NB' ! Normal Beam
                else if (mode == 1) then
                    nexus%geometry = '4C' ! Four Circle
                else if (mode == 2) then
                    nexus%geometry = 'MT' ! Minimum Tilt
                else
                    nexus%geometry = '??'
                end if
            end if
        end if

        ! Actual step
        call h5dopen_f(file_id,'entry0/data_scan/actual_step',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%actual_step,scalar,hdferr)
            if (hdferr /= -1) nexus%is_actual_step = .true.
        end if
        call h5dclose_f(dset,hdferr)

        ! Data ordering
        call h5dopen_f(file_id,instrument_address//'/Detector/data_ordering',dset,hdferr)
        if (hdferr == -1) call h5dopen_f(file_id,instrument_address//'/Det1/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if

        ! Reflection
        call h5dopen_f(file_id,instrument_address//'/SingleCrystalSettings/actual_reflection',dset,hdferr)
        if (hdferr == -1) call h5dopen_f(file_id,instrument_address//'/SingleCrystalSettings/reflection',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection,dims,hdferr)
        call h5dclose_f(dset,hdferr)

        ! UB-matrix
        call h5dopen_f(file_id,instrument_address//'/SingleCrystalSettings/orientation_matrix',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ub,dims,hdferr)
        if (hdferr /= -1) then
            nexus%ub = transpose(reshape(ub,[3,3]))
            nexus%is_ub = .true.
        end if
        call h5dclose_f(dset,hdferr)

        ! Counts
        if (.not. nexus%is_4D) then
            if (present(raw)) then
                if (index(nexus%instrument_name,'19') > -1) then
                    call h5dopen_f(file_id,'entry0/data_scan/detector_data/data_raw',dset,hdferr)
                else
                    call h5dopen_f(file_id,'entry0/data_scan/detector_data/raw_data',dset,hdferr)
                end if
            else
                call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
                if (hdferr == -1) call h5dopen_f(file_id,'entry0/data_scan/detector_data/data1',dset,hdferr)
            end if
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill: data not found in nexus.'
                return
            end if
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
            if (hdferr /= -1) then
                nexus%nz = dims(1)
                nexus%nx = dims(2)
                nexus%nf = dims(3)
                if (allocated(nexus%counts)) deallocate(nexus%counts)
                allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
                call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
                call h5dclose_f(dset,hdferr)
            end if
        else ! Nexus 4D
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/data1',dset,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill: data not found in nexus.'
                return
            end if
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,dims4,hdferr)
            if (hdferr /= -1) then
                nexus%nz = dims(1)
                nexus%nx = dims(2)
                nexus%nf = dims(3)
                nexus%n4 = dims(4)
                if (allocated(nexus%counts4D)) deallocate(nexus%counts4D)
                allocate(nexus%counts4D(nexus%nz,nexus%nx,nexus%nf,nexus%n4))
                call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts4D,dims,hdferr)
                call h5dclose_f(dset,hdferr)
            end if
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill: error reading counts.'
            return
        end if

        ! Scan variables
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        !!if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)
            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            !!if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)
            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            !!if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            !!if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = [ STR_MAX_LEN , dims(1) ]
            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            !!if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)
            !   motors: phi,chi,omega,gamma,psi,canne,nu,2theta
            !           if motors(i) == 1, this motor is moved during the scan
            motors(:) = 0
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(8,nexus%nf))
            nexus%angles(:,:) = 0.0
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('phi','Phi')
                            nexus%is_phi = .true.
                            nexus%angles(1,:) = datos(:,i)
                            if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(1) = 1
                        case('chi','Chi')
                            nexus%is_chi = .true.
                            nexus%angles(2,:) = datos(:,i)
                            if (abs(nexus%angles(2,nexus%nf)-nexus%angles(2,1)) > MIN_SCAN_ANGLE) motors(2) = 1
                        case('omega','a3','Omega')
                            nexus%is_omega = .true.
                            nexus%angles(3,:) = datos(:,i)
                            if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) > MIN_SCAN_ANGLE) motors(3) = 1
                        case('gamma','Gamma')
                            nexus%is_gamma = .true.
                            nexus%angles(4,:) = datos(:,i)
                            if (abs(nexus%angles(4,nexus%nf)-nexus%angles(4,1)) > MIN_SCAN_ANGLE) motors(4) = 1
                        case('psi')
                            nexus%is_psi = .true.
                            nexus%angles(5,:) = datos(:,i)
                            if (abs(nexus%angles(5,nexus%nf)-nexus%angles(5,1)) > MIN_SCAN_ANGLE) motors(5) = 1
                        case('canne','canne-ech')
                            nexus%is_canne = .true.
                            nexus%angles(6,:) = datos(:,i)
                            if (abs(nexus%angles(6,nexus%nf)-nexus%angles(6,1)) > MIN_SCAN_ANGLE) motors(6) = 1
                        case('nu','Nu')
                            nexus%is_nu = .true.
                            nexus%angles(7,:) = datos(:,i)
                            if (abs(nexus%angles(7,nexus%nf)-nexus%angles(7,1)) > MIN_SCAN_ANGLE) motors(7) = 1
                        case('2theta','analyser')
                            nexus%is_tth = .true.
                            nexus%angles(8,:) = datos(:,i)
                            if (abs(nexus%angles(8,nexus%nf)-nexus%angles(8,1)) > MIN_SCAN_ANGLE) motors(8) = 1
                        case('monitor1','monitor_1','Monitor1')
                            nexus%is_monitor = .true.
                            if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                            allocate(nexus%monitor(nexus%nf))
                            nexus%monitor(:) = datos(:,i)
                        case('acquisitionspy','AcquisitionSpy')
                            nexus%is_timef = .true.
                            if (allocated(nexus%timef)) deallocate(nexus%timef)
                            allocate(nexus%timef(nexus%nf))
                            nexus%timef(:) = datos(:,i)
                        case('detector','multi','Detector1')
                            nexus%is_total_counts = .true.
                            if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
                            allocate(nexus%total_counts(nexus%nf))
                            nexus%total_counts(:) = datos(:,i)
                    end select
                end do
                if (.not. nexus%is_phi) then
                    call h5dopen_f(file_id,instrument_address//'/phi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_phi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(1,:) = phi_val
                    else
                        call h5dopen_f(file_id,instrument_address//'/Phi/value',dset,hdferr)
                        if (hdferr /= -1) then
                            nexus%is_phi = .true.
                            call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                            call h5dclose_f(dset,hdferr)
                            nexus%angles(1,:) = phi_val
                        end if
                    end if
                end if
                if (.not. nexus%is_chi) then
                    call h5dopen_f(file_id,instrument_address//'/chi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_chi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(2,:) = chi_val
                    else
                        call h5dopen_f(file_id,instrument_address//'/Chi/value',dset,hdferr)
                        if (hdferr /= -1) then
                            nexus%is_chi = .true.
                            call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                            call h5dclose_f(dset,hdferr)
                            nexus%angles(2,:) = chi_val
                        end if
                    end if
                end if
                if (.not. nexus%is_omega) then
                    call h5dopen_f(file_id,instrument_address//'/omega/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_omega = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,omega_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(3,:) = omega_val
                    else
                        call h5dopen_f(file_id,instrument_address//'/Omega/value',dset,hdferr)
                        if (hdferr /= -1) then
                            nexus%is_omega = .true.
                            call h5dread_f(dset,H5T_NATIVE_REAL,omega_val,scalar,hdferr)
                            call h5dclose_f(dset,hdferr)
                            nexus%angles(3,:) = omega_val
                        end if
                    end if
                end if
                if (.not. nexus%is_gamma) then
                    call h5dopen_f(file_id,instrument_address//'/gamma/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_gamma = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(4,:) = gamma_val
                    else
                        call h5dopen_f(file_id,instrument_address//'/Gamma/value',dset,hdferr)
                        if (hdferr /= -1) then
                            nexus%is_gamma = .true.
                            call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                            call h5dclose_f(dset,hdferr)
                            nexus%angles(4,:) = gamma_val
                        end if
                    end if
                end if
                if (.not. nexus%is_psi) then
                    call h5dopen_f(file_id,instrument_address//'/psi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_psi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,psi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(5,:) = psi_val
                    end if
                end if
                if (.not. nexus%is_canne .and. .not. nexus%is_omega) then
                    call h5dopen_f(file_id,instrument_address//'/canne/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_canne = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,canne_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(6,:) = canne_val
                    end if
                end if
                if (.not. nexus%is_nu) then
                    call h5dopen_f(file_id,instrument_address//'/nu/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_nu = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(7,:) = nu_val
                    else
                        call h5dopen_f(file_id,instrument_address//'/Nu/value',dset,hdferr)
                        if (hdferr /= -1) then
                            nexus%is_nu = .true.
                            call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                            call h5dclose_f(dset,hdferr)
                            nexus%angles(7,:) = nu_val
                        end if
                    end if
                end if
                if (.not. nexus%is_tth) then
                    call h5dopen_f(file_id,instrument_address//'/2theta/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_tth = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,tth_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(8,:) = tth_val
                    end if
                end if
                call get_scan_type(nexus,motors)
                if (allocated(datos)) deallocate(datos)
            end if
        else ! Virtual detector?
            call h5dopen_f(file_id,instrument_address//'/Detector/virtual_cgap',dset,hdferr)
            if (hdferr == -1) call h5dopen_f(file_id,instrument_address//'/Det1/virtual_cgap',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%virtual_cgap,scalar,hdferr)
            if (hdferr /= -1) then
                call h5dclose_f(dset,hdferr)
                nexus%is_virtual = .true.
                ! Read gamma
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(8,nexus%nf))
                if (L_Case(nexus%instrument_name) == 'xtremed') then
                    call h5dopen_f(file_id,instrument_address//'/2theta/value',dset,hdferr)
                else
                    call h5dopen_f(file_id,instrument_address//'/gamma/value',dset,hdferr)
                end if
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(4,:) = gamma_val
                if (hdferr /= -1) call h5dclose_f(dset,hdferr)
                ! Read nu
                if (hdferr /= -1) then
                    call h5dopen_f(file_id,instrument_address//'/nu/value',dset,hdferr)
                    if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                    if (hdferr /= -1) then
                        nexus%angles(7,:) = nu_val
                    else
                        nexus%angles(7,:) = 0.0
                    end if
                end if
                ! Read monitor_norm
                call h5dopen_f(file_id,'entry0/monitor_norm',dset,hdferr)
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,monitor,scalar,hdferr)
                if (hdferr /= -1) then
                    nexus%is_monitor = .true.
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(1))
                    nexus%monitor(1) = monitor
                end if
            else
                call h5dclose_f(dset,hdferr)
            end if
        end if

        !   Close the datasets
        call h5dclose_f(dset,hdferr)
        call h5dclose_f(dset2,hdferr)
        call h5dclose_f(dset3,hdferr)

    end subroutine read_nexus_ill

    subroutine read_nexus_psi_dmc(nexus)

        ! Arguments
        type(nexus_type),  intent(inout) :: nexus

        ! Local variables
        integer :: i
        integer, dimension(8) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: monitor
        integer, dimension(:,:,:), allocatable :: counts
        real :: gamma,omega
        character(len=80) :: end_time,user

        ! Instrument name
        nexus%instrument_name = 'dmc'

        ! Run number: in dmc, run number corresponds to proposal_id,
        ! which is a string instead of an integer. So it is not read.

        ! Wavelength
        call h5dopen_f(file_id,'entry/DMC/monochromator/wavelength',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! User name
        call h5dopen_f(file_id,'entry/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user,dims,hdferr)
        if (hdferr /= -1) then
            i=index(user,char(0))
            nexus%user = user(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Local contact: not specified in zebra hdf

        ! End time
        call h5dopen_f(file_id,'entry/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        if (hdferr /= -1) then
            i=index(end_time,char(0))
            nexus%end_time = end_time(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Experiment identifier: not specified.

        ! Set-point temperature: not specified

        ! Regulation temperature: not specified

        ! Sample temperature: is an array, but in nexus is considered as an scalar.

        ! Magnetic field: is an array, but in nexus is considered as an scalar.

        ! Geometry
        nexus%geometry = 'NB' ! Normal Beam

        ! Data ordering: not specified

        ! Reflection: not specified

        ! UB-matrix: not specified

        ! Counts
        call h5dopen_f(file_id,'entry/DMC/detector/data',dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_psi_dmc: data not found in nexus.'
            return
        end if
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        if (hdferr /= -1) then
            if (hdferr /= -1) then
                nexus%nz = dims(2)
                nexus%nx = dims(1)
                nexus%nf = dims(3)
                if (allocated(nexus%counts)) deallocate(nexus%counts)
                allocate(counts(nexus%nx,nexus%nz,nexus%nf))
                allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
                call h5dread_f(dset,H5T_NATIVE_INTEGER,counts,dims,hdferr)
                do i = 1 , nexus%nf
                    nexus%counts(:,:,i) = transpose(counts(:,:,i))
                end do
                call h5dclose_f(dset,hdferr)
            end if
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_psi_dmc: error reading counts.'
            return
        end if

        ! Scan variables
        if (allocated(nexus%angles)) deallocate(nexus%angles)
        allocate(nexus%angles(8,nexus%nf))
        motors(:) = 0
        ! Monitor
        call h5dopen_f(file_id,'entry1/monitor1/data',dset,hdferr)
        if (hdferr == 0) then
            if (allocated(nexus%monitor)) deallocate(nexus%monitor)
            if (allocated(monitor)) deallocate(monitor)
            allocate(nexus%monitor(nexus%nf),monitor(nexus%nf))
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_INTEGER,monitor(:),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            nexus%monitor(:) = monitor(:)
            nexus%is_monitor = .true.
        end if
        ! Omega
        call h5dopen_f(file_id,'entry/sample/rotation_angle',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,omega,scalar,hdferr)
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(3,:),dims,hdferr)
                nexus%angles(3,:) = -nexus%angles(3,:)
            end if
            if (hdferr /= -1) then
                nexus%is_omega = .true.
                if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) > MIN_SCAN_ANGLE) motors(3) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Gamma
        call h5dopen_f(file_id,'entry/DMC/monochromator/wavelength',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_REAL,gamma,scalar,hdferr)
            ! 65.9474 = 0.5 * (nx-1) * dx / radius, nx = 1152, dx = 1.6mm, radius = 800mm
            if (hdferr /= -1) nexus%angles(4,:) = gamma + 65.9474
            if (hdferr /= -1) nexus%is_gamma = .true.
        end if
        ! Nu
        nexus%is_nu = .true.
        nexus%angles(7,:) = 0.0
        call get_scan_type(nexus,motors)

    end subroutine read_nexus_psi_dmc

    subroutine read_nexus_psi_zebra(nexus)

        ! Arguments
        type(nexus_type),  intent(inout) :: nexus

        ! Local variables
        integer :: i
        integer, dimension(8) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: monitor
        real :: chi,gamma,nu,omega,phi
        real, dimension(9) :: ub
        character(len=80) :: end_time,user,zebra_mode

        ! Instrument name
        nexus%instrument_name = 'zebra'

        ! Run number: in zebra, run number corresponds to proposal_id,
        ! which is a string instead of an integer. So it is not read.

        ! Wavelength
        call h5dopen_f(file_id,'entry1/ZEBRA/monochromator/wavelength',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! User name
        call h5dopen_f(file_id,'entry1/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user,dims,hdferr)
        if (hdferr /= -1) then
            i=index(user,char(0))
            nexus%user = user(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Local contact: not specified in zebra hdf

        ! End time
        call h5dopen_f(file_id,'entry1/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        if (hdferr /= -1) then
            i=index(end_time,char(0))
            nexus%end_time = end_time(1:i-1)
        end if
        call h5dclose_f(dset,hdferr)

        ! Experiment identifier: not specified.

        ! Set-point temperature: not specified

        ! Regulation temperature: not specified

        ! Sample temperature: is an array, but in nexus is considered as an scalar.

        ! Magnetic field: is an array, but in nexus is considered as an scalar.

        ! Geometry
        call h5dopen_f(file_id,'entry1/zebra_mode',dset,hdferr)
        if (hdferr /= -1) then
            if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,filetype,zebra_mode,dims,hdferr)
            if (hdferr /= -1) then
                i=index(zebra_mode,char(0))
                zebra_mode = adjustl(trim(zebra_mode(1:i-1)))
                if (zebra_mode == 'nb') then
                    nexus%geometry = 'NB' ! Normal Beam
                else if (zebra_mode == 'bi') then
                    nexus%geometry = '4C' ! 4-circle
                else
                    nexus%geometry = '??'
                end if
            end if
            call h5dclose_f(dset,hdferr)
        end if

        ! Data ordering: not specified

        ! Reflection
        call h5dopen_f(file_id,'entry1/sample/h',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection(1),scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5dopen_f(file_id,'entry1/sample/k',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection(2),scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        call h5dopen_f(file_id,'entry1/sample/l',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection(3),scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! UB-matrix
        call h5dopen_f(file_id,'entry1/sample/UB',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ub,dims,hdferr)
        if (hdferr /= -1) then
            nexus%ub = transpose(reshape(ub,[3,3]))
            nexus%is_ub = .true.
        end if
        call h5dclose_f(dset,hdferr)

        ! Counts
        call h5dopen_f(file_id,'entry1/ZEBRA/area_detector2/data',dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_psi_zebra: data not found in nexus.'
            return
        end if
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_psi_zebra: error reading counts.'
            return
        end if

        ! Scan variables
        if (allocated(nexus%angles)) deallocate(nexus%angles)
        allocate(nexus%angles(8,nexus%nf))
        motors(:) = 0
        ! Monitor
        call h5dopen_f(file_id,'entry1/beam_monitor/data',dset,hdferr)
        if (hdferr == 0) then
            if (allocated(nexus%monitor)) deallocate(nexus%monitor)
            if (allocated(monitor)) deallocate(monitor)
            allocate(nexus%monitor(nexus%nf),monitor(nexus%nf))
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_INTEGER,monitor(:),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            nexus%monitor(:) = monitor(:)
            nexus%is_monitor = .true.
        end if
        ! Phi
        call h5dopen_f(file_id,'entry1/sample/phi',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,phi,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(1,:) = phi
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(1,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_phi = .true.
                if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(1) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Chi
        call h5dopen_f(file_id,'entry1/sample/chi',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,chi,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(2,:) = chi
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(2,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_chi = .true.
                if (abs(nexus%angles(2,nexus%nf)-nexus%angles(2,1)) > MIN_SCAN_ANGLE) motors(2) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Omega
        call h5dopen_f(file_id,'entry1/sample/rotation_angle',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,omega,scalar,hdferr)
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(3,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_omega = .true.
                if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) > MIN_SCAN_ANGLE) motors(3) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Gamma
        call h5dopen_f(file_id,'entry1/ZEBRA/area_detector2/polar_angle',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,gamma,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(4,:) = gamma
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(4,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_gamma = .true.
                if (abs(nexus%angles(4,nexus%nf)-nexus%angles(4,1)) > MIN_SCAN_ANGLE) motors(4) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! Nu
        call h5dopen_f(file_id,'entry1/ZEBRA/area_detector2/tilt_angle',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) then
            if (dims(1) == 1) then
                call h5dread_f(dset,H5T_NATIVE_REAL,nu,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(7,:) = nu
            else
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(7,:),dims,hdferr)
            end if
            if (hdferr /= -1) then
                nexus%is_nu = .true.
                if (abs(nexus%angles(7,nexus%nf)-nexus%angles(7,1)) > MIN_SCAN_ANGLE) motors(7) = 1
            end if
            call h5dclose_f(dset,hdferr)
        end if
        call get_scan_type(nexus,motors)

    end subroutine read_nexus_psi_zebra

    subroutine set_data_ordering(data_ordering_,data_ordering)

        ! Arguments
        character(len=*),              intent(in)  :: data_ordering_
        character(len=:), allocatable, intent(out) :: data_ordering

        ! Local variables
        integer :: i
        character(len=:), allocatable :: key1,key2,key3

        i=index(data_ordering_,'top')
        if (i > 0) then
            key1 = 'top'
        else
            key1 = 'bottom'
        end if
        i=index(data_ordering_,'left')
        if (i > 0) then
            key2 = 'left'
        else
            key2 = 'right'
        end if
        i=index(data_ordering_,'row')
        if (i > 0) then
            key3 = 'row'
        else
            key3 = 'col'
        end if
        data_ordering=key1//key2//key3//'major'

    end subroutine set_data_ordering

end module nexus_mod