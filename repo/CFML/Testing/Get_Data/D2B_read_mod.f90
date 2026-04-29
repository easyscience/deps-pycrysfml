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

module D2B_read_mod

    ! Subroutines:
    !
    !   Public:
    !       read_cfl

    use hdf5
    use CFML_GlobalDeps,      only: Clear_Error,Err_CFML,OPS_Sep, CFML_DEBUG
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Read_Current_Instrm,Calibration_Detector_Type, &
                                    Read_Calibration_File, Write_Current_Instrm_data
    use CFML_Strings,         only: L_Case,Reading_File,File_Type, Get_words
    use CFML_SXTAL_Geom,      only: Set_PSD
    use nexus_mod

    implicit none

    private

    ! List of public subroutines
    public :: read_calibration_lamp,read_calibration_mantid,read_cfl_D2B, read_calibration_1D, write_cfl

    type, public :: cfl_D2B_type

        integer            :: nscans
        integer            :: nz_int
        integer            :: num1,num2
        integer            :: kc1,kc2
        real               :: scale_fac
        real               :: tth_min
        real               :: tth_max
        real               :: nsigma
        real               :: Norm_Monitor
        real               :: ef_cutoff
        logical            :: kc12       = .false.
        logical            :: is_tth_min = .false.
        logical            :: is_tth_max = .false.
        logical            :: is_label   = .false.
        logical            :: calibration,combine,raw
        logical            :: suma
        logical            :: is_nsigma  =.false.
        logical            :: Integ_1D
        logical            :: is_numor   = .false.
        logical            :: is_ef_cutoff = .false.
        logical            :: single,excl_dets,excl_cells
        logical            :: align
        logical            :: tubes_output
        logical            :: Apply_Shifts
        logical            :: verbose =.false.
        logical            :: is_calib_file,is_calib_path
        logical            :: monitor ! if true, a given Norm_Monitor is used for nomalization
        character(len=8)   :: calib_gen
        character(len=12)  :: instrument_name
        character(len=20)  :: suffix
        character(len=512) :: scan_path,calib_path,calib_file,combine_name
        character(len=512), dimension(:), allocatable :: scans
        character(len=:),                 allocatable :: label_sum
        character(len=:),                 allocatable :: label
        integer,            dimension(:), allocatable :: det_excluded, cell_excluded

    end type cfl_D2B_type

    character(len=3) :: suffix_DEFAULT = 'd2b'
    real, parameter  :: NSIGMA_DEFAULT = 3.0
    logical          :: CALIBRATION_DEFAULT = .false.
    logical, public  :: is_d2b = .false.

    ! Parameters
    integer,          parameter :: NZ_INT_DEFAULT     = 100
    real,             parameter :: SCALE_FAC_DEFAULT  = 1.0
    logical,          parameter :: ALIGN_DEFAULT      = .false.
    logical,          parameter :: SINGLE_DEFAULT     = .false.
    logical,          parameter :: SUM_DEFAULT        = .false.
    character(len=3), parameter :: LABEL_SUM_DEFAULT  = 'sum'
    character(len=3), parameter :: LABEL_DEFAULT      = 'w80'
    ! Error message
    character(len=1024), public :: war_D2B_mess


    contains

    subroutine read_calibration_1d(filename,Cal)
        ! Arguments
        character(len=*),                intent(in)  :: filename
        type(calibration_detector_type), intent(out) :: Cal

        ! Local variables
        integer :: i, j, i_cal
        real    :: ideal_pos,shift
        character(len=10) :: namef
        logical :: exist

        call clear_error()

        ! Check that calibration file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        namef = L_Case(current_instrm%name_inst)
        i = index(namef,'d2b')
        if (i < 1) then
           Err_CFML%ierr = 1
           Err_CFML%Msg  = 'read_calibration: calibration for instrument '//trim(namef)//' not implemented'
           return

        else

           Cal%Name_Instrm="D2B-1D"
           Cal%NDet=128                ! Number of detectors
           Cal%NPointsDet=128          ! Number of Points per Detector

           allocate( Cal%PosX(Cal%NDet), Cal%Effic(Cal%NPointsDet,Cal%NDet), Cal%Active(Cal%NPointsDet,Cal%NDet) ) ! Relative angular positions of detectors, efficiencies, mask
           allocate( Cal%sPosX(Cal%NDet), Cal%sEffic(Cal%NPointsDet,Cal%NDet) ) ! Sigma of angular positions of detectors and efficiencies
           Cal%PosX(:)=0.0;  Cal%Active=.true.
           open(newunit=i_cal, file=trim(filename),status="old", action="read",position="rewind")
           read(unit=i_cal,fmt=*)  !Just skip the first line
           do i=1, Cal%NDet
             read(unit=i_cal,fmt="(i4,5f12.4)",iostat=Err_CFML%ierr) j,ideal_pos,shift,Cal%sPosX(i),Cal%Effic(1,i),Cal%sEffic(1,i) !In reality theoretical position, shift, sigma, inverse efficiency, sigma
             if(Err_CFML%ierr /= 0) then
               Err_CFML%Flag=.true.
               Err_CFML%Msg="                                                                                    "
               write(Err_CFML%Msg,"(a,i4)")"Error reading the calibration file: "//trim(filename)//" at line: ",i+1
             end if
             Cal%PosX(i)= ideal_pos-shift  !New positions
           end do
           do i=2,Cal%NPointsDet
              Cal%Effic(i,:)= Cal%Effic(1,:)
              Cal%sEffic(i,:)= Cal%sEffic(1,:)
           end do
        end if
        Cal%True_Eff=.false.

    end subroutine read_calibration_1d

    subroutine read_calibration_lamp(filename,Cal)
        ! Arguments
        character(len=*),                intent(in)  :: filename
        type(calibration_detector_type), intent(out) :: Cal

        ! Local variables
        integer :: i
        character(len=10) :: namef
        logical :: exist

        call clear_error()

        ! Check that lamp file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        namef = L_Case(current_instrm%name_inst)
        i = index(namef,'d2b')
        if (i < 1) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: calibration for instrument '//trim(namef)//' not implemented'
            return
        else

            call Read_Calibration_File(filename, "D2B", Cal)
            if(Cal%Pos_read) write(*,"(12f10.4)") Cal%PosX

            if(Err_CFML%Flag) then
                write(*,"(a)") trim(Err_CFML%Msg)
                Err_CFML%ierr=1
            end if

        end if

    end subroutine read_calibration_lamp

    subroutine read_calibration_lamp_d2b(filename,Cal)

        ! Arguments
        character(len=*),               intent(in)  :: filename
        type(Calibration_Detector_Type),intent(out) :: Cal

        ! Local variables
        integer :: i,j,m,n,i1,i2,i1_,i2_,i_tubo, i_cal
        integer, dimension(2,128) :: active_pixels
        character(len=256)        :: line
        real, dimension(128,128)  :: calibration

        call clear_error()
        Cal%Name_Instrm="D2B-Lamp"
        Cal%NDet=128                ! Number of detectors
        Cal%NPointsDet=128          ! Number of Points per Detector

        allocate( Cal%PosX(Cal%NDet), Cal%Effic(Cal%NPointsDet,Cal%NDet), Cal%Active(Cal%NPointsDet,Cal%NDet) ) ! Relative angular positions of detectors, efficiencies, mask
        Cal%PosX(:)=0.0
        open(newunit=i_cal,file=filename,status='old',action='read')
        read(unit=i_cal,fmt=*)
        read(unit=i_cal,fmt=*)
        ! read active pixels
        do i = 0 , 63
            i_tubo = 2*i + 1
            read(unit=i_cal,fmt='(a)') line
            j = index(line,'*')
            if (j > 0) then
                read(unit=line(:j-1),fmt=*) active_pixels(1:2,i_tubo),active_pixels(1:2,i_tubo+1)
            else
                read(unit=line(:),fmt=*) active_pixels(1:2,i_tubo),active_pixels(1:2,i_tubo+1)
            end if
        end do
        active_pixels(:,:) = active_pixels(:,:) + 1
        ! read efficiencies
        read(i_cal,*)
        n = 0
        do i = 0,127
            n = i + 1
            if (mod(i,2) == 0) then
                m = 1
                do j = 1 , 21
                    read(unit=i_cal,fmt=*) calibration(m:m+5,n)
                    m = m + 6
                end do
                read(unit=i_cal,fmt=*) calibration(127:128,n)
            else
                m = 128
                do j = 1 , 21
                    read(unit=i_cal,fmt=*) calibration(m:m-5:-1,n)
                    m = m - 6
                end do
                read(unit=i_cal,fmt=*) calibration(2:1:-1,n)
            end if
        end do
        read(i_cal,*)
        read(i_cal,*,iostat=Err_CFML%ierr) Cal%PosX
        if(Err_CFML%ierr == 0) then
           Cal%Pos_read=.true.
        else
           Cal%Pos_read=.false.
        end if
        ! replace efficiencies of non-active pixels by a negative number
        do i = 0 , 127
            n = i + 1
            i1_ = active_pixels(1,n)
            i2_ = active_pixels(2,n)
            if (mod(i,2) == 0) then
                i1 = i1_
                i2 = i2_
            else
                i2 = 256 - i1_
                i1 = 256 - i2_
            end if
            if (i1 > 1) calibration(:i1-1,n) = -1
            if (i2 < 128) calibration(i2+1:,n) = -1
        end do

        where(calibration <= 0.0)
            Cal%Active=.false.
        elsewhere
            Cal%effic=1.0/calibration  !convert efficiencies to alphas
        end where
        Cal%True_Eff=.false.
    end subroutine read_calibration_lamp_d2b

    subroutine read_calibration_mantid(filename,path,Cal)

        ! Arguments
        character(len=*),               intent(in)  :: filename
        character(len=*),               intent(in)  :: path
        type(Calibration_Detector_Type),intent(out) :: Cal

        ! Local variables
        integer :: i,hdferr,nx,nz
        integer(HID_T) :: file_id,dset,space
        integer(HSIZE_T), dimension(3) :: dims,dims_
        logical :: exist
        real, dimension(:,:), allocatable :: calibration

        call clear_error()

        ! Check that nexus file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr == -1) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = "read_calibration: error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        if (hdferr /= -1) call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        if (hdferr /= -1) then
            call h5fopen_f(trim(filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
            if (hdferr == -1) then
                Err_CFML%ierr = 1
                Err_CFML%Msg = "read_calibration: error opening nexus file"
                return
            end if
        end if

        ! Get calibration
        call h5dopen_f(file_id,path,dset,hdferr)
        if (hdferr == -1) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: wrong path.'
            return
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims_,hdferr)
        !   Assign memory to arrays and read data
        if (hdferr /= -1) then
            i = index(current_instrm%name_inst,'d2b')
            if (i > 0) then
                ! for d2b, the calibration matrix produced by mantid is
                ! transposed with respect to the counts in nexus files
                nx = dims(1)
                nz = dims(2)
                allocate(calibration(nx,nz))
                call h5dread_f(dset,H5T_NATIVE_REAL,calibration,dims,hdferr)
                if (hdferr /= -1) calibration = transpose(calibration)
                call h5dclose_f(dset,hdferr)
            else
                Err_CFML%ierr = 1
                Err_CFML%Msg = 'read_calibration: calibration cannot be applied, only implemented for d2b.'
                return
            end if
        end if
        if (hdferr == -1) then
            Err_CFML%ierr = 1
            Err_CFML%Msg = 'read_calibration: error reading calibration data.'
            return
        end if

        ! Close NEXUS file.
        call h5fclose_f(file_id,hdferr)

        ! Close FORTRAN interface.
        call h5close_f(hdferr)

        !Construction of Cal object
        Cal%Name_Instrm="D2B-Mantid"
        Cal%NDet=128                ! Number of detectors
        Cal%NPointsDet=128          ! Number of Points per Detector

        allocate( Cal%PosX(Cal%NDet), Cal%Effic(Cal%NPointsDet,Cal%NDet), Cal%Active(Cal%NPointsDet,Cal%NDet) ) ! Relative angular positions of detectors, efficiencies, mask
        Cal%PosX(:)=[(-158.750+(i-1)*1.25,i=1,128)]
        Cal%Pos_read=.true.; Cal%effic=1.0; Cal%Active=.true.
        where(calibration <= 0.0)
            Cal%Active=.false.
        elsewhere
           where(Cal%effic > 0.0) Cal%effic=1.0/calibration
        end where
        Cal%True_Eff=.false.

    end subroutine read_calibration_mantid

    subroutine read_cfl_D2B(cfl_file,cfl)

        ! Read and process the cfl file

        ! Arguments
        character(len=*),    intent(in)  :: cfl_file
        type(cfl_D2B_type),  intent(out) :: cfl

        ! Local variables
        integer                       :: i,j,k,n,ic,ierror,num1,num2
        character(len=100)            :: keyword,gen_calib
        character(len=1024)           :: path_calib,combine_name,code_path
        character(len=:), allocatable :: line,file_inst,file_calib,namef
        logical                       :: is_file
        character(len=10), dimension(40) :: dire
        type(File_Type)               :: cfl_file_type

        call clear_error()

        ! Set defaults
        cfl%combine     = .false.
        cfl%monitor     = .false.
        cfl%align       = .false.
        cfl%raw         = .false.
        cfl%verbose     = .false.
        cfl%kc12        = .false.
        cfl%apply_shifts= .false.
        cfl%tubes_output= .false.
        cfl%Integ_1D    = .false.
        cfl%is_calib_file = .false.
        cfl%is_calib_path = .false.
        cfl%excl_dets     = .false.
        cfl%excl_cells    = .false.
        cfl%suffix      = suffix_DEFAULT
        cfl%nsigma      = NSIGMA_DEFAULT
        cfl%scale_fac   = SCALE_FAC_DEFAULT
        cfl%nscans      = 0
        cfl%num1        = 0
        cfl%num2        = 0
        cfl%kc1         = 51   ! 56-72 w=17,  54-74 w=21,   51-77 w=27,  59-69  w=11, 50-78 w=30
        cfl%kc2         = 77
        cfl%scan_path   = " "

        ! Put the content in cfl_file_type
        cfl_file_type = Reading_File(cfl_file)

        ! Read content
        do i = 1 , cfl_file_type%nlines
            line = adjustl(cfl_file_type%line(i)%str)
            if (len_trim(line) == 0) cycle
            j = index(line,' ')
            if (j > 0) then
                keyword = L_Case(line(1:j-1))
            else
                keyword = L_Case(line)
            end if

            select case (keyword)

                case('instrm')
                    read(line(j+1:),*,iostat=ierror) cfl%instrument_name
                    if (ierror == 0) then
                        cfl%instrument_name = adjustl(cfl%instrument_name)
                    else
                        Err_CFML%Ierr=1
                        Err_CFML%Msg=" Error reading the instrument name"
                        return
                    end if

                case('nz1-nz2')
                    read(line(j+1:),*,iostat=ierror) cfl%kc1,cfl%kc2
                    if (ierror /= 0) then
                       Err_CFML%Ierr=1
                       Err_CFML%Msg=" Error reading the vertical pixels items  nz1 & nz2"
                       return
                    end if
                    cfl%kc12=.true.
                    cfl%nz_int = cfl%kc2-cfl%kc1 + 1

                case('nz_int')
                    read(line(j+1:),*,iostat=ierror) cfl%nz_int
                    if (ierror /= 0) cfl%nz_int = NZ_INT_DEFAULT

                case('tubes_output')

                    cfl%tubes_output=.true.

                case('integ_1d')

                    cfl%Integ_1D=.true.

                case('monitor')
                    read(line(j+1:),*,iostat=ierror) cfl%Norm_monitor
                    if (ierror == 0) then
                        cfl%monitor = .true.
                    else
                        Err_CFML%Ierr=1
                        Err_CFML%Msg = 'read_cfl: error reading the normalization monitor'
                        return
                    end if

                case('ef_cutoff')
                    read(line(j+1:),*,iostat=ierror) cfl%ef_cutoff
                    if (ierror == 0) then
                        cfl%is_ef_cutoff = .true.
                    else
                        Err_CFML%Ierr=1
                        Err_CFML%Msg = 'read_cfl: error reading the Efficiency cutoff'
                        return
                    end if
                case('scan_path')
                    if (j > 0) read(unit=line(j:),fmt='(a)',iostat=ierror) code_path
                    if(ierror /= 0 .or. j == 0) then
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: error reading the scan_path name'
                        return
                    else
                       cfl%scan_path=trim(adjustl(code_path))
                       k=len_trim(cfl%scan_path)
                       if(cfl%scan_path(k:k) /= OPS_SEP) cfl%scan_path(k+1:k+1)=OPS_SEP
                    end if

                case('numors')
                    if(len_trim(cfl%scan_path) == 0) then
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: error, scan_path should be provided before reading numors'
                        return
                    end if
                    read(line(j+1:),*,iostat=ierror) num1,num2
                    if (ierror == 0) then
                        n = num2-num1+1
                        cfl%nscans = num2-num1+1
                        allocate(cfl%scans(n))
                        cfl%num1=num1
                        cfl%num2=num2
                        cfl%is_numor=.true.
                        n=0
                        do k = num1,num2
                            n=n+1
                            write(cfl%scans(n),"(a,i6.6,a)") trim(cfl%scan_path),k,".nxs"
                        end do
                    else
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: error reading numors'
                        return
                    end if

                case('combine')
                    cfl%combine = .true.
                    ierror = 0
                    if (j > 0) read(unit=line(j:),fmt='(a)',iostat=ierror) combine_name
                    if (ierror /= 0 .or. j == 0) then
                        cfl%combine_name = 'gsc'
                    else
                        cfl%combine_name = adjustl(combine_name)
                    end if


                case('instrument_file')
                    file_inst = adjustl(trim(line(j+1:)))
                    inquire(file = file_inst, exist = is_file)
                    if (.not. is_file) then
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: instrument file '//file_inst//' not found'
                        return
                    end if
                    call Read_Current_Instrm(trim(file_inst))
                    if (Err_CFML%Flag) then
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = Err_CFML%Msg
                        return
                    end if
                    namef = L_Case(current_instrm%name_inst)
                    k = index(namef,'d2b')
                    if (k > -1) is_d2b = .true.
                    call Set_PSD()
                    write(*,"(a)") "   CURRENT INSTRUMENT FILE "//trim(file_inst)//" READ"
                    call Write_Current_Instrm_data()

                case('calibration_file')
                    file_calib = adjustl(trim(line(j+1:)))
                    inquire(file = file_calib, exist = is_file)
                    if (.not. is_file) then
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: calibration file '//file_calib//' not found'
                        return
                    end if
                    cfl%calib_file = file_calib
                    cfl%is_calib_file = .true.

                case('calibration_gen')
                    read(unit=line(j+1:),fmt='(a)',iostat=ierror) gen_calib
                    if (ierror == 0) then
                        cfl%calib_gen = adjustl(trim(gen_calib))
                    else
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: Error reading the type of calibration file '
                        return
                    end if

                case('calibration_path')
                    read(unit=line(j+1:),fmt='(a)',iostat=ierror) path_calib
                    cfl%calib_path = adjustl(path_calib)
                    if (ierror == 0) then
                        cfl%is_calib_path = .true.
                    else
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: Error reading the calibration PATH '
                        return
                    end if

                case('suffix')
                    read(line(j+1:),*,iostat=ierror) cfl%suffix
                    if (ierror /= 0) cfl%suffix = suffix_DEFAULT

                case('nsigma')
                    read(line(j+1:),*,iostat=ierror) cfl%nsigma
                    if (ierror /= 0) cfl%nsigma = NSIGMA_DEFAULT


                case ('raw')
                    cfl%raw = .true.

                case ('apply_shifts')
                    cfl%apply_shifts = .true.

                case ('verbose')
                    cfl%verbose = .true.

                case('scans')
                    read(line(j+1:),*,iostat=ierror) n
                    if (ierror == 0) then
                        allocate(cfl%scans(n))
                        do k = 1 , n
                            line = adjustl(cfl_file_type%line(i+k)%str)
                            read(line,"(a)",iostat=ierror) cfl%scans(k)
                            if (ierror /= 0) exit
                        end do
                    else
                        Err_CFML%Ierr = 1
                        Err_CFML%Msg = 'read_cfl: Error Reading the number of Scans'
                        Return
                    end if
                    cfl%nscans = n

                case('align')
                    cfl%align = .true.

                case('scale_fac')
                    read(line(j+1:),*,iostat=ierror) cfl%scale_fac
                    if (ierror /= 0) cfl%scale_fac = SCALE_FAC_DEFAULT

                case('single')
                    cfl%single = .true.

                case('sum')
                    if (len(trim(line(j+1:))) > 0) cfl%label_sum = adjustl(trim(line(j+1:)))
                    cfl%suma = .true.

                case('label')
                    if (len(trim(line(j+1:))) > 0) cfl%label = adjustl(trim(line(j+1:)))
                    cfl%is_label = .true.

                case('tth_min')
                    read(line(j+1:),*,iostat=ierror) cfl%tth_min
                    if (ierror == 0) cfl%is_tth_min = .true.

                case('tth_max')
                    read(line(j+1:),*,iostat=ierror) cfl%tth_max
                    if (ierror == 0) cfl%is_tth_max = .true.

                case('excl_dets')
                    k=index(line,"!")
                    if(k /= 0) line=line(1:k-1)
                    k=index(line,"#")
                    if(k /= 0) line=line(1:k-1)
                    call Get_Words(line,dire,ic)
                    if(ic >= 2) then
                      k=ic-1
                      if(allocated(cfl%det_excluded)) deallocate(cfl%det_excluded)
                      allocate(cfl%det_excluded(k))
                      do k=2,ic
                         read(dire(k),*,iostat=ierror) cfl%det_excluded(k-1)
                         if(ierror /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg=" Error reading the EXCL_DETS instruction"
                           return
                         end if
                      end do
                      cfl%excl_dets=.true.
                    else
                      cfl%excl_dets=.false.
                    end if

                case('excl_cells')
                    k=index(line,"!")
                    if(k /= 0) line=line(1:k-1)
                    k=index(line,"#")
                    if(k /= 0) line=line(1:k-1)
                    call Get_Words(line,dire,ic)
                    if(ic >= 2) then
                      k=ic-1
                      if(allocated(cfl%cell_excluded)) deallocate(cfl%cell_excluded)
                      allocate(cfl%cell_excluded(k))
                      do k=2,ic
                         read(dire(k),*,iostat=ierror) cfl%cell_excluded(k-1)
                         if(ierror /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg=" Error reading the EXCL_CELLS instruction"
                           return
                         end if
                      end do
                      cfl%excl_cells=.true.
                    else
                      cfl%excl_cells=.false.
                    end if

            end select
        end do

        if (trim(cfl%calib_gen) == 'mantid') then
            if (cfl%is_calib_file .and. .not. cfl%is_calib_path) then
                write(*,'(4x,a,1x,a)') ' => Warning: Calibration path not given, calibration cannot be applied'
            else if (.not. cfl%is_calib_file .and. cfl%is_calib_path) then
                write(*,'(4x,a,1x,a)') ' => Warning: Calibration file not given, calibration cannot be applied'
            else if (cfl%is_calib_file .and. cfl%is_calib_path) then
                cfl%calibration = .true.
            end if
        else if (trim(cfl%calib_gen) == 'lamp' .and. cfl%is_calib_file) then
            cfl%calibration = .true.
        else if (trim(cfl%calib_gen) == 'calib_1d' .and. cfl%is_calib_file) then
            cfl%calibration = .true.
        end if

    end subroutine read_cfl_D2B

    subroutine write_cfl(cfl,lun)
      type(cfl_D2B_type), intent(in) :: cfl
      integer,            intent(in) :: lun
      ! Local variables
      integer :: i

      write(lun,"(a)")                           " =>      Content of the CFL file: "
      write(lun,"(a)")                           "                 Instrument Name: "//trim(cfl%instrument_name)
      write(lun,"(a,i7)")                        "                          nscans: ",cfl%nscans
      write(lun,"(a,i7)")                        "  Vertical integration  (nz_int): ",cfl%nz_int
      if(cfl%kc12) write(lun,"(a,2i7)")          "   Vertical integration (pixels): ",cfl%kc1,cfl%kc2
      if(cfl%is_numor)write(lun,"(a,2i7)")       "                          Numors: ",cfl%num1,cfl%num2
      write(lun,"(a,f7.3)")                      "                    Scale Factor: ",cfl%scale_fac
      if(cfl%is_tth_min) write(lun,"(a,f7.3)")   "                       2ThetaMin: ",cfl%tth_min
      if(cfl%is_tth_max) write(lun,"(a,f7.3)")   "                       2ThetaMax: ",cfl%tth_max
      if(cfl%is_nsigma)  write(lun,"(a,f7.3)")   "       Nsigma for 2D integration: ",cfl%nsigma
      if(cfl%Monitor)    write(lun,"(a,f7.3)")   "           Normalization Monitor: ",cfl%Norm_Monitor
      if(cfl%Integ_1D)   then
        write(lun,"(a)")                         "              Integration Method: Vertical Straight Sums"
      else
        write(lun,"(a)")                         "              Integration Method: 2Theta-Arcs in 2D"
      end if
      if(cfl%is_ef_cutoff) write(lun,"(a,f7.3)") "              Efficiency cut-off: ",cfl%ef_cutoff
      if(cfl%is_label)     write(lun,"(a)")      "                           Label: ",trim(cfl%label)
      if(cfl%suma)         write(lun,"(a)")      "                       Label Sum: "//trim(cfl%label_sum)


      if(cfl%calibration) then
        if(cfl%is_calib_path) write(lun,"(a)")   "       Path for calibration file: "//trim(cfl%calib_path)
        if(cfl%is_calib_file) write(lun,"(a)")   "        Name of calibration file: "//trim(cfl%calib_file)
        write(lun,"(a)")                         "        Type of calibration file: "//trim(cfl%calib_gen)
        if(cfl%Apply_Shifts) write(lun,"(a)")    "  Shifts of detectors taken into account  "
      else
                              write(lun,"(a)")   "       No calibration is applied  "
      end if
      if(cfl%tubes_output)    write(lun,"(a)")   "  Output of the diffraction pattern of individual detectors  "
      if(cfl%align)           write(lun,"(a)")   "  Align applied!  "
      if(cfl%verbose)         write(lun,"(a)")   "  Long output  "
      if(cfl%raw)             write(lun,"(a)")   "  Raw data of Nexus Files will be used  "
      if(cfl%combine)         write(lun,"(a/)")  "  Combined (all scans) Name of the final file: "//trim(cfl%combine_name)
      if(cfl%nscans > 0) then
         do i=1,cfl%nscans
            write(lun,"(a,i3,a)")                "  Scan#",i,"  "//trim(cfl%scans(i))
         end do
      end if
                            write(lun,"(a/)")    "  User-provided suffix: "//trim(cfl%suffix)
    end subroutine write_cfl

end module D2B_read_mod