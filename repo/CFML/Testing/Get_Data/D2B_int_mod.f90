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

Module D2B_int_mod

    use CFML_GlobalDeps,      only: to_deg, err_CFML, clear_error
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Calibration_Detector_Type
    use CFML_Strings,         only: L_Case
    use CFML_DiffPatt,        only: DiffPat_E_Type,write_pattern
    use D2B_data_mod
    use D2B_read_mod
    use nexus_mod

    implicit none
    private

    ! Local parameters
    real,    parameter :: EPSIL = 0.0001
    integer, parameter :: D2B_NPIXELS_HORIZ = 128
    real,    parameter :: D2B_WIDTH_HORIZ = 1.25 ! degrees

    ! Local variables
    integer            :: ierr,i,j,last
    real               :: t_ini,t_end
    character(len=512) :: scanf
    type(cfl_D2B_type) :: cfl
    type(nexus_type),dimension(:),allocatable   :: nexus
    type(nexus_type)   :: snexus  !Single nexus

    integer               :: np_horiz
    integer, dimension(3) :: dims
    real                  :: ga_D,nu_D,xmin,cgap, scale_fac,aver_mon
    type(DiffPat_E_Type), public  :: pow_pat

    Public  :: Integrate_D2B_data
    logical :: is_virtual_detector_set
    character(len=:), allocatable :: fname
    contains
    !logical, dimension(128) :: actlist=.true., Excl=.false.
    ! Starting time

    Subroutine Integrate_D2B_data(cfl_file,instr,cfl,i_log)
       character(len=*),   intent(in)  :: cfl_file, instr
       type(cfl_D2B_type), intent(out) :: cfl
       integer, optional,  intent(in)  :: i_log


       call cpu_time(t_ini)

       ! Read the cfl file

       write(*,'(a)') ' => Reading cfl file: '//trim(cfl_file)
       call read_cfl_D2B(cfl_file,cfl)
       if (err_CFML%ierr /= 0) then
          call finish(t_ini)
       else
          call clear_error()
       end if

       call write_header()
       if(present(i_log)) call write_header(i_log)
       if(L_case(instr) /= "d2b") then
        return ! This is just for reading the cfl files, it content is treated in Get_Data_nxs
       end if
       if(allocated(nexus)) deallocate(nexus)
       allocate(nexus(cfl%nscans))


       ! Read calibration file if it was given
       if(.not. (current_instrm%alpha_correct)) then
          if (cfl%calibration ) then
              write(*,"(a)") " => Reading calibration file: "//trim(cfl%calib_file)
              if (trim(L_Case(cfl%calib_gen)) == 'mantid') then
                  write(*,'(4x,a)') ' => Calibration generator: mantid'
                  write(*,'(8x,a,1x,a)') 'Data path:', trim(adjustl(cfl%calib_path))
                  call read_calibration_mantid(cfl%calib_file,cfl%calib_path,Cal)

                  if (Err_CFML%Ierr /= 0) then
                      write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_CFML%Msg)
                      write(*,'(8x,a)') 'Calibration will not be applied'
                      cfl%calibration = .false.
                  end if
              else if (trim(L_Case(cfl%calib_gen)) == 'lamp') then
                  write(*,'(4x,a)') ' => Calibration generator: lamp'
                  call read_calibration_lamp(cfl%calib_file,Cal)
                  if (Err_CFML%Ierr /= 0) then
                      write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_CFML%Msg)
                      write(*,'(8x,a)') 'Calibration will not be applied'
                      cfl%calibration = .false.
                  end if
              else if (trim(L_Case(cfl%calib_gen)) == 'calib_1d') then
                  write(*,'(4x,a)') ' => Calibration generator: D2B_tubes/D2B_calib'
                  call read_calibration_1D(cfl%calib_file,Cal)
                  if (Err_CFML%Ierr /= 0) then
                      write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_CFML%Msg)
                      write(*,'(8x,a)') 'Calibration will not be applied'
                      cfl%calibration = .false.
                  end if
              else
                  write(*,'(4x,a)') ' => Calibration generator: unknown'
                  write(*,'(8x,a)') 'Calibration will not be applied'
                  cfl%calibration = .false.
              end if
              if (cfl%calibration .and. cfl%is_ef_cutoff ) call set_state(cfl%ef_cutoff)
          else
              if (.not. allocated(Cal%Effic))  allocate(Cal%Effic(current_instrm%np_vert,current_instrm%np_horiz))
              if (.not. allocated(Cal%Active)) allocate(Cal%Active(current_instrm%np_vert,current_instrm%np_horiz))
              if (.not. allocated(Cal%PosX))   allocate(Cal%PosX(current_instrm%np_horiz))
              Cal%Effic = 1.0
              Cal%Active = .true.
              Cal%PosX=[(-158.750+(i-1)*1.25,i=1,128)]
          end if
          !Calculate the horizontal shifts in mm or the detector tubes in put them, as well as alphas, in (Current_instrm%alphas,Current_instrm%shifts)
          call set_alphas_shifts_D2B(Cal)

       end if
       !write(i_log,"(/,a)") "   CONTENT OF THE PROVIDED CFL-FILE: "//trim(cfl_file)
       !call write_cfl(cfl,i_log)

       write(*,'(4x,a)') ' => Reading all scans'
       if(present(i_log)) write(i_log,'(4x,a)') ' => Reading all scans'
       do i = 1 , cfl%nscans
           write(*,'(8x,a,1x,a)') 'Reading nexus file', trim(cfl%scans(i))
           if(present(i_log)) write(i_log,'(8x,a,1x,a)') 'Reading nexus file', trim(cfl%scans(i))
           if (cfl%raw) then
               call read_nexus(trim(cfl%scans(i)),nexus(i),raw=cfl%raw)
           else
               call read_nexus(trim(cfl%scans(i)),nexus(i))
           end if
           if (err_nexus) then
               write(*,'(8x,a)') trim(err_nexus_mess)
               cycle
           end if
           if (is_d2b) then
               ! Transform 2theta into gamma values
               if (nexus(i)%is_tth) then
                   do j = 1 , nexus(i)%nf
                       nexus(i)%angles(4,j) = nexus(i)%angles(8,j) - 0.5 * (D2B_NPIXELS_HORIZ-1) * D2B_WIDTH_HORIZ + 0.5 * D2B_WIDTH_HORIZ
                   end do
               end if
           end if

           if (cfl%single) then

              ga_D = nexus(i)%angles(4,1)
              nu_D = nexus(i)%angles(7,1)
              np_horiz = size(nexus(i)%counts,2)
              if (.not. is_virtual_detector_set) then
                  ga_D_virtual = ga_D
                  nu_D_virtual = nu_D
                  is_virtual_detector_set = .true.
                  if (nexus(i)%is_virtual) then
                      cgap = nexus(i)%virtual_cgap
                  else
                      cgap = current_instrm%cgap
                  end if
                  write(*,'(4x,a)') 'Setting parameters of the virtual detector'
                  write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels:',np_horiz
                  write(*,'(8x,a,1x,f6.2)') 'Horizontal pixel size (mm): ',cgap
                  write(*,'(8x,a,1x,f6.2)') 'Gamma:',ga_D_virtual
                  write(*,'(8x,a,1x,f6.2)') 'Nu:   ',nu_D_virtual
              end if
              write(*,'(4x,a)') 'Integrating individual scans'
              call get_powder_pattern(cfl%nz_int,cfl%scale_fac,np_horiz,cgap,ga_D,pow_pat,counts_int=nexus(i)%counts(:,:,1))
              if (cfl%is_tth_min) then
                  xmin = cfl%tth_min
              else
                  xmin = pow_pat%xmin
              end if
              if (cfl%label /= '') then
                  fname = trim(nexus(i)%filcod)//'_'//trim(cfl%label)//'.xys'
              else
                  fname = trim(nexus(i)%filcod)//'.xys'
              end if
              if (cfl%is_tth_max) then
                  call write_pattern(trim(fname),pow_pat,'xys',xmin=xmin,xmax=cfl%tth_max)
              else
                  call write_pattern(trim(fname),pow_pat,'xys',xmin=xmin)
              end if
           end if !single
       end do


       if(cfl%Integ_1D) then

          call VerticalStraight_Integration(nexus,cfl,cal,pow_pat)

       else

          if(cfl%calibration) then
            call construct_virtual_detector(nexus,cfl%nscans,cfl%nsigma,cfl%nz_int,cfl%Norm_Monitor,cfl%verbose,cfl%Apply_Shifts,trim(cfl%calib_file))
          else
            call construct_virtual_detector(nexus,cfl%nscans,cfl%nsigma,cfl%nz_int,cfl%Norm_Monitor,cfl%verbose)
          end if
          write(scanf,'(a)') trim(cfl%combine_name)//'_'//trim(cfl%suffix)//'.nxs'
          write(*,'(4x,a)') ' => Writing nexus file '//trim(scanf)

          !Setting snexus%virtual_cgap
          last=cfl%nscans
          dims(1)=size(Data2D,1)
          dims(2)=size(Data2D,2)
          dims(3)=1
          call initialize_nexus(snexus,dims)
          call partial_nexus_copy(nexus(last),snexus)
          snexus%virtual_cgap=virtual_instrm%cgap
          snexus%angles(4,1)=ga_D_virtual
          snexus%angles(7,1)=nu_D_virtual
          snexus%counts(:,:,1)=nint(Data2D)
          call write_simple_nexus(trim(scanf),snexus) !Corresponds to the virtual instrument

          !Integration

          if(cfl%verbose) call display_nexus(6,snexus)

          ga_D = snexus%angles(4,1)
          nu_D = snexus%angles(7,1)
          np_horiz = size(snexus%counts,2)
          cgap = snexus%virtual_cgap
          if(cfl%verbose) then
             write(*,'(4x,a)') 'Setting parameters of the virtual detector'
             write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels:',np_horiz
             write(*,'(8x,a,1x,f6.2)') 'Horizontal pixel size (mm): ',cgap
             write(*,'(8x,a,1x,f6.2)') 'Gamma:',ga_D
             write(*,'(8x,a,1x,f6.2)') 'Nu:   ',nu_D
          end if

          if(cfl%monitor) then
             aver_mon=0.0
             do i=1, cfl%nscans
               aver_mon = aver_mon + sum(nexus(i)%monitor(:))/nexus(i)%nf
             end do
             aver_mon = aver_mon / cfl%nscans
             scale_fac=cfl%Norm_monitor/aver_mon
             if(scale_fac < 1.0e-5) scale_fac=1.0
             call get_powder_pattern(cfl%nz_int,scale_fac,np_horiz,cgap,ga_D,pow_pat)
          else
             call get_powder_pattern(cfl%nz_int,cfl%scale_fac,np_horiz,cgap,ga_D,pow_pat)
          end if
       end if


       if(Err_CFML%flag) then
          write(*,"(a)") " => "//trim(Err_CFML%Msg)
          call finish(t_ini)
       end if

       if (cfl%is_tth_min) then
           xmin = cfl%tth_min
       else
           xmin = pow_pat%xmin
       end if
       write(*,'(4x,2a)') ' => Writing xys file ', trim(cfl%combine_name)//'.xys'
       if (cfl%is_tth_max) then
           call write_pattern(trim(cfl%combine_name)//'.xys',pow_pat,'xys',xmin=xmin,xmax=cfl%tth_max)
       else
           call write_pattern(trim(cfl%combine_name)//'.xys',pow_pat,'xys',xmin=xmin)
       end if

      ! call cpu_time(t_end)
      ! write(unit=*, fmt='(/,a)')       ' => D2B_int finished normally  '
      ! write(unit=*, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_end-t_ini,' seconds'

    End Subroutine Integrate_D2B_data

    Subroutine finish(t_ini)

        ! Finish the program due to an error

        ! Arguments
        real, intent(in) :: t_ini

        call write_error_message(6,t_ini)
        stop

    End Subroutine finish

    Subroutine write_header(iout)

        ! Arguments
        integer, intent(in), optional :: iout

        ! Local variables
        integer :: i,lun

        lun=6
        if(present(iout)) lun=iout

        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)
        write(unit=lun,fmt='(13x,a)') ' Integrating D2B data'
        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)
        write(unit=lun,fmt='(1x,a)') ' Program: D2B_int, February 2023'
        write(unit=lun,fmt='(1x,a)') ' Authors: Nebil A. Katcho and J. Rodriguez-Carvajal'
        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)

    End Subroutine write_header

    Subroutine write_error_message(lun,t_ini)

        ! Stop the program, printing out an error message

        ! Arguments
        integer, intent(in) :: lun
        real,    intent(in) :: t_ini

        ! Local variables
        real :: t_fin

        call cpu_time(t_fin)
        write(unit=lun, fmt='(a,a)')       ' => D2B_int stopped!: ', trim(err_CFML%Msg)
        write(unit=lun, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_fin-t_ini,' seconds'

    End Subroutine write_error_message

    Subroutine write_warning_message(lun)

        ! Print a warning message

        ! Arguments
        integer, intent(in) :: lun

        write(unit=lun, fmt='(a,a)') ' => Warning!: ', trim(war_D2B_mess)

    End Subroutine write_warning_message

End Module D2B_int_mod