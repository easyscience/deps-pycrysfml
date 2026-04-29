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

program D2B_int

    use CFML_GlobalDeps,      only: err_CFML
    use D2B_read_mod,         only: cfl_D2B_type
    use D2B_int_mod

    implicit none
    real               :: t_ini,t_end
    type(cfl_D2B_type) :: cfl
    character(len=256) :: cfl_file

    ! Starting time
    call cpu_time(t_ini)
    call write_header(6)

    ! Read the cfl file
    call Get_Command_Argument(1,cfl_file)


    call Integrate_D2B_data(cfl_file,"d2b",cfl)
    if(Err_CFML%Ierr /= 0) call finish(t_ini)


    call cpu_time(t_end)
    write(unit=*, fmt='(/,a)')       ' => D2B_int finished normally  '
    write(unit=*, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_end-t_ini,' seconds'

  contains

    subroutine finish(t_ini)

        ! Finish the program due to an error

        ! Arguments
        real, intent(in) :: t_ini

        call write_error_message(6,t_ini)
        stop

    end subroutine finish

    subroutine write_header(iout)

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

    end subroutine write_header

    subroutine write_error_message(lun,t_ini)

        ! Stop the program, printing out an error message

        ! Arguments
        integer, intent(in) :: lun
        real,    intent(in) :: t_ini

        ! Local variables
        real :: t_fin

        call cpu_time(t_fin)
        write(unit=lun, fmt='(a,a)')       ' => D2B_int stopped!: ', trim(err_CFML%Msg)
        write(unit=lun, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_fin-t_ini,' seconds'

    end subroutine write_error_message


end program D2B_int