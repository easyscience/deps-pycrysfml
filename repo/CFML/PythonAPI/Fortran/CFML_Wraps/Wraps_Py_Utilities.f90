submodule (CFML_Wraps) Wraps_Py_Utilities

    implicit none
    contains

    Module Subroutine Wrap_esmeralda_laue_image_type(for_var,py_var,ierror)

        ! Arguments
        type(esmeralda_laue_image_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_hkl,nd_xz,nd_xz_stereo

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','esmeralda_laue_image_type')
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (allocated(for_var%hkl)) then
            if (ierror == 0) ierror = ndarray_create(nd_hkl,for_var%hkl)
            if (ierror == 0) ierror = py_var%setitem('hkl',nd_hkl)
        end if
        if (allocated(for_var%xz)) then
            if (ierror == 0) ierror = ndarray_create(nd_xz,for_var%xz)
            if (ierror == 0) ierror = py_var%setitem('xz',nd_xz)
        end if
        if (allocated(for_var%xz_stereo)) then
            if (ierror == 0) ierror = ndarray_create(nd_xz_stereo,for_var%xz_stereo)
            if (ierror == 0) ierror = py_var%setitem('xz_stereo',nd_xz_stereo)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_esmeralda_laue_image_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_esmeralda_laue_image_type

    Module Subroutine Unwrap_type_esmeralda_laue_image_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(esmeralda_laue_image_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_esmeralda_laue_image_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'esmeralda_laue_image_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_esmeralda_laue_image_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_esmeralda_laue_image_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_esmeralda_laue_image_type','hkl',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_esmeralda_laue_image_type','hkl',p_real_2d,for_var%hkl,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_esmeralda_laue_image_type','xz',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_esmeralda_laue_image_type','xz',p_real_2d,for_var%xz,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_esmeralda_laue_image_type','xz_stereo',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_esmeralda_laue_image_type','xz_stereo',p_real_2d,for_var%xz_stereo,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_esmeralda_laue_image_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_esmeralda_laue_image_type

    Module Subroutine list_to_type_array1d_esmeralda_laue_image_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(esmeralda_laue_image_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_esmeralda_laue_image_type

    Module Subroutine list_to_type_array1d_esmeralda_laue_image_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(esmeralda_laue_image_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_esmeralda_laue_image_type_no_alloc

    Module Subroutine list_to_type_array2d_esmeralda_laue_image_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(esmeralda_laue_image_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_esmeralda_laue_image_type

    Module Subroutine list_to_type_array2d_esmeralda_laue_image_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(esmeralda_laue_image_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_esmeralda_laue_image_type_no_alloc

    Module Subroutine Wrap_coordination_asu_type(for_var,py_var,ierror)

        ! Arguments
        type(coordination_asu_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_orbit
        type(list) :: li_orbit

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','coordination_asu_type')
        if (allocated(for_var%orbit)) then
            if (ierror == 0) ierror = list_create(li_orbit)
            if (ierror == 0) then
                do i = 1 , size(for_var%orbit)
                    ierror = dict_create(di_orbit)
                    if (ierror == 0) call wrap_type(for_var%orbit(i),di_orbit,ierror)
                    if (ierror == 0) ierror = li_orbit%append(di_orbit)
                    if (ierror == 0) call di_orbit%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('orbit',li_orbit)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_coordination_asu_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_coordination_asu_type

    Module Subroutine Unwrap_type_coordination_asu_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(coordination_asu_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_coordination_asu_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'coordination_asu_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_coordination_asu_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_asu_type','orbit',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_coordination_asu_type','orbit',my_list,for_var%orbit,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_coordination_asu_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_coordination_asu_type

    Module Subroutine list_to_type_array1d_coordination_asu_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_asu_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_asu_type

    Module Subroutine list_to_type_array1d_coordination_asu_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_asu_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_asu_type_no_alloc

    Module Subroutine list_to_type_array2d_coordination_asu_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_asu_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_asu_type

    Module Subroutine list_to_type_array2d_coordination_asu_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_asu_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_asu_type_no_alloc

    Module Subroutine Wrap_coordination_crystal_type(for_var,py_var,ierror)

        ! Arguments
        type(coordination_crystal_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_asu
        type(list) :: li_asu
        type(ndarray) :: nd_dmin,nd_dmax

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','coordination_crystal_type')
        if (ierror == 0) ierror = py_var%setitem('nextra',for_var%nextra)
        if (allocated(for_var%dmin)) then
            if (ierror == 0) ierror = ndarray_create(nd_dmin,for_var%dmin)
            if (ierror == 0) ierror = py_var%setitem('dmin',nd_dmin)
        end if
        if (allocated(for_var%dmax)) then
            if (ierror == 0) ierror = ndarray_create(nd_dmax,for_var%dmax)
            if (ierror == 0) ierror = py_var%setitem('dmax',nd_dmax)
        end if
        if (allocated(for_var%asu)) then
            if (ierror == 0) ierror = list_create(li_asu)
            if (ierror == 0) then
                do i = 1 , size(for_var%asu)
                    ierror = dict_create(di_asu)
                    if (ierror == 0) call wrap_type(for_var%asu(i),di_asu,ierror)
                    if (ierror == 0) ierror = li_asu%append(di_asu)
                    if (ierror == 0) call di_asu%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('asu',li_asu)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_coordination_crystal_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_coordination_crystal_type

    Module Subroutine Unwrap_type_coordination_crystal_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(coordination_crystal_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_coordination_crystal_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'coordination_crystal_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_coordination_crystal_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_crystal_type','nextra',py_var,for_var%nextra,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_crystal_type','dmin',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_coordination_crystal_type','dmin',p_real_2d,for_var%dmin,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_crystal_type','dmax',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_coordination_crystal_type','dmax',p_real_2d,for_var%dmax,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_crystal_type','asu',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_coordination_crystal_type','asu',my_list,for_var%asu,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_coordination_crystal_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_coordination_crystal_type

    Module Subroutine list_to_type_array1d_coordination_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_crystal_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_crystal_type

    Module Subroutine list_to_type_array1d_coordination_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_crystal_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_crystal_type_no_alloc

    Module Subroutine list_to_type_array2d_coordination_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_crystal_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_crystal_type

    Module Subroutine list_to_type_array2d_coordination_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_crystal_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_crystal_type_no_alloc

    Module Subroutine Wrap_coordination_orbit_type(for_var,py_var,ierror)

        ! Arguments
        type(coordination_orbit_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_bond
        type(list) :: li_bond

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','coordination_orbit_type')
        if (ierror == 0) ierror = py_var%setitem('nbonds',for_var%nbonds)
        if (allocated(for_var%bond)) then
            if (ierror == 0) ierror = list_create(li_bond)
            if (ierror == 0) then
                do i = 1 , size(for_var%bond)
                    ierror = dict_create(di_bond)
                    if (ierror == 0) call wrap_type(for_var%bond(i),di_bond,ierror)
                    if (ierror == 0) ierror = li_bond%append(di_bond)
                    if (ierror == 0) call di_bond%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('bond',li_bond)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_coordination_orbit_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_coordination_orbit_type

    Module Subroutine Unwrap_type_coordination_orbit_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(coordination_orbit_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_coordination_orbit_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'coordination_orbit_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_coordination_orbit_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_orbit_type','nbonds',py_var,for_var%nbonds,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_orbit_type','bond',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_coordination_orbit_type','bond',my_list,for_var%bond,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_coordination_orbit_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_coordination_orbit_type

    Module Subroutine list_to_type_array1d_coordination_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_orbit_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_orbit_type

    Module Subroutine list_to_type_array1d_coordination_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_orbit_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_coordination_orbit_type_no_alloc

    Module Subroutine list_to_type_array2d_coordination_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_orbit_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_orbit_type

    Module Subroutine list_to_type_array2d_coordination_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_orbit_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_coordination_orbit_type_no_alloc

    Module Subroutine Wrap_crystal_bond_type(for_var,py_var,ierror)

        ! Arguments
        type(crystal_bond_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_tr,nd_rc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','crystal_bond_type')
        if (ierror == 0) ierror = py_var%setitem('asu',for_var%asu)
        if (ierror == 0) ierror = py_var%setitem('orbit',for_var%orbit)
        if (ierror == 0) ierror = py_var%setitem('id',for_var%id)
        if (ierror == 0) ierror = ndarray_create(nd_tr,for_var%tr)
        if (ierror == 0) ierror = py_var%setitem('tr',nd_tr)
        if (ierror == 0) ierror = ndarray_create(nd_rc,for_var%rc)
        if (ierror == 0) ierror = py_var%setitem('rc',nd_rc)
        if (ierror == 0) ierror = py_var%setitem('d',for_var%d)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_crystal_bond_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_crystal_bond_type

    Module Subroutine Unwrap_type_crystal_bond_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(crystal_bond_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_crystal_bond_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'crystal_bond_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_crystal_bond_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','asu',py_var,for_var%asu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','orbit',py_var,for_var%orbit,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','id',py_var,for_var%id,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','tr',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_crystal_bond_type','tr',p_int_1d,for_var%tr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','rc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_crystal_bond_type','rc',p_real_1d,for_var%rc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_bond_type','d',py_var,for_var%d,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_crystal_bond_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_crystal_bond_type

    Module Subroutine list_to_type_array1d_crystal_bond_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_bond_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_crystal_bond_type

    Module Subroutine list_to_type_array1d_crystal_bond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_bond_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_crystal_bond_type_no_alloc

    Module Subroutine list_to_type_array2d_crystal_bond_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_bond_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_crystal_bond_type

    Module Subroutine list_to_type_array2d_crystal_bond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_bond_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_crystal_bond_type_no_alloc

    Module Subroutine Wrap_crystal_type(for_var,py_var,ierror)

        ! Arguments
        type(crystal_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_cell,di_spg,di_asu,di_orbit,di_mgp,di_am,di_element
        type(list) :: li_orbit,li_element
        type(ndarray) :: nd_asu2elem,nd_magnetic,nd_split,nd_id,nd_magat,nd_magtr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','crystal_type')
        if (ierror == 0) then
            ierror = py_var%setitem('file',for_var%file)
            if (ierror /= 0) then
                for_var%file = ''
                ierror = py_var%setitem('file',for_var%file)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('maxmul',for_var%maxmul)
        if (ierror == 0) ierror = py_var%setitem('nat_cell',for_var%nat_cell)
        if (ierror == 0) ierror = py_var%setitem('nat_masu',for_var%nat_masu)
        if (ierror == 0) ierror = py_var%setitem('nelem',for_var%nelem)
        if (ierror == 0) ierror = py_var%setitem('fourier',for_var%fourier)
        if (ierror == 0) ierror = py_var%setitem('nuclear',for_var%nuclear)
        if (ierror == 0) ierror = py_var%setitem('super',for_var%super)
        if (ierror == 0) ierror = dict_create(di_cell)
        if (ierror == 0) call wrap_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) ierror = dict_create(di_spg)
        if (ierror == 0) call wrap_type(for_var%spg,di_spg,ierror)
        if (ierror == 0) ierror = py_var%setitem('spg',di_spg)
        if (ierror == 0) ierror = dict_create(di_asu)
        if (ierror == 0) call wrap_type(for_var%asu,di_asu,ierror)
        if (ierror == 0) ierror = py_var%setitem('asu',di_asu)
        if (allocated(for_var%orbit)) then
            if (ierror == 0) ierror = list_create(li_orbit)
            if (ierror == 0) then
                do i = 1 , size(for_var%orbit)
                    ierror = dict_create(di_orbit)
                    if (ierror == 0) call wrap_type(for_var%orbit(i),di_orbit,ierror)
                    if (ierror == 0) ierror = li_orbit%append(di_orbit)
                    if (ierror == 0) call di_orbit%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('orbit',li_orbit)
        end if
        if (ierror == 0) ierror = dict_create(di_mgp)
        if (ierror == 0) call wrap_type(for_var%mgp,di_mgp,ierror)
        if (ierror == 0) ierror = py_var%setitem('mgp',di_mgp)
        if (ierror == 0) ierror = dict_create(di_am)
        if (ierror == 0) call wrap_type(for_var%am,di_am,ierror)
        if (ierror == 0) ierror = py_var%setitem('am',di_am)
        if (allocated(for_var%element)) then
            if (ierror == 0) ierror = list_create(li_element)
            if (ierror == 0) then
                do i = 1 , size(for_var%element)
                    if (ierror == 0) then
                        ierror = li_element%append(for_var%element(i))
                        if (ierror /= 0) then
                            for_var%element(i) = ''
                            ierror = li_element%append(for_var%element(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('element',li_element)
        end if
        if (allocated(for_var%asu2elem)) then
            if (ierror == 0) ierror = ndarray_create(nd_asu2elem,for_var%asu2elem)
            if (ierror == 0) ierror = py_var%setitem('asu2elem',nd_asu2elem)
        end if
        if (allocated(for_var%magnetic)) then
            if (ierror == 0) ierror = ndarray_create(nd_magnetic,for_var%magnetic)
            if (ierror == 0) ierror = py_var%setitem('magnetic',nd_magnetic)
        end if
        if (allocated(for_var%split)) then
            if (ierror == 0) ierror = ndarray_create(nd_split,for_var%split)
            if (ierror == 0) ierror = py_var%setitem('split',nd_split)
        end if
        if (allocated(for_var%id)) then
            if (ierror == 0) ierror = ndarray_create(nd_id,for_var%id)
            if (ierror == 0) ierror = py_var%setitem('id',nd_id)
        end if
        if (allocated(for_var%magat)) then
            if (ierror == 0) ierror = ndarray_create(nd_magat,for_var%magat)
            if (ierror == 0) ierror = py_var%setitem('magat',nd_magat)
        end if
        if (allocated(for_var%magtr)) then
            if (ierror == 0) ierror = ndarray_create(nd_magtr,for_var%magtr)
            if (ierror == 0) ierror = py_var%setitem('magtr',nd_magtr)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_crystal_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_crystal_type

    Module Subroutine Unwrap_type_crystal_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(crystal_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list
        type(dict) :: dict_cell,dict_spg,dict_asu,dict_mgp,dict_am

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_crystal_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'crystal_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_crystal_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_crystal_type','file',py_var,for_var%file,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','maxmul',py_var,for_var%maxmul,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','nat_cell',py_var,for_var%nat_cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','nat_masu',py_var,for_var%nat_masu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','nelem',py_var,for_var%nelem,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','fourier',py_var,for_var%fourier,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','nuclear',py_var,for_var%nuclear,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','super',py_var,for_var%super,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','cell',py_var,dict_cell,ierror)
        if (ierror == 0) call unwrap_class_cell_g_type(dict_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','spg',py_var,dict_spg,ierror)
        if (ierror == 0) call unwrap_class_spg_type(dict_spg,for_var%spg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','asu',py_var,dict_asu,ierror)
        if (ierror == 0) call unwrap_type(dict_asu,for_var%asu,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','orbit',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_crystal_type','orbit',my_list,for_var%orbit,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','mgp',py_var,dict_mgp,ierror)
        if (ierror == 0) call unwrap_type(dict_mgp,for_var%mgp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','am',py_var,dict_am,ierror)
        if (ierror == 0) call unwrap_type(dict_am,for_var%am,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','element',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%element,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','asu2elem',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','asu2elem',p_int_1d,for_var%asu2elem,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','magnetic',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','magnetic',p_int_1d,for_var%magnetic,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','split',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','split',p_int_1d,for_var%split,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','id',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','id',p_int_2d,for_var%id,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','magat',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','magat',p_int_3d,for_var%magat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_crystal_type','magtr',py_var,p_real_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_crystal_type','magtr',p_real_3d,for_var%magtr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_crystal_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_crystal_type

    Module Subroutine list_to_type_array1d_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_crystal_type

    Module Subroutine list_to_type_array1d_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_crystal_type_no_alloc

    Module Subroutine list_to_type_array2d_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_crystal_type

    Module Subroutine list_to_type_array2d_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(crystal_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_crystal_type_no_alloc

    Module Subroutine Wrap_envelope_type(for_var,py_var,ierror)

        ! Arguments
        type(envelope_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_x0,nd_t,nd_x,nd_v

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','envelope_type')
        if (ierror == 0) ierror = py_var%setitem('np',for_var%np)
        if (ierror == 0) ierror = ndarray_create(nd_x0,for_var%x0)
        if (ierror == 0) ierror = py_var%setitem('x0',nd_x0)
        if (ierror == 0) ierror = ndarray_create(nd_t,for_var%t)
        if (ierror == 0) ierror = py_var%setitem('t',nd_t)
        if (allocated(for_var%x)) then
            if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
            if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        end if
        if (allocated(for_var%v)) then
            if (ierror == 0) ierror = ndarray_create(nd_v,for_var%v)
            if (ierror == 0) ierror = py_var%setitem('v',nd_v)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_envelope_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_envelope_type

    Module Subroutine Unwrap_type_envelope_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(envelope_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_envelope_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'envelope_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_envelope_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_type','np',py_var,for_var%np,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_type','x0',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_envelope_type','x0',p_real_1d,for_var%x0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_type','t',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_envelope_type','t',p_real_1d,for_var%t,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_type','x',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_envelope_type','x',p_real_2d,for_var%x,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_type','v',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_envelope_type','v',p_real_2d,for_var%v,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_envelope_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_envelope_type

    Module Subroutine list_to_type_array1d_envelope_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_envelope_type

    Module Subroutine list_to_type_array1d_envelope_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_envelope_type_no_alloc

    Module Subroutine list_to_type_array2d_envelope_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_envelope_type

    Module Subroutine list_to_type_array2d_envelope_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_envelope_type_no_alloc

    Module Subroutine Wrap_envelope_list_type(for_var,py_var,ierror)

        ! Arguments
        type(envelope_list_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_env
        type(list) :: li_env

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','envelope_list_type')
        if (ierror == 0) ierror = py_var%setitem('nenv',for_var%nenv)
        if (allocated(for_var%env)) then
            if (ierror == 0) ierror = list_create(li_env)
            if (ierror == 0) then
                do i = 1 , size(for_var%env)
                    ierror = dict_create(di_env)
                    if (ierror == 0) call wrap_type(for_var%env(i),di_env,ierror)
                    if (ierror == 0) ierror = li_env%append(di_env)
                    if (ierror == 0) call di_env%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('env',li_env)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_envelope_list_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_envelope_list_type

    Module Subroutine Unwrap_type_envelope_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(envelope_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_envelope_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'envelope_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_envelope_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_list_type','nenv',py_var,for_var%nenv,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_envelope_list_type','env',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_envelope_list_type','env',my_list,for_var%env,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_envelope_list_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_envelope_list_type

    Module Subroutine list_to_type_array1d_envelope_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_list_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_envelope_list_type

    Module Subroutine list_to_type_array1d_envelope_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_list_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_envelope_list_type_no_alloc

    Module Subroutine list_to_type_array2d_envelope_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_list_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_envelope_list_type

    Module Subroutine list_to_type_array2d_envelope_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(envelope_list_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_envelope_list_type_no_alloc

    Module Subroutine Wrap_graphical_crystal_type(for_var,py_var,ierror)

        ! Arguments
        type(graphical_crystal_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_magnetic,nd_z,nd_ncoor,nd_id_atom,nd_bond_asu,nd_id,nd_bond_id,nd_bond_z,nd_pos_c,nd_mom_c,nd_bond_orig,nd_bond_c

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','graphical_crystal_type')
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('nbonds',for_var%nbonds)
        if (allocated(for_var%magnetic)) then
            if (ierror == 0) ierror = ndarray_create(nd_magnetic,for_var%magnetic)
            if (ierror == 0) ierror = py_var%setitem('magnetic',nd_magnetic)
        end if
        if (allocated(for_var%z)) then
            if (ierror == 0) ierror = ndarray_create(nd_z,for_var%z)
            if (ierror == 0) ierror = py_var%setitem('z',nd_z)
        end if
        if (allocated(for_var%ncoor)) then
            if (ierror == 0) ierror = ndarray_create(nd_ncoor,for_var%ncoor)
            if (ierror == 0) ierror = py_var%setitem('ncoor',nd_ncoor)
        end if
        if (allocated(for_var%id_atom)) then
            if (ierror == 0) ierror = ndarray_create(nd_id_atom,for_var%id_atom)
            if (ierror == 0) ierror = py_var%setitem('id_atom',nd_id_atom)
        end if
        if (allocated(for_var%bond_asu)) then
            if (ierror == 0) ierror = ndarray_create(nd_bond_asu,for_var%bond_asu)
            if (ierror == 0) ierror = py_var%setitem('bond_asu',nd_bond_asu)
        end if
        if (allocated(for_var%id)) then
            if (ierror == 0) ierror = ndarray_create(nd_id,for_var%id)
            if (ierror == 0) ierror = py_var%setitem('id',nd_id)
        end if
        if (allocated(for_var%bond_id)) then
            if (ierror == 0) ierror = ndarray_create(nd_bond_id,for_var%bond_id)
            if (ierror == 0) ierror = py_var%setitem('bond_id',nd_bond_id)
        end if
        if (allocated(for_var%bond_z)) then
            if (ierror == 0) ierror = ndarray_create(nd_bond_z,for_var%bond_z)
            if (ierror == 0) ierror = py_var%setitem('bond_z',nd_bond_z)
        end if
        if (allocated(for_var%pos_c)) then
            if (ierror == 0) ierror = ndarray_create(nd_pos_c,for_var%pos_c)
            if (ierror == 0) ierror = py_var%setitem('pos_c',nd_pos_c)
        end if
        if (allocated(for_var%mom_c)) then
            if (ierror == 0) ierror = ndarray_create(nd_mom_c,for_var%mom_c)
            if (ierror == 0) ierror = py_var%setitem('mom_c',nd_mom_c)
        end if
        if (allocated(for_var%bond_orig)) then
            if (ierror == 0) ierror = ndarray_create(nd_bond_orig,for_var%bond_orig)
            if (ierror == 0) ierror = py_var%setitem('bond_orig',nd_bond_orig)
        end if
        if (allocated(for_var%bond_c)) then
            if (ierror == 0) ierror = ndarray_create(nd_bond_c,for_var%bond_c)
            if (ierror == 0) ierror = py_var%setitem('bond_c',nd_bond_c)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_graphical_crystal_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_graphical_crystal_type

    Module Subroutine Unwrap_type_graphical_crystal_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(graphical_crystal_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_graphical_crystal_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'graphical_crystal_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_graphical_crystal_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','nbonds',py_var,for_var%nbonds,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','magnetic',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','magnetic',p_int_1d,for_var%magnetic,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','z',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','z',p_int_1d,for_var%z,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','ncoor',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','ncoor',p_int_1d,for_var%ncoor,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','id_atom',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','id_atom',p_int_2d,for_var%id_atom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','bond_asu',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','bond_asu',p_int_1d,for_var%bond_asu,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','id',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','id',p_int_1d,for_var%id,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','bond_id',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','bond_id',p_int_1d,for_var%bond_id,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','bond_z',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','bond_z',p_int_1d,for_var%bond_z,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','pos_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','pos_c',p_real_2d,for_var%pos_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','mom_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','mom_c',p_real_2d,for_var%mom_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','bond_orig',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','bond_orig',p_real_2d,for_var%bond_orig,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_graphical_crystal_type','bond_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_graphical_crystal_type','bond_c',p_real_2d,for_var%bond_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_graphical_crystal_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_graphical_crystal_type

    Module Subroutine list_to_type_array1d_graphical_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(graphical_crystal_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_graphical_crystal_type

    Module Subroutine list_to_type_array1d_graphical_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(graphical_crystal_type), dimension(*), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_graphical_crystal_type_no_alloc

    Module Subroutine list_to_type_array2d_graphical_crystal_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(graphical_crystal_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                allocate(arr(n,m))
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_graphical_crystal_type

    Module Subroutine list_to_type_array2d_graphical_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(graphical_crystal_type), dimension(:,:), intent(inout) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_type(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_graphical_crystal_type_no_alloc

end submodule