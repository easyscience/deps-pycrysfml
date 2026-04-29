submodule (CFML_Wraps) Wraps_Laue

    implicit none
    contains

    Module Subroutine Wrap_excluded_regions_type(for_var,py_var,ierror)

        ! Arguments
        type(excluded_regions_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_exc_rect,nd_exc_circ

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','excluded_regions_type')
        if (ierror == 0) ierror = py_var%setitem('nexc_rect',for_var%nexc_rect)
        if (ierror == 0) ierror = py_var%setitem('nexc_circ',for_var%nexc_circ)
        if (ierror == 0) ierror = py_var%setitem('imax',for_var%imax)
        if (ierror == 0) ierror = py_var%setitem('jmax',for_var%jmax)
        if (ierror == 0) ierror = py_var%setitem('inv_circ',for_var%inv_circ)
        if (ierror == 0) ierror = py_var%setitem('inv_rect',for_var%inv_rect)
        if (allocated(for_var%exc_rect)) then
            if (ierror == 0) ierror = ndarray_create(nd_exc_rect,for_var%exc_rect)
            if (ierror == 0) ierror = py_var%setitem('exc_rect',nd_exc_rect)
        end if
        if (allocated(for_var%exc_circ)) then
            if (ierror == 0) ierror = ndarray_create(nd_exc_circ,for_var%exc_circ)
            if (ierror == 0) ierror = py_var%setitem('exc_circ',nd_exc_circ)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_excluded_regions_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_excluded_regions_type

    Module Subroutine Unwrap_type_excluded_regions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(excluded_regions_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_excluded_regions_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'excluded_regions_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_excluded_regions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','nexc_rect',py_var,for_var%nexc_rect,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','nexc_circ',py_var,for_var%nexc_circ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','imax',py_var,for_var%imax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','jmax',py_var,for_var%jmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','inv_circ',py_var,for_var%inv_circ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','inv_rect',py_var,for_var%inv_rect,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','exc_rect',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_excluded_regions_type','exc_rect',p_int_3d,for_var%exc_rect,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_excluded_regions_type','exc_circ',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_excluded_regions_type','exc_circ',p_int_2d,for_var%exc_circ,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_excluded_regions_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_excluded_regions_type

    Module Subroutine list_to_type_array1d_excluded_regions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excluded_regions_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_excluded_regions_type

    Module Subroutine list_to_type_array1d_excluded_regions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excluded_regions_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_excluded_regions_type_no_alloc

    Module Subroutine list_to_type_array2d_excluded_regions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excluded_regions_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_excluded_regions_type

    Module Subroutine list_to_type_array2d_excluded_regions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excluded_regions_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_excluded_regions_type_no_alloc

    Module Subroutine Wrap_header_type(for_var,py_var,ierror)

        ! Arguments
        type(header_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_comment,di_scalar_name,di_vector_name,di_matrix_name,di_envnt_name,di_envnt_unit,di_motor_name,di_motor_unit
        type(list) :: li_comment,li_scalar_name,li_vector_name,li_matrix_name,li_envnt_name,li_envnt_unit,li_motor_name,li_motor_unit
        type(ndarray) :: nd_flags,nd_time,nd_monitor,nd_counts,nd_scalar_value,nd_vector_value,nd_matrix_value,nd_envnt_value,nd_motor_value

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','header_type')
        if (ierror == 0) ierror = py_var%setitem('header_length',for_var%header_length)
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('user_lc_date_time',for_var%user_lc_date_time)
            if (ierror /= 0) then
                for_var%user_lc_date_time = ''
                ierror = py_var%setitem('user_lc_date_time',for_var%user_lc_date_time)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('nframes',for_var%nframes)
        if (ierror == 0) ierror = py_var%setitem('nbits',for_var%nbits)
        if (ierror == 0) ierror = py_var%setitem('nrow',for_var%nrow)
        if (ierror == 0) ierror = py_var%setitem('ncol',for_var%ncol)
        if (ierror == 0) then
            ierror = py_var%setitem('instrument_name',for_var%instrument_name)
            if (ierror /= 0) then
                for_var%instrument_name = ''
                ierror = py_var%setitem('instrument_name',for_var%instrument_name)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('detector_type',for_var%detector_type)
            if (ierror /= 0) then
                for_var%detector_type = ''
                ierror = py_var%setitem('detector_type',for_var%detector_type)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('colrow_order',for_var%colrow_order)
            if (ierror /= 0) then
                for_var%colrow_order = ''
                ierror = py_var%setitem('colrow_order',for_var%colrow_order)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_flags,for_var%flags)
        if (ierror == 0) ierror = py_var%setitem('flags',nd_flags)
        if (ierror == 0) ierror = py_var%setitem('pix_size_h',for_var%pix_size_h)
        if (ierror == 0) ierror = py_var%setitem('pix_size_v',for_var%pix_size_v)
        if (ierror == 0) ierror = py_var%setitem('sample_detector_dist',for_var%sample_detector_dist)
        if (ierror == 0) ierror = py_var%setitem('lambda_min',for_var%lambda_min)
        if (ierror == 0) ierror = py_var%setitem('lambda_max',for_var%lambda_max)
        if (ierror == 0) then
            ierror = py_var%setitem('scan_type',for_var%scan_type)
            if (ierror /= 0) then
                for_var%scan_type = ''
                ierror = py_var%setitem('scan_type',for_var%scan_type)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('init_val',for_var%init_val)
        if (ierror == 0) ierror = py_var%setitem('step_val',for_var%step_val)
        if (ierror == 0) ierror = py_var%setitem('final_val',for_var%final_val)
        if (ierror == 0) ierror = py_var%setitem('sigma_factor',for_var%sigma_factor)
        if (ierror == 0) ierror = py_var%setitem('normalization_time',for_var%normalization_time)
        if (ierror == 0) ierror = py_var%setitem('normalization_monitor',for_var%normalization_monitor)
        if (allocated(for_var%time)) then
            if (ierror == 0) ierror = ndarray_create(nd_time,for_var%time)
            if (ierror == 0) ierror = py_var%setitem('time',nd_time)
        end if
        if (allocated(for_var%monitor)) then
            if (ierror == 0) ierror = ndarray_create(nd_monitor,for_var%monitor)
            if (ierror == 0) ierror = py_var%setitem('monitor',nd_monitor)
        end if
        if (allocated(for_var%counts)) then
            if (ierror == 0) ierror = ndarray_create(nd_counts,for_var%counts)
            if (ierror == 0) ierror = py_var%setitem('counts',nd_counts)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_comment_items',for_var%n_comment_items)
        if (allocated(for_var%comment)) then
            if (ierror == 0) ierror = list_create(li_comment)
            if (ierror == 0) then
                do i = 1 , size(for_var%comment)
                    if (ierror == 0) then
                        ierror = li_comment%append(for_var%comment(i))
                        if (ierror /= 0) then
                            for_var%comment(i) = ''
                            ierror = li_comment%append(for_var%comment(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('comment',li_comment)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_scalar_items',for_var%n_scalar_items)
        if (allocated(for_var%scalar_name)) then
            if (ierror == 0) ierror = list_create(li_scalar_name)
            if (ierror == 0) then
                do i = 1 , size(for_var%scalar_name)
                    if (ierror == 0) then
                        ierror = li_scalar_name%append(for_var%scalar_name(i))
                        if (ierror /= 0) then
                            for_var%scalar_name(i) = ''
                            ierror = li_scalar_name%append(for_var%scalar_name(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('scalar_name',li_scalar_name)
        end if
        if (allocated(for_var%scalar_value)) then
            if (ierror == 0) ierror = ndarray_create(nd_scalar_value,for_var%scalar_value)
            if (ierror == 0) ierror = py_var%setitem('scalar_value',nd_scalar_value)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_vector_items',for_var%n_vector_items)
        if (allocated(for_var%vector_name)) then
            if (ierror == 0) ierror = list_create(li_vector_name)
            if (ierror == 0) then
                do i = 1 , size(for_var%vector_name)
                    if (ierror == 0) then
                        ierror = li_vector_name%append(for_var%vector_name(i))
                        if (ierror /= 0) then
                            for_var%vector_name(i) = ''
                            ierror = li_vector_name%append(for_var%vector_name(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('vector_name',li_vector_name)
        end if
        if (allocated(for_var%vector_value)) then
            if (ierror == 0) ierror = ndarray_create(nd_vector_value,for_var%vector_value)
            if (ierror == 0) ierror = py_var%setitem('vector_value',nd_vector_value)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_matrix_items',for_var%n_matrix_items)
        if (allocated(for_var%matrix_name)) then
            if (ierror == 0) ierror = list_create(li_matrix_name)
            if (ierror == 0) then
                do i = 1 , size(for_var%matrix_name)
                    if (ierror == 0) then
                        ierror = li_matrix_name%append(for_var%matrix_name(i))
                        if (ierror /= 0) then
                            for_var%matrix_name(i) = ''
                            ierror = li_matrix_name%append(for_var%matrix_name(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('matrix_name',li_matrix_name)
        end if
        if (allocated(for_var%matrix_value)) then
            if (ierror == 0) ierror = ndarray_create(nd_matrix_value,for_var%matrix_value)
            if (ierror == 0) ierror = py_var%setitem('matrix_value',nd_matrix_value)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_environment',for_var%n_environment)
        if (allocated(for_var%envnt_name)) then
            if (ierror == 0) ierror = list_create(li_envnt_name)
            if (ierror == 0) then
                do i = 1 , size(for_var%envnt_name)
                    if (ierror == 0) then
                        ierror = li_envnt_name%append(for_var%envnt_name(i))
                        if (ierror /= 0) then
                            for_var%envnt_name(i) = ''
                            ierror = li_envnt_name%append(for_var%envnt_name(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('envnt_name',li_envnt_name)
        end if
        if (allocated(for_var%envnt_unit)) then
            if (ierror == 0) ierror = list_create(li_envnt_unit)
            if (ierror == 0) then
                do i = 1 , size(for_var%envnt_unit)
                    if (ierror == 0) then
                        ierror = li_envnt_unit%append(for_var%envnt_unit(i))
                        if (ierror /= 0) then
                            for_var%envnt_unit(i) = ''
                            ierror = li_envnt_unit%append(for_var%envnt_unit(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('envnt_unit',li_envnt_unit)
        end if
        if (allocated(for_var%envnt_value)) then
            if (ierror == 0) ierror = ndarray_create(nd_envnt_value,for_var%envnt_value)
            if (ierror == 0) ierror = py_var%setitem('envnt_value',nd_envnt_value)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_motors',for_var%n_motors)
        if (allocated(for_var%motor_name)) then
            if (ierror == 0) ierror = list_create(li_motor_name)
            if (ierror == 0) then
                do i = 1 , size(for_var%motor_name)
                    if (ierror == 0) then
                        ierror = li_motor_name%append(for_var%motor_name(i))
                        if (ierror /= 0) then
                            for_var%motor_name(i) = ''
                            ierror = li_motor_name%append(for_var%motor_name(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('motor_name',li_motor_name)
        end if
        if (allocated(for_var%motor_unit)) then
            if (ierror == 0) ierror = list_create(li_motor_unit)
            if (ierror == 0) then
                do i = 1 , size(for_var%motor_unit)
                    if (ierror == 0) then
                        ierror = li_motor_unit%append(for_var%motor_unit(i))
                        if (ierror /= 0) then
                            for_var%motor_unit(i) = ''
                            ierror = li_motor_unit%append(for_var%motor_unit(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('motor_unit',li_motor_unit)
        end if
        if (allocated(for_var%motor_value)) then
            if (ierror == 0) ierror = ndarray_create(nd_motor_value,for_var%motor_value)
            if (ierror == 0) ierror = py_var%setitem('motor_value',nd_motor_value)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_header_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_header_type

    Module Subroutine Unwrap_type_header_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(header_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_header_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'header_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_header_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','header_length',py_var,for_var%header_length,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','user_lc_date_time',py_var,for_var%user_lc_date_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','nframes',py_var,for_var%nframes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','nbits',py_var,for_var%nbits,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','nrow',py_var,for_var%nrow,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','ncol',py_var,for_var%ncol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','instrument_name',py_var,for_var%instrument_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','detector_type',py_var,for_var%detector_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','colrow_order',py_var,for_var%colrow_order,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','flags',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_header_type','flags',p_int_1d,for_var%flags,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','pix_size_h',py_var,for_var%pix_size_h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','pix_size_v',py_var,for_var%pix_size_v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','sample_detector_dist',py_var,for_var%sample_detector_dist,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','lambda_min',py_var,for_var%lambda_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','lambda_max',py_var,for_var%lambda_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','scan_type',py_var,for_var%scan_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','init_val',py_var,for_var%init_val,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','step_val',py_var,for_var%step_val,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','final_val',py_var,for_var%final_val,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','sigma_factor',py_var,for_var%sigma_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','normalization_time',py_var,for_var%normalization_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','normalization_monitor',py_var,for_var%normalization_monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','time',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','time',p_real_1d,for_var%time,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','monitor',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','monitor',p_real_1d,for_var%monitor,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','counts',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','counts',p_real_1d,for_var%counts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_comment_items',py_var,for_var%n_comment_items,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','comment',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%comment,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_scalar_items',py_var,for_var%n_scalar_items,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','scalar_name',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%scalar_name,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','scalar_value',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','scalar_value',p_real_1d,for_var%scalar_value,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_vector_items',py_var,for_var%n_vector_items,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','vector_name',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%vector_name,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','vector_value',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','vector_value',p_real_2d,for_var%vector_value,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_matrix_items',py_var,for_var%n_matrix_items,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','matrix_name',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%matrix_name,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','matrix_value',py_var,p_real_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','matrix_value',p_real_3d,for_var%matrix_value,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_environment',py_var,for_var%n_environment,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','envnt_name',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%envnt_name,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','envnt_unit',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%envnt_unit,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','envnt_value',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','envnt_value',p_real_2d,for_var%envnt_value,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','n_motors',py_var,for_var%n_motors,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','motor_name',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%motor_name,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','motor_unit',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%motor_unit,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_header_type','motor_value',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_header_type','motor_value',p_real_2d,for_var%motor_value,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_header_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_header_type

    Module Subroutine list_to_type_array1d_header_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(header_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_header_type

    Module Subroutine list_to_type_array1d_header_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(header_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_header_type_no_alloc

    Module Subroutine list_to_type_array2d_header_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(header_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_header_type

    Module Subroutine list_to_type_array2d_header_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(header_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_header_type_no_alloc

    Module Subroutine Wrap_image_conditions(for_var,py_var,ierror)

        ! Arguments
        type(image_conditions), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','image_conditions')
        if (ierror == 0) then
            ierror = py_var%setitem('file_name',for_var%file_name)
            if (ierror /= 0) then
                for_var%file_name = ''
                ierror = py_var%setitem('file_name',for_var%file_name)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('host',for_var%host)
            if (ierror /= 0) then
                for_var%host = ''
                ierror = py_var%setitem('host',for_var%host)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('datetime',for_var%datetime)
            if (ierror /= 0) then
                for_var%datetime = ''
                ierror = py_var%setitem('datetime',for_var%datetime)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('user',for_var%user)
            if (ierror /= 0) then
                for_var%user = ''
                ierror = py_var%setitem('user',for_var%user)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('sample',for_var%sample)
            if (ierror /= 0) then
                for_var%sample = ''
                ierror = py_var%setitem('sample',for_var%sample)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('comment',for_var%comment)
            if (ierror /= 0) then
                for_var%comment = ''
                ierror = py_var%setitem('comment',for_var%comment)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('temp_begin',for_var%temp_begin)
        if (ierror == 0) ierror = py_var%setitem('temp_end',for_var%temp_end)
        if (ierror == 0) ierror = py_var%setitem('expose_time',for_var%expose_time)
        if (ierror == 0) ierror = py_var%setitem('expose_phi',for_var%expose_phi)
        if (ierror == 0) ierror = py_var%setitem('temp_min',for_var%temp_min)
        if (ierror == 0) ierror = py_var%setitem('temp_max',for_var%temp_max)
        if (ierror == 0) ierror = py_var%setitem('xyres',for_var%xyres)
        if (ierror == 0) ierror = py_var%setitem('numx',for_var%numx)
        if (ierror == 0) ierror = py_var%setitem('numy',for_var%numy)
        if (ierror == 0) ierror = py_var%setitem('startx',for_var%startx)
        if (ierror == 0) ierror = py_var%setitem('starty',for_var%starty)
        if (ierror == 0) ierror = py_var%setitem('speed',for_var%speed)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_image_conditions: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_image_conditions

    Module Subroutine Unwrap_type_image_conditions(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(image_conditions), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_image_conditions: Cannot determine fortran type'
        else
            if (fortran_type /= 'image_conditions') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_image_conditions: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','file_name',py_var,for_var%file_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','host',py_var,for_var%host,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','datetime',py_var,for_var%datetime,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','user',py_var,for_var%user,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','sample',py_var,for_var%sample,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','comment',py_var,for_var%comment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','temp_begin',py_var,for_var%temp_begin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','temp_end',py_var,for_var%temp_end,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','expose_time',py_var,for_var%expose_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','expose_phi',py_var,for_var%expose_phi,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','temp_min',py_var,for_var%temp_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','temp_max',py_var,for_var%temp_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','xyres',py_var,for_var%xyres,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','numx',py_var,for_var%numx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','numy',py_var,for_var%numy,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','startx',py_var,for_var%startx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','starty',py_var,for_var%starty,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_image_conditions','speed',py_var,for_var%speed,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_image_conditions: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_image_conditions

    Module Subroutine list_to_type_array1d_image_conditions(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(image_conditions), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_image_conditions

    Module Subroutine list_to_type_array1d_image_conditions_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(image_conditions), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_image_conditions_no_alloc

    Module Subroutine list_to_type_array2d_image_conditions(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(image_conditions), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_image_conditions

    Module Subroutine list_to_type_array2d_image_conditions_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(image_conditions), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_image_conditions_no_alloc

    Module Subroutine Wrap_laue_instrument_type(for_var,py_var,ierror)

        ! Arguments
        type(laue_instrument_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rod,nd_rd,nd_dism

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','laue_instrument_type')
        if (ierror == 0) then
            ierror = py_var%setitem('info',for_var%info)
            if (ierror /= 0) then
                for_var%info = ''
                ierror = py_var%setitem('info',for_var%info)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('name',for_var%name)
            if (ierror /= 0) then
                for_var%name = ''
                ierror = py_var%setitem('name',for_var%name)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('dtype',for_var%dtype)
            if (ierror /= 0) then
                for_var%dtype = ''
                ierror = py_var%setitem('dtype',for_var%dtype)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('r_ord',for_var%r_ord)
            if (ierror /= 0) then
                for_var%r_ord = ''
                ierror = py_var%setitem('r_ord',for_var%r_ord)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('invert',for_var%invert)
            if (ierror /= 0) then
                for_var%invert = ''
                ierror = py_var%setitem('invert',for_var%invert)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('np_h',for_var%np_h)
        if (ierror == 0) ierror = py_var%setitem('np_v',for_var%np_v)
        if (ierror == 0) ierror = py_var%setitem('d',for_var%d)
        if (ierror == 0) ierror = py_var%setitem('ga_d',for_var%ga_d)
        if (ierror == 0) ierror = py_var%setitem('tiltx_d',for_var%tiltx_d)
        if (ierror == 0) ierror = py_var%setitem('tilty_d',for_var%tilty_d)
        if (ierror == 0) ierror = py_var%setitem('tiltz_d',for_var%tiltz_d)
        if (ierror == 0) ierror = py_var%setitem('h',for_var%h)
        if (ierror == 0) ierror = ndarray_create(nd_rod,for_var%rod)
        if (ierror == 0) ierror = py_var%setitem('rod',nd_rod)
        if (ierror == 0) ierror = ndarray_create(nd_rd,for_var%rd)
        if (ierror == 0) ierror = py_var%setitem('rd',nd_rd)
        if (ierror == 0) ierror = py_var%setitem('tilted',for_var%tilted)
        if (ierror == 0) ierror = py_var%setitem('displaced',for_var%displaced)
        if (ierror == 0) ierror = py_var%setitem('flip_hor',for_var%flip_hor)
        if (ierror == 0) ierror = py_var%setitem('flip_ver',for_var%flip_ver)
        if (ierror == 0) ierror = py_var%setitem('l_min',for_var%l_min)
        if (ierror == 0) ierror = py_var%setitem('l_max',for_var%l_max)
        if (ierror == 0) ierror = py_var%setitem('l_central',for_var%l_central)
        if (ierror == 0) ierror = py_var%setitem('gap_min',for_var%gap_min)
        if (ierror == 0) ierror = py_var%setitem('gap_max',for_var%gap_max)
        if (ierror == 0) ierror = py_var%setitem('gan_min',for_var%gan_min)
        if (ierror == 0) ierror = py_var%setitem('gan_max',for_var%gan_max)
        if (ierror == 0) ierror = py_var%setitem('nu_min',for_var%nu_min)
        if (ierror == 0) ierror = py_var%setitem('nu_max',for_var%nu_max)
        if (ierror == 0) ierror = py_var%setitem('d_min',for_var%d_min)
        if (ierror == 0) ierror = py_var%setitem('x_min',for_var%x_min)
        if (ierror == 0) ierror = py_var%setitem('x_max',for_var%x_max)
        if (ierror == 0) ierror = py_var%setitem('z_min',for_var%z_min)
        if (ierror == 0) ierror = py_var%setitem('z_max',for_var%z_max)
        if (ierror == 0) ierror = py_var%setitem('xo',for_var%xo)
        if (ierror == 0) ierror = py_var%setitem('zo',for_var%zo)
        if (ierror == 0) ierror = ndarray_create(nd_dism,for_var%dism)
        if (ierror == 0) ierror = py_var%setitem('dism',nd_dism)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_laue_instrument_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_laue_instrument_type

    Module Subroutine Unwrap_type_laue_instrument_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(laue_instrument_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_laue_instrument_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'laue_instrument_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_laue_instrument_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','info',py_var,for_var%info,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','name',py_var,for_var%name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','dtype',py_var,for_var%dtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','r_ord',py_var,for_var%r_ord,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','invert',py_var,for_var%invert,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','np_h',py_var,for_var%np_h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','np_v',py_var,for_var%np_v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','ga_d',py_var,for_var%ga_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','tiltx_d',py_var,for_var%tiltx_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','tilty_d',py_var,for_var%tilty_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','tiltz_d',py_var,for_var%tiltz_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','h',py_var,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','rod',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_instrument_type','rod',p_real_1d,for_var%rod,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','rd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_instrument_type','rd',p_real_2d,for_var%rd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','tilted',py_var,for_var%tilted,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','displaced',py_var,for_var%displaced,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','flip_hor',py_var,for_var%flip_hor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','flip_ver',py_var,for_var%flip_ver,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','l_min',py_var,for_var%l_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','l_max',py_var,for_var%l_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','l_central',py_var,for_var%l_central,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','gap_min',py_var,for_var%gap_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','gap_max',py_var,for_var%gap_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','gan_min',py_var,for_var%gan_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','gan_max',py_var,for_var%gan_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','nu_min',py_var,for_var%nu_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','nu_max',py_var,for_var%nu_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','d_min',py_var,for_var%d_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','x_min',py_var,for_var%x_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','x_max',py_var,for_var%x_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','z_min',py_var,for_var%z_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','z_max',py_var,for_var%z_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','xo',py_var,for_var%xo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','zo',py_var,for_var%zo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_instrument_type','dism',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_instrument_type','dism',p_real_1d,for_var%dism,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_laue_instrument_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_laue_instrument_type

    Module Subroutine list_to_type_array1d_laue_instrument_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_instrument_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_laue_instrument_type

    Module Subroutine list_to_type_array1d_laue_instrument_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_instrument_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_laue_instrument_type_no_alloc

    Module Subroutine list_to_type_array2d_laue_instrument_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_instrument_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_laue_instrument_type

    Module Subroutine list_to_type_array2d_laue_instrument_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_instrument_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_laue_instrument_type_no_alloc

    Module Subroutine Wrap_laue_ref_type(for_var,py_var,ierror)

        ! Arguments
        type(laue_ref_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_h,nd_zv,nd_kindex,nd_hs

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','laue_ref_type')
        if (ierror == 0) ierror = py_var%setitem('stats',for_var%stats)
        if (ierror == 0) ierror = py_var%setitem('domain',for_var%domain)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('nodal',for_var%nodal)
        if (ierror == 0) ierror = py_var%setitem('ds',for_var%ds)
        if (ierror == 0) ierror = py_var%setitem('gamma',for_var%gamma)
        if (ierror == 0) ierror = py_var%setitem('nu',for_var%nu)
        if (ierror == 0) ierror = py_var%setitem('ttheta',for_var%ttheta)
        if (ierror == 0) ierror = py_var%setitem('lambda',for_var%lambda)
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = ndarray_create(nd_zv,for_var%zv)
        if (ierror == 0) ierror = py_var%setitem('zv',nd_zv)
        if (ierror == 0) ierror = py_var%setitem('obs_int',for_var%obs_int)
        if (ierror == 0) ierror = py_var%setitem('cal_int',for_var%cal_int)
        if (ierror == 0) ierror = py_var%setitem('sigma',for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('x',for_var%x)
        if (ierror == 0) ierror = py_var%setitem('stheta',for_var%stheta)
        if (ierror == 0) ierror = py_var%setitem('sphi',for_var%sphi)
        if (ierror == 0) ierror = ndarray_create(nd_kindex,for_var%kindex)
        if (ierror == 0) ierror = py_var%setitem('kindex',nd_kindex)
        if (allocated(for_var%hs)) then
            if (ierror == 0) ierror = ndarray_create(nd_hs,for_var%hs)
            if (ierror == 0) ierror = py_var%setitem('hs',nd_hs)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_laue_ref_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_laue_ref_type

    Module Subroutine Unwrap_type_laue_ref_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(laue_ref_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_laue_ref_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'laue_ref_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_laue_ref_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','stats',py_var,for_var%stats,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','domain',py_var,for_var%domain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','nodal',py_var,for_var%nodal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','ds',py_var,for_var%ds,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','gamma',py_var,for_var%gamma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','nu',py_var,for_var%nu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','ttheta',py_var,for_var%ttheta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','lambda',py_var,for_var%lambda,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_ref_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','zv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_ref_type','zv',p_real_1d,for_var%zv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','obs_int',py_var,for_var%obs_int,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','cal_int',py_var,for_var%cal_int,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','sigma',py_var,for_var%sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','x',py_var,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','stheta',py_var,for_var%stheta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','sphi',py_var,for_var%sphi,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','kindex',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_laue_ref_type','kindex',p_int_1d,for_var%kindex,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_type','hs',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_laue_ref_type','hs',p_int_1d,for_var%hs,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_laue_ref_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_laue_ref_type

    Module Subroutine list_to_type_array1d_laue_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_laue_ref_type

    Module Subroutine list_to_type_array1d_laue_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_laue_ref_type_no_alloc

    Module Subroutine list_to_type_array2d_laue_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_laue_ref_type

    Module Subroutine list_to_type_array2d_laue_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_laue_ref_type_no_alloc

    Module Subroutine Wrap_laue_ref_list_type(for_var,py_var,ierror)

        ! Arguments
        type(laue_ref_list_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_lr
        type(list) :: li_lr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','laue_ref_list_type')
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (allocated(for_var%lr)) then
            if (ierror == 0) ierror = list_create(li_lr)
            if (ierror == 0) then
                do i = 1 , size(for_var%lr)
                    ierror = dict_create(di_lr)
                    if (ierror == 0) call wrap_type(for_var%lr(i),di_lr,ierror)
                    if (ierror == 0) ierror = li_lr%append(di_lr)
                    if (ierror == 0) call di_lr%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('lr',li_lr)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_laue_ref_list_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_laue_ref_list_type

    Module Subroutine Unwrap_type_laue_ref_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(laue_ref_list_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_laue_ref_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'laue_ref_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_laue_ref_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_laue_ref_list_type','lr',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_laue_ref_list_type','lr',my_list,for_var%lr,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_laue_ref_list_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_laue_ref_list_type

    Module Subroutine list_to_type_array1d_laue_ref_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_list_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_laue_ref_list_type

    Module Subroutine list_to_type_array1d_laue_ref_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_list_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_laue_ref_list_type_no_alloc

    Module Subroutine list_to_type_array2d_laue_ref_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_list_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_laue_ref_list_type

    Module Subroutine list_to_type_array2d_laue_ref_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(laue_ref_list_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_laue_ref_list_type_no_alloc

    Module Subroutine Wrap_peakfind_parameters_type(for_var,py_var,ierror)

        ! Arguments
        type(peakfind_parameters_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','peakfind_parameters_type')
        if (ierror == 0) ierror = py_var%setitem('d_min',for_var%d_min)
        if (ierror == 0) ierror = py_var%setitem('m_pk',for_var%m_pk)
        if (ierror == 0) ierror = py_var%setitem('pk_ar',for_var%pk_ar)
        if (ierror == 0) ierror = py_var%setitem('l_siz',for_var%l_siz)
        if (ierror == 0) ierror = py_var%setitem('cutoff',for_var%cutoff)
        if (ierror == 0) ierror = py_var%setitem('blocksz',for_var%blocksz)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_peakfind_parameters_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_peakfind_parameters_type

    Module Subroutine Unwrap_type_peakfind_parameters_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(peakfind_parameters_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_peakfind_parameters_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'peakfind_parameters_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_peakfind_parameters_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','d_min',py_var,for_var%d_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','m_pk',py_var,for_var%m_pk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','pk_ar',py_var,for_var%pk_ar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','l_siz',py_var,for_var%l_siz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','cutoff',py_var,for_var%cutoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peakfind_parameters_type','blocksz',py_var,for_var%blocksz,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_peakfind_parameters_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_peakfind_parameters_type

    Module Subroutine list_to_type_array1d_peakfind_parameters_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peakfind_parameters_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_peakfind_parameters_type

    Module Subroutine list_to_type_array1d_peakfind_parameters_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peakfind_parameters_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_peakfind_parameters_type_no_alloc

    Module Subroutine list_to_type_array2d_peakfind_parameters_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peakfind_parameters_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_peakfind_parameters_type

    Module Subroutine list_to_type_array2d_peakfind_parameters_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peakfind_parameters_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_peakfind_parameters_type_no_alloc

    Module Subroutine Wrap_peak_type(for_var,py_var,ierror)

        ! Arguments
        type(peak_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_cdm,nd_pos,nd_itens

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','peak_type')
        if (ierror == 0) ierror = py_var%setitem('npix',for_var%npix)
        if (ierror == 0) ierror = py_var%setitem('pnt',for_var%pnt)
        if (ierror == 0) ierror = ndarray_create(nd_cdm,for_var%cdm)
        if (ierror == 0) ierror = py_var%setitem('cdm',nd_cdm)
        if (ierror == 0) ierror = ndarray_create(nd_pos,for_var%pos)
        if (ierror == 0) ierror = py_var%setitem('pos',nd_pos)
        if (ierror == 0) ierror = ndarray_create(nd_itens,for_var%itens)
        if (ierror == 0) ierror = py_var%setitem('itens',nd_itens)
        if (ierror == 0) ierror = py_var%setitem('intensity',for_var%intensity)
        if (ierror == 0) ierror = py_var%setitem('sigma',for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('is_sat',for_var%is_sat)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_peak_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_peak_type

    Module Subroutine Unwrap_type_peak_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(peak_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_peak_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'peak_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_peak_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','npix',py_var,for_var%npix,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','pnt',py_var,for_var%pnt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','cdm',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_peak_type','cdm',p_real_1d,for_var%cdm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','pos',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_peak_type','pos',p_real_1d,for_var%pos,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','itens',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_peak_type','itens',p_real_1d,for_var%itens,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','intensity',py_var,for_var%intensity,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','sigma',py_var,for_var%sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_type','is_sat',py_var,for_var%is_sat,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_peak_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_peak_type

    Module Subroutine list_to_type_array1d_peak_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_peak_type

    Module Subroutine list_to_type_array1d_peak_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_peak_type_no_alloc

    Module Subroutine list_to_type_array2d_peak_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_peak_type

    Module Subroutine list_to_type_array2d_peak_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_peak_type_no_alloc

    Module Subroutine Wrap_peak_list_type(for_var,py_var,ierror)

        ! Arguments
        type(peak_list_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_peak
        type(list) :: li_peak

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','peak_list_type')
        if (ierror == 0) ierror = py_var%setitem('n_peaks',for_var%n_peaks)
        if (allocated(for_var%peak)) then
            if (ierror == 0) ierror = list_create(li_peak)
            if (ierror == 0) then
                do i = 1 , size(for_var%peak)
                    ierror = dict_create(di_peak)
                    if (ierror == 0) call wrap_type(for_var%peak(i),di_peak,ierror)
                    if (ierror == 0) ierror = li_peak%append(di_peak)
                    if (ierror == 0) call di_peak%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('peak',li_peak)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_peak_list_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_peak_list_type

    Module Subroutine Unwrap_type_peak_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(peak_list_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_peak_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'peak_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_peak_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_list_type','n_peaks',py_var,for_var%n_peaks,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_list_type','peak',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_peak_list_type','peak',my_list,for_var%peak,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_peak_list_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_peak_list_type

    Module Subroutine list_to_type_array1d_peak_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_list_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_peak_list_type

    Module Subroutine list_to_type_array1d_peak_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_list_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_peak_list_type_no_alloc

    Module Subroutine list_to_type_array2d_peak_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_list_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_peak_list_type

    Module Subroutine list_to_type_array2d_peak_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(peak_list_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_peak_list_type_no_alloc

end submodule