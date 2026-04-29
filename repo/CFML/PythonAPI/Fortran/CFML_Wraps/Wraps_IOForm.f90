submodule (CFML_Wraps) Wraps_IOForm

    implicit none
    contains

    Module Subroutine Wrap_job_info_type(for_var,py_var,ierror)

        ! Arguments
        type(job_info_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_patt_typ,di_phas_nam,di_cmd,di_range_stl,di_range_q,di_range_d,di_range_2theta,di_range_energy,di_range_tof,di_lambda
        type(list) :: li_patt_typ,li_phas_nam,li_cmd,li_range_stl,li_range_q,li_range_d,li_range_2theta,li_range_energy,li_range_tof,li_lambda
        type(ndarray) :: nd_ratio,nd_dtt1,nd_dtt2

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','job_info_type')
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('num_phases',for_var%num_phases)
        if (ierror == 0) ierror = py_var%setitem('num_patterns',for_var%num_patterns)
        if (ierror == 0) ierror = py_var%setitem('num_cmd',for_var%num_cmd)
        if (allocated(for_var%patt_typ)) then
            if (ierror == 0) ierror = list_create(li_patt_typ)
            if (ierror == 0) then
                do i = 1 , size(for_var%patt_typ)
                    if (ierror == 0) then
                        ierror = li_patt_typ%append(for_var%patt_typ(i))
                        if (ierror /= 0) then
                            for_var%patt_typ(i) = ''
                            ierror = li_patt_typ%append(for_var%patt_typ(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('patt_typ',li_patt_typ)
        end if
        if (allocated(for_var%phas_nam)) then
            if (ierror == 0) ierror = list_create(li_phas_nam)
            if (ierror == 0) then
                do i = 1 , size(for_var%phas_nam)
                    if (ierror == 0) then
                        ierror = li_phas_nam%append(for_var%phas_nam(i))
                        if (ierror /= 0) then
                            for_var%phas_nam(i) = ''
                            ierror = li_phas_nam%append(for_var%phas_nam(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('phas_nam',li_phas_nam)
        end if
        if (allocated(for_var%cmd)) then
            if (ierror == 0) ierror = list_create(li_cmd)
            if (ierror == 0) then
                do i = 1 , size(for_var%cmd)
                    if (ierror == 0) then
                        ierror = li_cmd%append(for_var%cmd(i))
                        if (ierror /= 0) then
                            for_var%cmd(i) = ''
                            ierror = li_cmd%append(for_var%cmd(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('cmd',li_cmd)
        end if
        if (allocated(for_var%range_stl)) then
            if (ierror == 0) ierror = list_create(li_range_stl)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_stl)
                    ierror = dict_create(di_range_stl)
                    if (ierror == 0) call wrap_type(for_var%range_stl(i),di_range_stl,ierror)
                    if (ierror == 0) ierror = li_range_stl%append(di_range_stl)
                    if (ierror == 0) call di_range_stl%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_stl',li_range_stl)
        end if
        if (allocated(for_var%range_q)) then
            if (ierror == 0) ierror = list_create(li_range_q)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_q)
                    ierror = dict_create(di_range_q)
                    if (ierror == 0) call wrap_type(for_var%range_q(i),di_range_q,ierror)
                    if (ierror == 0) ierror = li_range_q%append(di_range_q)
                    if (ierror == 0) call di_range_q%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_q',li_range_q)
        end if
        if (allocated(for_var%range_d)) then
            if (ierror == 0) ierror = list_create(li_range_d)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_d)
                    ierror = dict_create(di_range_d)
                    if (ierror == 0) call wrap_type(for_var%range_d(i),di_range_d,ierror)
                    if (ierror == 0) ierror = li_range_d%append(di_range_d)
                    if (ierror == 0) call di_range_d%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_d',li_range_d)
        end if
        if (allocated(for_var%range_2theta)) then
            if (ierror == 0) ierror = list_create(li_range_2theta)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_2theta)
                    ierror = dict_create(di_range_2theta)
                    if (ierror == 0) call wrap_type(for_var%range_2theta(i),di_range_2theta,ierror)
                    if (ierror == 0) ierror = li_range_2theta%append(di_range_2theta)
                    if (ierror == 0) call di_range_2theta%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_2theta',li_range_2theta)
        end if
        if (allocated(for_var%range_energy)) then
            if (ierror == 0) ierror = list_create(li_range_energy)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_energy)
                    ierror = dict_create(di_range_energy)
                    if (ierror == 0) call wrap_type(for_var%range_energy(i),di_range_energy,ierror)
                    if (ierror == 0) ierror = li_range_energy%append(di_range_energy)
                    if (ierror == 0) call di_range_energy%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_energy',li_range_energy)
        end if
        if (allocated(for_var%range_tof)) then
            if (ierror == 0) ierror = list_create(li_range_tof)
            if (ierror == 0) then
                do i = 1 , size(for_var%range_tof)
                    ierror = dict_create(di_range_tof)
                    if (ierror == 0) call wrap_type(for_var%range_tof(i),di_range_tof,ierror)
                    if (ierror == 0) ierror = li_range_tof%append(di_range_tof)
                    if (ierror == 0) call di_range_tof%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('range_tof',li_range_tof)
        end if
        if (allocated(for_var%lambda)) then
            if (ierror == 0) ierror = list_create(li_lambda)
            if (ierror == 0) then
                do i = 1 , size(for_var%lambda)
                    ierror = dict_create(di_lambda)
                    if (ierror == 0) call wrap_type(for_var%lambda(i),di_lambda,ierror)
                    if (ierror == 0) ierror = li_lambda%append(di_lambda)
                    if (ierror == 0) call di_lambda%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('lambda',li_lambda)
        end if
        if (allocated(for_var%ratio)) then
            if (ierror == 0) ierror = ndarray_create(nd_ratio,for_var%ratio)
            if (ierror == 0) ierror = py_var%setitem('ratio',nd_ratio)
        end if
        if (allocated(for_var%dtt1)) then
            if (ierror == 0) ierror = ndarray_create(nd_dtt1,for_var%dtt1)
            if (ierror == 0) ierror = py_var%setitem('dtt1',nd_dtt1)
        end if
        if (allocated(for_var%dtt2)) then
            if (ierror == 0) ierror = ndarray_create(nd_dtt2,for_var%dtt2)
            if (ierror == 0) ierror = py_var%setitem('dtt2',nd_dtt2)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_job_info_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_job_info_type

    Module Subroutine Unwrap_type_job_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(job_info_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_job_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'job_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_job_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_phases',py_var,for_var%num_phases,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_patterns',py_var,for_var%num_patterns,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_cmd',py_var,for_var%num_cmd,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','patt_typ',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%patt_typ,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','phas_nam',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%phas_nam,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','cmd',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%cmd,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_stl',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_stl',my_list,for_var%range_stl,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_q',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_q',my_list,for_var%range_q,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_d',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_d',my_list,for_var%range_d,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_2theta',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_2theta',my_list,for_var%range_2theta,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_energy',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_energy',my_list,for_var%range_energy,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_tof',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','range_tof',my_list,for_var%range_tof,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','lambda',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_job_info_type','lambda',my_list,for_var%lambda,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','ratio',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_job_info_type','ratio',p_real_1d,for_var%ratio,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','dtt1',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_job_info_type','dtt1',p_real_1d,for_var%dtt1,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','dtt2',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_job_info_type','dtt2',p_real_1d,for_var%dtt2,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_job_info_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_job_info_type

    Module Subroutine list_to_type_array1d_job_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_job_info_type

    Module Subroutine list_to_type_array1d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_job_info_type_no_alloc

    Module Subroutine list_to_type_array2d_job_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_job_info_type

    Module Subroutine list_to_type_array2d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_job_info_type_no_alloc

    Module Subroutine Wrap_blockinfo_type(for_var,py_var,ierror)

        ! Arguments
        type(blockinfo_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_nl,nd_iex

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','blockinfo_type')
        if (ierror == 0) then
            ierror = py_var%setitem('strname',for_var%strname)
            if (ierror /= 0) then
                for_var%strname = ''
                ierror = py_var%setitem('strname',for_var%strname)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('blname',for_var%blname)
            if (ierror /= 0) then
                for_var%blname = ''
                ierror = py_var%setitem('blname',for_var%blname)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('ibl',for_var%ibl)
        if (ierror == 0) ierror = ndarray_create(nd_nl,for_var%nl)
        if (ierror == 0) ierror = py_var%setitem('nl',nd_nl)
        if (ierror == 0) ierror = ndarray_create(nd_iex,for_var%iex)
        if (ierror == 0) ierror = py_var%setitem('iex',nd_iex)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_blockinfo_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_blockinfo_type

    Module Subroutine Unwrap_type_blockinfo_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(blockinfo_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_blockinfo_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'blockinfo_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_blockinfo_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','strname',py_var,for_var%strname,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','blname',py_var,for_var%blname,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','ibl',py_var,for_var%ibl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','nl',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_blockinfo_type','nl',p_int_1d,for_var%nl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','iex',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_blockinfo_type','iex',p_int_1d,for_var%iex,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_blockinfo_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_blockinfo_type

    Module Subroutine list_to_type_array1d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_blockinfo_type

    Module Subroutine list_to_type_array1d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_blockinfo_type_no_alloc

    Module Subroutine list_to_type_array2d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_blockinfo_type

    Module Subroutine list_to_type_array2d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_blockinfo_type_no_alloc

    Module Subroutine Wrap_genvec_type(for_var,py_var,ierror)

        ! Arguments
        type(genvec_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_cv
        type(list) :: li_cv
        type(ndarray) :: nd_iv,nd_rv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','genvec_type')
        if (ierror == 0) then
            ierror = py_var%setitem('str',for_var%str)
            if (ierror /= 0) then
                for_var%str = ''
                ierror = py_var%setitem('str',for_var%str)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('npar',for_var%npar)
        if (ierror == 0) ierror = ndarray_create(nd_iv,for_var%iv)
        if (ierror == 0) ierror = py_var%setitem('iv',nd_iv)
        if (ierror == 0) ierror = ndarray_create(nd_rv,for_var%rv)
        if (ierror == 0) ierror = py_var%setitem('rv',nd_rv)
        if (ierror == 0) ierror = list_create(li_cv)
        if (ierror == 0) then
            do i = 1 , size(for_var%cv)
                if (ierror == 0) then
                    ierror = li_cv%append(for_var%cv(i))
                    if (ierror /= 0) then
                        for_var%cv(i) = ''
                        ierror = li_cv%append(for_var%cv(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('cv',li_cv)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_genvec_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_genvec_type

    Module Subroutine Unwrap_type_genvec_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(genvec_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_genvec_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'genvec_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_genvec_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','str',py_var,for_var%str,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','npar',py_var,for_var%npar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','iv',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_genvec_type','iv',p_int_1d,for_var%iv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','rv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_genvec_type','rv',p_real_1d,for_var%rv,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','cv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%cv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_genvec_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_genvec_type

    Module Subroutine list_to_type_array1d_genvec_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_genvec_type

    Module Subroutine list_to_type_array1d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_genvec_type_no_alloc

    Module Subroutine list_to_type_array2d_genvec_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_genvec_type

    Module Subroutine list_to_type_array2d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_genvec_type_no_alloc

    Module Subroutine Wrap_sxtal_attributes_type(for_var,py_var,ierror)

        ! Arguments
        type(sxtal_attributes_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_dom_oper
        type(list) :: li_dom_oper
        type(ndarray) :: nd_extinct,nd_mextinct,nd_lextinct,nd_dom_mat,nd_dom_fract,nd_mdom_fract,nd_ldom_fract

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sxtal_attributes_type')
        if (ierror == 0) ierror = py_var%setitem('sxtal',for_var%sxtal)
        if (ierror == 0) ierror = py_var%setitem('ext_model',for_var%ext_model)
        if (ierror == 0) ierror = ndarray_create(nd_extinct,for_var%extinct)
        if (ierror == 0) ierror = py_var%setitem('extinct',nd_extinct)
        if (ierror == 0) ierror = ndarray_create(nd_mextinct,for_var%mextinct)
        if (ierror == 0) ierror = py_var%setitem('mextinct',nd_mextinct)
        if (ierror == 0) ierror = ndarray_create(nd_lextinct,for_var%lextinct)
        if (ierror == 0) ierror = py_var%setitem('lextinct',nd_lextinct)
        if (ierror == 0) ierror = py_var%setitem('dom_type',for_var%dom_type)
        if (ierror == 0) ierror = py_var%setitem('num_dom',for_var%num_dom)
        if (ierror == 0) ierror = list_create(li_dom_oper)
        if (ierror == 0) then
            do i = 1 , size(for_var%dom_oper)
                if (ierror == 0) then
                    ierror = li_dom_oper%append(for_var%dom_oper(i))
                    if (ierror /= 0) then
                        for_var%dom_oper(i) = ''
                        ierror = li_dom_oper%append(for_var%dom_oper(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('dom_oper',li_dom_oper)
        if (ierror == 0) ierror = ndarray_create(nd_dom_mat,for_var%dom_mat)
        if (ierror == 0) ierror = py_var%setitem('dom_mat',nd_dom_mat)
        if (ierror == 0) ierror = ndarray_create(nd_dom_fract,for_var%dom_fract)
        if (ierror == 0) ierror = py_var%setitem('dom_fract',nd_dom_fract)
        if (ierror == 0) ierror = ndarray_create(nd_mdom_fract,for_var%mdom_fract)
        if (ierror == 0) ierror = py_var%setitem('mdom_fract',nd_mdom_fract)
        if (ierror == 0) ierror = ndarray_create(nd_ldom_fract,for_var%ldom_fract)
        if (ierror == 0) ierror = py_var%setitem('ldom_fract',nd_ldom_fract)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sxtal_attributes_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sxtal_attributes_type

    Module Subroutine Unwrap_type_sxtal_attributes_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sxtal_attributes_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_sxtal_attributes_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sxtal_attributes_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sxtal_attributes_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','sxtal',py_var,for_var%sxtal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','ext_model',py_var,for_var%ext_model,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','extinct',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','extinct',p_real_1d,for_var%extinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','mextinct',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','mextinct',p_real_1d,for_var%mextinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','lextinct',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','lextinct',p_real_1d,for_var%lextinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','dom_type',py_var,for_var%dom_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','num_dom',py_var,for_var%num_dom,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','dom_oper',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%dom_oper,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','dom_mat',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','dom_mat',p_real_3d,for_var%dom_mat,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','dom_fract',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','dom_fract',p_real_1d,for_var%dom_fract,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','mdom_fract',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','mdom_fract',p_real_1d,for_var%mdom_fract,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_attributes_type','ldom_fract',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_attributes_type','ldom_fract',p_real_1d,for_var%ldom_fract,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sxtal_attributes_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sxtal_attributes_type

    Module Subroutine list_to_type_array1d_sxtal_attributes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_attributes_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_attributes_type

    Module Subroutine list_to_type_array1d_sxtal_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_attributes_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_attributes_type_no_alloc

    Module Subroutine list_to_type_array2d_sxtal_attributes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_attributes_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_attributes_type

    Module Subroutine list_to_type_array2d_sxtal_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_attributes_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_attributes_type_no_alloc

    Module Subroutine Wrap_powder_attributes_type(for_var,py_var,ierror)

        ! Arguments
        type(powder_attributes_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_axis_size,nd_axis_strain,nd_aniso_size,nd_aniso_strain,nd_maniso_size,nd_maniso_strain,nd_laniso_size,nd_laniso_strain,nd_axes_pref,nd_pref,nd_mpref,nd_lpref,nd_abs_corr,nd_mabs_corr,nd_labs_corr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','powder_attributes_type')
        if (ierror == 0) ierror = py_var%setitem('powder',for_var%powder)
        if (ierror == 0) ierror = py_var%setitem('extinct',for_var%extinct)
        if (ierror == 0) ierror = py_var%setitem('mextinct',for_var%mextinct)
        if (ierror == 0) ierror = py_var%setitem('lextinct',for_var%lextinct)
        if (ierror == 0) ierror = py_var%setitem('iso_size',for_var%iso_size)
        if (ierror == 0) ierror = py_var%setitem('gauss_iso_size_frac',for_var%gauss_iso_size_frac)
        if (ierror == 0) ierror = py_var%setitem('miso_size',for_var%miso_size)
        if (ierror == 0) ierror = py_var%setitem('mgauss_iso_size_frac',for_var%mgauss_iso_size_frac)
        if (ierror == 0) ierror = py_var%setitem('liso_size',for_var%liso_size)
        if (ierror == 0) ierror = py_var%setitem('lgauss_iso_size_frac',for_var%lgauss_iso_size_frac)
        if (ierror == 0) ierror = py_var%setitem('iso_strain',for_var%iso_strain)
        if (ierror == 0) ierror = py_var%setitem('lorentz_iso_strain_frac',for_var%lorentz_iso_strain_frac)
        if (ierror == 0) ierror = py_var%setitem('miso_strain',for_var%miso_strain)
        if (ierror == 0) ierror = py_var%setitem('mlorent_iso_strain_frac',for_var%mlorent_iso_strain_frac)
        if (ierror == 0) ierror = py_var%setitem('liso_strain',for_var%liso_strain)
        if (ierror == 0) ierror = py_var%setitem('llorent_iso_strain_frac',for_var%llorent_iso_strain_frac)
        if (ierror == 0) then
            ierror = py_var%setitem('aniso_size_model',for_var%aniso_size_model)
            if (ierror /= 0) then
                for_var%aniso_size_model = ''
                ierror = py_var%setitem('aniso_size_model',for_var%aniso_size_model)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('aniso_strain_model',for_var%aniso_strain_model)
            if (ierror /= 0) then
                for_var%aniso_strain_model = ''
                ierror = py_var%setitem('aniso_strain_model',for_var%aniso_strain_model)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('laue_class',for_var%laue_class)
            if (ierror /= 0) then
                for_var%laue_class = ''
                ierror = py_var%setitem('laue_class',for_var%laue_class)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('nani_size',for_var%nani_size)
        if (ierror == 0) ierror = py_var%setitem('nani_strain',for_var%nani_strain)
        if (ierror == 0) ierror = ndarray_create(nd_axis_size,for_var%axis_size)
        if (ierror == 0) ierror = py_var%setitem('axis_size',nd_axis_size)
        if (ierror == 0) ierror = ndarray_create(nd_axis_strain,for_var%axis_strain)
        if (ierror == 0) ierror = py_var%setitem('axis_strain',nd_axis_strain)
        if (ierror == 0) ierror = ndarray_create(nd_aniso_size,for_var%aniso_size)
        if (ierror == 0) ierror = py_var%setitem('aniso_size',nd_aniso_size)
        if (ierror == 0) ierror = ndarray_create(nd_aniso_strain,for_var%aniso_strain)
        if (ierror == 0) ierror = py_var%setitem('aniso_strain',nd_aniso_strain)
        if (ierror == 0) ierror = ndarray_create(nd_maniso_size,for_var%maniso_size)
        if (ierror == 0) ierror = py_var%setitem('maniso_size',nd_maniso_size)
        if (ierror == 0) ierror = ndarray_create(nd_maniso_strain,for_var%maniso_strain)
        if (ierror == 0) ierror = py_var%setitem('maniso_strain',nd_maniso_strain)
        if (ierror == 0) ierror = ndarray_create(nd_laniso_size,for_var%laniso_size)
        if (ierror == 0) ierror = py_var%setitem('laniso_size',nd_laniso_size)
        if (ierror == 0) ierror = ndarray_create(nd_laniso_strain,for_var%laniso_strain)
        if (ierror == 0) ierror = py_var%setitem('laniso_strain',nd_laniso_strain)
        if (ierror == 0) ierror = py_var%setitem('n_pref',for_var%n_pref)
        if (ierror == 0) ierror = ndarray_create(nd_axes_pref,for_var%axes_pref)
        if (ierror == 0) ierror = py_var%setitem('axes_pref',nd_axes_pref)
        if (ierror == 0) ierror = ndarray_create(nd_pref,for_var%pref)
        if (ierror == 0) ierror = py_var%setitem('pref',nd_pref)
        if (ierror == 0) ierror = ndarray_create(nd_mpref,for_var%mpref)
        if (ierror == 0) ierror = py_var%setitem('mpref',nd_mpref)
        if (ierror == 0) ierror = ndarray_create(nd_lpref,for_var%lpref)
        if (ierror == 0) ierror = py_var%setitem('lpref',nd_lpref)
        if (ierror == 0) ierror = ndarray_create(nd_abs_corr,for_var%abs_corr)
        if (ierror == 0) ierror = py_var%setitem('abs_corr',nd_abs_corr)
        if (ierror == 0) ierror = ndarray_create(nd_mabs_corr,for_var%mabs_corr)
        if (ierror == 0) ierror = py_var%setitem('mabs_corr',nd_mabs_corr)
        if (ierror == 0) ierror = ndarray_create(nd_labs_corr,for_var%labs_corr)
        if (ierror == 0) ierror = py_var%setitem('labs_corr',nd_labs_corr)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_powder_attributes_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_powder_attributes_type

    Module Subroutine Unwrap_type_powder_attributes_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(powder_attributes_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_powder_attributes_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'powder_attributes_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powder_attributes_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','powder',py_var,for_var%powder,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','extinct',py_var,for_var%extinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','mextinct',py_var,for_var%mextinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','lextinct',py_var,for_var%lextinct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','iso_size',py_var,for_var%iso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','gauss_iso_size_frac',py_var,for_var%gauss_iso_size_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','miso_size',py_var,for_var%miso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','mgauss_iso_size_frac',py_var,for_var%mgauss_iso_size_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','liso_size',py_var,for_var%liso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','lgauss_iso_size_frac',py_var,for_var%lgauss_iso_size_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','iso_strain',py_var,for_var%iso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','lorentz_iso_strain_frac',py_var,for_var%lorentz_iso_strain_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','miso_strain',py_var,for_var%miso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','mlorent_iso_strain_frac',py_var,for_var%mlorent_iso_strain_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','liso_strain',py_var,for_var%liso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','llorent_iso_strain_frac',py_var,for_var%llorent_iso_strain_frac,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','aniso_size_model',py_var,for_var%aniso_size_model,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','aniso_strain_model',py_var,for_var%aniso_strain_model,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','laue_class',py_var,for_var%laue_class,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','nani_size',py_var,for_var%nani_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','nani_strain',py_var,for_var%nani_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','axis_size',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','axis_size',p_real_1d,for_var%axis_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','axis_strain',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','axis_strain',p_real_1d,for_var%axis_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','aniso_size',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','aniso_size',p_real_1d,for_var%aniso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','aniso_strain',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','aniso_strain',p_real_1d,for_var%aniso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','maniso_size',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','maniso_size',p_real_1d,for_var%maniso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','maniso_strain',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','maniso_strain',p_real_1d,for_var%maniso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','laniso_size',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','laniso_size',p_int_1d,for_var%laniso_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','laniso_strain',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','laniso_strain',p_int_1d,for_var%laniso_strain,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','n_pref',py_var,for_var%n_pref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','axes_pref',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','axes_pref',p_real_2d,for_var%axes_pref,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','pref',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','pref',p_real_2d,for_var%pref,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','mpref',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','mpref',p_real_2d,for_var%mpref,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','lpref',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','lpref',p_int_2d,for_var%lpref,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','abs_corr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','abs_corr',p_real_1d,for_var%abs_corr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','mabs_corr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','mabs_corr',p_real_1d,for_var%mabs_corr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_attributes_type','labs_corr',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_attributes_type','labs_corr',p_int_1d,for_var%labs_corr,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powder_attributes_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_powder_attributes_type

    Module Subroutine list_to_type_array1d_powder_attributes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_attributes_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_powder_attributes_type

    Module Subroutine list_to_type_array1d_powder_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_attributes_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_powder_attributes_type_no_alloc

    Module Subroutine list_to_type_array2d_powder_attributes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_attributes_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_powder_attributes_type

    Module Subroutine list_to_type_array2d_powder_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_attributes_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_powder_attributes_type_no_alloc

    Module Subroutine Wrap_phase_type(for_var,py_var,ierror)

        ! Arguments
        type(phase_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_cell,di_atm_list,di_spg,di_kvec,di_mol,di_pat_mode,di_pat_sample,di_pow,di_sxt
        type(list) :: li_mol,li_pat_mode,li_pat_sample,li_pow,li_sxt
        type(ndarray) :: nd_patterns,nd_scale_factors,nd_mscale_factors,nd_lscale_factors

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','phase_type')
        if (ierror == 0) ierror = py_var%setitem('iph',for_var%iph)
        if (ierror == 0) ierror = py_var%setitem('mag',for_var%mag)
        if (ierror == 0) ierror = py_var%setitem('mag_only',for_var%mag_only)
        if (ierror == 0) then
            ierror = py_var%setitem('name',for_var%name)
            if (ierror /= 0) then
                for_var%name = ''
                ierror = py_var%setitem('name',for_var%name)
            end if
        end if
        if (ierror == 0) ierror = dict_create(di_cell)
        if (ierror == 0) call wrap_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) ierror = dict_create(di_atm_list)
        if (ierror == 0) call wrap_type(for_var%atm_list,di_atm_list,ierror)
        if (ierror == 0) ierror = py_var%setitem('atm_list',di_atm_list)
        if (ierror == 0) ierror = dict_create(di_spg)
        if (ierror == 0) call wrap_type(for_var%spg,di_spg,ierror)
        if (ierror == 0) ierror = py_var%setitem('spg',di_spg)
        if (ierror == 0) ierror = dict_create(di_kvec)
        if (ierror == 0) call wrap_type(for_var%kvec,di_kvec,ierror)
        if (ierror == 0) ierror = py_var%setitem('kvec',di_kvec)
        if (ierror == 0) ierror = py_var%setitem('occ_fact',for_var%occ_fact)
        if (ierror == 0) ierror = py_var%setitem('n_species',for_var%n_species)
        if (ierror == 0) ierror = py_var%setitem('nmol',for_var%nmol)
        if (allocated(for_var%mol)) then
            if (ierror == 0) ierror = list_create(li_mol)
            if (ierror == 0) then
                do i = 1 , size(for_var%mol)
                    ierror = dict_create(di_mol)
                    if (ierror == 0) call wrap_type(for_var%mol(i),di_mol,ierror)
                    if (ierror == 0) ierror = li_mol%append(di_mol)
                    if (ierror == 0) call di_mol%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('mol',li_mol)
        end if
        if (ierror == 0) ierror = py_var%setitem('ncontr',for_var%ncontr)
        if (allocated(for_var%patterns)) then
            if (ierror == 0) ierror = ndarray_create(nd_patterns,for_var%patterns)
            if (ierror == 0) ierror = py_var%setitem('patterns',nd_patterns)
        end if
        if (allocated(for_var%pat_mode)) then
            if (ierror == 0) ierror = list_create(li_pat_mode)
            if (ierror == 0) then
                do i = 1 , size(for_var%pat_mode)
                    if (ierror == 0) then
                        ierror = li_pat_mode%append(for_var%pat_mode(i))
                        if (ierror /= 0) then
                            for_var%pat_mode(i) = ''
                            ierror = li_pat_mode%append(for_var%pat_mode(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('pat_mode',li_pat_mode)
        end if
        if (allocated(for_var%pat_sample)) then
            if (ierror == 0) ierror = list_create(li_pat_sample)
            if (ierror == 0) then
                do i = 1 , size(for_var%pat_sample)
                    if (ierror == 0) then
                        ierror = li_pat_sample%append(for_var%pat_sample(i))
                        if (ierror /= 0) then
                            for_var%pat_sample(i) = ''
                            ierror = li_pat_sample%append(for_var%pat_sample(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('pat_sample',li_pat_sample)
        end if
        if (allocated(for_var%scale_factors)) then
            if (ierror == 0) ierror = ndarray_create(nd_scale_factors,for_var%scale_factors)
            if (ierror == 0) ierror = py_var%setitem('scale_factors',nd_scale_factors)
        end if
        if (allocated(for_var%mscale_factors)) then
            if (ierror == 0) ierror = ndarray_create(nd_mscale_factors,for_var%mscale_factors)
            if (ierror == 0) ierror = py_var%setitem('mscale_factors',nd_mscale_factors)
        end if
        if (allocated(for_var%lscale_factors)) then
            if (ierror == 0) ierror = ndarray_create(nd_lscale_factors,for_var%lscale_factors)
            if (ierror == 0) ierror = py_var%setitem('lscale_factors',nd_lscale_factors)
        end if
        if (ierror == 0) ierror = py_var%setitem('n_pow',for_var%n_pow)
        if (ierror == 0) ierror = py_var%setitem('n_sxt',for_var%n_sxt)
        if (allocated(for_var%pow)) then
            if (ierror == 0) ierror = list_create(li_pow)
            if (ierror == 0) then
                do i = 1 , size(for_var%pow)
                    ierror = dict_create(di_pow)
                    if (ierror == 0) call wrap_type(for_var%pow(i),di_pow,ierror)
                    if (ierror == 0) ierror = li_pow%append(di_pow)
                    if (ierror == 0) call di_pow%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('pow',li_pow)
        end if
        if (allocated(for_var%sxt)) then
            if (ierror == 0) ierror = list_create(li_sxt)
            if (ierror == 0) then
                do i = 1 , size(for_var%sxt)
                    ierror = dict_create(di_sxt)
                    if (ierror == 0) call wrap_type(for_var%sxt(i),di_sxt,ierror)
                    if (ierror == 0) ierror = li_sxt%append(di_sxt)
                    if (ierror == 0) call di_sxt%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('sxt',li_sxt)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_phase_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_phase_type

    Module Subroutine Unwrap_type_phase_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(phase_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list
        type(dict) :: dict_cell,dict_atm_list,dict_spg,dict_kvec

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_phase_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'phase_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_phase_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','iph',py_var,for_var%iph,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','mag',py_var,for_var%mag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','mag_only',py_var,for_var%mag_only,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_phase_type','name',py_var,for_var%name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','cell',py_var,dict_cell,ierror)
        if (ierror == 0) call unwrap_class_cell_g_type(dict_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','atm_list',py_var,dict_atm_list,ierror)
        if (ierror == 0) call unwrap_type(dict_atm_list,for_var%atm_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','spg',py_var,dict_spg,ierror)
        if (ierror == 0) call unwrap_class_spg_type(dict_spg,for_var%spg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','kvec',py_var,dict_kvec,ierror)
        if (ierror == 0) call unwrap_type(dict_kvec,for_var%kvec,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','occ_fact',py_var,for_var%occ_fact,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','n_species',py_var,for_var%n_species,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','nmol',py_var,for_var%nmol,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','mol',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_phase_type','mol',my_list,for_var%mol,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','ncontr',py_var,for_var%ncontr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','patterns',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_phase_type','patterns',p_int_1d,for_var%patterns,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','pat_mode',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%pat_mode,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','pat_sample',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%pat_sample,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','scale_factors',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_phase_type','scale_factors',p_real_1d,for_var%scale_factors,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','mscale_factors',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_phase_type','mscale_factors',p_real_1d,for_var%mscale_factors,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','lscale_factors',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_phase_type','lscale_factors',p_int_1d,for_var%lscale_factors,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','n_pow',py_var,for_var%n_pow,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','n_sxt',py_var,for_var%n_sxt,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','pow',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_phase_type','pow',my_list,for_var%pow,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_phase_type','sxt',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_phase_type','sxt',my_list,for_var%sxt,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_phase_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_phase_type

    Module Subroutine list_to_type_array1d_phase_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(phase_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_phase_type

    Module Subroutine list_to_type_array1d_phase_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(phase_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_phase_type_no_alloc

    Module Subroutine list_to_type_array2d_phase_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(phase_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_phase_type

    Module Subroutine list_to_type_array2d_phase_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(phase_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_phase_type_no_alloc

end submodule