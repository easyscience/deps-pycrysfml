submodule (CFML_Wraps) Wraps_kvec_Symmetry

    implicit none
    contains

    Module Subroutine Wrap_sym_oper_type(for_var,py_var,ierror)

        ! Arguments
        type(sym_oper_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rot,nd_tr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sym_oper_type')
        if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        if (ierror == 0) ierror = ndarray_create(nd_tr,for_var%tr)
        if (ierror == 0) ierror = py_var%setitem('tr',nd_tr)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sym_oper_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sym_oper_type

    Module Subroutine Unwrap_type_sym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sym_oper_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_sym_oper_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sym_oper_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sym_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sym_oper_type','rot',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sym_oper_type','rot',p_int_2d,for_var%rot,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sym_oper_type','tr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sym_oper_type','tr',p_real_1d,for_var%tr,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sym_oper_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sym_oper_type

    Module Subroutine list_to_type_array1d_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sym_oper_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sym_oper_type

    Module Subroutine list_to_type_array1d_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sym_oper_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sym_oper_type_no_alloc

    Module Subroutine list_to_type_array2d_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sym_oper_type

    Module Subroutine list_to_type_array2d_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sym_oper_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sym_oper_type_no_alloc

    Module Subroutine Wrap_msym_oper_type(for_var,py_var,ierror)

        ! Arguments
        type(msym_oper_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rot

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','msym_oper_type')
        if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        if (ierror == 0) ierror = py_var%setitem('phas',for_var%phas)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_msym_oper_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_msym_oper_type

    Module Subroutine Unwrap_type_msym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(msym_oper_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_msym_oper_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'msym_oper_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_msym_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_msym_oper_type','rot',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_msym_oper_type','rot',p_int_2d,for_var%rot,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_msym_oper_type','phas',py_var,for_var%phas,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_msym_oper_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_msym_oper_type

    Module Subroutine list_to_type_array1d_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(msym_oper_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_msym_oper_type

    Module Subroutine list_to_type_array1d_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(msym_oper_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_msym_oper_type_no_alloc

    Module Subroutine list_to_type_array2d_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(msym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_msym_oper_type

    Module Subroutine list_to_type_array2d_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(msym_oper_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_msym_oper_type_no_alloc

    Module Subroutine Wrap_magnetic_domain_type(for_var,py_var,ierror)

        ! Arguments
        type(magnetic_domain_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i,j
        type(dict) :: di_lab
        type(list) :: li_lab,li_lab_2
        type(ndarray) :: nd_dmat,nd_dt,nd_pop,nd_lpop,nd_mpop,nd_pop_std

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','magnetic_domain_type')
        if (ierror == 0) ierror = py_var%setitem('nd',for_var%nd)
        if (ierror == 0) ierror = py_var%setitem('chir',for_var%chir)
        if (ierror == 0) ierror = py_var%setitem('trans',for_var%trans)
        if (ierror == 0) ierror = py_var%setitem('twin',for_var%twin)
        if (ierror == 0) ierror = ndarray_create(nd_dmat,for_var%dmat)
        if (ierror == 0) ierror = py_var%setitem('dmat',nd_dmat)
        if (ierror == 0) ierror = ndarray_create(nd_dt,for_var%dt)
        if (ierror == 0) ierror = py_var%setitem('dt',nd_dt)
        if (ierror == 0) ierror = ndarray_create(nd_pop,for_var%pop)
        if (ierror == 0) ierror = py_var%setitem('pop',nd_pop)
        if (ierror == 0) ierror = ndarray_create(nd_lpop,for_var%lpop)
        if (ierror == 0) ierror = py_var%setitem('lpop',nd_lpop)
        if (ierror == 0) ierror = ndarray_create(nd_mpop,for_var%mpop)
        if (ierror == 0) ierror = py_var%setitem('mpop',nd_mpop)
        if (ierror == 0) ierror = ndarray_create(nd_pop_std,for_var%pop_std)
        if (ierror == 0) ierror = py_var%setitem('pop_std',nd_pop_std)
        if (ierror == 0) ierror = list_create(li_lab)
        if (ierror == 0) then
            do i = 1 , size(for_var%lab,1)
                if (ierror == 0) ierror = list_create(li_lab_2)
                do j = 1 , size(for_var%lab,2)
                if (ierror == 0) then
                    ierror = li_lab_2%append(for_var%lab(i,j))
                    if (ierror /= 0) then
                        for_var%lab(i,j) = ''
                        ierror = li_lab_2%append(for_var%lab(i,j))
                    end if
                end if
                end do
                if (ierror == 0) ierror = li_lab%append(li_lab_2)
                call li_lab_2%destroy
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_magnetic_domain_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_magnetic_domain_type

    Module Subroutine Unwrap_type_magnetic_domain_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magnetic_domain_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magnetic_domain_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'magnetic_domain_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magnetic_domain_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','nd',py_var,for_var%nd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','chir',py_var,for_var%chir,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','trans',py_var,for_var%trans,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','twin',py_var,for_var%twin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','dmat',py_var,p_int_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','dmat',p_int_3d,for_var%dmat,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','dt',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','dt',p_real_2d,for_var%dt,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','pop',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','pop',p_real_2d,for_var%pop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','lpop',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','lpop',p_int_2d,for_var%lpop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','mpop',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','mpop',p_real_2d,for_var%mpop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','pop_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','pop_std',p_real_2d,for_var%pop_std,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','lab',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%lab,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_magnetic_domain_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_magnetic_domain_type

    Module Subroutine list_to_type_array1d_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magnetic_domain_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_magnetic_domain_type

    Module Subroutine list_to_type_array1d_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magnetic_domain_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_magnetic_domain_type_no_alloc

    Module Subroutine list_to_type_array2d_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magnetic_domain_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_magnetic_domain_type

    Module Subroutine list_to_type_array2d_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magnetic_domain_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_magnetic_domain_type_no_alloc

    Module Subroutine Wrap_magsymm_k_type(for_var,py_var,ierror)

        ! Arguments
        type(magsymm_k_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i,j
        type(dict) :: di_irrep_id,di_irrep_direction,di_irrep_action,di_kv_label,di_k_eqv_minusk,di_symopsymb,di_symop,di_msymopsymb,di_msymop
        type(list) :: li_irrep_id,li_irrep_direction,li_irrep_action,li_kv_label,li_k_eqv_minusk,li_symopsymb,li_symop,li_msymopsymb,li_msymopsymb_2,li_msymop,li_msymop_2
        type(ndarray) :: nd_irrep_dim,nd_small_irrep_dim,nd_irrep_modes_number,nd_kvec,nd_ltr,nd_nbas,nd_icomp,nd_basf

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','magsymm_k_type')
        if (ierror == 0) then
            ierror = py_var%setitem('magmodel',for_var%magmodel)
            if (ierror /= 0) then
                for_var%magmodel = ''
                ierror = py_var%setitem('magmodel',for_var%magmodel)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('sk_type',for_var%sk_type)
            if (ierror /= 0) then
                for_var%sk_type = ''
                ierror = py_var%setitem('sk_type',for_var%sk_type)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('fcoef_type',for_var%fcoef_type)
            if (ierror /= 0) then
                for_var%fcoef_type = ''
                ierror = py_var%setitem('fcoef_type',for_var%fcoef_type)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('bns_number',for_var%bns_number)
            if (ierror /= 0) then
                for_var%bns_number = ''
                ierror = py_var%setitem('bns_number',for_var%bns_number)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('og_number',for_var%og_number)
            if (ierror /= 0) then
                for_var%og_number = ''
                ierror = py_var%setitem('og_number',for_var%og_number)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('bns_symbol',for_var%bns_symbol)
            if (ierror /= 0) then
                for_var%bns_symbol = ''
                ierror = py_var%setitem('bns_symbol',for_var%bns_symbol)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('og_symbol',for_var%og_symbol)
            if (ierror /= 0) then
                for_var%og_symbol = ''
                ierror = py_var%setitem('og_symbol',for_var%og_symbol)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('magtype',for_var%magtype)
        if (ierror == 0) ierror = py_var%setitem('parent_num',for_var%parent_num)
        if (ierror == 0) then
            ierror = py_var%setitem('parent_spg',for_var%parent_spg)
            if (ierror /= 0) then
                for_var%parent_spg = ''
                ierror = py_var%setitem('parent_spg',for_var%parent_spg)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('latt',for_var%latt)
            if (ierror /= 0) then
                for_var%latt = ''
                ierror = py_var%setitem('latt',for_var%latt)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('nirreps',for_var%nirreps)
        if (ierror == 0) ierror = ndarray_create(nd_irrep_dim,for_var%irrep_dim)
        if (ierror == 0) ierror = py_var%setitem('irrep_dim',nd_irrep_dim)
        if (ierror == 0) ierror = ndarray_create(nd_small_irrep_dim,for_var%small_irrep_dim)
        if (ierror == 0) ierror = py_var%setitem('small_irrep_dim',nd_small_irrep_dim)
        if (ierror == 0) ierror = ndarray_create(nd_irrep_modes_number,for_var%irrep_modes_number)
        if (ierror == 0) ierror = py_var%setitem('irrep_modes_number',nd_irrep_modes_number)
        if (ierror == 0) ierror = list_create(li_irrep_id)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_id)
                if (ierror == 0) then
                    ierror = li_irrep_id%append(for_var%irrep_id(i))
                    if (ierror /= 0) then
                        for_var%irrep_id(i) = ''
                        ierror = li_irrep_id%append(for_var%irrep_id(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_id',li_irrep_id)
        if (ierror == 0) ierror = list_create(li_irrep_direction)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_direction)
                if (ierror == 0) then
                    ierror = li_irrep_direction%append(for_var%irrep_direction(i))
                    if (ierror /= 0) then
                        for_var%irrep_direction(i) = ''
                        ierror = li_irrep_direction%append(for_var%irrep_direction(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_direction',li_irrep_direction)
        if (ierror == 0) ierror = list_create(li_irrep_action)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_action)
                if (ierror == 0) then
                    ierror = li_irrep_action%append(for_var%irrep_action(i))
                    if (ierror /= 0) then
                        for_var%irrep_action(i) = ''
                        ierror = li_irrep_action%append(for_var%irrep_action(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_action',li_irrep_action)
        if (ierror == 0) ierror = py_var%setitem('nmsym',for_var%nmsym)
        if (ierror == 0) ierror = py_var%setitem('centred',for_var%centred)
        if (ierror == 0) ierror = py_var%setitem('mcentred',for_var%mcentred)
        if (ierror == 0) ierror = py_var%setitem('nkv',for_var%nkv)
        if (ierror == 0) ierror = ndarray_create(nd_kvec,for_var%kvec)
        if (ierror == 0) ierror = py_var%setitem('kvec',nd_kvec)
        if (ierror == 0) ierror = list_create(li_kv_label)
        if (ierror == 0) then
            do i = 1 , size(for_var%kv_label)
                if (ierror == 0) then
                    ierror = li_kv_label%append(for_var%kv_label(i))
                    if (ierror /= 0) then
                        for_var%kv_label(i) = ''
                        ierror = li_kv_label%append(for_var%kv_label(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('kv_label',li_kv_label)
        if (ierror == 0) ierror = list_create(li_k_eqv_minusk)
        if (ierror == 0) then
            do i = 1 , size(for_var%k_eqv_minusk)
                if (ierror == 0) ierror = li_k_eqv_minusk%append(for_var%k_eqv_minusk(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('k_eqv_minusk',li_k_eqv_minusk)
        if (ierror == 0) ierror = py_var%setitem('num_lat',for_var%num_lat)
        if (ierror == 0) ierror = ndarray_create(nd_ltr,for_var%ltr)
        if (ierror == 0) ierror = py_var%setitem('ltr',nd_ltr)
        if (ierror == 0) ierror = py_var%setitem('numops',for_var%numops)
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (ierror == 0) ierror = ndarray_create(nd_nbas,for_var%nbas)
        if (ierror == 0) ierror = py_var%setitem('nbas',nd_nbas)
        if (ierror == 0) ierror = ndarray_create(nd_icomp,for_var%icomp)
        if (ierror == 0) ierror = py_var%setitem('icomp',nd_icomp)
        if (ierror == 0) ierror = ndarray_create(nd_basf,for_var%basf)
        if (ierror == 0) ierror = py_var%setitem('basf',nd_basf)
        if (allocated(for_var%symopsymb)) then
            if (ierror == 0) ierror = list_create(li_symopsymb)
            if (ierror == 0) then
                do i = 1 , size(for_var%symopsymb)
                    if (ierror == 0) then
                        ierror = li_symopsymb%append(for_var%symopsymb(i))
                        if (ierror /= 0) then
                            for_var%symopsymb(i) = ''
                            ierror = li_symopsymb%append(for_var%symopsymb(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symopsymb',li_symopsymb)
        end if
        if (allocated(for_var%symop)) then
            if (ierror == 0) ierror = list_create(li_symop)
            if (ierror == 0) then
                do i = 1 , size(for_var%symop)
                    ierror = dict_create(di_symop)
                    if (ierror == 0) call wrap_type(for_var%symop(i),di_symop,ierror)
                    if (ierror == 0) ierror = li_symop%append(di_symop)
                    if (ierror == 0) call di_symop%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symop',li_symop)
        end if
        if (allocated(for_var%msymopsymb)) then
            if (ierror == 0) ierror = list_create(li_msymopsymb)
            if (ierror == 0) then
                do i = 1 , size(for_var%msymopsymb,1)
                    if (ierror == 0) ierror = list_create(li_msymopsymb_2)
                    do j = 1 , size(for_var%msymopsymb,2)
                    if (ierror == 0) then
                        ierror = li_msymopsymb_2%append(for_var%msymopsymb(i,j))
                        if (ierror /= 0) then
                            for_var%msymopsymb(i,j) = ''
                            ierror = li_msymopsymb_2%append(for_var%msymopsymb(i,j))
                        end if
                    end if
                    end do
                    if (ierror == 0) ierror = li_msymopsymb%append(li_msymopsymb_2)
                    call li_msymopsymb_2%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('msymopsymb',li_msymopsymb)
        end if
        if (allocated(for_var%msymop)) then
            if (ierror == 0) ierror = list_create(li_msymop)
            if (ierror == 0) then
                do i = 1 , size(for_var%msymop,1)
                    if (ierror == 0) ierror = list_create(li_msymop_2)
                    do j = 1 , size(for_var%msymop,2)
                        ierror = dict_create(di_msymop)
                        if (ierror == 0) call wrap_type(for_var%msymop(i,j),di_msymop,ierror)
                        if (ierror == 0) ierror = li_msymop_2%append(di_msymop)
                        if (ierror == 0) call di_msymop%destroy
                    end do
                    if (ierror == 0) ierror = li_msymop%append(li_msymop_2)
                    call li_msymop_2%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('msymop',li_msymop)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_magsymm_k_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_magsymm_k_type

    Module Subroutine Unwrap_type_magsymm_k_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magsymm_k_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        complex, dimension(:,:,:,:), pointer :: p_complex_4d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magsymm_k_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'magsymm_k_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magsymm_k_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','magmodel',py_var,for_var%magmodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','sk_type',py_var,for_var%sk_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','fcoef_type',py_var,for_var%fcoef_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','bns_number',py_var,for_var%bns_number,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','og_number',py_var,for_var%og_number,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','bns_symbol',py_var,for_var%bns_symbol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','og_symbol',py_var,for_var%og_symbol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','magtype',py_var,for_var%magtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','latt',py_var,for_var%latt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nirreps',py_var,for_var%nirreps,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_dim',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','irrep_dim',p_int_1d,for_var%irrep_dim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','small_irrep_dim',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','small_irrep_dim',p_int_1d,for_var%small_irrep_dim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_modes_number',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','irrep_modes_number',p_int_1d,for_var%irrep_modes_number,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_id',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%irrep_id,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_direction',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%irrep_direction,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_action',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%irrep_action,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nmsym',py_var,for_var%nmsym,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','mcentred',py_var,for_var%mcentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nkv',py_var,for_var%nkv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','kvec',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','kvec',p_real_2d,for_var%kvec,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','kv_label',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%kv_label,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','k_eqv_minusk',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%k_eqv_minusk,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','ltr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','ltr',p_real_2d,for_var%ltr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nbas',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','nbas',p_int_1d,for_var%nbas,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','icomp',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','icomp',p_int_2d,for_var%icomp,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','basf',py_var,p_complex_4d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','basf',p_complex_4d,for_var%basf,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','symopsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symopsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','symop',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_magsymm_k_type','symop',my_list,for_var%symop,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','msymopsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%msymopsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','msymop',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_magsymm_k_type','msymop',my_list,for_var%msymop,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_magsymm_k_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_magsymm_k_type

    Module Subroutine list_to_type_array1d_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magsymm_k_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_magsymm_k_type

    Module Subroutine list_to_type_array1d_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magsymm_k_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_magsymm_k_type_no_alloc

    Module Subroutine list_to_type_array2d_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magsymm_k_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_magsymm_k_type

    Module Subroutine list_to_type_array2d_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(magsymm_k_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_magsymm_k_type_no_alloc

    Module Subroutine Wrap_point_orbit_kv(for_var,py_var,ierror)

        ! Arguments
        class(point_orbit_kv), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_labs,di_chemsymb
        type(list) :: li_labs,li_chemsymb
        type(ndarray) :: nd_pos,nd_mom,nd_latt,nd_pts,nd_latr,nd_tfourier

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','point_orbit_kv')
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (allocated(for_var%labs)) then
            if (ierror == 0) ierror = list_create(li_labs)
            if (ierror == 0) then
                do i = 1 , size(for_var%labs)
                    if (ierror == 0) then
                        ierror = li_labs%append(for_var%labs(i))
                        if (ierror /= 0) then
                            for_var%labs(i) = ''
                            ierror = li_labs%append(for_var%labs(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('labs',li_labs)
        end if
        if (allocated(for_var%chemsymb)) then
            if (ierror == 0) ierror = list_create(li_chemsymb)
            if (ierror == 0) then
                do i = 1 , size(for_var%chemsymb)
                    if (ierror == 0) then
                        ierror = li_chemsymb%append(for_var%chemsymb(i))
                        if (ierror /= 0) then
                            for_var%chemsymb(i) = ''
                            ierror = li_chemsymb%append(for_var%chemsymb(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('chemsymb',li_chemsymb)
        end if
        if (allocated(for_var%pos)) then
            if (ierror == 0) ierror = ndarray_create(nd_pos,for_var%pos)
            if (ierror == 0) ierror = py_var%setitem('pos',nd_pos)
        end if
        if (allocated(for_var%mom)) then
            if (ierror == 0) ierror = ndarray_create(nd_mom,for_var%mom)
            if (ierror == 0) ierror = py_var%setitem('mom',nd_mom)
        end if
        if (allocated(for_var%latt)) then
            if (ierror == 0) ierror = ndarray_create(nd_latt,for_var%latt)
            if (ierror == 0) ierror = py_var%setitem('latt',nd_latt)
        end if
        if (allocated(for_var%pts)) then
            if (ierror == 0) ierror = ndarray_create(nd_pts,for_var%pts)
            if (ierror == 0) ierror = py_var%setitem('pts',nd_pts)
        end if
        if (ierror == 0) ierror = ndarray_create(nd_latr,for_var%latr)
        if (ierror == 0) ierror = py_var%setitem('latr',nd_latr)
        if (ierror == 0) then
            select type (A => for_var)
                class is (mod_orbit)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','mod_orbit')
                    if (ierror == 0) ierror = py_var%setitem('nd',A%nd)
                    if (allocated(A%tfourier)) then
                        if (ierror == 0) ierror = ndarray_create(nd_tfourier,A%tfourier)
                        if (ierror == 0) ierror = py_var%setitem('tfourier',nd_tfourier)
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_point_orbit_kv: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_point_orbit_kv

    Module Subroutine Unwrap_class_point_orbit_kv_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(point_orbit_kv), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        complex, dimension(:,:,:), pointer :: p_complex_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_orbit_kv_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'point_orbit_kv' &
                .and. fortran_type /= 'mod_orbit' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_orbit_kv_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','labs',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%labs,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','chemsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%chemsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_point_orbit_kv','latr',p_real_1d,for_var%latr,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (mod_orbit)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','nd',py_var,A%nd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','tfourier',py_var,p_complex_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_mod_orbit','tfourier',p_complex_3d,A%tfourier,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_point_orbit_kv_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_point_orbit_kv_no_alloc

    Module Subroutine List_to_class_array1d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(point_orbit_kv), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: fortran_type
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ! Get the fortran type from the first element of the list
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(my_dict,item)
            if (ierror == 0) ierror = my_dict%getitem(fortran_type,'fortran_type')
            if (ierror /= 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_point_orbit_kv: Cannot determine fortran type'
            else if (fortran_type == 'point_orbit_kv') then
                allocate(point_orbit_kv :: arr(n))
            else if (fortran_type == 'mod_orbit') then
                allocate(mod_orbit :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_point_orbit_kv: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_point_orbit_kv

    Module Subroutine List_to_class_array2d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(point_orbit_kv), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        character(len=:), allocatable :: fortran_type
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                ierror = li%getitem(item,0)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) then
                    ierror = my_dict%getitem(fortran_type,'fortran_type')
                    if (ierror /= 0) then
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_point_orbit_kv: Cannot determine fortran type'
                    else if (fortran_type == 'point_orbit_kv') then
                        allocate(point_orbit_kv :: arr(n,m))
                    else if (fortran_type == 'mod_orbit') then
                        allocate(mod_orbit :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_point_orbit_kv: Wrong fortran type'
                        return
                    end if
                end if
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_point_orbit_kv

    Module Subroutine Unwrap_class_point_orbit_kv(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(point_orbit_kv), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        complex, dimension(:,:,:), pointer :: p_complex_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_orbit_kv: Cannot determine fortran type'
        else
            if (fortran_type == 'point_orbit_kv') then
                allocate(point_orbit_kv :: for_var)
            else if (fortran_type == 'mod_orbit') then
                allocate(mod_orbit :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_orbit_kv: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','labs',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%labs,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','chemsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%chemsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_point_orbit_kv','latr',p_real_1d,for_var%latr,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (mod_orbit)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','nd',py_var,A%nd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','tfourier',py_var,p_complex_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_mod_orbit','tfourier',p_complex_3d,A%tfourier,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_point_orbit_kv: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_point_orbit_kv

    Module Subroutine list_to_type_array1d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit_kv), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_point_orbit_kv

    Module Subroutine list_to_type_array1d_point_orbit_kv_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit_kv), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_point_orbit_kv_no_alloc

    Module Subroutine list_to_type_array2d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit_kv), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_point_orbit_kv

    Module Subroutine list_to_type_array2d_point_orbit_kv_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit_kv), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_point_orbit_kv_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_point_orbit_kv_no_alloc

    Module Subroutine Unwrap_class_mod_orbit_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(mod_orbit), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        complex, dimension(:,:,:), pointer :: p_complex_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_mod_orbit_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'mod_orbit' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_mod_orbit_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','labs',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%labs,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','chemsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%chemsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_point_orbit_kv','latr',p_real_1d,for_var%latr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','nd',py_var,for_var%nd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','tfourier',py_var,p_complex_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_mod_orbit','tfourier',p_complex_3d,for_var%tfourier,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_mod_orbit_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_mod_orbit_no_alloc

    Module Subroutine List_to_class_array1d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(mod_orbit), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: fortran_type
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ! Get the fortran type from the first element of the list
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(my_dict,item)
            if (ierror == 0) ierror = my_dict%getitem(fortran_type,'fortran_type')
            if (ierror /= 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_mod_orbit: Cannot determine fortran type'
            else if (fortran_type == 'mod_orbit') then
                allocate(mod_orbit :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_mod_orbit: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_mod_orbit

    Module Subroutine List_to_class_array2d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(mod_orbit), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,n,m
        character(len=:), allocatable :: fortran_type
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(m)
            if (ierror == 0 .and. m > 0) then
                ierror = li%getitem(item,0)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) then
                    ierror = my_dict%getitem(fortran_type,'fortran_type')
                    if (ierror /= 0) then
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_mod_orbit: Cannot determine fortran type'
                    else if (fortran_type == 'mod_orbit') then
                        allocate(mod_orbit :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_mod_orbit: Wrong fortran type'
                        return
                    end if
                end if
                if (ierror == 0) then
                    do i = 0 , n-1
                        if (ierror == 0) ierror = my_list%getitem(item,i)
                        if (ierror == 0) ierror = cast(li,item)
                        do j = 0 , m-1
                            if (ierror == 0) ierror = li%getitem(item,j)
                            if (ierror == 0) ierror = cast(my_dict,item)
                            if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_mod_orbit

    Module Subroutine Unwrap_class_mod_orbit(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(mod_orbit), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        complex, dimension(:,:,:), pointer :: p_complex_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_mod_orbit: Cannot determine fortran type'
        else
            if (fortran_type /= 'mod_orbit') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_mod_orbit: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','labs',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%labs,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','chemsymb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%chemsymb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit_kv','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit_kv','latr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_point_orbit_kv','latr',p_real_1d,for_var%latr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','nd',py_var,for_var%nd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mod_orbit','tfourier',py_var,p_complex_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_mod_orbit','tfourier',p_complex_3d,for_var%tfourier,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_mod_orbit: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_mod_orbit

    Module Subroutine list_to_type_array1d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mod_orbit), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_mod_orbit

    Module Subroutine list_to_type_array1d_mod_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mod_orbit), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_mod_orbit_no_alloc

    Module Subroutine list_to_type_array2d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mod_orbit), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_mod_orbit

    Module Subroutine list_to_type_array2d_mod_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mod_orbit), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_mod_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_mod_orbit_no_alloc

end submodule