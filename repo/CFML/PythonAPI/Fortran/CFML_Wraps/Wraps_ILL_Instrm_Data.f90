submodule (CFML_Wraps) Wraps_ILL_Instrm_Data

    implicit none
    contains

    Module Subroutine Wrap_basic_numc_type(for_var,py_var,ierror)

        ! Arguments
        type(basic_numc_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_namevar,di_cvalues
        type(list) :: li_namevar,li_cvalues

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','basic_numc_type')
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (allocated(for_var%namevar)) then
            if (ierror == 0) ierror = list_create(li_namevar)
            if (ierror == 0) then
                do i = 1 , size(for_var%namevar)
                    if (ierror == 0) then
                        ierror = li_namevar%append(for_var%namevar(i))
                        if (ierror /= 0) then
                            for_var%namevar(i) = ''
                            ierror = li_namevar%append(for_var%namevar(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('namevar',li_namevar)
        end if
        if (allocated(for_var%cvalues)) then
            if (ierror == 0) ierror = list_create(li_cvalues)
            if (ierror == 0) then
                do i = 1 , size(for_var%cvalues)
                    if (ierror == 0) then
                        ierror = li_cvalues%append(for_var%cvalues(i))
                        if (ierror /= 0) then
                            for_var%cvalues(i) = ''
                            ierror = li_cvalues%append(for_var%cvalues(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('cvalues',li_cvalues)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_basic_numc_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_basic_numc_type

    Module Subroutine Unwrap_type_basic_numc_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(basic_numc_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_basic_numc_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'basic_numc_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_basic_numc_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numc_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numc_type','namevar',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%namevar,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numc_type','cvalues',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%cvalues,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_basic_numc_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_basic_numc_type

    Module Subroutine list_to_type_array1d_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numc_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_basic_numc_type

    Module Subroutine list_to_type_array1d_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numc_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_basic_numc_type_no_alloc

    Module Subroutine list_to_type_array2d_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numc_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_basic_numc_type

    Module Subroutine list_to_type_array2d_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numc_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_basic_numc_type_no_alloc

    Module Subroutine Wrap_basic_numi_type(for_var,py_var,ierror)

        ! Arguments
        type(basic_numi_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_namevar
        type(list) :: li_namevar
        type(ndarray) :: nd_ivalues

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','basic_numi_type')
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (allocated(for_var%namevar)) then
            if (ierror == 0) ierror = list_create(li_namevar)
            if (ierror == 0) then
                do i = 1 , size(for_var%namevar)
                    if (ierror == 0) then
                        ierror = li_namevar%append(for_var%namevar(i))
                        if (ierror /= 0) then
                            for_var%namevar(i) = ''
                            ierror = li_namevar%append(for_var%namevar(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('namevar',li_namevar)
        end if
        if (allocated(for_var%ivalues)) then
            if (ierror == 0) ierror = ndarray_create(nd_ivalues,for_var%ivalues)
            if (ierror == 0) ierror = py_var%setitem('ivalues',nd_ivalues)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_basic_numi_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_basic_numi_type

    Module Subroutine Unwrap_type_basic_numi_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(basic_numi_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_basic_numi_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'basic_numi_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_basic_numi_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numi_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numi_type','namevar',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%namevar,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numi_type','ivalues',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_basic_numi_type','ivalues',p_int_1d,for_var%ivalues,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_basic_numi_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_basic_numi_type

    Module Subroutine list_to_type_array1d_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numi_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_basic_numi_type

    Module Subroutine list_to_type_array1d_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numi_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_basic_numi_type_no_alloc

    Module Subroutine list_to_type_array2d_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numi_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_basic_numi_type

    Module Subroutine list_to_type_array2d_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numi_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_basic_numi_type_no_alloc

    Module Subroutine Wrap_basic_numr_type(for_var,py_var,ierror)

        ! Arguments
        type(basic_numr_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_namevar
        type(list) :: li_namevar
        type(ndarray) :: nd_rvalues

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','basic_numr_type')
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (allocated(for_var%namevar)) then
            if (ierror == 0) ierror = list_create(li_namevar)
            if (ierror == 0) then
                do i = 1 , size(for_var%namevar)
                    if (ierror == 0) then
                        ierror = li_namevar%append(for_var%namevar(i))
                        if (ierror /= 0) then
                            for_var%namevar(i) = ''
                            ierror = li_namevar%append(for_var%namevar(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('namevar',li_namevar)
        end if
        if (allocated(for_var%rvalues)) then
            if (ierror == 0) ierror = ndarray_create(nd_rvalues,for_var%rvalues)
            if (ierror == 0) ierror = py_var%setitem('rvalues',nd_rvalues)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_basic_numr_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_basic_numr_type

    Module Subroutine Unwrap_type_basic_numr_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(basic_numr_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_basic_numr_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'basic_numr_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_basic_numr_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numr_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numr_type','namevar',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%namevar,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_basic_numr_type','rvalues',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_basic_numr_type','rvalues',p_real_1d,for_var%rvalues,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_basic_numr_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_basic_numr_type

    Module Subroutine list_to_type_array1d_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numr_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_basic_numr_type

    Module Subroutine list_to_type_array1d_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numr_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_basic_numr_type_no_alloc

    Module Subroutine list_to_type_array2d_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numr_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_basic_numr_type

    Module Subroutine list_to_type_array2d_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(basic_numr_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_basic_numr_type_no_alloc

    Module Subroutine Wrap_calibration_detector_type(for_var,py_var,ierror)

        ! Arguments
        type(calibration_detector_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i,j
        type(dict) :: di_active
        type(list) :: li_active,li_active_2
        type(ndarray) :: nd_posx,nd_effic,nd_sposx,nd_seffic

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','calibration_detector_type')
        if (ierror == 0) then
            ierror = py_var%setitem('name_instrm',for_var%name_instrm)
            if (ierror /= 0) then
                for_var%name_instrm = ''
                ierror = py_var%setitem('name_instrm',for_var%name_instrm)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('ndet',for_var%ndet)
        if (ierror == 0) ierror = py_var%setitem('npointsdet',for_var%npointsdet)
        if (ierror == 0) ierror = py_var%setitem('pos_read',for_var%pos_read)
        if (ierror == 0) ierror = py_var%setitem('true_eff',for_var%true_eff)
        if (allocated(for_var%posx)) then
            if (ierror == 0) ierror = ndarray_create(nd_posx,for_var%posx)
            if (ierror == 0) ierror = py_var%setitem('posx',nd_posx)
        end if
        if (allocated(for_var%effic)) then
            if (ierror == 0) ierror = ndarray_create(nd_effic,for_var%effic)
            if (ierror == 0) ierror = py_var%setitem('effic',nd_effic)
        end if
        if (allocated(for_var%sposx)) then
            if (ierror == 0) ierror = ndarray_create(nd_sposx,for_var%sposx)
            if (ierror == 0) ierror = py_var%setitem('sposx',nd_sposx)
        end if
        if (allocated(for_var%seffic)) then
            if (ierror == 0) ierror = ndarray_create(nd_seffic,for_var%seffic)
            if (ierror == 0) ierror = py_var%setitem('seffic',nd_seffic)
        end if
        if (allocated(for_var%active)) then
            if (ierror == 0) ierror = list_create(li_active)
            if (ierror == 0) then
                do i = 1 , size(for_var%active,1)
                    if (ierror == 0) ierror = list_create(li_active_2)
                    do j = 1 , size(for_var%active,2)
                    if (ierror == 0) ierror = li_active_2%append(for_var%active(i,j))
                    end do
                    if (ierror == 0) ierror = li_active%append(li_active_2)
                    call li_active_2%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('active',li_active)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_calibration_detector_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_calibration_detector_type

    Module Subroutine Unwrap_type_calibration_detector_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(calibration_detector_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_calibration_detector_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'calibration_detector_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_calibration_detector_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','name_instrm',py_var,for_var%name_instrm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','ndet',py_var,for_var%ndet,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','npointsdet',py_var,for_var%npointsdet,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','pos_read',py_var,for_var%pos_read,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','true_eff',py_var,for_var%true_eff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','posx',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_calibration_detector_type','posx',p_real_1d,for_var%posx,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','effic',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_calibration_detector_type','effic',p_real_2d,for_var%effic,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','sposx',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_calibration_detector_type','sposx',p_real_1d,for_var%sposx,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','seffic',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_calibration_detector_type','seffic',p_real_2d,for_var%seffic,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_calibration_detector_type','active',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%active,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_calibration_detector_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_calibration_detector_type

    Module Subroutine list_to_type_array1d_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(calibration_detector_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_calibration_detector_type

    Module Subroutine list_to_type_array1d_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(calibration_detector_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_calibration_detector_type_no_alloc

    Module Subroutine list_to_type_array2d_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(calibration_detector_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_calibration_detector_type

    Module Subroutine list_to_type_array2d_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(calibration_detector_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_calibration_detector_type_no_alloc

    Module Subroutine Wrap_diffractometer_type(for_var,py_var,ierror)

        ! Arguments
        type(diffractometer_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_ang_names,di_disp_names
        type(list) :: li_ang_names,li_disp_names
        type(ndarray) :: nd_rd,nd_e1,nd_e2,nd_e3,nd_ang_limits,nd_ang_offsets,nd_ang_velocity,nd_disp_limits,nd_disp_offsets,nd_det_offsets,nd_range_time,nd_alphas,nd_sigma_alphas,nd_shifts,nd_resurf

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','diffractometer_type')
        if (ierror == 0) then
            ierror = py_var%setitem('info',for_var%info)
            if (ierror /= 0) then
                for_var%info = ''
                ierror = py_var%setitem('info',for_var%info)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('name_inst',for_var%name_inst)
            if (ierror /= 0) then
                for_var%name_inst = ''
                ierror = py_var%setitem('name_inst',for_var%name_inst)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('geom',for_var%geom)
            if (ierror /= 0) then
                for_var%geom = ''
                ierror = py_var%setitem('geom',for_var%geom)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('bl_frame',for_var%bl_frame)
            if (ierror /= 0) then
                for_var%bl_frame = ''
                ierror = py_var%setitem('bl_frame',for_var%bl_frame)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('dist_units',for_var%dist_units)
            if (ierror /= 0) then
                for_var%dist_units = ''
                ierror = py_var%setitem('dist_units',for_var%dist_units)
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
            ierror = py_var%setitem('angl_units',for_var%angl_units)
            if (ierror /= 0) then
                for_var%angl_units = ''
                ierror = py_var%setitem('angl_units',for_var%angl_units)
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
            ierror = py_var%setitem('data_ordering',for_var%data_ordering)
            if (ierror /= 0) then
                for_var%data_ordering = ''
                ierror = py_var%setitem('data_ordering',for_var%data_ordering)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('dist_samp_detector',for_var%dist_samp_detector)
        if (ierror == 0) ierror = py_var%setitem('wave',for_var%wave)
        if (ierror == 0) ierror = py_var%setitem('wave_min',for_var%wave_min)
        if (ierror == 0) ierror = py_var%setitem('wave_max',for_var%wave_max)
        if (ierror == 0) ierror = py_var%setitem('vert',for_var%vert)
        if (ierror == 0) ierror = py_var%setitem('horiz',for_var%horiz)
        if (ierror == 0) ierror = py_var%setitem('agap',for_var%agap)
        if (ierror == 0) ierror = py_var%setitem('cgap',for_var%cgap)
        if (ierror == 0) ierror = py_var%setitem('np_vert',for_var%np_vert)
        if (ierror == 0) ierror = py_var%setitem('np_horiz',for_var%np_horiz)
        if (ierror == 0) ierror = py_var%setitem('tilted',for_var%tilted)
        if (ierror == 0) ierror = ndarray_create(nd_rd,for_var%rd)
        if (ierror == 0) ierror = py_var%setitem('rd',nd_rd)
        if (ierror == 0) ierror = py_var%setitem('ga_d',for_var%ga_d)
        if (ierror == 0) ierror = py_var%setitem('nu_d',for_var%nu_d)
        if (ierror == 0) ierror = py_var%setitem('tiltx_d',for_var%tiltx_d)
        if (ierror == 0) ierror = py_var%setitem('tilty_d',for_var%tilty_d)
        if (ierror == 0) ierror = py_var%setitem('tiltz_d',for_var%tiltz_d)
        if (ierror == 0) ierror = py_var%setitem('igeom',for_var%igeom)
        if (ierror == 0) ierror = py_var%setitem('ipsd',for_var%ipsd)
        if (ierror == 0) ierror = ndarray_create(nd_e1,for_var%e1)
        if (ierror == 0) ierror = py_var%setitem('e1',nd_e1)
        if (ierror == 0) ierror = ndarray_create(nd_e2,for_var%e2)
        if (ierror == 0) ierror = py_var%setitem('e2',nd_e2)
        if (ierror == 0) ierror = ndarray_create(nd_e3,for_var%e3)
        if (ierror == 0) ierror = py_var%setitem('e3',nd_e3)
        if (ierror == 0) ierror = py_var%setitem('gnxz_limited',for_var%gnxz_limited)
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
        if (ierror == 0) ierror = py_var%setitem('num_ang',for_var%num_ang)
        if (ierror == 0) ierror = list_create(li_ang_names)
        if (ierror == 0) then
            do i = 1 , size(for_var%ang_names)
                if (ierror == 0) then
                    ierror = li_ang_names%append(for_var%ang_names(i))
                    if (ierror /= 0) then
                        for_var%ang_names(i) = ''
                        ierror = li_ang_names%append(for_var%ang_names(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('ang_names',li_ang_names)
        if (ierror == 0) ierror = ndarray_create(nd_ang_limits,for_var%ang_limits)
        if (ierror == 0) ierror = py_var%setitem('ang_limits',nd_ang_limits)
        if (ierror == 0) ierror = ndarray_create(nd_ang_offsets,for_var%ang_offsets)
        if (ierror == 0) ierror = py_var%setitem('ang_offsets',nd_ang_offsets)
        if (ierror == 0) ierror = ndarray_create(nd_ang_velocity,for_var%ang_velocity)
        if (ierror == 0) ierror = py_var%setitem('ang_velocity',nd_ang_velocity)
        if (ierror == 0) ierror = py_var%setitem('num_disp',for_var%num_disp)
        if (ierror == 0) ierror = list_create(li_disp_names)
        if (ierror == 0) then
            do i = 1 , size(for_var%disp_names)
                if (ierror == 0) then
                    ierror = li_disp_names%append(for_var%disp_names(i))
                    if (ierror /= 0) then
                        for_var%disp_names(i) = ''
                        ierror = li_disp_names%append(for_var%disp_names(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('disp_names',li_disp_names)
        if (ierror == 0) ierror = ndarray_create(nd_disp_limits,for_var%disp_limits)
        if (ierror == 0) ierror = py_var%setitem('disp_limits',nd_disp_limits)
        if (ierror == 0) ierror = ndarray_create(nd_disp_offsets,for_var%disp_offsets)
        if (ierror == 0) ierror = py_var%setitem('disp_offsets',nd_disp_offsets)
        if (ierror == 0) ierror = py_var%setitem('displaced',for_var%displaced)
        if (ierror == 0) ierror = ndarray_create(nd_det_offsets,for_var%det_offsets)
        if (ierror == 0) ierror = py_var%setitem('det_offsets',nd_det_offsets)
        if (ierror == 0) ierror = py_var%setitem('rangtim',for_var%rangtim)
        if (allocated(for_var%range_time)) then
            if (ierror == 0) ierror = ndarray_create(nd_range_time,for_var%range_time)
            if (ierror == 0) ierror = py_var%setitem('range_time',nd_range_time)
        end if
        if (allocated(for_var%alphas)) then
            if (ierror == 0) ierror = ndarray_create(nd_alphas,for_var%alphas)
            if (ierror == 0) ierror = py_var%setitem('alphas',nd_alphas)
        end if
        if (allocated(for_var%sigma_alphas)) then
            if (ierror == 0) ierror = ndarray_create(nd_sigma_alphas,for_var%sigma_alphas)
            if (ierror == 0) ierror = py_var%setitem('sigma_alphas',nd_sigma_alphas)
        end if
        if (ierror == 0) ierror = py_var%setitem('shift_correct',for_var%shift_correct)
        if (allocated(for_var%shifts)) then
            if (ierror == 0) ierror = ndarray_create(nd_shifts,for_var%shifts)
            if (ierror == 0) ierror = py_var%setitem('shifts',nd_shifts)
        end if
        if (ierror == 0) ierror = py_var%setitem('alpha_correct',for_var%alpha_correct)
        if (ierror == 0) ierror = py_var%setitem('sigma_alpha_correct',for_var%sigma_alpha_correct)
        if (ierror == 0) then
            ierror = py_var%setitem('alpha_file',for_var%alpha_file)
            if (ierror /= 0) then
                for_var%alpha_file = ''
                ierror = py_var%setitem('alpha_file',for_var%alpha_file)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('resol_given',for_var%resol_given)
        if (ierror == 0) ierror = py_var%setitem('nga',for_var%nga)
        if (ierror == 0) ierror = py_var%setitem('nnu',for_var%nnu)
        if (allocated(for_var%resurf)) then
            if (ierror == 0) ierror = ndarray_create(nd_resurf,for_var%resurf)
            if (ierror == 0) ierror = py_var%setitem('resurf',nd_resurf)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_diffractometer_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_diffractometer_type

    Module Subroutine Unwrap_type_diffractometer_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(diffractometer_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
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
            err_cfml%msg  = 'Unwrap_diffractometer_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffractometer_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffractometer_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','info',py_var,for_var%info,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','name_inst',py_var,for_var%name_inst,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','bl_frame',py_var,for_var%bl_frame,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','dist_units',py_var,for_var%dist_units,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','r_ord',py_var,for_var%r_ord,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','angl_units',py_var,for_var%angl_units,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','detector_type',py_var,for_var%detector_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','data_ordering',py_var,for_var%data_ordering,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','dist_samp_detector',py_var,for_var%dist_samp_detector,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','wave',py_var,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','wave_min',py_var,for_var%wave_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','wave_max',py_var,for_var%wave_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','vert',py_var,for_var%vert,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','horiz',py_var,for_var%horiz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','agap',py_var,for_var%agap,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','cgap',py_var,for_var%cgap,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','np_vert',py_var,for_var%np_vert,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','np_horiz',py_var,for_var%np_horiz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','tilted',py_var,for_var%tilted,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','rd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','rd',p_real_2d,for_var%rd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ga_d',py_var,for_var%ga_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','nu_d',py_var,for_var%nu_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','tiltx_d',py_var,for_var%tiltx_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','tilty_d',py_var,for_var%tilty_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','tiltz_d',py_var,for_var%tiltz_d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','igeom',py_var,for_var%igeom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ipsd',py_var,for_var%ipsd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','e1',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','e1',p_real_1d,for_var%e1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','e2',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','e2',p_real_1d,for_var%e2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','e3',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','e3',p_real_1d,for_var%e3,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','gnxz_limited',py_var,for_var%gnxz_limited,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','gap_min',py_var,for_var%gap_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','gap_max',py_var,for_var%gap_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','gan_min',py_var,for_var%gan_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','gan_max',py_var,for_var%gan_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','nu_min',py_var,for_var%nu_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','nu_max',py_var,for_var%nu_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','d_min',py_var,for_var%d_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','x_min',py_var,for_var%x_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','x_max',py_var,for_var%x_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','z_min',py_var,for_var%z_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','z_max',py_var,for_var%z_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','num_ang',py_var,for_var%num_ang,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ang_names',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%ang_names,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ang_limits',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','ang_limits',p_real_2d,for_var%ang_limits,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ang_offsets',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','ang_offsets',p_real_1d,for_var%ang_offsets,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','ang_velocity',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','ang_velocity',p_real_1d,for_var%ang_velocity,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','num_disp',py_var,for_var%num_disp,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','disp_names',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%disp_names,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','disp_limits',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','disp_limits',p_real_2d,for_var%disp_limits,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','disp_offsets',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','disp_offsets',p_real_1d,for_var%disp_offsets,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','displaced',py_var,for_var%displaced,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','det_offsets',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffractometer_type','det_offsets',p_real_1d,for_var%det_offsets,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','rangtim',py_var,for_var%rangtim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','range_time',py_var,p_real_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffractometer_type','range_time',p_real_3d,for_var%range_time,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','alphas',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffractometer_type','alphas',p_real_2d,for_var%alphas,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','sigma_alphas',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffractometer_type','sigma_alphas',p_real_2d,for_var%sigma_alphas,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','shift_correct',py_var,for_var%shift_correct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','shifts',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffractometer_type','shifts',p_real_1d,for_var%shifts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','alpha_correct',py_var,for_var%alpha_correct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','sigma_alpha_correct',py_var,for_var%sigma_alpha_correct,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','alpha_file',py_var,for_var%alpha_file,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','resol_given',py_var,for_var%resol_given,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','nga',py_var,for_var%nga,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','nnu',py_var,for_var%nnu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffractometer_type','resurf',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffractometer_type','resurf',p_real_2d,for_var%resurf,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffractometer_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_diffractometer_type

    Module Subroutine list_to_type_array1d_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffractometer_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_diffractometer_type

    Module Subroutine list_to_type_array1d_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffractometer_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_diffractometer_type_no_alloc

    Module Subroutine list_to_type_array2d_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffractometer_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_diffractometer_type

    Module Subroutine list_to_type_array2d_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffractometer_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_diffractometer_type_no_alloc

    Module Subroutine Wrap_generic_numor_type(for_var,py_var,ierror)

        ! Arguments
        type(generic_numor_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_sampleid,di_diffopt,di_monmpar,di_diffmpar,di_detpar,di_dacflags,di_dacparam,di_samplest,di_icounts,di_rcounts

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','generic_numor_type')
        if (ierror == 0) ierror = py_var%setitem('numor',for_var%numor)
        if (ierror == 0) then
            ierror = py_var%setitem('instr',for_var%instr)
            if (ierror /= 0) then
                for_var%instr = ''
                ierror = py_var%setitem('instr',for_var%instr)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('expname',for_var%expname)
            if (ierror /= 0) then
                for_var%expname = ''
                ierror = py_var%setitem('expname',for_var%expname)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('date',for_var%date)
            if (ierror /= 0) then
                for_var%date = ''
                ierror = py_var%setitem('date',for_var%date)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) ierror = dict_create(di_sampleid)
        if (ierror == 0) call wrap_type(for_var%sampleid,di_sampleid,ierror)
        if (ierror == 0) ierror = py_var%setitem('sampleid',di_sampleid)
        if (ierror == 0) ierror = dict_create(di_diffopt)
        if (ierror == 0) call wrap_type(for_var%diffopt,di_diffopt,ierror)
        if (ierror == 0) ierror = py_var%setitem('diffopt',di_diffopt)
        if (ierror == 0) ierror = dict_create(di_monmpar)
        if (ierror == 0) call wrap_type(for_var%monmpar,di_monmpar,ierror)
        if (ierror == 0) ierror = py_var%setitem('monmpar',di_monmpar)
        if (ierror == 0) ierror = dict_create(di_diffmpar)
        if (ierror == 0) call wrap_type(for_var%diffmpar,di_diffmpar,ierror)
        if (ierror == 0) ierror = py_var%setitem('diffmpar',di_diffmpar)
        if (ierror == 0) ierror = dict_create(di_detpar)
        if (ierror == 0) call wrap_type(for_var%detpar,di_detpar,ierror)
        if (ierror == 0) ierror = py_var%setitem('detpar',di_detpar)
        if (ierror == 0) ierror = dict_create(di_dacflags)
        if (ierror == 0) call wrap_type(for_var%dacflags,di_dacflags,ierror)
        if (ierror == 0) ierror = py_var%setitem('dacflags',di_dacflags)
        if (ierror == 0) ierror = dict_create(di_dacparam)
        if (ierror == 0) call wrap_type(for_var%dacparam,di_dacparam,ierror)
        if (ierror == 0) ierror = py_var%setitem('dacparam',di_dacparam)
        if (ierror == 0) ierror = dict_create(di_samplest)
        if (ierror == 0) call wrap_type(for_var%samplest,di_samplest,ierror)
        if (ierror == 0) ierror = py_var%setitem('samplest',di_samplest)
        if (ierror == 0) ierror = dict_create(di_icounts)
        if (ierror == 0) call wrap_type(for_var%icounts,di_icounts,ierror)
        if (ierror == 0) ierror = py_var%setitem('icounts',di_icounts)
        if (ierror == 0) ierror = dict_create(di_rcounts)
        if (ierror == 0) call wrap_type(for_var%rcounts,di_rcounts,ierror)
        if (ierror == 0) ierror = py_var%setitem('rcounts',di_rcounts)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_generic_numor_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_generic_numor_type

    Module Subroutine Unwrap_type_generic_numor_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(generic_numor_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        type(dict) :: dict_sampleid,dict_diffopt,dict_monmpar,dict_diffmpar,dict_detpar,dict_dacflags,dict_dacparam,dict_samplest,dict_icounts,dict_rcounts

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_generic_numor_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'generic_numor_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_generic_numor_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','numor',py_var,for_var%numor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','instr',py_var,for_var%instr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','expname',py_var,for_var%expname,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','date',py_var,for_var%date,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','sampleid',py_var,dict_sampleid,ierror)
        if (ierror == 0) call unwrap_type(dict_sampleid,for_var%sampleid,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','diffopt',py_var,dict_diffopt,ierror)
        if (ierror == 0) call unwrap_type(dict_diffopt,for_var%diffopt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','monmpar',py_var,dict_monmpar,ierror)
        if (ierror == 0) call unwrap_type(dict_monmpar,for_var%monmpar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','diffmpar',py_var,dict_diffmpar,ierror)
        if (ierror == 0) call unwrap_type(dict_diffmpar,for_var%diffmpar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','detpar',py_var,dict_detpar,ierror)
        if (ierror == 0) call unwrap_type(dict_detpar,for_var%detpar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','dacflags',py_var,dict_dacflags,ierror)
        if (ierror == 0) call unwrap_type(dict_dacflags,for_var%dacflags,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','dacparam',py_var,dict_dacparam,ierror)
        if (ierror == 0) call unwrap_type(dict_dacparam,for_var%dacparam,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','samplest',py_var,dict_samplest,ierror)
        if (ierror == 0) call unwrap_type(dict_samplest,for_var%samplest,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','icounts',py_var,dict_icounts,ierror)
        if (ierror == 0) call unwrap_type(dict_icounts,for_var%icounts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_generic_numor_type','rcounts',py_var,dict_rcounts,ierror)
        if (ierror == 0) call unwrap_type(dict_rcounts,for_var%rcounts,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_generic_numor_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_generic_numor_type

    Module Subroutine list_to_type_array1d_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(generic_numor_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_generic_numor_type

    Module Subroutine list_to_type_array1d_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(generic_numor_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_generic_numor_type_no_alloc

    Module Subroutine list_to_type_array2d_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(generic_numor_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_generic_numor_type

    Module Subroutine list_to_type_array2d_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(generic_numor_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_generic_numor_type_no_alloc

    Module Subroutine Wrap_ill_data_record_type(for_var,py_var,ierror)

        ! Arguments
        type(ill_data_record_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_icdesc,nd_valco,nd_valdef,nd_valenv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','ill_data_record_type')
        if (ierror == 0) ierror = py_var%setitem('numor',for_var%numor)
        if (ierror == 0) ierror = py_var%setitem('nset_prime',for_var%nset_prime)
        if (ierror == 0) ierror = py_var%setitem('ntran',for_var%ntran)
        if (ierror == 0) then
            ierror = py_var%setitem('inst_ch',for_var%inst_ch)
            if (ierror /= 0) then
                for_var%inst_ch = ''
                ierror = py_var%setitem('inst_ch',for_var%inst_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('date_ch',for_var%date_ch)
            if (ierror /= 0) then
                for_var%date_ch = ''
                ierror = py_var%setitem('date_ch',for_var%date_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('fill_ch',for_var%fill_ch)
            if (ierror /= 0) then
                for_var%fill_ch = ''
                ierror = py_var%setitem('fill_ch',for_var%fill_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('user_ch',for_var%user_ch)
            if (ierror /= 0) then
                for_var%user_ch = ''
                ierror = py_var%setitem('user_ch',for_var%user_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('lc_ch',for_var%lc_ch)
            if (ierror /= 0) then
                for_var%lc_ch = ''
                ierror = py_var%setitem('lc_ch',for_var%lc_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('text_ch',for_var%text_ch)
            if (ierror /= 0) then
                for_var%text_ch = ''
                ierror = py_var%setitem('text_ch',for_var%text_ch)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('scan_motor',for_var%scan_motor)
            if (ierror /= 0) then
                for_var%scan_motor = ''
                ierror = py_var%setitem('scan_motor',for_var%scan_motor)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('nvers',for_var%nvers)
        if (ierror == 0) ierror = py_var%setitem('ntype',for_var%ntype)
        if (ierror == 0) ierror = py_var%setitem('kctrl',for_var%kctrl)
        if (ierror == 0) ierror = py_var%setitem('manip',for_var%manip)
        if (ierror == 0) ierror = py_var%setitem('nbang',for_var%nbang)
        if (ierror == 0) ierror = py_var%setitem('nkmes',for_var%nkmes)
        if (ierror == 0) ierror = py_var%setitem('npdone',for_var%npdone)
        if (ierror == 0) ierror = py_var%setitem('jcode',for_var%jcode)
        if (ierror == 0) ierror = py_var%setitem('icalc',for_var%icalc)
        if (ierror == 0) ierror = py_var%setitem('ianal',for_var%ianal)
        if (ierror == 0) ierror = py_var%setitem('imode',for_var%imode)
        if (ierror == 0) ierror = py_var%setitem('itgv',for_var%itgv)
        if (ierror == 0) ierror = py_var%setitem('iregul',for_var%iregul)
        if (ierror == 0) ierror = py_var%setitem('ivolt',for_var%ivolt)
        if (ierror == 0) ierror = py_var%setitem('naxe',for_var%naxe)
        if (ierror == 0) ierror = py_var%setitem('npstart',for_var%npstart)
        if (ierror == 0) ierror = py_var%setitem('ilasti',for_var%ilasti)
        if (ierror == 0) ierror = py_var%setitem('isa',for_var%isa)
        if (ierror == 0) ierror = py_var%setitem('flgkif',for_var%flgkif)
        if (ierror == 0) ierror = py_var%setitem('ih_sqs',for_var%ih_sqs)
        if (ierror == 0) ierror = py_var%setitem('ik_sqs',for_var%ik_sqs)
        if (ierror == 0) ierror = py_var%setitem('nbsqs',for_var%nbsqs)
        if (ierror == 0) ierror = py_var%setitem('nb_cells',for_var%nb_cells)
        if (ierror == 0) ierror = py_var%setitem('nfree1',for_var%nfree1)
        if (ierror == 0) ierror = ndarray_create(nd_icdesc,for_var%icdesc)
        if (ierror == 0) ierror = py_var%setitem('icdesc',nd_icdesc)
        if (ierror == 0) ierror = ndarray_create(nd_valco,for_var%valco)
        if (ierror == 0) ierror = py_var%setitem('valco',nd_valco)
        if (ierror == 0) ierror = ndarray_create(nd_valdef,for_var%valdef)
        if (ierror == 0) ierror = py_var%setitem('valdef',nd_valdef)
        if (ierror == 0) ierror = ndarray_create(nd_valenv,for_var%valenv)
        if (ierror == 0) ierror = py_var%setitem('valenv',nd_valenv)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_ill_data_record_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_ill_data_record_type

    Module Subroutine Unwrap_type_ill_data_record_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(ill_data_record_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_ill_data_record_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'ill_data_record_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_ill_data_record_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','numor',py_var,for_var%numor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nset_prime',py_var,for_var%nset_prime,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ntran',py_var,for_var%ntran,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','inst_ch',py_var,for_var%inst_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','date_ch',py_var,for_var%date_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','fill_ch',py_var,for_var%fill_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','user_ch',py_var,for_var%user_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','lc_ch',py_var,for_var%lc_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','text_ch',py_var,for_var%text_ch,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','scan_motor',py_var,for_var%scan_motor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nvers',py_var,for_var%nvers,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ntype',py_var,for_var%ntype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','kctrl',py_var,for_var%kctrl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','manip',py_var,for_var%manip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nbang',py_var,for_var%nbang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nkmes',py_var,for_var%nkmes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','npdone',py_var,for_var%npdone,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','jcode',py_var,for_var%jcode,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','icalc',py_var,for_var%icalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ianal',py_var,for_var%ianal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','imode',py_var,for_var%imode,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','itgv',py_var,for_var%itgv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','iregul',py_var,for_var%iregul,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ivolt',py_var,for_var%ivolt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','naxe',py_var,for_var%naxe,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','npstart',py_var,for_var%npstart,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ilasti',py_var,for_var%ilasti,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','isa',py_var,for_var%isa,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','flgkif',py_var,for_var%flgkif,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ih_sqs',py_var,for_var%ih_sqs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','ik_sqs',py_var,for_var%ik_sqs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nbsqs',py_var,for_var%nbsqs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nb_cells',py_var,for_var%nb_cells,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','nfree1',py_var,for_var%nfree1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','icdesc',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_ill_data_record_type','icdesc',p_int_1d,for_var%icdesc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','valco',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_ill_data_record_type','valco',p_real_1d,for_var%valco,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','valdef',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_ill_data_record_type','valdef',p_real_1d,for_var%valdef,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_ill_data_record_type','valenv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_ill_data_record_type','valenv',p_real_1d,for_var%valenv,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_ill_data_record_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_ill_data_record_type

    Module Subroutine list_to_type_array1d_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(ill_data_record_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_ill_data_record_type

    Module Subroutine list_to_type_array1d_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(ill_data_record_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_ill_data_record_type_no_alloc

    Module Subroutine list_to_type_array2d_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(ill_data_record_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_ill_data_record_type

    Module Subroutine list_to_type_array2d_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(ill_data_record_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_ill_data_record_type_no_alloc

    Module Subroutine Wrap_powder_numor_type(for_var,py_var,ierror)

        ! Arguments
        type(powder_numor_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_angles,nd_scans,nd_conditions,nd_icdesc,nd_tmc_ang,nd_counts

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','powder_numor_type')
        if (ierror == 0) ierror = py_var%setitem('numor',for_var%numor)
        if (ierror == 0) ierror = py_var%setitem('manip',for_var%manip)
        if (ierror == 0) ierror = py_var%setitem('icalc',for_var%icalc)
        if (ierror == 0) then
            ierror = py_var%setitem('header',for_var%header)
            if (ierror /= 0) then
                for_var%header = ''
                ierror = py_var%setitem('header',for_var%header)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('instrm',for_var%instrm)
            if (ierror /= 0) then
                for_var%instrm = ''
                ierror = py_var%setitem('instrm',for_var%instrm)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('scantype',for_var%scantype)
            if (ierror /= 0) then
                for_var%scantype = ''
                ierror = py_var%setitem('scantype',for_var%scantype)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_angles,for_var%angles)
        if (ierror == 0) ierror = py_var%setitem('angles',nd_angles)
        if (ierror == 0) ierror = ndarray_create(nd_scans,for_var%scans)
        if (ierror == 0) ierror = py_var%setitem('scans',nd_scans)
        if (ierror == 0) ierror = py_var%setitem('monitor',for_var%monitor)
        if (ierror == 0) ierror = py_var%setitem('time',for_var%time)
        if (ierror == 0) ierror = py_var%setitem('wave',for_var%wave)
        if (ierror == 0) ierror = ndarray_create(nd_conditions,for_var%conditions)
        if (ierror == 0) ierror = py_var%setitem('conditions',nd_conditions)
        if (ierror == 0) ierror = py_var%setitem('nbdata',for_var%nbdata)
        if (ierror == 0) ierror = py_var%setitem('nframes',for_var%nframes)
        if (ierror == 0) ierror = py_var%setitem('nbang',for_var%nbang)
        if (ierror == 0) ierror = ndarray_create(nd_icdesc,for_var%icdesc)
        if (ierror == 0) ierror = py_var%setitem('icdesc',nd_icdesc)
        if (allocated(for_var%tmc_ang)) then
            if (ierror == 0) ierror = ndarray_create(nd_tmc_ang,for_var%tmc_ang)
            if (ierror == 0) ierror = py_var%setitem('tmc_ang',nd_tmc_ang)
        end if
        if (allocated(for_var%counts)) then
            if (ierror == 0) ierror = ndarray_create(nd_counts,for_var%counts)
            if (ierror == 0) ierror = py_var%setitem('counts',nd_counts)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_powder_numor_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_powder_numor_type

    Module Subroutine Unwrap_type_powder_numor_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(powder_numor_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_powder_numor_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'powder_numor_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powder_numor_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','numor',py_var,for_var%numor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','manip',py_var,for_var%manip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','icalc',py_var,for_var%icalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','header',py_var,for_var%header,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','instrm',py_var,for_var%instrm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','scantype',py_var,for_var%scantype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','angles',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_numor_type','angles',p_real_1d,for_var%angles,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','scans',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_numor_type','scans',p_real_1d,for_var%scans,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','monitor',py_var,for_var%monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','time',py_var,for_var%time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','wave',py_var,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','conditions',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_numor_type','conditions',p_real_1d,for_var%conditions,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','nbdata',py_var,for_var%nbdata,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','nframes',py_var,for_var%nframes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','nbang',py_var,for_var%nbang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','icdesc',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powder_numor_type','icdesc',p_int_1d,for_var%icdesc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','tmc_ang',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powder_numor_type','tmc_ang',p_real_2d,for_var%tmc_ang,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powder_numor_type','counts',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powder_numor_type','counts',p_real_2d,for_var%counts,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powder_numor_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_powder_numor_type

    Module Subroutine list_to_type_array1d_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_numor_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_powder_numor_type

    Module Subroutine list_to_type_array1d_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_numor_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_powder_numor_type_no_alloc

    Module Subroutine list_to_type_array2d_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_numor_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_powder_numor_type

    Module Subroutine list_to_type_array2d_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powder_numor_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_powder_numor_type_no_alloc

    Module Subroutine Wrap_sxtal_numor_type(for_var,py_var,ierror)

        ! Arguments
        type(sxtal_numor_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_hmin,nd_hmax,nd_angles,nd_ub,nd_dh,nd_scans,nd_conditions,nd_icdesc,nd_selected_frames,nd_tmc_ang,nd_counts

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sxtal_numor_type')
        if (ierror == 0) then
            ierror = py_var%setitem('filename',for_var%filename)
            if (ierror /= 0) then
                for_var%filename = ''
                ierror = py_var%setitem('filename',for_var%filename)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('numor',for_var%numor)
        if (ierror == 0) ierror = py_var%setitem('manip',for_var%manip)
        if (ierror == 0) ierror = py_var%setitem('icalc',for_var%icalc)
        if (ierror == 0) then
            ierror = py_var%setitem('header',for_var%header)
            if (ierror /= 0) then
                for_var%header = ''
                ierror = py_var%setitem('header',for_var%header)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('instrm',for_var%instrm)
            if (ierror /= 0) then
                for_var%instrm = ''
                ierror = py_var%setitem('instrm',for_var%instrm)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('scantype',for_var%scantype)
            if (ierror /= 0) then
                for_var%scantype = ''
                ierror = py_var%setitem('scantype',for_var%scantype)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_hmin,for_var%hmin)
        if (ierror == 0) ierror = py_var%setitem('hmin',nd_hmin)
        if (ierror == 0) ierror = ndarray_create(nd_hmax,for_var%hmax)
        if (ierror == 0) ierror = py_var%setitem('hmax',nd_hmax)
        if (ierror == 0) ierror = ndarray_create(nd_angles,for_var%angles)
        if (ierror == 0) ierror = py_var%setitem('angles',nd_angles)
        if (ierror == 0) ierror = ndarray_create(nd_ub,for_var%ub)
        if (ierror == 0) ierror = py_var%setitem('ub',nd_ub)
        if (ierror == 0) ierror = ndarray_create(nd_dh,for_var%dh)
        if (ierror == 0) ierror = py_var%setitem('dh',nd_dh)
        if (ierror == 0) ierror = ndarray_create(nd_scans,for_var%scans)
        if (ierror == 0) ierror = py_var%setitem('scans',nd_scans)
        if (ierror == 0) ierror = py_var%setitem('preset',for_var%preset)
        if (ierror == 0) ierror = py_var%setitem('wave',for_var%wave)
        if (ierror == 0) ierror = py_var%setitem('dist',for_var%dist)
        if (ierror == 0) ierror = py_var%setitem('cpl_fact',for_var%cpl_fact)
        if (ierror == 0) ierror = ndarray_create(nd_conditions,for_var%conditions)
        if (ierror == 0) ierror = py_var%setitem('conditions',nd_conditions)
        if (ierror == 0) ierror = py_var%setitem('nbdata',for_var%nbdata)
        if (ierror == 0) ierror = py_var%setitem('nframes',for_var%nframes)
        if (ierror == 0) ierror = py_var%setitem('nbang',for_var%nbang)
        if (ierror == 0) ierror = ndarray_create(nd_icdesc,for_var%icdesc)
        if (ierror == 0) ierror = py_var%setitem('icdesc',nd_icdesc)
        if (ierror == 0) ierror = py_var%setitem('header_size',for_var%header_size)
        if (ierror == 0) ierror = py_var%setitem('frame_size',for_var%frame_size)
        if (allocated(for_var%selected_frames)) then
            if (ierror == 0) ierror = ndarray_create(nd_selected_frames,for_var%selected_frames)
            if (ierror == 0) ierror = py_var%setitem('selected_frames',nd_selected_frames)
        end if
        if (allocated(for_var%tmc_ang)) then
            if (ierror == 0) ierror = ndarray_create(nd_tmc_ang,for_var%tmc_ang)
            if (ierror == 0) ierror = py_var%setitem('tmc_ang',nd_tmc_ang)
        end if
        if (allocated(for_var%counts)) then
            if (ierror == 0) ierror = ndarray_create(nd_counts,for_var%counts)
            if (ierror == 0) ierror = py_var%setitem('counts',nd_counts)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sxtal_numor_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sxtal_numor_type

    Module Subroutine Unwrap_type_sxtal_numor_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sxtal_numor_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_sxtal_numor_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sxtal_numor_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sxtal_numor_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','numor',py_var,for_var%numor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','manip',py_var,for_var%manip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','icalc',py_var,for_var%icalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','header',py_var,for_var%header,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','instrm',py_var,for_var%instrm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','scantype',py_var,for_var%scantype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','hmin',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','hmin',p_real_1d,for_var%hmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','hmax',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','hmax',p_real_1d,for_var%hmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','angles',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','angles',p_real_1d,for_var%angles,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','ub',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','ub',p_real_2d,for_var%ub,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','dh',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','dh',p_real_1d,for_var%dh,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','scans',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','scans',p_real_1d,for_var%scans,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','preset',py_var,for_var%preset,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','wave',py_var,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','dist',py_var,for_var%dist,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','cpl_fact',py_var,for_var%cpl_fact,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','conditions',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','conditions',p_real_1d,for_var%conditions,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','nbdata',py_var,for_var%nbdata,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','nframes',py_var,for_var%nframes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','nbang',py_var,for_var%nbang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','icdesc',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_numor_type','icdesc',p_int_1d,for_var%icdesc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','header_size',py_var,for_var%header_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','frame_size',py_var,for_var%frame_size,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','selected_frames',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_sxtal_numor_type','selected_frames',p_int_1d,for_var%selected_frames,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','tmc_ang',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_sxtal_numor_type','tmc_ang',p_real_2d,for_var%tmc_ang,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_numor_type','counts',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_sxtal_numor_type','counts',p_real_2d,for_var%counts,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sxtal_numor_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sxtal_numor_type

    Module Subroutine list_to_type_array1d_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_numor_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_numor_type

    Module Subroutine list_to_type_array1d_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_numor_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_numor_type_no_alloc

    Module Subroutine list_to_type_array2d_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_numor_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_numor_type

    Module Subroutine list_to_type_array2d_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_numor_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_numor_type_no_alloc

    Module Subroutine Wrap_sxtal_orient_type(for_var,py_var,ierror)

        ! Arguments
        type(sxtal_orient_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_ub,nd_ubinv,nd_conv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sxtal_orient_type')
        if (ierror == 0) ierror = py_var%setitem('orient_set',for_var%orient_set)
        if (ierror == 0) ierror = py_var%setitem('wave',for_var%wave)
        if (ierror == 0) ierror = ndarray_create(nd_ub,for_var%ub)
        if (ierror == 0) ierror = py_var%setitem('ub',nd_ub)
        if (ierror == 0) ierror = ndarray_create(nd_ubinv,for_var%ubinv)
        if (ierror == 0) ierror = py_var%setitem('ubinv',nd_ubinv)
        if (ierror == 0) ierror = ndarray_create(nd_conv,for_var%conv)
        if (ierror == 0) ierror = py_var%setitem('conv',nd_conv)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sxtal_orient_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sxtal_orient_type

    Module Subroutine Unwrap_type_sxtal_orient_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sxtal_orient_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_sxtal_orient_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sxtal_orient_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sxtal_orient_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_orient_type','orient_set',py_var,for_var%orient_set,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_orient_type','wave',py_var,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_orient_type','ub',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_orient_type','ub',p_real_2d,for_var%ub,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_orient_type','ubinv',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_orient_type','ubinv',p_real_2d,for_var%ubinv,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxtal_orient_type','conv',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sxtal_orient_type','conv',p_real_2d,for_var%conv,ierror,order)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sxtal_orient_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sxtal_orient_type

    Module Subroutine list_to_type_array1d_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_orient_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_orient_type

    Module Subroutine list_to_type_array1d_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_orient_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sxtal_orient_type_no_alloc

    Module Subroutine list_to_type_array2d_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_orient_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_orient_type

    Module Subroutine list_to_type_array2d_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxtal_orient_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sxtal_orient_type_no_alloc

end submodule