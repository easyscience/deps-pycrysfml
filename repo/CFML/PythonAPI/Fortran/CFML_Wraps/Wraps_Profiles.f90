submodule (CFML_Wraps) Wraps_Profiles

    implicit none
    contains

    Module Subroutine Wrap_deriv_tof_type(for_var,py_var,ierror)

        ! Arguments
        type(deriv_tof_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','deriv_tof_type')
        if (ierror == 0) ierror = py_var%setitem('alfa',for_var%alfa)
        if (ierror == 0) ierror = py_var%setitem('beta',for_var%beta)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (ierror == 0) ierror = py_var%setitem('sigma',for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('gamm',for_var%gamm)
        if (ierror == 0) ierror = py_var%setitem('eta',for_var%eta)
        if (ierror == 0) ierror = py_var%setitem('kappa',for_var%kappa)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_deriv_tof_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_deriv_tof_type

    Module Subroutine Unwrap_type_deriv_tof_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(deriv_tof_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_deriv_tof_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'deriv_tof_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_deriv_tof_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','alfa',py_var,for_var%alfa,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','beta',py_var,for_var%beta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','sigma',py_var,for_var%sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','gamm',py_var,for_var%gamm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','eta',py_var,for_var%eta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','kappa',py_var,for_var%kappa,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_deriv_tof_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_deriv_tof_type

    Module Subroutine list_to_type_array1d_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(deriv_tof_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_deriv_tof_type

    Module Subroutine list_to_type_array1d_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(deriv_tof_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_deriv_tof_type_no_alloc

    Module Subroutine list_to_type_array2d_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(deriv_tof_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_deriv_tof_type

    Module Subroutine list_to_type_array2d_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(deriv_tof_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_deriv_tof_type_no_alloc

end submodule