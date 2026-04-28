submodule (CFML_Wraps) Wraps_Simulated_Annealing

    implicit none
    contains

    Module Subroutine Wrap_multistate_vector_type(for_var,py_var,ierror)

        ! Arguments
        type(multistate_vector_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_nampar
        type(list) :: li_nampar
        type(ndarray) :: nd_code,nd_bound,nd_state,nd_stp,nd_cost,nd_low,nd_high,nd_config,nd_sigma

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','multistate_vector_type')
        if (ierror == 0) ierror = py_var%setitem('npar',for_var%npar)
        if (ierror == 0) ierror = py_var%setitem('nconf',for_var%nconf)
        if (ierror == 0) ierror = ndarray_create(nd_code,for_var%code)
        if (ierror == 0) ierror = py_var%setitem('code',nd_code)
        if (ierror == 0) ierror = ndarray_create(nd_bound,for_var%bound)
        if (ierror == 0) ierror = py_var%setitem('bound',nd_bound)
        if (ierror == 0) ierror = ndarray_create(nd_state,for_var%state)
        if (ierror == 0) ierror = py_var%setitem('state',nd_state)
        if (ierror == 0) ierror = ndarray_create(nd_stp,for_var%stp)
        if (ierror == 0) ierror = py_var%setitem('stp',nd_stp)
        if (ierror == 0) ierror = ndarray_create(nd_cost,for_var%cost)
        if (ierror == 0) ierror = py_var%setitem('cost',nd_cost)
        if (ierror == 0) ierror = ndarray_create(nd_low,for_var%low)
        if (ierror == 0) ierror = py_var%setitem('low',nd_low)
        if (ierror == 0) ierror = ndarray_create(nd_high,for_var%high)
        if (ierror == 0) ierror = py_var%setitem('high',nd_high)
        if (ierror == 0) ierror = ndarray_create(nd_config,for_var%config)
        if (ierror == 0) ierror = py_var%setitem('config',nd_config)
        if (ierror == 0) ierror = ndarray_create(nd_sigma,for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('sigma',nd_sigma)
        if (ierror == 0) ierror = list_create(li_nampar)
        if (ierror == 0) then
            do i = 1 , size(for_var%nampar)
                if (ierror == 0) then
                    ierror = li_nampar%append(for_var%nampar(i))
                    if (ierror /= 0) then
                        for_var%nampar(i) = ''
                        ierror = li_nampar%append(for_var%nampar(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('nampar',li_nampar)
        if (ierror == 0) ierror = py_var%setitem('best_cost',for_var%best_cost)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_multistate_vector_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_multistate_vector_type

    Module Subroutine Unwrap_type_multistate_vector_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(multistate_vector_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
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
            err_cfml%msg  = 'Unwrap_multistate_vector_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'multistate_vector_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_multistate_vector_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','npar',py_var,for_var%npar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','nconf',py_var,for_var%nconf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','code',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','code',p_int_1d,for_var%code,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','bound',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','bound',p_int_1d,for_var%bound,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','state',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','state',p_real_2d,for_var%state,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','stp',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','stp',p_real_2d,for_var%stp,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','cost',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','cost',p_real_1d,for_var%cost,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','low',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','low',p_real_1d,for_var%low,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','high',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','high',p_real_1d,for_var%high,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','config',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','config',p_real_1d,for_var%config,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','sigma',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_multistate_vector_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','nampar',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%nampar,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_multistate_vector_type','best_cost',py_var,for_var%best_cost,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_multistate_vector_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_multistate_vector_type

    Module Subroutine list_to_type_array1d_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(multistate_vector_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_multistate_vector_type

    Module Subroutine list_to_type_array1d_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(multistate_vector_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_multistate_vector_type_no_alloc

    Module Subroutine list_to_type_array2d_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(multistate_vector_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_multistate_vector_type

    Module Subroutine list_to_type_array2d_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(multistate_vector_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_multistate_vector_type_no_alloc

    Module Subroutine Wrap_simann_conditions_type(for_var,py_var,ierror)

        ! Arguments
        type(simann_conditions_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','simann_conditions_type')
        if (ierror == 0) ierror = py_var%setitem('t_ini',for_var%t_ini)
        if (ierror == 0) ierror = py_var%setitem('anneal',for_var%anneal)
        if (ierror == 0) ierror = py_var%setitem('accept',for_var%accept)
        if (ierror == 0) ierror = py_var%setitem('threshold',for_var%threshold)
        if (ierror == 0) ierror = py_var%setitem('initconfig',for_var%initconfig)
        if (ierror == 0) ierror = py_var%setitem('nalgor',for_var%nalgor)
        if (ierror == 0) ierror = py_var%setitem('nm_cycl',for_var%nm_cycl)
        if (ierror == 0) ierror = py_var%setitem('num_temps',for_var%num_temps)
        if (ierror == 0) ierror = py_var%setitem('num_therm',for_var%num_therm)
        if (ierror == 0) ierror = py_var%setitem('num_conf',for_var%num_conf)
        if (ierror == 0) ierror = py_var%setitem('seed',for_var%seed)
        if (ierror == 0) then
            ierror = py_var%setitem('cost_function_name',for_var%cost_function_name)
            if (ierror /= 0) then
                for_var%cost_function_name = ''
                ierror = py_var%setitem('cost_function_name',for_var%cost_function_name)
            end if
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_simann_conditions_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_simann_conditions_type

    Module Subroutine Unwrap_type_simann_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(simann_conditions_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_simann_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'simann_conditions_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_simann_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','t_ini',py_var,for_var%t_ini,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','anneal',py_var,for_var%anneal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','accept',py_var,for_var%accept,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','threshold',py_var,for_var%threshold,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','initconfig',py_var,for_var%initconfig,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','nalgor',py_var,for_var%nalgor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','nm_cycl',py_var,for_var%nm_cycl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','num_temps',py_var,for_var%num_temps,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','num_therm',py_var,for_var%num_therm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','num_conf',py_var,for_var%num_conf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','seed',py_var,for_var%seed,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_simann_conditions_type','cost_function_name',py_var,for_var%cost_function_name,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_simann_conditions_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_simann_conditions_type

    Module Subroutine list_to_type_array1d_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(simann_conditions_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_simann_conditions_type

    Module Subroutine list_to_type_array1d_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(simann_conditions_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_simann_conditions_type_no_alloc

    Module Subroutine list_to_type_array2d_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(simann_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_simann_conditions_type

    Module Subroutine list_to_type_array2d_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(simann_conditions_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_simann_conditions_type_no_alloc

    Module Subroutine Wrap_state_vector_type(for_var,py_var,ierror)

        ! Arguments
        type(state_vector_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_nampar
        type(list) :: li_nampar
        type(ndarray) :: nd_code,nd_bound,nd_state,nd_stp,nd_low,nd_high,nd_config

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','state_vector_type')
        if (ierror == 0) ierror = py_var%setitem('npar',for_var%npar)
        if (ierror == 0) ierror = ndarray_create(nd_code,for_var%code)
        if (ierror == 0) ierror = py_var%setitem('code',nd_code)
        if (ierror == 0) ierror = ndarray_create(nd_bound,for_var%bound)
        if (ierror == 0) ierror = py_var%setitem('bound',nd_bound)
        if (ierror == 0) ierror = ndarray_create(nd_state,for_var%state)
        if (ierror == 0) ierror = py_var%setitem('state',nd_state)
        if (ierror == 0) ierror = ndarray_create(nd_stp,for_var%stp)
        if (ierror == 0) ierror = py_var%setitem('stp',nd_stp)
        if (ierror == 0) ierror = ndarray_create(nd_low,for_var%low)
        if (ierror == 0) ierror = py_var%setitem('low',nd_low)
        if (ierror == 0) ierror = ndarray_create(nd_high,for_var%high)
        if (ierror == 0) ierror = py_var%setitem('high',nd_high)
        if (ierror == 0) ierror = ndarray_create(nd_config,for_var%config)
        if (ierror == 0) ierror = py_var%setitem('config',nd_config)
        if (ierror == 0) ierror = py_var%setitem('cost',for_var%cost)
        if (ierror == 0) ierror = list_create(li_nampar)
        if (ierror == 0) then
            do i = 1 , size(for_var%nampar)
                if (ierror == 0) then
                    ierror = li_nampar%append(for_var%nampar(i))
                    if (ierror /= 0) then
                        for_var%nampar(i) = ''
                        ierror = li_nampar%append(for_var%nampar(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('nampar',li_nampar)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_state_vector_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_state_vector_type

    Module Subroutine Unwrap_type_state_vector_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(state_vector_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_state_vector_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'state_vector_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_state_vector_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','npar',py_var,for_var%npar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','code',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','code',p_int_1d,for_var%code,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','bound',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','bound',p_int_1d,for_var%bound,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','state',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','state',p_real_1d,for_var%state,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','stp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','stp',p_real_1d,for_var%stp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','low',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','low',p_real_1d,for_var%low,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','high',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','high',p_real_1d,for_var%high,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','config',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_state_vector_type','config',p_real_1d,for_var%config,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','cost',py_var,for_var%cost,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_state_vector_type','nampar',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%nampar,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_state_vector_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_state_vector_type

    Module Subroutine list_to_type_array1d_state_vector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(state_vector_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_state_vector_type

    Module Subroutine list_to_type_array1d_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(state_vector_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_state_vector_type_no_alloc

    Module Subroutine list_to_type_array2d_state_vector_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(state_vector_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_state_vector_type

    Module Subroutine list_to_type_array2d_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(state_vector_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_state_vector_type_no_alloc

end submodule