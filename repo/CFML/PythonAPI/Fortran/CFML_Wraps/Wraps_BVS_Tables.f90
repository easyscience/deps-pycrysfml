submodule (CFML_Wraps) Wraps_BVS_Tables

    implicit none
    contains

    Module Subroutine Wrap_atomic_properties_type(for_var,py_var,ierror)

        ! Arguments
        type(atomic_properties_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atomic_properties_type')
        if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
        if (ierror == 0) then
            ierror = py_var%setitem('symb',for_var%symb)
            if (ierror /= 0) then
                for_var%symb = ''
                ierror = py_var%setitem('symb',for_var%symb)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('oxs',for_var%oxs)
        if (ierror == 0) ierror = py_var%setitem('dox',for_var%dox)
        if (ierror == 0) ierror = py_var%setitem('mass',for_var%mass)
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (ierror == 0) ierror = py_var%setitem('g',for_var%g)
        if (ierror == 0) ierror = py_var%setitem('b',for_var%b)
        if (ierror == 0) ierror = py_var%setitem('rc',for_var%rc)
        if (ierror == 0) ierror = py_var%setitem('sigma',for_var%sigma)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atomic_properties_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atomic_properties_type

    Module Subroutine Unwrap_type_atomic_properties_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atomic_properties_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atomic_properties_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atomic_properties_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atomic_properties_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','oxs',py_var,for_var%oxs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','dox',py_var,for_var%dox,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','mass',py_var,for_var%mass,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','g',py_var,for_var%g,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','b',py_var,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','rc',py_var,for_var%rc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atomic_properties_type','sigma',py_var,for_var%sigma,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atomic_properties_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_atomic_properties_type

    Module Subroutine list_to_type_array1d_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atomic_properties_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_atomic_properties_type

    Module Subroutine list_to_type_array1d_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atomic_properties_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_atomic_properties_type_no_alloc

    Module Subroutine list_to_type_array2d_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atomic_properties_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_atomic_properties_type

    Module Subroutine list_to_type_array2d_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atomic_properties_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_atomic_properties_type_no_alloc

    Module Subroutine Wrap_bvel_par_type(for_var,py_var,ierror)

        ! Arguments
        type(bvel_par_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_avcoor,nd_rzero,nd_rcutoff,nd_dzero,nd_rmin,nd_alpha,nd_refnum

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','bvel_par_type')
        if (ierror == 0) then
            ierror = py_var%setitem('symb',for_var%symb)
            if (ierror /= 0) then
                for_var%symb = ''
                ierror = py_var%setitem('symb',for_var%symb)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_avcoor,for_var%avcoor)
        if (ierror == 0) ierror = py_var%setitem('avcoor',nd_avcoor)
        if (ierror == 0) ierror = ndarray_create(nd_rzero,for_var%rzero)
        if (ierror == 0) ierror = py_var%setitem('rzero',nd_rzero)
        if (ierror == 0) ierror = ndarray_create(nd_rcutoff,for_var%rcutoff)
        if (ierror == 0) ierror = py_var%setitem('rcutoff',nd_rcutoff)
        if (ierror == 0) ierror = ndarray_create(nd_dzero,for_var%dzero)
        if (ierror == 0) ierror = py_var%setitem('dzero',nd_dzero)
        if (ierror == 0) ierror = ndarray_create(nd_rmin,for_var%rmin)
        if (ierror == 0) ierror = py_var%setitem('rmin',nd_rmin)
        if (ierror == 0) ierror = ndarray_create(nd_alpha,for_var%alpha)
        if (ierror == 0) ierror = py_var%setitem('alpha',nd_alpha)
        if (ierror == 0) ierror = ndarray_create(nd_refnum,for_var%refnum)
        if (ierror == 0) ierror = py_var%setitem('refnum',nd_refnum)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_bvel_par_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_bvel_par_type

    Module Subroutine Unwrap_type_bvel_par_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(bvel_par_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_bvel_par_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'bvel_par_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_bvel_par_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','avcoor',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','avcoor',p_real_1d,for_var%avcoor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','rzero',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','rzero',p_real_1d,for_var%rzero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','rcutoff',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','rcutoff',p_real_1d,for_var%rcutoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','dzero',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','dzero',p_real_1d,for_var%dzero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','rmin',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','rmin',p_real_1d,for_var%rmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','alpha',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','alpha',p_real_1d,for_var%alpha,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvel_par_type','refnum',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvel_par_type','refnum',p_int_1d,for_var%refnum,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_bvel_par_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_bvel_par_type

    Module Subroutine list_to_type_array1d_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvel_par_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_bvel_par_type

    Module Subroutine list_to_type_array1d_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvel_par_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_bvel_par_type_no_alloc

    Module Subroutine list_to_type_array2d_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvel_par_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_bvel_par_type

    Module Subroutine list_to_type_array2d_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvel_par_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_bvel_par_type_no_alloc

    Module Subroutine Wrap_bvs_par_type(for_var,py_var,ierror)

        ! Arguments
        type(bvs_par_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_d0,nd_b_par,nd_refnum

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','bvs_par_type')
        if (ierror == 0) then
            ierror = py_var%setitem('symb',for_var%symb)
            if (ierror /= 0) then
                for_var%symb = ''
                ierror = py_var%setitem('symb',for_var%symb)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_d0,for_var%d0)
        if (ierror == 0) ierror = py_var%setitem('d0',nd_d0)
        if (ierror == 0) ierror = ndarray_create(nd_b_par,for_var%b_par)
        if (ierror == 0) ierror = py_var%setitem('b_par',nd_b_par)
        if (ierror == 0) ierror = ndarray_create(nd_refnum,for_var%refnum)
        if (ierror == 0) ierror = py_var%setitem('refnum',nd_refnum)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_bvs_par_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_bvs_par_type

    Module Subroutine Unwrap_type_bvs_par_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(bvs_par_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_bvs_par_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'bvs_par_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_bvs_par_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvs_par_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvs_par_type','d0',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvs_par_type','d0',p_real_1d,for_var%d0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvs_par_type','b_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvs_par_type','b_par',p_real_1d,for_var%b_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bvs_par_type','refnum',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_bvs_par_type','refnum',p_int_1d,for_var%refnum,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_bvs_par_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_bvs_par_type

    Module Subroutine list_to_type_array1d_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvs_par_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_bvs_par_type

    Module Subroutine list_to_type_array1d_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvs_par_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_bvs_par_type_no_alloc

    Module Subroutine list_to_type_array2d_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_bvs_par_type

    Module Subroutine list_to_type_array2d_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bvs_par_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_bvs_par_type_no_alloc

    Module Subroutine Wrap_sbvs_par_type(for_var,py_var,ierror)

        ! Arguments
        type(sbvs_par_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_d0,nd_b_par,nd_cn,nd_ctoff,nd_refnum

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sbvs_par_type')
        if (ierror == 0) then
            ierror = py_var%setitem('symb',for_var%symb)
            if (ierror /= 0) then
                for_var%symb = ''
                ierror = py_var%setitem('symb',for_var%symb)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_d0,for_var%d0)
        if (ierror == 0) ierror = py_var%setitem('d0',nd_d0)
        if (ierror == 0) ierror = ndarray_create(nd_b_par,for_var%b_par)
        if (ierror == 0) ierror = py_var%setitem('b_par',nd_b_par)
        if (ierror == 0) ierror = ndarray_create(nd_cn,for_var%cn)
        if (ierror == 0) ierror = py_var%setitem('cn',nd_cn)
        if (ierror == 0) ierror = ndarray_create(nd_ctoff,for_var%ctoff)
        if (ierror == 0) ierror = py_var%setitem('ctoff',nd_ctoff)
        if (ierror == 0) ierror = ndarray_create(nd_refnum,for_var%refnum)
        if (ierror == 0) ierror = py_var%setitem('refnum',nd_refnum)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sbvs_par_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sbvs_par_type

    Module Subroutine Unwrap_type_sbvs_par_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sbvs_par_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_sbvs_par_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sbvs_par_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sbvs_par_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','d0',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sbvs_par_type','d0',p_real_1d,for_var%d0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','b_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sbvs_par_type','b_par',p_real_1d,for_var%b_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','cn',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sbvs_par_type','cn',p_real_1d,for_var%cn,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','ctoff',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sbvs_par_type','ctoff',p_real_1d,for_var%ctoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sbvs_par_type','refnum',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sbvs_par_type','refnum',p_int_1d,for_var%refnum,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sbvs_par_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sbvs_par_type

    Module Subroutine list_to_type_array1d_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sbvs_par_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sbvs_par_type

    Module Subroutine list_to_type_array1d_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sbvs_par_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sbvs_par_type_no_alloc

    Module Subroutine list_to_type_array2d_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sbvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sbvs_par_type

    Module Subroutine list_to_type_array2d_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sbvs_par_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sbvs_par_type_no_alloc

end submodule