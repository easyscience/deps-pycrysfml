submodule (CFML_Wraps) Wraps_Metrics

    implicit none
    contains

    Module Subroutine Wrap_cell_type(for_var,py_var,ierror)

        ! Arguments
        class(cell_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_cell,nd_scell,nd_ang,nd_sang,nd_rcell,nd_rang,nd_gd,nd_gr,nd_cr_orth_cel,nd_orth_cr_cel,nd_bl_m,nd_inv_bl_m,nd_lcell,nd_lang

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_type')
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = py_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_scell,for_var%scell)
        if (ierror == 0) ierror = py_var%setitem('scell',nd_scell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = py_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = ndarray_create(nd_sang,for_var%sang)
        if (ierror == 0) ierror = py_var%setitem('sang',nd_sang)
        if (ierror == 0) ierror = py_var%setitem('vol',for_var%vol)
        if (ierror == 0) ierror = py_var%setitem('svol',for_var%svol)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_g_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_g_type')
                    if (ierror == 0) ierror = ndarray_create(nd_rcell,A%rcell)
                    if (ierror == 0) ierror = py_var%setitem('rcell',nd_rcell)
                    if (ierror == 0) ierror = ndarray_create(nd_rang,A%rang)
                    if (ierror == 0) ierror = py_var%setitem('rang',nd_rang)
                    if (ierror == 0) ierror = py_var%setitem('rvol',A%rvol)
                    if (ierror == 0) ierror = ndarray_create(nd_gd,A%gd)
                    if (ierror == 0) ierror = py_var%setitem('gd',nd_gd)
                    if (ierror == 0) ierror = ndarray_create(nd_gr,A%gr)
                    if (ierror == 0) ierror = py_var%setitem('gr',nd_gr)
                    if (ierror == 0) ierror = ndarray_create(nd_cr_orth_cel,A%cr_orth_cel)
                    if (ierror == 0) ierror = py_var%setitem('cr_orth_cel',nd_cr_orth_cel)
                    if (ierror == 0) ierror = ndarray_create(nd_orth_cr_cel,A%orth_cr_cel)
                    if (ierror == 0) ierror = py_var%setitem('orth_cr_cel',nd_orth_cr_cel)
                    if (ierror == 0) ierror = ndarray_create(nd_bl_m,A%bl_m)
                    if (ierror == 0) ierror = py_var%setitem('bl_m',nd_bl_m)
                    if (ierror == 0) ierror = ndarray_create(nd_inv_bl_m,A%inv_bl_m)
                    if (ierror == 0) ierror = py_var%setitem('inv_bl_m',nd_inv_bl_m)
                    if (ierror == 0) then
                        ierror = py_var%setitem('carttype',A%carttype)
                        if (ierror /= 0) then
                            A%carttype = ''
                            ierror = py_var%setitem('carttype',A%carttype)
                        end if
                    end if
                class is (cell_ls_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_ls_type')
                    if (ierror == 0) ierror = ndarray_create(nd_lcell,A%lcell)
                    if (ierror == 0) ierror = py_var%setitem('lcell',nd_lcell)
                    if (ierror == 0) ierror = ndarray_create(nd_lang,A%lang)
                    if (ierror == 0) ierror = py_var%setitem('lang',nd_lang)
            end select
            select type (A => for_var)
                class is (cell_gls_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_gls_type')
                    if (ierror == 0) ierror = ndarray_create(nd_lcell,A%lcell)
                    if (ierror == 0) ierror = py_var%setitem('lcell',nd_lcell)
                    if (ierror == 0) ierror = ndarray_create(nd_lang,A%lang)
                    if (ierror == 0) ierror = py_var%setitem('lang',nd_lang)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_cell_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_cell_type

    Module Subroutine Unwrap_class_cell_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_type' &
                .and. fortran_type /= 'cell_g_type' &
                .and. fortran_type /= 'cell_ls_type' &
                .and. fortran_type /= 'cell_gls_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,A%rcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,A%rang,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,A%rvol,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,A%gd,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,A%gr,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,A%cr_orth_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,A%orth_cr_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,A%bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,A%inv_bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,A%carttype,ierror)
                class is (cell_ls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lang',p_int_1d,A%lang,ierror)
            end select
            select type (A => for_var)
                class is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_type_no_alloc

    Module Subroutine List_to_class_array1d_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_cell_type: Cannot determine fortran type'
            else if (fortran_type == 'cell_type') then
                allocate(cell_type :: arr(n))
            else if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: arr(n))
            else if (fortran_type == 'cell_ls_type') then
                allocate(cell_ls_type :: arr(n))
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_cell_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_cell_type

    Module Subroutine List_to_class_array2d_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_cell_type: Cannot determine fortran type'
                    else if (fortran_type == 'cell_type') then
                        allocate(cell_type :: arr(n,m))
                    else if (fortran_type == 'cell_g_type') then
                        allocate(cell_g_type :: arr(n,m))
                    else if (fortran_type == 'cell_ls_type') then
                        allocate(cell_ls_type :: arr(n,m))
                    else if (fortran_type == 'cell_gls_type') then
                        allocate(cell_gls_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_cell_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_cell_type

    Module Subroutine Unwrap_class_cell_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_type: Cannot determine fortran type'
        else
            if (fortran_type == 'cell_type') then
                allocate(cell_type :: for_var)
            else if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: for_var)
            else if (fortran_type == 'cell_ls_type') then
                allocate(cell_ls_type :: for_var)
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,A%rcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,A%rang,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,A%rvol,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,A%gd,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,A%gr,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,A%cr_orth_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,A%orth_cr_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,A%bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,A%inv_bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,A%carttype,ierror)
                class is (cell_ls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lang',p_int_1d,A%lang,ierror)
            end select
            select type (A => for_var)
                class is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_type

    Module Subroutine list_to_type_array1d_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_type

    Module Subroutine list_to_type_array1d_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_type_no_alloc

    Module Subroutine list_to_type_array2d_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_type

    Module Subroutine list_to_type_array2d_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_type_no_alloc

    Module Subroutine Unwrap_class_cell_g_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_g_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_g_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_g_type' &
                .and. fortran_type /= 'cell_gls_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_g_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,for_var%rcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,for_var%rang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,for_var%rvol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,for_var%gd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,for_var%gr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,for_var%cr_orth_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,for_var%orth_cr_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,for_var%bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,for_var%inv_bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,for_var%carttype,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_g_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_g_type_no_alloc

    Module Subroutine List_to_class_array1d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_g_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_cell_g_type: Cannot determine fortran type'
            else if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: arr(n))
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_cell_g_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_cell_g_type

    Module Subroutine List_to_class_array2d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_g_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_cell_g_type: Cannot determine fortran type'
                    else if (fortran_type == 'cell_g_type') then
                        allocate(cell_g_type :: arr(n,m))
                    else if (fortran_type == 'cell_gls_type') then
                        allocate(cell_gls_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_cell_g_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_cell_g_type

    Module Subroutine Unwrap_class_cell_g_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_g_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_g_type: Cannot determine fortran type'
        else
            if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: for_var)
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_g_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,for_var%rcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,for_var%rang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,for_var%rvol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,for_var%gd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,for_var%gr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,for_var%cr_orth_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,for_var%orth_cr_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,for_var%bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,for_var%inv_bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,for_var%carttype,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_g_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_g_type

    Module Subroutine list_to_type_array1d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_g_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_g_type

    Module Subroutine list_to_type_array1d_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_g_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_g_type_no_alloc

    Module Subroutine list_to_type_array2d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_g_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_g_type

    Module Subroutine list_to_type_array2d_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_g_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_g_type_no_alloc

    Module Subroutine Unwrap_class_cell_ls_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_ls_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_ls_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_ls_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_ls_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lcell',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lcell',p_int_1d,for_var%lcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lang',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lang',p_int_1d,for_var%lang,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_ls_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_ls_type_no_alloc

    Module Subroutine List_to_class_array1d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_ls_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_cell_ls_type: Cannot determine fortran type'
            else if (fortran_type == 'cell_ls_type') then
                allocate(cell_ls_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_cell_ls_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_cell_ls_type

    Module Subroutine List_to_class_array2d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_ls_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_cell_ls_type: Cannot determine fortran type'
                    else if (fortran_type == 'cell_ls_type') then
                        allocate(cell_ls_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_cell_ls_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_cell_ls_type

    Module Subroutine Unwrap_class_cell_ls_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_ls_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_ls_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_ls_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_ls_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lcell',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lcell',p_int_1d,for_var%lcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_ls_type','lang',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_ls_type','lang',p_int_1d,for_var%lang,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_ls_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_ls_type

    Module Subroutine list_to_type_array1d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_ls_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_ls_type

    Module Subroutine list_to_type_array1d_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_ls_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_ls_type_no_alloc

    Module Subroutine list_to_type_array2d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_ls_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_ls_type

    Module Subroutine list_to_type_array2d_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_ls_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_ls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_ls_type_no_alloc

    Module Subroutine Unwrap_class_cell_gls_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_gls_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_gls_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_gls_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_gls_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,for_var%rcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,for_var%rang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,for_var%rvol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,for_var%gd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,for_var%gr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,for_var%cr_orth_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,for_var%orth_cr_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,for_var%bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,for_var%inv_bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,for_var%carttype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,for_var%lcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,for_var%lang,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_gls_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_gls_type_no_alloc

    Module Subroutine List_to_class_array1d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_gls_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_cell_gls_type: Cannot determine fortran type'
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_cell_gls_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_cell_gls_type

    Module Subroutine List_to_class_array2d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(cell_gls_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_cell_gls_type: Cannot determine fortran type'
                    else if (fortran_type == 'cell_gls_type') then
                        allocate(cell_gls_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_cell_gls_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_cell_gls_type

    Module Subroutine Unwrap_class_cell_gls_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(cell_gls_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_cell_gls_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'cell_gls_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cell_gls_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rcell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rcell',p_real_1d,for_var%rcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','rang',p_real_1d,for_var%rang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','rvol',py_var,for_var%rvol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gd',p_real_2d,for_var%gd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','gr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','gr',p_real_2d,for_var%gr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','cr_orth_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','cr_orth_cel',p_real_2d,for_var%cr_orth_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','orth_cr_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','orth_cr_cel',p_real_2d,for_var%orth_cr_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','bl_m',p_real_2d,for_var%bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','inv_bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_g_type','inv_bl_m',p_real_2d,for_var%inv_bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_g_type','carttype',py_var,for_var%carttype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lcell',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lcell',p_int_1d,for_var%lcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cell_gls_type','lang',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cell_gls_type','lang',p_int_1d,for_var%lang,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_cell_gls_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_cell_gls_type

    Module Subroutine list_to_type_array1d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_gls_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_gls_type

    Module Subroutine list_to_type_array1d_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_gls_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_cell_gls_type_no_alloc

    Module Subroutine list_to_type_array2d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_gls_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_gls_type

    Module Subroutine list_to_type_array2d_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(cell_gls_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_cell_gls_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_cell_gls_type_no_alloc

    Module Subroutine Wrap_twofold_axes_type(for_var,py_var,ierror)

        ! Arguments
        type(twofold_axes_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_caxes,nd_dtwofold,nd_rtwofold,nd_dot,nd_cross,nd_maxes,nd_a,nd_b,nd_c

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','twofold_axes_type')
        if (ierror == 0) ierror = py_var%setitem('ntwo',for_var%ntwo)
        if (ierror == 0) ierror = py_var%setitem('tol',for_var%tol)
        if (ierror == 0) ierror = ndarray_create(nd_caxes,for_var%caxes)
        if (ierror == 0) ierror = py_var%setitem('caxes',nd_caxes)
        if (ierror == 0) ierror = ndarray_create(nd_dtwofold,for_var%dtwofold)
        if (ierror == 0) ierror = py_var%setitem('dtwofold',nd_dtwofold)
        if (ierror == 0) ierror = ndarray_create(nd_rtwofold,for_var%rtwofold)
        if (ierror == 0) ierror = py_var%setitem('rtwofold',nd_rtwofold)
        if (ierror == 0) ierror = ndarray_create(nd_dot,for_var%dot)
        if (ierror == 0) ierror = py_var%setitem('dot',nd_dot)
        if (ierror == 0) ierror = ndarray_create(nd_cross,for_var%cross)
        if (ierror == 0) ierror = py_var%setitem('cross',nd_cross)
        if (ierror == 0) ierror = ndarray_create(nd_maxes,for_var%maxes)
        if (ierror == 0) ierror = py_var%setitem('maxes',nd_maxes)
        if (ierror == 0) ierror = ndarray_create(nd_a,for_var%a)
        if (ierror == 0) ierror = py_var%setitem('a',nd_a)
        if (ierror == 0) ierror = ndarray_create(nd_b,for_var%b)
        if (ierror == 0) ierror = py_var%setitem('b',nd_b)
        if (ierror == 0) ierror = ndarray_create(nd_c,for_var%c)
        if (ierror == 0) ierror = py_var%setitem('c',nd_c)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_twofold_axes_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_twofold_axes_type

    Module Subroutine Unwrap_type_twofold_axes_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(twofold_axes_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_twofold_axes_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'twofold_axes_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_twofold_axes_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','ntwo',py_var,for_var%ntwo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','tol',py_var,for_var%tol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','caxes',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','caxes',p_real_2d,for_var%caxes,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','dtwofold',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','dtwofold',p_int_2d,for_var%dtwofold,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','rtwofold',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','rtwofold',p_int_2d,for_var%rtwofold,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','dot',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','dot',p_int_1d,for_var%dot,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','cross',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','cross',p_real_1d,for_var%cross,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','maxes',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','maxes',p_real_1d,for_var%maxes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twofold_axes_type','c',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twofold_axes_type','c',p_real_1d,for_var%c,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_twofold_axes_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_twofold_axes_type

    Module Subroutine list_to_type_array1d_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twofold_axes_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_twofold_axes_type

    Module Subroutine list_to_type_array1d_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twofold_axes_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_twofold_axes_type_no_alloc

    Module Subroutine list_to_type_array2d_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twofold_axes_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_twofold_axes_type

    Module Subroutine list_to_type_array2d_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twofold_axes_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_twofold_axes_type_no_alloc

    Module Subroutine Wrap_zone_axis_type(for_var,py_var,ierror)

        ! Arguments
        type(zone_axis_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_uvw,nd_rx,nd_ry

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','zone_axis_type')
        if (ierror == 0) ierror = py_var%setitem('nlayer',for_var%nlayer)
        if (ierror == 0) ierror = ndarray_create(nd_uvw,for_var%uvw)
        if (ierror == 0) ierror = py_var%setitem('uvw',nd_uvw)
        if (ierror == 0) ierror = ndarray_create(nd_rx,for_var%rx)
        if (ierror == 0) ierror = py_var%setitem('rx',nd_rx)
        if (ierror == 0) ierror = ndarray_create(nd_ry,for_var%ry)
        if (ierror == 0) ierror = py_var%setitem('ry',nd_ry)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_zone_axis_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_zone_axis_type

    Module Subroutine Unwrap_type_zone_axis_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(zone_axis_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_zone_axis_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'zone_axis_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_zone_axis_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_zone_axis_type','nlayer',py_var,for_var%nlayer,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_zone_axis_type','uvw',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_zone_axis_type','uvw',p_int_1d,for_var%uvw,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_zone_axis_type','rx',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_zone_axis_type','rx',p_int_1d,for_var%rx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_zone_axis_type','ry',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_zone_axis_type','ry',p_int_1d,for_var%ry,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_zone_axis_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_zone_axis_type

    Module Subroutine list_to_type_array1d_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(zone_axis_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_zone_axis_type

    Module Subroutine list_to_type_array1d_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(zone_axis_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_zone_axis_type_no_alloc

    Module Subroutine list_to_type_array2d_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(zone_axis_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_zone_axis_type

    Module Subroutine list_to_type_array2d_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(zone_axis_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_zone_axis_type_no_alloc

    Module Subroutine Wrap_strain_tensor_type(for_var,py_var,ierror)

        ! Arguments
        type(strain_tensor_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_cell0,di_cell1
        type(ndarray) :: nd_pt,nd_e,nd_esd,nd_eval,nd_evalesd,nd_evec,nd_cart_ang,nd_cell_ang,nd_dir_close,nd_ep,nd_esdp,nd_evalp,nd_evalpesd

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','strain_tensor_type')
        if (ierror == 0) ierror = py_var%setitem('iref',for_var%iref)
        if (ierror == 0) ierror = py_var%setitem('icell',for_var%icell)
        if (ierror == 0) ierror = py_var%setitem('istype',for_var%istype)
        if (ierror == 0) ierror = dict_create(di_cell0)
        if (ierror == 0) call wrap_type(for_var%cell0,di_cell0,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell0',di_cell0)
        if (ierror == 0) ierror = dict_create(di_cell1)
        if (ierror == 0) call wrap_type(for_var%cell1,di_cell1,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell1',di_cell1)
        if (ierror == 0) then
            ierror = py_var%setitem('cartype',for_var%cartype)
            if (ierror /= 0) then
                for_var%cartype = ''
                ierror = py_var%setitem('cartype',for_var%cartype)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('system',for_var%system)
            if (ierror /= 0) then
                for_var%system = ''
                ierror = py_var%setitem('system',for_var%system)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_pt,for_var%pt)
        if (ierror == 0) ierror = py_var%setitem('pt',nd_pt)
        if (ierror == 0) ierror = ndarray_create(nd_e,for_var%e)
        if (ierror == 0) ierror = py_var%setitem('e',nd_e)
        if (ierror == 0) ierror = ndarray_create(nd_esd,for_var%esd)
        if (ierror == 0) ierror = py_var%setitem('esd',nd_esd)
        if (ierror == 0) ierror = ndarray_create(nd_eval,for_var%eval)
        if (ierror == 0) ierror = py_var%setitem('eval',nd_eval)
        if (ierror == 0) ierror = ndarray_create(nd_evalesd,for_var%evalesd)
        if (ierror == 0) ierror = py_var%setitem('evalesd',nd_evalesd)
        if (ierror == 0) ierror = ndarray_create(nd_evec,for_var%evec)
        if (ierror == 0) ierror = py_var%setitem('evec',nd_evec)
        if (ierror == 0) ierror = ndarray_create(nd_cart_ang,for_var%cart_ang)
        if (ierror == 0) ierror = py_var%setitem('cart_ang',nd_cart_ang)
        if (ierror == 0) ierror = ndarray_create(nd_cell_ang,for_var%cell_ang)
        if (ierror == 0) ierror = py_var%setitem('cell_ang',nd_cell_ang)
        if (ierror == 0) ierror = ndarray_create(nd_dir_close,for_var%dir_close)
        if (ierror == 0) ierror = py_var%setitem('dir_close',nd_dir_close)
        if (ierror == 0) ierror = ndarray_create(nd_ep,for_var%ep)
        if (ierror == 0) ierror = py_var%setitem('ep',nd_ep)
        if (ierror == 0) ierror = ndarray_create(nd_esdp,for_var%esdp)
        if (ierror == 0) ierror = py_var%setitem('esdp',nd_esdp)
        if (ierror == 0) ierror = ndarray_create(nd_evalp,for_var%evalp)
        if (ierror == 0) ierror = py_var%setitem('evalp',nd_evalp)
        if (ierror == 0) ierror = ndarray_create(nd_evalpesd,for_var%evalpesd)
        if (ierror == 0) ierror = py_var%setitem('evalpesd',nd_evalpesd)
        if (ierror == 0) then
            ierror = py_var%setitem('property',for_var%property)
            if (ierror /= 0) then
                for_var%property = ''
                ierror = py_var%setitem('property',for_var%property)
            end if
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_strain_tensor_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_strain_tensor_type

    Module Subroutine Unwrap_type_strain_tensor_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(strain_tensor_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(dict) :: dict_cell0,dict_cell1

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_strain_tensor_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'strain_tensor_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_strain_tensor_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','iref',py_var,for_var%iref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','icell',py_var,for_var%icell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','istype',py_var,for_var%istype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','cell0',py_var,dict_cell0,ierror)
        if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(dict_cell0,for_var%cell0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','cell1',py_var,dict_cell1,ierror)
        if (ierror == 0) call unwrap_class_cell_g_type_no_alloc(dict_cell1,for_var%cell1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','cartype',py_var,for_var%cartype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','system',py_var,for_var%system,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','pt',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','pt',p_real_3d,for_var%pt,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','e',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','e',p_real_2d,for_var%e,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','esd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','esd',p_real_2d,for_var%esd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','eval',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','eval',p_real_1d,for_var%eval,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','evalesd',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','evalesd',p_real_1d,for_var%evalesd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','evec',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','evec',p_real_2d,for_var%evec,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','cart_ang',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','cart_ang',p_real_3d,for_var%cart_ang,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','cell_ang',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','cell_ang',p_real_3d,for_var%cell_ang,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','dir_close',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','dir_close',p_real_3d,for_var%dir_close,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','ep',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','ep',p_real_2d,for_var%ep,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','esdp',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','esdp',p_real_2d,for_var%esdp,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','evalp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','evalp',p_real_1d,for_var%evalp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','evalpesd',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strain_tensor_type','evalpesd',p_real_1d,for_var%evalpesd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strain_tensor_type','property',py_var,for_var%property,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_strain_tensor_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_strain_tensor_type

    Module Subroutine list_to_type_array1d_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strain_tensor_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_strain_tensor_type

    Module Subroutine list_to_type_array1d_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strain_tensor_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_strain_tensor_type_no_alloc

    Module Subroutine list_to_type_array2d_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strain_tensor_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_strain_tensor_type

    Module Subroutine list_to_type_array2d_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strain_tensor_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_strain_tensor_type_no_alloc

end submodule