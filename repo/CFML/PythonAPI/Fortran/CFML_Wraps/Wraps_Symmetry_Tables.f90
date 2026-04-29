submodule (CFML_Wraps) Wraps_Symmetry_Tables

    implicit none
    contains

    Module Subroutine Wrap_shub_spgr_info_type(for_var,py_var,ierror)

        ! Arguments
        type(shub_spgr_info_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','shub_spgr_info_type')
        if (ierror == 0) then
            ierror = py_var%setitem('id_bns',for_var%id_bns)
            if (ierror /= 0) then
                for_var%id_bns = ''
                ierror = py_var%setitem('id_bns',for_var%id_bns)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('bns',for_var%bns)
            if (ierror /= 0) then
                for_var%bns = ''
                ierror = py_var%setitem('bns',for_var%bns)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('id_og',for_var%id_og)
            if (ierror /= 0) then
                for_var%id_og = ''
                ierror = py_var%setitem('id_og',for_var%id_og)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('og',for_var%og)
            if (ierror /= 0) then
                for_var%og = ''
                ierror = py_var%setitem('og',for_var%og)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('std',for_var%std)
            if (ierror /= 0) then
                for_var%std = ''
                ierror = py_var%setitem('std',for_var%std)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('mhall',for_var%mhall)
            if (ierror /= 0) then
                for_var%mhall = ''
                ierror = py_var%setitem('mhall',for_var%mhall)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('generators',for_var%generators)
            if (ierror /= 0) then
                for_var%generators = ''
                ierror = py_var%setitem('generators',for_var%generators)
            end if
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_shub_spgr_info_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_shub_spgr_info_type

    Module Subroutine Unwrap_type_shub_spgr_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(shub_spgr_info_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_shub_spgr_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'shub_spgr_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_shub_spgr_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','id_bns',py_var,for_var%id_bns,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','bns',py_var,for_var%bns,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','id_og',py_var,for_var%id_og,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','og',py_var,for_var%og,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','std',py_var,for_var%std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','mhall',py_var,for_var%mhall,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_shub_spgr_info_type','generators',py_var,for_var%generators,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_shub_spgr_info_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_shub_spgr_info_type

    Module Subroutine list_to_type_array1d_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(shub_spgr_info_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_shub_spgr_info_type

    Module Subroutine list_to_type_array1d_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(shub_spgr_info_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_shub_spgr_info_type_no_alloc

    Module Subroutine list_to_type_array2d_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(shub_spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_shub_spgr_info_type

    Module Subroutine list_to_type_array2d_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(shub_spgr_info_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_shub_spgr_info_type_no_alloc

    Module Subroutine Wrap_spgr_info_type(for_var,py_var,ierror)

        ! Arguments
        type(spgr_info_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_asu

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','spgr_info_type')
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (ierror == 0) then
            ierror = py_var%setitem('hm',for_var%hm)
            if (ierror /= 0) then
                for_var%hm = ''
                ierror = py_var%setitem('hm',for_var%hm)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('hall',for_var%hall)
            if (ierror /= 0) then
                for_var%hall = ''
                ierror = py_var%setitem('hall',for_var%hall)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('laue',for_var%laue)
        if (ierror == 0) ierror = py_var%setitem('pg',for_var%pg)
        if (ierror == 0) ierror = ndarray_create(nd_asu,for_var%asu)
        if (ierror == 0) ierror = py_var%setitem('asu',nd_asu)
        if (ierror == 0) then
            ierror = py_var%setitem('inf_extra',for_var%inf_extra)
            if (ierror /= 0) then
                for_var%inf_extra = ''
                ierror = py_var%setitem('inf_extra',for_var%inf_extra)
            end if
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_spgr_info_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_spgr_info_type

    Module Subroutine Unwrap_type_spgr_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(spgr_info_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_spgr_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'spgr_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_spgr_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','hm',py_var,for_var%hm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','asu',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_spgr_info_type','asu',p_int_1d,for_var%asu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spgr_info_type','inf_extra',py_var,for_var%inf_extra,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_spgr_info_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_spgr_info_type

    Module Subroutine list_to_type_array1d_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spgr_info_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_spgr_info_type

    Module Subroutine list_to_type_array1d_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spgr_info_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_spgr_info_type_no_alloc

    Module Subroutine list_to_type_array2d_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_spgr_info_type

    Module Subroutine list_to_type_array2d_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spgr_info_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_spgr_info_type_no_alloc

    Module Subroutine Wrap_table_equiv_type(for_var,py_var,ierror)

        ! Arguments
        type(table_equiv_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','table_equiv_type')
        if (ierror == 0) then
            ierror = py_var%setitem('sc',for_var%sc)
            if (ierror /= 0) then
                for_var%sc = ''
                ierror = py_var%setitem('sc',for_var%sc)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('ml',for_var%ml)
            if (ierror /= 0) then
                for_var%ml = ''
                ierror = py_var%setitem('ml',for_var%ml)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('ko',for_var%ko)
            if (ierror /= 0) then
                for_var%ko = ''
                ierror = py_var%setitem('ko',for_var%ko)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('bc',for_var%bc)
            if (ierror /= 0) then
                for_var%bc = ''
                ierror = py_var%setitem('bc',for_var%bc)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('za',for_var%za)
            if (ierror /= 0) then
                for_var%za = ''
                ierror = py_var%setitem('za',for_var%za)
            end if
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_table_equiv_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_table_equiv_type

    Module Subroutine Unwrap_type_table_equiv_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(table_equiv_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_table_equiv_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'table_equiv_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_table_equiv_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_table_equiv_type','sc',py_var,for_var%sc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_table_equiv_type','ml',py_var,for_var%ml,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_table_equiv_type','ko',py_var,for_var%ko,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_table_equiv_type','bc',py_var,for_var%bc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_table_equiv_type','za',py_var,for_var%za,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_table_equiv_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_table_equiv_type

    Module Subroutine list_to_type_array1d_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(table_equiv_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_table_equiv_type

    Module Subroutine list_to_type_array1d_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(table_equiv_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_table_equiv_type_no_alloc

    Module Subroutine list_to_type_array2d_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(table_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_table_equiv_type

    Module Subroutine list_to_type_array2d_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(table_equiv_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_table_equiv_type_no_alloc

    Module Subroutine Wrap_wyck_info_type(for_var,py_var,ierror)

        ! Arguments
        type(wyck_info_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_corbit
        type(list) :: li_corbit

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','wyck_info_type')
        if (ierror == 0) then
            ierror = py_var%setitem('hm',for_var%hm)
            if (ierror /= 0) then
                for_var%hm = ''
                ierror = py_var%setitem('hm',for_var%hm)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('norbit',for_var%norbit)
        if (ierror == 0) ierror = list_create(li_corbit)
        if (ierror == 0) then
            do i = 1 , size(for_var%corbit)
                if (ierror == 0) then
                    ierror = li_corbit%append(for_var%corbit(i))
                    if (ierror /= 0) then
                        for_var%corbit(i) = ''
                        ierror = li_corbit%append(for_var%corbit(i))
                    end if
                end if
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('corbit',li_corbit)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_wyck_info_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_wyck_info_type

    Module Subroutine Unwrap_type_wyck_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(wyck_info_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_wyck_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'wyck_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_wyck_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_wyck_info_type','hm',py_var,for_var%hm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_wyck_info_type','norbit',py_var,for_var%norbit,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_wyck_info_type','corbit',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%corbit,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_wyck_info_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_wyck_info_type

    Module Subroutine list_to_type_array1d_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(wyck_info_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_wyck_info_type

    Module Subroutine list_to_type_array1d_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(wyck_info_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_wyck_info_type_no_alloc

    Module Subroutine list_to_type_array2d_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(wyck_info_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_wyck_info_type

    Module Subroutine list_to_type_array2d_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(wyck_info_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_wyck_info_type_no_alloc

end submodule