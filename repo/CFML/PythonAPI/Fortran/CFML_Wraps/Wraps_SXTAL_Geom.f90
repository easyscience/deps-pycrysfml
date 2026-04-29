submodule (CFML_Wraps) Wraps_SXTAL_Geom

    implicit none
    contains

    Module Subroutine Wrap_psd_val_type(for_var,py_var,ierror)

        ! Arguments
        type(psd_val_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','psd_val_type')
        if (ierror == 0) then
            ierror = py_var%setitem('name_inst',for_var%name_inst)
            if (ierror /= 0) then
                for_var%name_inst = ''
                ierror = py_var%setitem('name_inst',for_var%name_inst)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('xoff',for_var%xoff)
        if (ierror == 0) ierror = py_var%setitem('zoff',for_var%zoff)
        if (ierror == 0) ierror = py_var%setitem('radius',for_var%radius)
        if (ierror == 0) ierror = py_var%setitem('yoff',for_var%yoff)
        if (ierror == 0) ierror = py_var%setitem('cgap',for_var%cgap)
        if (ierror == 0) ierror = py_var%setitem('agap',for_var%agap)
        if (ierror == 0) ierror = py_var%setitem('ncat',for_var%ncat)
        if (ierror == 0) ierror = py_var%setitem('nano',for_var%nano)
        if (ierror == 0) ierror = py_var%setitem('ipsd',for_var%ipsd)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_psd_val_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_psd_val_type

    Module Subroutine Unwrap_type_psd_val_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(psd_val_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_psd_val_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'psd_val_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_psd_val_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','name_inst',py_var,for_var%name_inst,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','xoff',py_var,for_var%xoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','zoff',py_var,for_var%zoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','radius',py_var,for_var%radius,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','yoff',py_var,for_var%yoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','cgap',py_var,for_var%cgap,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','agap',py_var,for_var%agap,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','ncat',py_var,for_var%ncat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','nano',py_var,for_var%nano,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_psd_val_type','ipsd',py_var,for_var%ipsd,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_psd_val_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_psd_val_type

    Module Subroutine list_to_type_array1d_psd_val_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(psd_val_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_psd_val_type

    Module Subroutine list_to_type_array1d_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(psd_val_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_psd_val_type_no_alloc

    Module Subroutine list_to_type_array2d_psd_val_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(psd_val_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_psd_val_type

    Module Subroutine list_to_type_array2d_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(psd_val_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_psd_val_type_no_alloc

    Module Subroutine Wrap_sxd_val_type(for_var,py_var,ierror)

        ! Arguments
        type(sxd_val_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','sxd_val_type')
        if (ierror == 0) ierror = py_var%setitem('distms',for_var%distms)
        if (ierror == 0) ierror = py_var%setitem('distsd',for_var%distsd)
        if (ierror == 0) ierror = py_var%setitem('dimx',for_var%dimx)
        if (ierror == 0) ierror = py_var%setitem('dimz',for_var%dimz)
        if (ierror == 0) ierror = py_var%setitem('xoff',for_var%xoff)
        if (ierror == 0) ierror = py_var%setitem('yoff',for_var%yoff)
        if (ierror == 0) ierror = py_var%setitem('zoff',for_var%zoff)
        if (ierror == 0) ierror = py_var%setitem('toff',for_var%toff)
        if (ierror == 0) ierror = py_var%setitem('velcon',for_var%velcon)
        if (ierror == 0) ierror = py_var%setitem('nxcel',for_var%nxcel)
        if (ierror == 0) ierror = py_var%setitem('nzcel',for_var%nzcel)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_sxd_val_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_sxd_val_type

    Module Subroutine Unwrap_type_sxd_val_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sxd_val_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_sxd_val_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'sxd_val_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sxd_val_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','distms',py_var,for_var%distms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','distsd',py_var,for_var%distsd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','dimx',py_var,for_var%dimx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','dimz',py_var,for_var%dimz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','xoff',py_var,for_var%xoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','yoff',py_var,for_var%yoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','zoff',py_var,for_var%zoff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','toff',py_var,for_var%toff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','velcon',py_var,for_var%velcon,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','nxcel',py_var,for_var%nxcel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sxd_val_type','nzcel',py_var,for_var%nzcel,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_sxd_val_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_sxd_val_type

    Module Subroutine list_to_type_array1d_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxd_val_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_sxd_val_type

    Module Subroutine list_to_type_array1d_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxd_val_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_sxd_val_type_no_alloc

    Module Subroutine list_to_type_array2d_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxd_val_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_sxd_val_type

    Module Subroutine list_to_type_array2d_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(sxd_val_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_sxd_val_type_no_alloc

    Module Subroutine Wrap_twin_type(for_var,py_var,ierror)

        ! Arguments
        type(twin_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_twin_mat,nd_twin_matinv,nd_twin_axis,nd_twin_ang

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','twin_type')
        if (ierror == 0) then
            ierror = py_var%setitem('twin_name',for_var%twin_name)
            if (ierror /= 0) then
                for_var%twin_name = ''
                ierror = py_var%setitem('twin_name',for_var%twin_name)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('ityp',for_var%ityp)
        if (ierror == 0) ierror = py_var%setitem('n_twins',for_var%n_twins)
        if (ierror == 0) ierror = ndarray_create(nd_twin_mat,for_var%twin_mat)
        if (ierror == 0) ierror = py_var%setitem('twin_mat',nd_twin_mat)
        if (ierror == 0) ierror = ndarray_create(nd_twin_matinv,for_var%twin_matinv)
        if (ierror == 0) ierror = py_var%setitem('twin_matinv',nd_twin_matinv)
        if (ierror == 0) ierror = ndarray_create(nd_twin_axis,for_var%twin_axis)
        if (ierror == 0) ierror = py_var%setitem('twin_axis',nd_twin_axis)
        if (ierror == 0) ierror = ndarray_create(nd_twin_ang,for_var%twin_ang)
        if (ierror == 0) ierror = py_var%setitem('twin_ang',nd_twin_ang)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_twin_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_twin_type

    Module Subroutine Unwrap_type_twin_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(twin_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_twin_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'twin_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_twin_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','twin_name',py_var,for_var%twin_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','ityp',py_var,for_var%ityp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','n_twins',py_var,for_var%n_twins,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','twin_mat',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twin_type','twin_mat',p_real_3d,for_var%twin_mat,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','twin_matinv',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twin_type','twin_matinv',p_real_3d,for_var%twin_matinv,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','twin_axis',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_twin_type','twin_axis',p_real_2d,for_var%twin_axis,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_twin_type','twin_ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_twin_type','twin_ang',p_real_1d,for_var%twin_ang,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_twin_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_twin_type

    Module Subroutine list_to_type_array1d_twin_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twin_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_twin_type

    Module Subroutine list_to_type_array1d_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twin_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_twin_type_no_alloc

    Module Subroutine list_to_type_array2d_twin_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twin_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_twin_type

    Module Subroutine list_to_type_array2d_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(twin_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_twin_type_no_alloc

end submodule