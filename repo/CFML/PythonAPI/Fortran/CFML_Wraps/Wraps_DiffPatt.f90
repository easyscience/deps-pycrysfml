submodule (CFML_Wraps) Wraps_DiffPatt

    implicit none
    contains

    Module Subroutine Wrap_diffpat_type(for_var,py_var,ierror)

        ! Arguments
        class(diffpat_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_wave,nd_x,nd_y,nd_sigma,nd_ycalc,nd_bgr,nd_istat,nd_nd

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_type')
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('kindrad',for_var%kindrad)
            if (ierror /= 0) then
                for_var%kindrad = ''
                ierror = py_var%setitem('kindrad',for_var%kindrad)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('scatvar',for_var%scatvar)
            if (ierror /= 0) then
                for_var%scatvar = ''
                ierror = py_var%setitem('scatvar',for_var%scatvar)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('xmin',for_var%xmin)
        if (ierror == 0) ierror = py_var%setitem('xmax',for_var%xmax)
        if (ierror == 0) ierror = py_var%setitem('ymin',for_var%ymin)
        if (ierror == 0) ierror = py_var%setitem('ymax',for_var%ymax)
        if (ierror == 0) ierror = py_var%setitem('step',for_var%step)
        if (ierror == 0) ierror = py_var%setitem('npts',for_var%npts)
        if (ierror == 0) ierror = py_var%setitem('sigvar',for_var%sigvar)
        if (ierror == 0) ierror = ndarray_create(nd_wave,for_var%wave)
        if (ierror == 0) ierror = py_var%setitem('wave',nd_wave)
        if (allocated(for_var%x)) then
            if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
            if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        end if
        if (allocated(for_var%y)) then
            if (ierror == 0) ierror = ndarray_create(nd_y,for_var%y)
            if (ierror == 0) ierror = py_var%setitem('y',nd_y)
        end if
        if (allocated(for_var%sigma)) then
            if (ierror == 0) ierror = ndarray_create(nd_sigma,for_var%sigma)
            if (ierror == 0) ierror = py_var%setitem('sigma',nd_sigma)
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_e_type')
                    if (ierror == 0) then
                        ierror = py_var%setitem('instr',A%instr)
                        if (ierror /= 0) then
                            A%instr = ''
                            ierror = py_var%setitem('instr',A%instr)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('filename',A%filename)
                        if (ierror /= 0) then
                            A%filename = ''
                            ierror = py_var%setitem('filename',A%filename)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('filepath',A%filepath)
                        if (ierror /= 0) then
                            A%filepath = ''
                            ierror = py_var%setitem('filepath',A%filepath)
                        end if
                    end if
                    if (ierror == 0) ierror = py_var%setitem('scal',A%scal)
                    if (ierror == 0) ierror = py_var%setitem('monitor',A%monitor)
                    if (ierror == 0) ierror = py_var%setitem('norm_mon',A%norm_mon)
                    if (ierror == 0) ierror = py_var%setitem('col_time',A%col_time)
                    if (ierror == 0) ierror = py_var%setitem('tsample',A%tsample)
                    if (ierror == 0) ierror = py_var%setitem('tset',A%tset)
                    if (ierror == 0) ierror = py_var%setitem('ct_step',A%ct_step)
                    if (ierror == 0) ierror = py_var%setitem('al_x',A%al_x)
                    if (ierror == 0) ierror = py_var%setitem('al_y',A%al_y)
                    if (ierror == 0) ierror = py_var%setitem('al_sigma',A%al_sigma)
                    if (ierror == 0) ierror = py_var%setitem('al_ycalc',A%al_ycalc)
                    if (ierror == 0) ierror = py_var%setitem('al_bgr',A%al_bgr)
                    if (ierror == 0) ierror = py_var%setitem('al_istat',A%al_istat)
                    if (allocated(A%ycalc)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ycalc,A%ycalc)
                        if (ierror == 0) ierror = py_var%setitem('ycalc',nd_ycalc)
                    end if
                    if (allocated(A%bgr)) then
                        if (ierror == 0) ierror = ndarray_create(nd_bgr,A%bgr)
                        if (ierror == 0) ierror = py_var%setitem('bgr',nd_bgr)
                    end if
                    if (allocated(A%istat)) then
                        if (ierror == 0) ierror = ndarray_create(nd_istat,A%istat)
                        if (ierror == 0) ierror = py_var%setitem('istat',nd_istat)
                    end if
                    if (allocated(A%nd)) then
                        if (ierror == 0) ierror = ndarray_create(nd_nd,A%nd)
                        if (ierror == 0) ierror = py_var%setitem('nd',nd_nd)
                    end if
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_g_type')
                    if (ierror == 0) then
                        ierror = py_var%setitem('legend_x',A%legend_x)
                        if (ierror /= 0) then
                            A%legend_x = ''
                            ierror = py_var%setitem('legend_x',A%legend_x)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('legend_y',A%legend_y)
                        if (ierror /= 0) then
                            A%legend_y = ''
                            ierror = py_var%setitem('legend_y',A%legend_y)
                        end if
                    end if
                    if (ierror == 0) ierror = py_var%setitem('gy',A%gy)
                    if (ierror == 0) ierror = py_var%setitem('gycalc',A%gycalc)
                    if (ierror == 0) ierror = py_var%setitem('gsigma',A%gsigma)
                    if (ierror == 0) ierror = py_var%setitem('gbgr',A%gbgr)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_diffpat_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_diffpat_type

    Module Subroutine Unwrap_class_diffpat_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpat_type' &
                .and. fortran_type /= 'diffpat_e_type' &
                .and. fortran_type /= 'diffpat_g_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,A%instr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,A%filename,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,A%filepath,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,A%scal,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,A%monitor,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,A%norm_mon,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,A%col_time,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,A%tsample,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,A%tset,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,A%ct_step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,A%al_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,A%al_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,A%al_sigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,A%al_ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,A%al_bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,A%al_istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,A%ycalc,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,A%bgr,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,A%istat,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,A%nd,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_type_no_alloc

    Module Subroutine List_to_class_array1d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_diffpat_type: Cannot determine fortran type'
            else if (fortran_type == 'diffpat_type') then
                allocate(diffpat_type :: arr(n))
            else if (fortran_type == 'diffpat_e_type') then
                allocate(diffpat_e_type :: arr(n))
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_diffpat_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_diffpat_type

    Module Subroutine List_to_class_array2d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_type: Cannot determine fortran type'
                    else if (fortran_type == 'diffpat_type') then
                        allocate(diffpat_type :: arr(n,m))
                    else if (fortran_type == 'diffpat_e_type') then
                        allocate(diffpat_e_type :: arr(n,m))
                    else if (fortran_type == 'diffpat_g_type') then
                        allocate(diffpat_g_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_diffpat_type

    Module Subroutine Unwrap_class_diffpat_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_type: Cannot determine fortran type'
        else
            if (fortran_type == 'diffpat_type') then
                allocate(diffpat_type :: for_var)
            else if (fortran_type == 'diffpat_e_type') then
                allocate(diffpat_e_type :: for_var)
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,A%instr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,A%filename,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,A%filepath,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,A%scal,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,A%monitor,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,A%norm_mon,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,A%col_time,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,A%tsample,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,A%tset,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,A%ct_step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,A%al_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,A%al_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,A%al_sigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,A%al_ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,A%al_bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,A%al_istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,A%ycalc,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,A%bgr,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,A%istat,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,A%nd,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_type

    Module Subroutine list_to_type_array1d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_type

    Module Subroutine list_to_type_array1d_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_type_no_alloc

    Module Subroutine list_to_type_array2d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_type

    Module Subroutine list_to_type_array2d_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_type_no_alloc

    Module Subroutine Unwrap_class_diffpat_e_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_e_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_e_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpat_e_type' &
                .and. fortran_type /= 'diffpat_g_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_e_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,for_var%instr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,for_var%filepath,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,for_var%scal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,for_var%monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,for_var%norm_mon,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,for_var%col_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,for_var%tsample,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,for_var%tset,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,for_var%ct_step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,for_var%al_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,for_var%al_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,for_var%al_sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,for_var%al_ycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,for_var%al_bgr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,for_var%al_istat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,for_var%ycalc,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,for_var%bgr,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,for_var%istat,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,for_var%nd,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_e_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_e_type_no_alloc

    Module Subroutine List_to_class_array1d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_e_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_diffpat_e_type: Cannot determine fortran type'
            else if (fortran_type == 'diffpat_e_type') then
                allocate(diffpat_e_type :: arr(n))
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_diffpat_e_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_diffpat_e_type

    Module Subroutine List_to_class_array2d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_e_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_e_type: Cannot determine fortran type'
                    else if (fortran_type == 'diffpat_e_type') then
                        allocate(diffpat_e_type :: arr(n,m))
                    else if (fortran_type == 'diffpat_g_type') then
                        allocate(diffpat_g_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_e_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_diffpat_e_type

    Module Subroutine Unwrap_class_diffpat_e_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_e_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_e_type: Cannot determine fortran type'
        else
            if (fortran_type == 'diffpat_e_type') then
                allocate(diffpat_e_type :: for_var)
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_e_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,for_var%instr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,for_var%filepath,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,for_var%scal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,for_var%monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,for_var%norm_mon,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,for_var%col_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,for_var%tsample,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,for_var%tset,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,for_var%ct_step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,for_var%al_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,for_var%al_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,for_var%al_sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,for_var%al_ycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,for_var%al_bgr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,for_var%al_istat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,for_var%ycalc,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,for_var%bgr,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,for_var%istat,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,for_var%nd,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_e_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_e_type

    Module Subroutine list_to_type_array1d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_e_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_e_type

    Module Subroutine list_to_type_array1d_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_e_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_e_type_no_alloc

    Module Subroutine list_to_type_array2d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_e_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_e_type

    Module Subroutine list_to_type_array2d_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_e_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_e_type_no_alloc

    Module Subroutine Unwrap_class_diffpat_g_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_g_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_g_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpat_g_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_g_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,for_var%instr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,for_var%filepath,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,for_var%scal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,for_var%monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,for_var%norm_mon,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,for_var%col_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,for_var%tsample,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,for_var%tset,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,for_var%ct_step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,for_var%al_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,for_var%al_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,for_var%al_sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,for_var%al_ycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,for_var%al_bgr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,for_var%al_istat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,for_var%ycalc,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,for_var%bgr,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,for_var%istat,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,for_var%nd,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,for_var%legend_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,for_var%legend_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,for_var%gy,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,for_var%gycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,for_var%gsigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,for_var%gbgr,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_g_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_g_type_no_alloc

    Module Subroutine List_to_class_array1d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_g_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_diffpat_g_type: Cannot determine fortran type'
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_diffpat_g_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_diffpat_g_type

    Module Subroutine List_to_class_array2d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpat_g_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_g_type: Cannot determine fortran type'
                    else if (fortran_type == 'diffpat_g_type') then
                        allocate(diffpat_g_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_diffpat_g_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_diffpat_g_type

    Module Subroutine Unwrap_class_diffpat_g_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_g_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpat_g_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpat_g_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_g_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,for_var%instr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,for_var%filepath,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,for_var%scal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,for_var%monitor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,for_var%norm_mon,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,for_var%col_time,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,for_var%tsample,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,for_var%tset,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,for_var%ct_step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,for_var%al_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,for_var%al_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,for_var%al_sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,for_var%al_ycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,for_var%al_bgr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,for_var%al_istat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','ycalc',p_real_1d,for_var%ycalc,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','bgr',p_real_1d,for_var%bgr,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','istat',p_int_1d,for_var%istat,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_e_type','nd',p_int_1d,for_var%nd,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,for_var%legend_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,for_var%legend_y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,for_var%gy,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,for_var%gycalc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,for_var%gsigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,for_var%gbgr,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpat_g_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpat_g_type

    Module Subroutine list_to_type_array1d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_g_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_g_type

    Module Subroutine list_to_type_array1d_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_g_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpat_g_type_no_alloc

    Module Subroutine list_to_type_array2d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_g_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_g_type

    Module Subroutine list_to_type_array2d_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpat_g_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpat_g_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpat_g_type_no_alloc

    Module Subroutine Wrap_diffpatt_conditions_type(for_var,py_var,ierror)

        ! Arguments
        class(diffpatt_conditions_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_tth,nd_lambda,nd_prf_par,nd_multip,nd_lprof,nd_tof

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpatt_conditions_type')
        if (ierror == 0) ierror = py_var%setitem('job',for_var%job)
        if (ierror == 0) ierror = py_var%setitem('scale_factor',for_var%scale_factor)
        if (ierror == 0) then
            ierror = py_var%setitem('title',for_var%title)
            if (ierror /= 0) then
                for_var%title = ''
                ierror = py_var%setitem('title',for_var%title)
            end if
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (powpatt_cw_conditions_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','powpatt_cw_conditions_type')
                    if (ierror == 0) then
                        ierror = py_var%setitem('profile',A%profile)
                        if (ierror /= 0) then
                            A%profile = ''
                            ierror = py_var%setitem('profile',A%profile)
                        end if
                    end if
                    if (ierror == 0) ierror = py_var%setitem('tthmin',A%tthmin)
                    if (ierror == 0) ierror = py_var%setitem('step',A%step)
                    if (ierror == 0) ierror = py_var%setitem('tthmax',A%tthmax)
                    if (ierror == 0) ierror = py_var%setitem('wdt',A%wdt)
                    if (ierror == 0) ierror = py_var%setitem('is_asym',A%is_asym)
                    if (ierror == 0) ierror = py_var%setitem('is_tth',A%is_tth)
                    if (ierror == 0) ierror = py_var%setitem('twowaves',A%twowaves)
                    if (allocated(A%tth)) then
                        if (ierror == 0) ierror = ndarray_create(nd_tth,A%tth)
                        if (ierror == 0) ierror = py_var%setitem('tth',nd_tth)
                    end if
                    if (ierror == 0) ierror = py_var%setitem('zero',A%zero)
                    if (ierror == 0) ierror = py_var%setitem('sycos',A%sycos)
                    if (ierror == 0) ierror = py_var%setitem('sysin',A%sysin)
                    if (ierror == 0) ierror = ndarray_create(nd_lambda,A%lambda)
                    if (ierror == 0) ierror = py_var%setitem('lambda',nd_lambda)
                    if (ierror == 0) ierror = py_var%setitem('ratio',A%ratio)
                    if (ierror == 0) ierror = py_var%setitem('u',A%u)
                    if (ierror == 0) ierror = py_var%setitem('v',A%v)
                    if (ierror == 0) ierror = py_var%setitem('w',A%w)
                    if (ierror == 0) ierror = py_var%setitem('x',A%x)
                    if (ierror == 0) ierror = py_var%setitem('y',A%y)
                    if (ierror == 0) ierror = py_var%setitem('asym1',A%asym1)
                    if (ierror == 0) ierror = py_var%setitem('asym2',A%asym2)
                    if (ierror == 0) ierror = ndarray_create(nd_prf_par,A%prf_par)
                    if (ierror == 0) ierror = py_var%setitem('prf_par',nd_prf_par)
                    if (ierror == 0) ierror = ndarray_create(nd_multip,A%multip)
                    if (ierror == 0) ierror = py_var%setitem('multip',nd_multip)
                    if (ierror == 0) ierror = ndarray_create(nd_lprof,A%lprof)
                    if (ierror == 0) ierror = py_var%setitem('lprof',nd_lprof)
                class is (powpatt_tof_conditions_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','powpatt_tof_conditions_type')
                    if (ierror == 0) then
                        ierror = py_var%setitem('profile',A%profile)
                        if (ierror /= 0) then
                            A%profile = ''
                            ierror = py_var%setitem('profile',A%profile)
                        end if
                    end if
                    if (ierror == 0) ierror = py_var%setitem('tof_min',A%tof_min)
                    if (ierror == 0) ierror = py_var%setitem('step',A%step)
                    if (ierror == 0) ierror = py_var%setitem('tof_max',A%tof_max)
                    if (ierror == 0) ierror = py_var%setitem('wdt',A%wdt)
                    if (ierror == 0) ierror = py_var%setitem('bank_angle',A%bank_angle)
                    if (ierror == 0) ierror = py_var%setitem('d_min',A%d_min)
                    if (ierror == 0) ierror = py_var%setitem('d_max',A%d_max)
                    if (ierror == 0) ierror = py_var%setitem('is_tof',A%is_tof)
                    if (allocated(A%tof)) then
                        if (ierror == 0) ierror = ndarray_create(nd_tof,A%tof)
                        if (ierror == 0) ierror = py_var%setitem('tof',nd_tof)
                    end if
                    if (ierror == 0) ierror = py_var%setitem('zero',A%zero)
                    if (ierror == 0) ierror = py_var%setitem('dtt1',A%dtt1)
                    if (ierror == 0) ierror = py_var%setitem('dtt2',A%dtt2)
                    if (ierror == 0) ierror = py_var%setitem('dtt_1overd',A%dtt_1overd)
                    if (ierror == 0) ierror = py_var%setitem('sigma2',A%sigma2)
                    if (ierror == 0) ierror = py_var%setitem('sigma1',A%sigma1)
                    if (ierror == 0) ierror = py_var%setitem('sigma0',A%sigma0)
                    if (ierror == 0) ierror = py_var%setitem('sigmaq',A%sigmaq)
                    if (ierror == 0) ierror = py_var%setitem('gamma2',A%gamma2)
                    if (ierror == 0) ierror = py_var%setitem('gamma1',A%gamma1)
                    if (ierror == 0) ierror = py_var%setitem('gamma0',A%gamma0)
                    if (ierror == 0) ierror = py_var%setitem('alpha0',A%alpha0)
                    if (ierror == 0) ierror = py_var%setitem('alpha1',A%alpha1)
                    if (ierror == 0) ierror = py_var%setitem('alphaq',A%alphaq)
                    if (ierror == 0) ierror = py_var%setitem('beta0',A%beta0)
                    if (ierror == 0) ierror = py_var%setitem('beta1',A%beta1)
                    if (ierror == 0) ierror = py_var%setitem('betaq',A%betaq)
                    if (ierror == 0) ierror = ndarray_create(nd_prf_par,A%prf_par)
                    if (ierror == 0) ierror = py_var%setitem('prf_par',nd_prf_par)
                    if (ierror == 0) ierror = ndarray_create(nd_multip,A%multip)
                    if (ierror == 0) ierror = py_var%setitem('multip',nd_multip)
                    if (ierror == 0) ierror = ndarray_create(nd_lprof,A%lprof)
                    if (ierror == 0) ierror = py_var%setitem('lprof',nd_lprof)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_diffpatt_conditions_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_diffpatt_conditions_type

    Module Subroutine Unwrap_class_diffpatt_conditions_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpatt_conditions_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpatt_conditions_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpatt_conditions_type' &
                .and. fortran_type /= 'powpatt_cw_conditions_type' &
                .and. fortran_type /= 'powpatt_tof_conditions_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpatt_conditions_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (powpatt_cw_conditions_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','profile',py_var,A%profile,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmin',py_var,A%tthmin,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','step',py_var,A%step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmax',py_var,A%tthmax,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','wdt',py_var,A%wdt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_asym',py_var,A%is_asym,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_tth',py_var,A%is_tth,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','twowaves',py_var,A%twowaves,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tth',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_cw_conditions_type','tth',p_real_1d,A%tth,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','zero',py_var,A%zero,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sycos',py_var,A%sycos,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sysin',py_var,A%sysin,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lambda',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lambda',p_real_1d,A%lambda,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','ratio',py_var,A%ratio,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','u',py_var,A%u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','v',py_var,A%v,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','w',py_var,A%w,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','x',py_var,A%x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','y',py_var,A%y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym1',py_var,A%asym1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym2',py_var,A%asym2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','prf_par',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','prf_par',p_real_1d,A%prf_par,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','multip',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','multip',p_real_1d,A%multip,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lprof',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lprof',p_int_1d,A%lprof,ierror)
                class is (powpatt_tof_conditions_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','profile',py_var,A%profile,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_min',py_var,A%tof_min,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','step',py_var,A%step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_max',py_var,A%tof_max,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','wdt',py_var,A%wdt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','bank_angle',py_var,A%bank_angle,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_min',py_var,A%d_min,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_max',py_var,A%d_max,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','is_tof',py_var,A%is_tof,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_tof_conditions_type','tof',p_real_1d,A%tof,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','zero',py_var,A%zero,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt1',py_var,A%dtt1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt2',py_var,A%dtt2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt_1overd',py_var,A%dtt_1overd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma2',py_var,A%sigma2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma1',py_var,A%sigma1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma0',py_var,A%sigma0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigmaq',py_var,A%sigmaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma2',py_var,A%gamma2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma1',py_var,A%gamma1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma0',py_var,A%gamma0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha0',py_var,A%alpha0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha1',py_var,A%alpha1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alphaq',py_var,A%alphaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta0',py_var,A%beta0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta1',py_var,A%beta1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','betaq',py_var,A%betaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','prf_par',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','prf_par',p_real_1d,A%prf_par,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','multip',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','multip',p_real_1d,A%multip,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','lprof',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','lprof',p_int_1d,A%lprof,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpatt_conditions_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpatt_conditions_type_no_alloc

    Module Subroutine List_to_class_array1d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpatt_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_diffpatt_conditions_type: Cannot determine fortran type'
            else if (fortran_type == 'diffpatt_conditions_type') then
                allocate(diffpatt_conditions_type :: arr(n))
            else if (fortran_type == 'powpatt_cw_conditions_type') then
                allocate(powpatt_cw_conditions_type :: arr(n))
            else if (fortran_type == 'powpatt_tof_conditions_type') then
                allocate(powpatt_tof_conditions_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_diffpatt_conditions_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_diffpatt_conditions_type

    Module Subroutine List_to_class_array2d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(diffpatt_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_diffpatt_conditions_type: Cannot determine fortran type'
                    else if (fortran_type == 'diffpatt_conditions_type') then
                        allocate(diffpatt_conditions_type :: arr(n,m))
                    else if (fortran_type == 'powpatt_cw_conditions_type') then
                        allocate(powpatt_cw_conditions_type :: arr(n,m))
                    else if (fortran_type == 'powpatt_tof_conditions_type') then
                        allocate(powpatt_tof_conditions_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_diffpatt_conditions_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_diffpatt_conditions_type

    Module Subroutine Unwrap_class_diffpatt_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpatt_conditions_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_diffpatt_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type == 'diffpatt_conditions_type') then
                allocate(diffpatt_conditions_type :: for_var)
            else if (fortran_type == 'powpatt_cw_conditions_type') then
                allocate(powpatt_cw_conditions_type :: for_var)
            else if (fortran_type == 'powpatt_tof_conditions_type') then
                allocate(powpatt_tof_conditions_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpatt_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (powpatt_cw_conditions_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','profile',py_var,A%profile,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmin',py_var,A%tthmin,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','step',py_var,A%step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmax',py_var,A%tthmax,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','wdt',py_var,A%wdt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_asym',py_var,A%is_asym,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_tth',py_var,A%is_tth,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','twowaves',py_var,A%twowaves,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tth',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_cw_conditions_type','tth',p_real_1d,A%tth,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','zero',py_var,A%zero,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sycos',py_var,A%sycos,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sysin',py_var,A%sysin,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lambda',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lambda',p_real_1d,A%lambda,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','ratio',py_var,A%ratio,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','u',py_var,A%u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','v',py_var,A%v,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','w',py_var,A%w,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','x',py_var,A%x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','y',py_var,A%y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym1',py_var,A%asym1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym2',py_var,A%asym2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','prf_par',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','prf_par',p_real_1d,A%prf_par,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','multip',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','multip',p_real_1d,A%multip,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lprof',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lprof',p_int_1d,A%lprof,ierror)
                class is (powpatt_tof_conditions_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','profile',py_var,A%profile,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_min',py_var,A%tof_min,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','step',py_var,A%step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_max',py_var,A%tof_max,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','wdt',py_var,A%wdt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','bank_angle',py_var,A%bank_angle,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_min',py_var,A%d_min,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_max',py_var,A%d_max,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','is_tof',py_var,A%is_tof,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_tof_conditions_type','tof',p_real_1d,A%tof,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','zero',py_var,A%zero,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt1',py_var,A%dtt1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt2',py_var,A%dtt2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt_1overd',py_var,A%dtt_1overd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma2',py_var,A%sigma2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma1',py_var,A%sigma1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma0',py_var,A%sigma0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigmaq',py_var,A%sigmaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma2',py_var,A%gamma2,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma1',py_var,A%gamma1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma0',py_var,A%gamma0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha0',py_var,A%alpha0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha1',py_var,A%alpha1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alphaq',py_var,A%alphaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta0',py_var,A%beta0,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta1',py_var,A%beta1,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','betaq',py_var,A%betaq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','prf_par',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','prf_par',p_real_1d,A%prf_par,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','multip',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','multip',p_real_1d,A%multip,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','lprof',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','lprof',p_int_1d,A%lprof,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_diffpatt_conditions_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_diffpatt_conditions_type

    Module Subroutine list_to_type_array1d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpatt_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpatt_conditions_type

    Module Subroutine list_to_type_array1d_diffpatt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpatt_conditions_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_diffpatt_conditions_type_no_alloc

    Module Subroutine list_to_type_array2d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpatt_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpatt_conditions_type

    Module Subroutine list_to_type_array2d_diffpatt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(diffpatt_conditions_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_diffpatt_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_diffpatt_conditions_type_no_alloc

    Module Subroutine Unwrap_class_powpatt_cw_conditions_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(powpatt_cw_conditions_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'powpatt_cw_conditions_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','profile',py_var,for_var%profile,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmin',py_var,for_var%tthmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmax',py_var,for_var%tthmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','wdt',py_var,for_var%wdt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_asym',py_var,for_var%is_asym,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_tth',py_var,for_var%is_tth,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','twowaves',py_var,for_var%twowaves,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tth',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_cw_conditions_type','tth',p_real_1d,for_var%tth,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','zero',py_var,for_var%zero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sycos',py_var,for_var%sycos,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sysin',py_var,for_var%sysin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lambda',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lambda',p_real_1d,for_var%lambda,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','ratio',py_var,for_var%ratio,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','u',py_var,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','v',py_var,for_var%v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','w',py_var,for_var%w,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','x',py_var,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','y',py_var,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym1',py_var,for_var%asym1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym2',py_var,for_var%asym2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','prf_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','prf_par',p_real_1d,for_var%prf_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','multip',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','multip',p_real_1d,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lprof',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lprof',p_int_1d,for_var%lprof,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_powpatt_cw_conditions_type_no_alloc

    Module Subroutine List_to_class_array1d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(powpatt_cw_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_powpatt_cw_conditions_type: Cannot determine fortran type'
            else if (fortran_type == 'powpatt_cw_conditions_type') then
                allocate(powpatt_cw_conditions_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_powpatt_cw_conditions_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_powpatt_cw_conditions_type

    Module Subroutine List_to_class_array2d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(powpatt_cw_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_powpatt_cw_conditions_type: Cannot determine fortran type'
                    else if (fortran_type == 'powpatt_cw_conditions_type') then
                        allocate(powpatt_cw_conditions_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_powpatt_cw_conditions_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_powpatt_cw_conditions_type

    Module Subroutine Unwrap_class_powpatt_cw_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(powpatt_cw_conditions_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'powpatt_cw_conditions_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','profile',py_var,for_var%profile,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmin',py_var,for_var%tthmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tthmax',py_var,for_var%tthmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','wdt',py_var,for_var%wdt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_asym',py_var,for_var%is_asym,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','is_tth',py_var,for_var%is_tth,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','twowaves',py_var,for_var%twowaves,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','tth',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_cw_conditions_type','tth',p_real_1d,for_var%tth,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','zero',py_var,for_var%zero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sycos',py_var,for_var%sycos,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','sysin',py_var,for_var%sysin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lambda',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lambda',p_real_1d,for_var%lambda,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','ratio',py_var,for_var%ratio,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','u',py_var,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','v',py_var,for_var%v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','w',py_var,for_var%w,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','x',py_var,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','y',py_var,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym1',py_var,for_var%asym1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','asym2',py_var,for_var%asym2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','prf_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','prf_par',p_real_1d,for_var%prf_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','multip',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','multip',p_real_1d,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_cw_conditions_type','lprof',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_cw_conditions_type','lprof',p_int_1d,for_var%lprof,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powpatt_cw_conditions_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_powpatt_cw_conditions_type

    Module Subroutine list_to_type_array1d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_cw_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_powpatt_cw_conditions_type

    Module Subroutine list_to_type_array1d_powpatt_cw_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_cw_conditions_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_powpatt_cw_conditions_type_no_alloc

    Module Subroutine list_to_type_array2d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_cw_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_powpatt_cw_conditions_type

    Module Subroutine list_to_type_array2d_powpatt_cw_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_cw_conditions_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_powpatt_cw_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_powpatt_cw_conditions_type_no_alloc

    Module Subroutine Unwrap_class_powpatt_tof_conditions_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(powpatt_tof_conditions_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'powpatt_tof_conditions_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','profile',py_var,for_var%profile,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_min',py_var,for_var%tof_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_max',py_var,for_var%tof_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','wdt',py_var,for_var%wdt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','bank_angle',py_var,for_var%bank_angle,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_min',py_var,for_var%d_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_max',py_var,for_var%d_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','is_tof',py_var,for_var%is_tof,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_tof_conditions_type','tof',p_real_1d,for_var%tof,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','zero',py_var,for_var%zero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt1',py_var,for_var%dtt1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt2',py_var,for_var%dtt2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt_1overd',py_var,for_var%dtt_1overd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma2',py_var,for_var%sigma2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma1',py_var,for_var%sigma1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma0',py_var,for_var%sigma0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigmaq',py_var,for_var%sigmaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma2',py_var,for_var%gamma2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma1',py_var,for_var%gamma1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma0',py_var,for_var%gamma0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha0',py_var,for_var%alpha0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha1',py_var,for_var%alpha1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alphaq',py_var,for_var%alphaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta0',py_var,for_var%beta0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta1',py_var,for_var%beta1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','betaq',py_var,for_var%betaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','prf_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','prf_par',p_real_1d,for_var%prf_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','multip',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','multip',p_real_1d,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','lprof',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','lprof',p_int_1d,for_var%lprof,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_powpatt_tof_conditions_type_no_alloc

    Module Subroutine List_to_class_array1d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(powpatt_tof_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_powpatt_tof_conditions_type: Cannot determine fortran type'
            else if (fortran_type == 'powpatt_tof_conditions_type') then
                allocate(powpatt_tof_conditions_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_powpatt_tof_conditions_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_powpatt_tof_conditions_type

    Module Subroutine List_to_class_array2d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(powpatt_tof_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_powpatt_tof_conditions_type: Cannot determine fortran type'
                    else if (fortran_type == 'powpatt_tof_conditions_type') then
                        allocate(powpatt_tof_conditions_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_powpatt_tof_conditions_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_powpatt_tof_conditions_type

    Module Subroutine Unwrap_class_powpatt_tof_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(powpatt_tof_conditions_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'powpatt_tof_conditions_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','job',py_var,for_var%job,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','scale_factor',py_var,for_var%scale_factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpatt_conditions_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','profile',py_var,for_var%profile,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_min',py_var,for_var%tof_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof_max',py_var,for_var%tof_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','wdt',py_var,for_var%wdt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','bank_angle',py_var,for_var%bank_angle,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_min',py_var,for_var%d_min,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','d_max',py_var,for_var%d_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','is_tof',py_var,for_var%is_tof,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','tof',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_powpatt_tof_conditions_type','tof',p_real_1d,for_var%tof,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','zero',py_var,for_var%zero,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt1',py_var,for_var%dtt1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt2',py_var,for_var%dtt2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','dtt_1overd',py_var,for_var%dtt_1overd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma2',py_var,for_var%sigma2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma1',py_var,for_var%sigma1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigma0',py_var,for_var%sigma0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','sigmaq',py_var,for_var%sigmaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma2',py_var,for_var%gamma2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma1',py_var,for_var%gamma1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','gamma0',py_var,for_var%gamma0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha0',py_var,for_var%alpha0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alpha1',py_var,for_var%alpha1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','alphaq',py_var,for_var%alphaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta0',py_var,for_var%beta0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','beta1',py_var,for_var%beta1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','betaq',py_var,for_var%betaq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','prf_par',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','prf_par',p_real_1d,for_var%prf_par,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','multip',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','multip',p_real_1d,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_powpatt_tof_conditions_type','lprof',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_powpatt_tof_conditions_type','lprof',p_int_1d,for_var%lprof,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_powpatt_tof_conditions_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_powpatt_tof_conditions_type

    Module Subroutine list_to_type_array1d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_tof_conditions_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_powpatt_tof_conditions_type

    Module Subroutine list_to_type_array1d_powpatt_tof_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_tof_conditions_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_powpatt_tof_conditions_type_no_alloc

    Module Subroutine list_to_type_array2d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_tof_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_powpatt_tof_conditions_type

    Module Subroutine list_to_type_array2d_powpatt_tof_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(powpatt_tof_conditions_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_powpatt_tof_conditions_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_powpatt_tof_conditions_type_no_alloc

    Module Subroutine Wrap_bck_type(for_var,py_var,ierror)

        ! Arguments
        type(bck_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_fpar,nd_xfpar,nd_ppar

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','bck_type')
        if (ierror == 0) ierror = py_var%setitem('ipatt',for_var%ipatt)
        if (ierror == 0) then
            ierror = py_var%setitem('funct_typ',for_var%funct_typ)
            if (ierror /= 0) then
                for_var%funct_typ = ''
                ierror = py_var%setitem('funct_typ',for_var%funct_typ)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('num_fpar',for_var%num_fpar)
        if (allocated(for_var%fpar)) then
            if (ierror == 0) ierror = ndarray_create(nd_fpar,for_var%fpar)
            if (ierror == 0) ierror = py_var%setitem('fpar',nd_fpar)
        end if
        if (allocated(for_var%xfpar)) then
            if (ierror == 0) ierror = ndarray_create(nd_xfpar,for_var%xfpar)
            if (ierror == 0) ierror = py_var%setitem('xfpar',nd_xfpar)
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('peak_typ',for_var%peak_typ)
            if (ierror /= 0) then
                for_var%peak_typ = ''
                ierror = py_var%setitem('peak_typ',for_var%peak_typ)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('num_peaks',for_var%num_peaks)
        if (allocated(for_var%ppar)) then
            if (ierror == 0) ierror = ndarray_create(nd_ppar,for_var%ppar)
            if (ierror == 0) ierror = py_var%setitem('ppar',nd_ppar)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_bck_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_bck_type

    Module Subroutine Unwrap_type_bck_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(bck_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_bck_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'bck_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_bck_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','ipatt',py_var,for_var%ipatt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_bck_type','funct_typ',py_var,for_var%funct_typ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','num_fpar',py_var,for_var%num_fpar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','fpar',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_bck_type','fpar',p_real_1d,for_var%fpar,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','xfpar',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_bck_type','xfpar',p_real_1d,for_var%xfpar,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_bck_type','peak_typ',py_var,for_var%peak_typ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','num_peaks',py_var,for_var%num_peaks,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_bck_type','ppar',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_bck_type','ppar',p_real_2d,for_var%ppar,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_bck_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_bck_type

    Module Subroutine list_to_type_array1d_bck_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bck_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_bck_type

    Module Subroutine list_to_type_array1d_bck_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bck_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_bck_type_no_alloc

    Module Subroutine list_to_type_array2d_bck_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bck_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_bck_type

    Module Subroutine list_to_type_array2d_bck_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(bck_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_bck_type_no_alloc

    Module Subroutine Wrap_interval_type(for_var,py_var,ierror)

        ! Arguments
        type(interval_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','interval_type')
        if (ierror == 0) ierror = py_var%setitem('mina',for_var%mina)
        if (ierror == 0) ierror = py_var%setitem('maxb',for_var%maxb)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_interval_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_interval_type

    Module Subroutine Unwrap_type_interval_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(interval_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_interval_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'interval_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_interval_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_interval_type','mina',py_var,for_var%mina,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_interval_type','maxb',py_var,for_var%maxb,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_interval_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_interval_type

    Module Subroutine list_to_type_array1d_interval_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_interval_type

    Module Subroutine list_to_type_array1d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_interval_type_no_alloc

    Module Subroutine list_to_type_array2d_interval_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_interval_type

    Module Subroutine list_to_type_array2d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_interval_type_no_alloc

    Module Subroutine Wrap_excl_reg_type(for_var,py_var,ierror)

        ! Arguments
        type(excl_reg_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_exc
        type(list) :: li_exc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','excl_reg_type')
        if (ierror == 0) ierror = py_var%setitem('num_excl',for_var%num_excl)
        if (allocated(for_var%exc)) then
            if (ierror == 0) ierror = list_create(li_exc)
            if (ierror == 0) then
                do i = 1 , size(for_var%exc)
                    ierror = dict_create(di_exc)
                    if (ierror == 0) call wrap_type(for_var%exc(i),di_exc,ierror)
                    if (ierror == 0) ierror = li_exc%append(di_exc)
                    if (ierror == 0) call di_exc%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('exc',li_exc)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_excl_reg_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_excl_reg_type

    Module Subroutine Unwrap_type_excl_reg_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(excl_reg_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_excl_reg_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'excl_reg_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_excl_reg_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_excl_reg_type','num_excl',py_var,for_var%num_excl,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_excl_reg_type','exc',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_excl_reg_type','exc',my_list,for_var%exc,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_excl_reg_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_excl_reg_type

    Module Subroutine list_to_type_array1d_excl_reg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excl_reg_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_excl_reg_type

    Module Subroutine list_to_type_array1d_excl_reg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excl_reg_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_excl_reg_type_no_alloc

    Module Subroutine list_to_type_array2d_excl_reg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excl_reg_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_excl_reg_type

    Module Subroutine list_to_type_array2d_excl_reg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(excl_reg_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_excl_reg_type_no_alloc

    Module Subroutine Wrap_pattern_type(for_var,py_var,ierror)

        ! Arguments
        type(pattern_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_cond,di_pdat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','pattern_type')
        if (ierror == 0) then
            ierror = py_var%setitem('filename',for_var%filename)
            if (ierror /= 0) then
                for_var%filename = ''
                ierror = py_var%setitem('filename',for_var%filename)
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
            ierror = py_var%setitem('mode',for_var%mode)
            if (ierror /= 0) then
                for_var%mode = ''
                ierror = py_var%setitem('mode',for_var%mode)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('radiation',for_var%radiation)
            if (ierror /= 0) then
                for_var%radiation = ''
                ierror = py_var%setitem('radiation',for_var%radiation)
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
            ierror = py_var%setitem('patt_type',for_var%patt_type)
            if (ierror /= 0) then
                for_var%patt_type = ''
                ierror = py_var%setitem('patt_type',for_var%patt_type)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('irf',for_var%irf)
        if (ierror == 0) ierror = dict_create(di_cond)
        if (ierror == 0) call wrap_type(for_var%cond,di_cond,ierror)
        if (ierror == 0) ierror = py_var%setitem('cond',di_cond)
        if (ierror == 0) ierror = dict_create(di_pdat)
        if (ierror == 0) call wrap_type(for_var%pdat,di_pdat,ierror)
        if (ierror == 0) ierror = py_var%setitem('pdat',di_pdat)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_pattern_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_pattern_type

    Module Subroutine Unwrap_type_pattern_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(pattern_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        type(dict) :: dict_cond,dict_pdat

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_pattern_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'pattern_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_pattern_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_pattern_type','filename',py_var,for_var%filename,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_pattern_type','name',py_var,for_var%name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','mode',py_var,for_var%mode,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','radiation',py_var,for_var%radiation,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','sample',py_var,for_var%sample,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_pattern_type','patt_type',py_var,for_var%patt_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','irf',py_var,for_var%irf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','cond',py_var,dict_cond,ierror)
        if (ierror == 0) call unwrap_class_diffpatt_conditions_type(dict_cond,for_var%cond,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pattern_type','pdat',py_var,dict_pdat,ierror)
        if (ierror == 0) call unwrap_class_diffpat_e_type(dict_pdat,for_var%pdat,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_pattern_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_pattern_type

    Module Subroutine list_to_type_array1d_pattern_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(pattern_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_pattern_type

    Module Subroutine list_to_type_array1d_pattern_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(pattern_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_pattern_type_no_alloc

    Module Subroutine list_to_type_array2d_pattern_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(pattern_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_pattern_type

    Module Subroutine list_to_type_array2d_pattern_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(pattern_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_pattern_type_no_alloc

end submodule