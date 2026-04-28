submodule (CFML_Wraps) Wraps_Structure_Factors

    implicit none
    contains

    Module Subroutine Wrap_scattering_species_type(for_var,py_var,ierror)

        ! Arguments
        type(scattering_species_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_symb,di_symb_mag,di_xcoef,di_mcoef
        type(list) :: li_symb,li_symb_mag,li_xcoef,li_mcoef
        type(ndarray) :: nd_br,nd_bi,nd_delta_fp,nd_delta_fpp

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','scattering_species_type')
        if (ierror == 0) ierror = py_var%setitem('num_species',for_var%num_species)
        if (ierror == 0) ierror = py_var%setitem('num_magspc',for_var%num_magspc)
        if (allocated(for_var%symb)) then
            if (ierror == 0) ierror = list_create(li_symb)
            if (ierror == 0) then
                do i = 1 , size(for_var%symb)
                    if (ierror == 0) then
                        ierror = li_symb%append(for_var%symb(i))
                        if (ierror /= 0) then
                            for_var%symb(i) = ''
                            ierror = li_symb%append(for_var%symb(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symb',li_symb)
        end if
        if (allocated(for_var%symb_mag)) then
            if (ierror == 0) ierror = list_create(li_symb_mag)
            if (ierror == 0) then
                do i = 1 , size(for_var%symb_mag)
                    if (ierror == 0) then
                        ierror = li_symb_mag%append(for_var%symb_mag(i))
                        if (ierror /= 0) then
                            for_var%symb_mag(i) = ''
                            ierror = li_symb_mag%append(for_var%symb_mag(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symb_mag',li_symb_mag)
        end if
        if (allocated(for_var%br)) then
            if (ierror == 0) ierror = ndarray_create(nd_br,for_var%br)
            if (ierror == 0) ierror = py_var%setitem('br',nd_br)
        end if
        if (allocated(for_var%bi)) then
            if (ierror == 0) ierror = ndarray_create(nd_bi,for_var%bi)
            if (ierror == 0) ierror = py_var%setitem('bi',nd_bi)
        end if
        if (allocated(for_var%delta_fp)) then
            if (ierror == 0) ierror = ndarray_create(nd_delta_fp,for_var%delta_fp)
            if (ierror == 0) ierror = py_var%setitem('delta_fp',nd_delta_fp)
        end if
        if (allocated(for_var%delta_fpp)) then
            if (ierror == 0) ierror = ndarray_create(nd_delta_fpp,for_var%delta_fpp)
            if (ierror == 0) ierror = py_var%setitem('delta_fpp',nd_delta_fpp)
        end if
        if (allocated(for_var%xcoef)) then
            if (ierror == 0) ierror = list_create(li_xcoef)
            if (ierror == 0) then
                do i = 1 , size(for_var%xcoef)
                    ierror = dict_create(di_xcoef)
                    if (ierror == 0) call wrap_type(for_var%xcoef(i),di_xcoef,ierror)
                    if (ierror == 0) ierror = li_xcoef%append(di_xcoef)
                    if (ierror == 0) call di_xcoef%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('xcoef',li_xcoef)
        end if
        if (allocated(for_var%mcoef)) then
            if (ierror == 0) ierror = list_create(li_mcoef)
            if (ierror == 0) then
                do i = 1 , size(for_var%mcoef)
                    ierror = dict_create(di_mcoef)
                    if (ierror == 0) call wrap_type(for_var%mcoef(i),di_mcoef,ierror)
                    if (ierror == 0) ierror = li_mcoef%append(di_mcoef)
                    if (ierror == 0) call di_mcoef%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('mcoef',li_mcoef)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_scattering_species_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_scattering_species_type

    Module Subroutine Unwrap_type_scattering_species_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(scattering_species_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_scattering_species_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'scattering_species_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_scattering_species_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','num_species',py_var,for_var%num_species,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','num_magspc',py_var,for_var%num_magspc,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','symb',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','symb_mag',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_mag,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','br',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_scattering_species_type','br',p_real_1d,for_var%br,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','bi',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_scattering_species_type','bi',p_real_1d,for_var%bi,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','delta_fp',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_scattering_species_type','delta_fp',p_real_1d,for_var%delta_fp,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','delta_fpp',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_scattering_species_type','delta_fpp',p_real_1d,for_var%delta_fpp,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','xcoef',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_scattering_species_type','xcoef',my_list,for_var%xcoef,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','mcoef',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_scattering_species_type','mcoef',my_list,for_var%mcoef,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_scattering_species_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_scattering_species_type

    Module Subroutine list_to_type_array1d_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(scattering_species_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_scattering_species_type

    Module Subroutine list_to_type_array1d_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(scattering_species_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_scattering_species_type_no_alloc

    Module Subroutine list_to_type_array2d_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(scattering_species_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_scattering_species_type

    Module Subroutine list_to_type_array2d_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(scattering_species_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_scattering_species_type_no_alloc

    Module Subroutine Wrap_strf_type(for_var,py_var,ierror)

        ! Arguments
        type(strf_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_msf,nd_miv,nd_mivc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','strf_type')
        if (ierror == 0) ierror = py_var%setitem('sqnuc',for_var%sqnuc)
        if (ierror == 0) ierror = py_var%setitem('sqmiv',for_var%sqmiv)
        if (ierror == 0) ierror = py_var%setitem('nsf',for_var%nsf)
        if (ierror == 0) ierror = ndarray_create(nd_msf,for_var%msf)
        if (ierror == 0) ierror = py_var%setitem('msf',nd_msf)
        if (ierror == 0) ierror = ndarray_create(nd_miv,for_var%miv)
        if (ierror == 0) ierror = py_var%setitem('miv',nd_miv)
        if (ierror == 0) ierror = ndarray_create(nd_mivc,for_var%mivc)
        if (ierror == 0) ierror = py_var%setitem('mivc',nd_mivc)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_strf_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_strf_type

    Module Subroutine Unwrap_type_strf_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(strf_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_strf_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'strf_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_strf_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','sqnuc',py_var,for_var%sqnuc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','nsf',py_var,for_var%nsf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','msf',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strf_type','msf',p_complex_1d,for_var%msf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','miv',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strf_type','miv',p_complex_1d,for_var%miv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','mivc',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_strf_type','mivc',p_complex_1d,for_var%mivc,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_strf_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_strf_type

    Module Subroutine list_to_type_array1d_strf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strf_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_strf_type

    Module Subroutine list_to_type_array1d_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strf_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_strf_type_no_alloc

    Module Subroutine list_to_type_array2d_strf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strf_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_strf_type

    Module Subroutine list_to_type_array2d_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strf_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_strf_type_no_alloc

    Module Subroutine Wrap_strflist_type(for_var,py_var,ierror)

        ! Arguments
        type(strflist_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_strf
        type(list) :: li_strf

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','strflist_type')
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (allocated(for_var%strf)) then
            if (ierror == 0) ierror = list_create(li_strf)
            if (ierror == 0) then
                do i = 1 , size(for_var%strf)
                    ierror = dict_create(di_strf)
                    if (ierror == 0) call wrap_type(for_var%strf(i),di_strf,ierror)
                    if (ierror == 0) ierror = li_strf%append(di_strf)
                    if (ierror == 0) call di_strf%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('strf',li_strf)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_strflist_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_strflist_type

    Module Subroutine Unwrap_type_strflist_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(strflist_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_strflist_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'strflist_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_strflist_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_strflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strflist_type','strf',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_strflist_type','strf',my_list,for_var%strf,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_strflist_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_strflist_type

    Module Subroutine list_to_type_array1d_strflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strflist_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_strflist_type

    Module Subroutine list_to_type_array1d_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strflist_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_strflist_type_no_alloc

    Module Subroutine list_to_type_array2d_strflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strflist_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_strflist_type

    Module Subroutine list_to_type_array2d_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(strflist_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_strflist_type_no_alloc

end submodule