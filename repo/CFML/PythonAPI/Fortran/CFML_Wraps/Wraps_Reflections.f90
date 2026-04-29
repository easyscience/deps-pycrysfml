submodule (CFML_Wraps) Wraps_Reflections

    implicit none
    contains

    Module Subroutine Wrap_refl_type(for_var,py_var,ierror)

        ! Arguments
        class(refl_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_h,nd_hr,nd_fo,nd_fc,nd_sfo,nd_phase,nd_a,nd_b,nd_w,nd_msf,nd_miv,nd_mivc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','refl_type')
        if (allocated(for_var%h)) then
            if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
            if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        end if
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = py_var%setitem('imag',for_var%imag)
        if (ierror == 0) ierror = py_var%setitem('pcoeff',for_var%pcoeff)
        if (ierror == 0) ierror = ndarray_create(nd_hr,for_var%hr)
        if (ierror == 0) ierror = py_var%setitem('hr',nd_hr)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','srefl_type')
                    if (ierror == 0) ierror = ndarray_create(nd_fo,A%fo)
                    if (ierror == 0) ierror = py_var%setitem('fo',nd_fo)
                    if (ierror == 0) ierror = ndarray_create(nd_fc,A%fc)
                    if (ierror == 0) ierror = py_var%setitem('fc',nd_fc)
                    if (ierror == 0) ierror = ndarray_create(nd_sfo,A%sfo)
                    if (ierror == 0) ierror = py_var%setitem('sfo',nd_sfo)
                    if (ierror == 0) ierror = ndarray_create(nd_phase,A%phase)
                    if (ierror == 0) ierror = py_var%setitem('phase',nd_phase)
                    if (ierror == 0) ierror = ndarray_create(nd_a,A%a)
                    if (ierror == 0) ierror = py_var%setitem('a',nd_a)
                    if (ierror == 0) ierror = ndarray_create(nd_b,A%b)
                    if (ierror == 0) ierror = py_var%setitem('b',nd_b)
                    if (ierror == 0) ierror = ndarray_create(nd_w,A%w)
                    if (ierror == 0) ierror = py_var%setitem('w',nd_w)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','mrefl_type')
                    if (ierror == 0) ierror = py_var%setitem('mivo',A%mivo)
                    if (ierror == 0) ierror = py_var%setitem('smivo',A%smivo)
                    if (ierror == 0) ierror = ndarray_create(nd_msf,A%msf)
                    if (ierror == 0) ierror = py_var%setitem('msf',nd_msf)
                    if (ierror == 0) ierror = ndarray_create(nd_miv,A%miv)
                    if (ierror == 0) ierror = py_var%setitem('miv',nd_miv)
                    if (ierror == 0) ierror = ndarray_create(nd_mivc,A%mivc)
                    if (ierror == 0) ierror = py_var%setitem('mivc',nd_mivc)
                    if (ierror == 0) ierror = py_var%setitem('sqmiv',A%sqmiv)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_refl_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_refl_type

    Module Subroutine Unwrap_class_refl_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refl_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'refl_type' &
                .and. fortran_type /= 'srefl_type' &
                .and. fortran_type /= 'mrefl_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,A%w,ierror)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,A%msf,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,A%miv,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,A%mivc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,A%sqmiv,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_refl_type_no_alloc

    Module Subroutine List_to_class_array1d_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(refl_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_refl_type: Cannot determine fortran type'
            else if (fortran_type == 'refl_type') then
                allocate(refl_type :: arr(n))
            else if (fortran_type == 'srefl_type') then
                allocate(srefl_type :: arr(n))
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_refl_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_refl_type

    Module Subroutine List_to_class_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(refl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_refl_type: Cannot determine fortran type'
                    else if (fortran_type == 'refl_type') then
                        allocate(refl_type :: arr(n,m))
                    else if (fortran_type == 'srefl_type') then
                        allocate(srefl_type :: arr(n,m))
                    else if (fortran_type == 'mrefl_type') then
                        allocate(mrefl_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_refl_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_refl_type

    Module Subroutine Unwrap_class_refl_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refl_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_refl_type: Cannot determine fortran type'
        else
            if (fortran_type == 'refl_type') then
                allocate(refl_type :: for_var)
            else if (fortran_type == 'srefl_type') then
                allocate(srefl_type :: for_var)
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refl_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,A%w,ierror)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,A%msf,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,A%miv,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,A%mivc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,A%sqmiv,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_refl_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_refl_type

    Module Subroutine list_to_type_array1d_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_refl_type

    Module Subroutine list_to_type_array1d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_refl_type_no_alloc

    Module Subroutine list_to_type_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_refl_type

    Module Subroutine list_to_type_array2d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_refl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_refl_type_no_alloc

    Module Subroutine Unwrap_class_srefl_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(srefl_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_srefl_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'srefl_type' &
                .and. fortran_type /= 'mrefl_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_srefl_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,for_var%fo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,for_var%fc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,for_var%sfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,for_var%phase,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,for_var%w,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,A%msf,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,A%miv,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,A%mivc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,A%sqmiv,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_srefl_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_srefl_type_no_alloc

    Module Subroutine List_to_class_array1d_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(srefl_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_srefl_type: Cannot determine fortran type'
            else if (fortran_type == 'srefl_type') then
                allocate(srefl_type :: arr(n))
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_srefl_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_srefl_type

    Module Subroutine List_to_class_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_srefl_type: Cannot determine fortran type'
                    else if (fortran_type == 'srefl_type') then
                        allocate(srefl_type :: arr(n,m))
                    else if (fortran_type == 'mrefl_type') then
                        allocate(mrefl_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_srefl_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_srefl_type

    Module Subroutine Unwrap_class_srefl_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(srefl_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_srefl_type: Cannot determine fortran type'
        else
            if (fortran_type == 'srefl_type') then
                allocate(srefl_type :: for_var)
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_srefl_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,for_var%fo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,for_var%fc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,for_var%sfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,for_var%phase,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,for_var%w,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,A%msf,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,A%miv,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,A%mivc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,A%sqmiv,ierror)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_srefl_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_srefl_type

    Module Subroutine list_to_type_array1d_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_srefl_type

    Module Subroutine list_to_type_array1d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_srefl_type_no_alloc

    Module Subroutine list_to_type_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_srefl_type

    Module Subroutine list_to_type_array2d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_srefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_srefl_type_no_alloc

    Module Subroutine Unwrap_class_mrefl_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(mrefl_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_mrefl_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'mrefl_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_mrefl_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,for_var%fo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,for_var%fc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,for_var%sfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,for_var%phase,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,for_var%w,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,for_var%mivo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,for_var%smivo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,for_var%msf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,for_var%miv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,for_var%mivc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_mrefl_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_mrefl_type_no_alloc

    Module Subroutine List_to_class_array1d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(mrefl_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_mrefl_type: Cannot determine fortran type'
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_mrefl_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_mrefl_type

    Module Subroutine List_to_class_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_mrefl_type: Cannot determine fortran type'
                    else if (fortran_type == 'mrefl_type') then
                        allocate(mrefl_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_mrefl_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_mrefl_type

    Module Subroutine Unwrap_class_mrefl_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(mrefl_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        complex, dimension(:), pointer :: p_complex_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_mrefl_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'mrefl_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_mrefl_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fo',p_real_1d,for_var%fo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','fc',p_real_1d,for_var%fc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','sfo',p_real_1d,for_var%sfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','phase',p_real_1d,for_var%phase,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_srefl_type','w',p_real_1d,for_var%w,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','hr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_refl_type','hr',p_real_1d,for_var%hr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,for_var%mivo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,for_var%smivo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','msf',p_complex_1d,for_var%msf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','miv',p_complex_1d,for_var%miv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivc',py_var,p_complex_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_mrefl_type','mivc',p_complex_1d,for_var%mivc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_mrefl_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_mrefl_type

    Module Subroutine list_to_type_array1d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_mrefl_type

    Module Subroutine list_to_type_array1d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_mrefl_type_no_alloc

    Module Subroutine list_to_type_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_mrefl_type

    Module Subroutine list_to_type_array2d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_mrefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_mrefl_type_no_alloc

    Module Subroutine Wrap_reflist_type(for_var,py_var,ierror)

        ! Arguments
        class(reflist_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_ref
        type(list) :: li_ref
        type(ndarray) :: nd_patts,nd_ptr,nd_pos,nd_hg,nd_hl,nd_fwhm,nd_eta,nd_alpha,nd_beta,nd_corr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','reflist_type')
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = py_var%setitem('i_ph',for_var%i_ph)
        if (allocated(for_var%ref)) then
            if (ierror == 0) ierror = list_create(li_ref)
            if (ierror == 0) then
                do i = 1 , size(for_var%ref)
                    ierror = dict_create(di_ref)
                    if (ierror == 0) call wrap_type(for_var%ref(i),di_ref,ierror)
                    if (ierror == 0) ierror = li_ref%append(di_ref)
                    if (ierror == 0) call di_ref%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('ref',li_ref)
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (refp_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','refp_type')
                    if (ierror == 0) ierror = py_var%setitem('n_patt',A%n_patt)
                    if (allocated(A%patts)) then
                        if (ierror == 0) ierror = ndarray_create(nd_patts,A%patts)
                        if (ierror == 0) ierror = py_var%setitem('patts',nd_patts)
                    end if
                    if (allocated(A%ptr)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ptr,A%ptr)
                        if (ierror == 0) ierror = py_var%setitem('ptr',nd_ptr)
                    end if
                    if (allocated(A%pos)) then
                        if (ierror == 0) ierror = ndarray_create(nd_pos,A%pos)
                        if (ierror == 0) ierror = py_var%setitem('pos',nd_pos)
                    end if
                    if (allocated(A%hg)) then
                        if (ierror == 0) ierror = ndarray_create(nd_hg,A%hg)
                        if (ierror == 0) ierror = py_var%setitem('hg',nd_hg)
                    end if
                    if (allocated(A%hl)) then
                        if (ierror == 0) ierror = ndarray_create(nd_hl,A%hl)
                        if (ierror == 0) ierror = py_var%setitem('hl',nd_hl)
                    end if
                    if (allocated(A%fwhm)) then
                        if (ierror == 0) ierror = ndarray_create(nd_fwhm,A%fwhm)
                        if (ierror == 0) ierror = py_var%setitem('fwhm',nd_fwhm)
                    end if
                    if (allocated(A%eta)) then
                        if (ierror == 0) ierror = ndarray_create(nd_eta,A%eta)
                        if (ierror == 0) ierror = py_var%setitem('eta',nd_eta)
                    end if
                    if (allocated(A%alpha)) then
                        if (ierror == 0) ierror = ndarray_create(nd_alpha,A%alpha)
                        if (ierror == 0) ierror = py_var%setitem('alpha',nd_alpha)
                    end if
                    if (allocated(A%beta)) then
                        if (ierror == 0) ierror = ndarray_create(nd_beta,A%beta)
                        if (ierror == 0) ierror = py_var%setitem('beta',nd_beta)
                    end if
                    if (allocated(A%corr)) then
                        if (ierror == 0) ierror = ndarray_create(nd_corr,A%corr)
                        if (ierror == 0) ierror = py_var%setitem('corr',nd_corr)
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_reflist_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_reflist_type

    Module Subroutine Unwrap_class_reflist_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(reflist_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
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
            err_cfml%msg  = 'Unwrap_reflist_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'reflist_type' &
                .and. fortran_type /= 'refp_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_reflist_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','i_ph',py_var,for_var%i_ph,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','ref',py_var,my_list,ierror2)
        if (ierror == 0) call list_to_class_array_refl_type('Unwrap_reflist_type','ref',my_list,for_var%ref,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (refp_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','n_patt',py_var,A%n_patt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','patts',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','patts',p_int_1d,A%patts,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','ptr',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','ptr',p_int_3d,A%ptr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','pos',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','pos',p_real_2d,A%pos,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hg',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hg',p_real_2d,A%hg,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hl',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hl',p_real_2d,A%hl,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','fwhm',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','fwhm',p_real_2d,A%fwhm,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','eta',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','eta',p_real_2d,A%eta,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','alpha',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','alpha',p_real_2d,A%alpha,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','beta',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','beta',p_real_2d,A%beta,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','corr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','corr',p_real_2d,A%corr,ierror,order)
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
                err_cfml%msg  = 'Unwrap_reflist_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_reflist_type_no_alloc

    Module Subroutine List_to_class_array1d_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(reflist_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_reflist_type: Cannot determine fortran type'
            else if (fortran_type == 'reflist_type') then
                allocate(reflist_type :: arr(n))
            else if (fortran_type == 'refp_type') then
                allocate(refp_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_reflist_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_reflist_type

    Module Subroutine List_to_class_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_reflist_type: Cannot determine fortran type'
                    else if (fortran_type == 'reflist_type') then
                        allocate(reflist_type :: arr(n,m))
                    else if (fortran_type == 'refp_type') then
                        allocate(refp_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_reflist_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_reflist_type

    Module Subroutine Unwrap_class_reflist_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(reflist_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
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
            err_cfml%msg  = 'Unwrap_reflist_type: Cannot determine fortran type'
        else
            if (fortran_type == 'reflist_type') then
                allocate(reflist_type :: for_var)
            else if (fortran_type == 'refp_type') then
                allocate(refp_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_reflist_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','i_ph',py_var,for_var%i_ph,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','ref',py_var,my_list,ierror2)
        if (ierror == 0) call list_to_class_array_refl_type('Unwrap_reflist_type','ref',my_list,for_var%ref,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (refp_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','n_patt',py_var,A%n_patt,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','patts',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','patts',p_int_1d,A%patts,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','ptr',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','ptr',p_int_3d,A%ptr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','pos',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','pos',p_real_2d,A%pos,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hg',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hg',p_real_2d,A%hg,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hl',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hl',p_real_2d,A%hl,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','fwhm',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','fwhm',p_real_2d,A%fwhm,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','eta',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','eta',p_real_2d,A%eta,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','alpha',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','alpha',p_real_2d,A%alpha,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','beta',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','beta',p_real_2d,A%beta,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','corr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','corr',p_real_2d,A%corr,ierror,order)
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
                err_cfml%msg  = 'Unwrap_reflist_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_reflist_type

    Module Subroutine list_to_type_array1d_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_reflist_type

    Module Subroutine list_to_type_array1d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_reflist_type_no_alloc

    Module Subroutine list_to_type_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_reflist_type

    Module Subroutine list_to_type_array2d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_reflist_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_reflist_type_no_alloc

    Module Subroutine Unwrap_class_refp_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refp_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
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
            err_cfml%msg  = 'Unwrap_refp_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'refp_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refp_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','i_ph',py_var,for_var%i_ph,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','ref',py_var,my_list,ierror2)
        if (ierror == 0) call list_to_class_array_refl_type('Unwrap_reflist_type','ref',my_list,for_var%ref,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','n_patt',py_var,for_var%n_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','patts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','patts',p_int_1d,for_var%patts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','ptr',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','ptr',p_int_3d,for_var%ptr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hg',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hg',p_real_2d,for_var%hg,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hl',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hl',p_real_2d,for_var%hl,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','fwhm',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','fwhm',p_real_2d,for_var%fwhm,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','eta',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','eta',p_real_2d,for_var%eta,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','alpha',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','alpha',p_real_2d,for_var%alpha,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','beta',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','beta',p_real_2d,for_var%beta,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','corr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','corr',p_real_2d,for_var%corr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_refp_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_refp_type_no_alloc

    Module Subroutine List_to_class_array1d_refp_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(refp_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_refp_type: Cannot determine fortran type'
            else if (fortran_type == 'refp_type') then
                allocate(refp_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_refp_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_refp_type

    Module Subroutine List_to_class_array2d_refp_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(refp_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_refp_type: Cannot determine fortran type'
                    else if (fortran_type == 'refp_type') then
                        allocate(refp_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_refp_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_refp_type

    Module Subroutine Unwrap_class_refp_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refp_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
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
            err_cfml%msg  = 'Unwrap_refp_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'refp_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refp_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','i_ph',py_var,for_var%i_ph,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','ref',py_var,my_list,ierror2)
        if (ierror == 0) call list_to_class_array_refl_type('Unwrap_reflist_type','ref',my_list,for_var%ref,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','n_patt',py_var,for_var%n_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','patts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','patts',p_int_1d,for_var%patts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','ptr',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','ptr',p_int_3d,for_var%ptr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hg',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hg',p_real_2d,for_var%hg,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','hl',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','hl',p_real_2d,for_var%hl,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','fwhm',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','fwhm',p_real_2d,for_var%fwhm,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','eta',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','eta',p_real_2d,for_var%eta,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','alpha',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','alpha',p_real_2d,for_var%alpha,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','beta',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','beta',p_real_2d,for_var%beta,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refp_type','corr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_refp_type','corr',p_real_2d,for_var%corr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_refp_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_refp_type

    Module Subroutine list_to_type_array1d_refp_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refp_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_refp_type

    Module Subroutine list_to_type_array1d_refp_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refp_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_refp_type_no_alloc

    Module Subroutine list_to_type_array2d_refp_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refp_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_refp_type

    Module Subroutine list_to_type_array2d_refp_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refp_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_refp_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_refp_type_no_alloc

end submodule