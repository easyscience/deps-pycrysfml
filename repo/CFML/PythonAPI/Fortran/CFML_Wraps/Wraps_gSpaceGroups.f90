submodule (CFML_Wraps) Wraps_gSpaceGroups

    implicit none
    contains

    Module Subroutine Wrap_symm_oper_type(for_var,py_var,ierror)

        ! Arguments
        type(symm_oper_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        real, dimension(:,:), allocatable :: mat_real
        type(ndarray) :: nd_mat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','symm_oper_type')
        if (ierror == 0) ierror = py_var%setitem('time_inv',for_var%time_inv)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (allocated(for_var%mat)) then
            if (ierror == 0) allocate(mat_real(size(for_var%mat,1),size(for_var%mat,2)))
            if (ierror == 0) mat_real = for_var%mat
            if (ierror == 0) ierror = ndarray_create(nd_mat,mat_real)
            if (ierror == 0) ierror = py_var%setitem('mat',nd_mat)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_symm_oper_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_symm_oper_type

    Module Subroutine Unwrap_type_symm_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(symm_oper_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_symm_oper_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'symm_oper_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_symm_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','time_inv',py_var,for_var%time_inv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','mat',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_symm_oper_type','mat',p_real_2d,for_var%mat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_symm_oper_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_symm_oper_type

    Module Subroutine list_to_type_array1d_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(symm_oper_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_symm_oper_type

    Module Subroutine list_to_type_array1d_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(symm_oper_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_symm_oper_type_no_alloc

    Module Subroutine list_to_type_array2d_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(symm_oper_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_symm_oper_type

    Module Subroutine list_to_type_array2d_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(symm_oper_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_symm_oper_type_no_alloc

    Module Subroutine Wrap_group_type(for_var,py_var,ierror)

        ! Arguments
        class(group_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        real, dimension(:), allocatable :: centre_coord_real
        real, dimension(:), allocatable :: anticentre_coord_real
        real, dimension(:,:), allocatable :: lat_tr_real
        real, dimension(:,:), allocatable :: alat_tr_real
        type(dict) :: di_op,di_symb_op,di_shu_lat
        type(list) :: li_op,li_symb_op,li_shu_lat
        type(ndarray) :: nd_inv,nd_centre_coord,nd_anticentre_coord,nd_lat_tr,nd_alat_tr,nd_kv,nd_kv_std,nd_sintlim,nd_nharm,nd_q_coeff,nd_rot,nd_m,nd_ep,nd_t,nd_ti

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','group_type')
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (ierror == 0) ierror = py_var%setitem('d',for_var%d)
        if (allocated(for_var%inv)) then
            if (ierror == 0) ierror = ndarray_create(nd_inv,for_var%inv)
            if (ierror == 0) ierror = py_var%setitem('inv',nd_inv)
        end if
        if (allocated(for_var%op)) then
            if (ierror == 0) ierror = list_create(li_op)
            if (ierror == 0) then
                do i = 1 , size(for_var%op)
                    ierror = dict_create(di_op)
                    if (ierror == 0) call wrap_type(for_var%op(i),di_op,ierror)
                    if (ierror == 0) ierror = li_op%append(di_op)
                    if (ierror == 0) call di_op%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('op',li_op)
        end if
        if (allocated(for_var%symb_op)) then
            if (ierror == 0) ierror = list_create(li_symb_op)
            if (ierror == 0) then
                do i = 1 , size(for_var%symb_op)
                    if (ierror == 0) then
                        ierror = li_symb_op%append(for_var%symb_op(i))
                        if (ierror /= 0) then
                            for_var%symb_op(i) = ''
                            ierror = li_symb_op%append(for_var%symb_op(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symb_op',li_symb_op)
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','spg_type')
                    if (ierror == 0) ierror = py_var%setitem('magnetic',A%magnetic)
                    if (ierror == 0) ierror = py_var%setitem('standard_setting',A%standard_setting)
                    if (ierror == 0) ierror = py_var%setitem('numspg',A%numspg)
                    if (ierror == 0) ierror = py_var%setitem('numshu',A%numshu)
                    if (ierror == 0) ierror = py_var%setitem('numops',A%numops)
                    if (ierror == 0) ierror = py_var%setitem('centred',A%centred)
                    if (ierror == 0) ierror = py_var%setitem('anticentred',A%anticentred)
                    if (ierror == 0) ierror = py_var%setitem('mag_type',A%mag_type)
                    if (ierror == 0) ierror = py_var%setitem('num_lat',A%num_lat)
                    if (ierror == 0) ierror = py_var%setitem('num_alat',A%num_alat)
                    if (ierror == 0) ierror = py_var%setitem('parent_num',A%parent_num)
                    if (ierror == 0) ierror = py_var%setitem('bravais_num',A%bravais_num)
                    if (ierror == 0) then
                        ierror = py_var%setitem('spg_lat',A%spg_lat)
                        if (ierror /= 0) then
                            A%spg_lat = ''
                            ierror = py_var%setitem('spg_lat',A%spg_lat)
                        end if
                    end if
                    if (ierror == 0) ierror = list_create(li_shu_lat)
                    if (ierror == 0) then
                        do i = 1 , size(A%shu_lat)
                            if (ierror == 0) then
                                ierror = li_shu_lat%append(A%shu_lat(i))
                                if (ierror /= 0) then
                                    A%shu_lat(i) = ''
                                    ierror = li_shu_lat%append(A%shu_lat(i))
                                end if
                            end if
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('shu_lat',li_shu_lat)
                    if (ierror == 0) then
                        ierror = py_var%setitem('init_label',A%init_label)
                        if (ierror /= 0) then
                            A%init_label = ''
                            ierror = py_var%setitem('init_label',A%init_label)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('parent_spg',A%parent_spg)
                        if (ierror /= 0) then
                            A%parent_spg = ''
                            ierror = py_var%setitem('parent_spg',A%parent_spg)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('tfrom_parent',A%tfrom_parent)
                        if (ierror /= 0) then
                            A%tfrom_parent = ''
                            ierror = py_var%setitem('tfrom_parent',A%tfrom_parent)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('centre',A%centre)
                        if (ierror /= 0) then
                            A%centre = ''
                            ierror = py_var%setitem('centre',A%centre)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('spg_symb',A%spg_symb)
                        if (ierror /= 0) then
                            A%spg_symb = ''
                            ierror = py_var%setitem('spg_symb',A%spg_symb)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('bns_num',A%bns_num)
                        if (ierror /= 0) then
                            A%bns_num = ''
                            ierror = py_var%setitem('bns_num',A%bns_num)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('og_num',A%og_num)
                        if (ierror /= 0) then
                            A%og_num = ''
                            ierror = py_var%setitem('og_num',A%og_num)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('bns_symb',A%bns_symb)
                        if (ierror /= 0) then
                            A%bns_symb = ''
                            ierror = py_var%setitem('bns_symb',A%bns_symb)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('og_symb',A%og_symb)
                        if (ierror /= 0) then
                            A%og_symb = ''
                            ierror = py_var%setitem('og_symb',A%og_symb)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('hall',A%hall)
                        if (ierror /= 0) then
                            A%hall = ''
                            ierror = py_var%setitem('hall',A%hall)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('uni',A%uni)
                        if (ierror /= 0) then
                            A%uni = ''
                            ierror = py_var%setitem('uni',A%uni)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('uni_num',A%uni_num)
                        if (ierror /= 0) then
                            A%uni_num = ''
                            ierror = py_var%setitem('uni_num',A%uni_num)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('crystalsys',A%crystalsys)
                        if (ierror /= 0) then
                            A%crystalsys = ''
                            ierror = py_var%setitem('crystalsys',A%crystalsys)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('pg',A%pg)
                        if (ierror /= 0) then
                            A%pg = ''
                            ierror = py_var%setitem('pg',A%pg)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('mag_pg',A%mag_pg)
                        if (ierror /= 0) then
                            A%mag_pg = ''
                            ierror = py_var%setitem('mag_pg',A%mag_pg)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('laue',A%laue)
                        if (ierror /= 0) then
                            A%laue = ''
                            ierror = py_var%setitem('laue',A%laue)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('setting',A%setting)
                        if (ierror /= 0) then
                            A%setting = ''
                            ierror = py_var%setitem('setting',A%setting)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('mat2std',A%mat2std)
                        if (ierror /= 0) then
                            A%mat2std = ''
                            ierror = py_var%setitem('mat2std',A%mat2std)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('mat2std_shu',A%mat2std_shu)
                        if (ierror /= 0) then
                            A%mat2std_shu = ''
                            ierror = py_var%setitem('mat2std_shu',A%mat2std_shu)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('matfrom',A%matfrom)
                        if (ierror /= 0) then
                            A%matfrom = ''
                            ierror = py_var%setitem('matfrom',A%matfrom)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('generators_list',A%generators_list)
                        if (ierror /= 0) then
                            A%generators_list = ''
                            ierror = py_var%setitem('generators_list',A%generators_list)
                        end if
                    end if
                    if (allocated(A%centre_coord)) then
                        if (ierror == 0) allocate(centre_coord_real(size(A%centre_coord)))
                        if (ierror == 0) centre_coord_real = A%centre_coord
                        if (ierror == 0) ierror = ndarray_create(nd_centre_coord,centre_coord_real)
                        if (ierror == 0) ierror = py_var%setitem('centre_coord',nd_centre_coord)
                    end if
                    if (allocated(A%anticentre_coord)) then
                        if (ierror == 0) allocate(anticentre_coord_real(size(A%anticentre_coord)))
                        if (ierror == 0) anticentre_coord_real = A%anticentre_coord
                        if (ierror == 0) ierror = ndarray_create(nd_anticentre_coord,anticentre_coord_real)
                        if (ierror == 0) ierror = py_var%setitem('anticentre_coord',nd_anticentre_coord)
                    end if
                    if (allocated(A%lat_tr)) then
                        if (ierror == 0) allocate(lat_tr_real(size(A%lat_tr,1),size(A%lat_tr,2)))
                        if (ierror == 0) lat_tr_real = A%lat_tr
                        if (ierror == 0) ierror = ndarray_create(nd_lat_tr,lat_tr_real)
                        if (ierror == 0) ierror = py_var%setitem('lat_tr',nd_lat_tr)
                    end if
                    if (allocated(A%alat_tr)) then
                        if (ierror == 0) allocate(alat_tr_real(size(A%alat_tr,1),size(A%alat_tr,2)))
                        if (ierror == 0) alat_tr_real = A%alat_tr
                        if (ierror == 0) ierror = ndarray_create(nd_alat_tr,alat_tr_real)
                        if (ierror == 0) ierror = py_var%setitem('alat_tr',nd_alat_tr)
                    end if
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','superspacegroup_type')
                    if (ierror == 0) ierror = py_var%setitem('nk',A%nk)
                    if (ierror == 0) ierror = py_var%setitem('nq',A%nq)
                    if (ierror == 0) then
                        ierror = py_var%setitem('ssg_symb',A%ssg_symb)
                        if (ierror /= 0) then
                            A%ssg_symb = ''
                            ierror = py_var%setitem('ssg_symb',A%ssg_symb)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('ssg_bravais',A%ssg_bravais)
                        if (ierror /= 0) then
                            A%ssg_bravais = ''
                            ierror = py_var%setitem('ssg_bravais',A%ssg_bravais)
                        end if
                    end if
                    if (ierror == 0) then
                        ierror = py_var%setitem('ssg_nlabel',A%ssg_nlabel)
                        if (ierror /= 0) then
                            A%ssg_nlabel = ''
                            ierror = py_var%setitem('ssg_nlabel',A%ssg_nlabel)
                        end if
                    end if
                    if (allocated(A%kv)) then
                        if (ierror == 0) ierror = ndarray_create(nd_kv,A%kv)
                        if (ierror == 0) ierror = py_var%setitem('kv',nd_kv)
                    end if
                    if (allocated(A%kv_std)) then
                        if (ierror == 0) ierror = ndarray_create(nd_kv_std,A%kv_std)
                        if (ierror == 0) ierror = py_var%setitem('kv_std',nd_kv_std)
                    end if
                    if (allocated(A%sintlim)) then
                        if (ierror == 0) ierror = ndarray_create(nd_sintlim,A%sintlim)
                        if (ierror == 0) ierror = py_var%setitem('sintlim',nd_sintlim)
                    end if
                    if (allocated(A%nharm)) then
                        if (ierror == 0) ierror = ndarray_create(nd_nharm,A%nharm)
                        if (ierror == 0) ierror = py_var%setitem('nharm',nd_nharm)
                    end if
                    if (allocated(A%q_coeff)) then
                        if (ierror == 0) ierror = ndarray_create(nd_q_coeff,A%q_coeff)
                        if (ierror == 0) ierror = py_var%setitem('q_coeff',nd_q_coeff)
                    end if
                    if (allocated(A%rot)) then
                        if (ierror == 0) ierror = ndarray_create(nd_rot,A%rot)
                        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
                    end if
                    if (allocated(A%m)) then
                        if (ierror == 0) ierror = ndarray_create(nd_m,A%m)
                        if (ierror == 0) ierror = py_var%setitem('m',nd_m)
                    end if
                    if (allocated(A%ep)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ep,A%ep)
                        if (ierror == 0) ierror = py_var%setitem('ep',nd_ep)
                    end if
                    if (allocated(A%t)) then
                        if (ierror == 0) ierror = ndarray_create(nd_t,A%t)
                        if (ierror == 0) ierror = py_var%setitem('t',nd_t)
                    end if
                    if (allocated(A%ti)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ti,A%ti)
                        if (ierror == 0) ierror = py_var%setitem('ti',nd_ti)
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_group_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_group_type

    Module Subroutine Unwrap_class_group_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(group_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_group_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'group_type' &
                .and. fortran_type /= 'spg_type' &
                .and. fortran_type /= 'superspacegroup_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_group_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,A%magnetic,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,A%standard_setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,A%numspg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,A%numshu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,A%numops,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,A%centred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,A%anticentred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,A%mag_type,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,A%num_lat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,A%num_alat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,A%parent_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,A%bravais_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,A%spg_lat,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,A%shu_lat,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,A%init_label,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,A%parent_spg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,A%tfrom_parent,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,A%centre,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,A%spg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,A%bns_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,A%og_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,A%bns_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,A%og_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,A%hall,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,A%uni,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,A%uni_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,A%crystalsys,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,A%pg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,A%mag_pg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,A%laue,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,A%setting,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,A%mat2std,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,A%mat2std_shu,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,A%matfrom,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,A%generators_list,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,A%centre_coord,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,A%anticentre_coord,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,A%lat_tr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,A%alat_tr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
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
                err_cfml%msg  = 'Unwrap_group_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_group_type_no_alloc

    Module Subroutine List_to_class_array1d_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(group_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_group_type: Cannot determine fortran type'
            else if (fortran_type == 'group_type') then
                allocate(group_type :: arr(n))
            else if (fortran_type == 'spg_type') then
                allocate(spg_type :: arr(n))
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_group_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_group_type

    Module Subroutine List_to_class_array2d_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(group_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_group_type: Cannot determine fortran type'
                    else if (fortran_type == 'group_type') then
                        allocate(group_type :: arr(n,m))
                    else if (fortran_type == 'spg_type') then
                        allocate(spg_type :: arr(n,m))
                    else if (fortran_type == 'superspacegroup_type') then
                        allocate(superspacegroup_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_group_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_group_type

    Module Subroutine Unwrap_class_group_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(group_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_group_type: Cannot determine fortran type'
        else
            if (fortran_type == 'group_type') then
                allocate(group_type :: for_var)
            else if (fortran_type == 'spg_type') then
                allocate(spg_type :: for_var)
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_group_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,A%magnetic,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,A%standard_setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,A%numspg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,A%numshu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,A%numops,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,A%centred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,A%anticentred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,A%mag_type,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,A%num_lat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,A%num_alat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,A%parent_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,A%bravais_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,A%spg_lat,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,A%shu_lat,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,A%init_label,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,A%parent_spg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,A%tfrom_parent,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,A%centre,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,A%spg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,A%bns_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,A%og_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,A%bns_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,A%og_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,A%hall,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,A%uni,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,A%uni_num,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,A%crystalsys,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,A%pg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,A%mag_pg,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,A%laue,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,A%setting,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,A%mat2std,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,A%mat2std_shu,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,A%matfrom,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,A%generators_list,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,A%centre_coord,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,A%anticentre_coord,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,A%lat_tr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,A%alat_tr,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
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
                err_cfml%msg  = 'Unwrap_group_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_group_type

    Module Subroutine list_to_type_array1d_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(group_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_group_type

    Module Subroutine list_to_type_array1d_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(group_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_group_type_no_alloc

    Module Subroutine list_to_type_array2d_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(group_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_group_type

    Module Subroutine list_to_type_array2d_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(group_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_group_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_group_type_no_alloc

    Module Subroutine Wrap_rot_mat_type(for_var,py_var,ierror)

        ! Arguments
        type(rot_mat_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rot

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','rot_mat_type')
        if (ierror == 0) ierror = py_var%setitem('numops',for_var%numops)
        if (allocated(for_var%rot)) then
            if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
            if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_rot_mat_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_rot_mat_type

    Module Subroutine Unwrap_type_rot_mat_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(rot_mat_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:,:), pointer :: p_int_3d
        character(len=1) :: order

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_rot_mat_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'rot_mat_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_rot_mat_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_rot_mat_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_rot_mat_type','rot',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_rot_mat_type','rot',p_int_3d,for_var%rot,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_rot_mat_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_rot_mat_type

    Module Subroutine list_to_type_array1d_rot_mat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(rot_mat_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_rot_mat_type

    Module Subroutine list_to_type_array1d_rot_mat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(rot_mat_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_rot_mat_type_no_alloc

    Module Subroutine list_to_type_array2d_rot_mat_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(rot_mat_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_rot_mat_type

    Module Subroutine list_to_type_array2d_rot_mat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(rot_mat_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_rot_mat_type_no_alloc

    Module Subroutine Unwrap_class_spg_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(spg_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_spg_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'spg_type' &
                .and. fortran_type /= 'superspacegroup_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_spg_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,for_var%standard_setting,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,for_var%numspg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,for_var%numshu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,for_var%anticentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,for_var%mag_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,for_var%num_alat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,for_var%bravais_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,for_var%spg_lat,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%shu_lat,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,for_var%init_label,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,for_var%tfrom_parent,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,for_var%centre,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,for_var%spg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,for_var%bns_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,for_var%og_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,for_var%bns_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,for_var%og_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,for_var%uni,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,for_var%uni_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,for_var%crystalsys,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,for_var%mag_pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,for_var%setting,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,for_var%mat2std,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,for_var%mat2std_shu,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,for_var%matfrom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,for_var%generators_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,for_var%centre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,for_var%anticentre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,for_var%lat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,for_var%alat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
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
                err_cfml%msg  = 'Unwrap_spg_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_spg_type_no_alloc

    Module Subroutine List_to_class_array1d_spg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(spg_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_spg_type: Cannot determine fortran type'
            else if (fortran_type == 'spg_type') then
                allocate(spg_type :: arr(n))
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_spg_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_spg_type

    Module Subroutine List_to_class_array2d_spg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(spg_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_spg_type: Cannot determine fortran type'
                    else if (fortran_type == 'spg_type') then
                        allocate(spg_type :: arr(n,m))
                    else if (fortran_type == 'superspacegroup_type') then
                        allocate(superspacegroup_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_spg_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_spg_type

    Module Subroutine Unwrap_class_spg_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(spg_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_spg_type: Cannot determine fortran type'
        else
            if (fortran_type == 'spg_type') then
                allocate(spg_type :: for_var)
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_spg_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,for_var%standard_setting,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,for_var%numspg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,for_var%numshu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,for_var%anticentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,for_var%mag_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,for_var%num_alat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,for_var%bravais_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,for_var%spg_lat,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%shu_lat,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,for_var%init_label,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,for_var%tfrom_parent,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,for_var%centre,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,for_var%spg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,for_var%bns_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,for_var%og_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,for_var%bns_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,for_var%og_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,for_var%uni,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,for_var%uni_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,for_var%crystalsys,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,for_var%mag_pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,for_var%setting,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,for_var%mat2std,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,for_var%mat2std_shu,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,for_var%matfrom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,for_var%generators_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,for_var%centre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,for_var%anticentre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,for_var%lat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,for_var%alat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
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
                err_cfml%msg  = 'Unwrap_spg_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_spg_type

    Module Subroutine list_to_type_array1d_spg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spg_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_spg_type

    Module Subroutine list_to_type_array1d_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spg_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_spg_type_no_alloc

    Module Subroutine list_to_type_array2d_spg_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spg_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_spg_type

    Module Subroutine list_to_type_array2d_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spg_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_spg_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_spg_type_no_alloc

    Module Subroutine Unwrap_class_superspacegroup_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(superspacegroup_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_superspacegroup_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'superspacegroup_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_superspacegroup_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,for_var%standard_setting,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,for_var%numspg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,for_var%numshu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,for_var%anticentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,for_var%mag_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,for_var%num_alat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,for_var%bravais_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,for_var%spg_lat,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%shu_lat,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,for_var%init_label,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,for_var%tfrom_parent,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,for_var%centre,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,for_var%spg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,for_var%bns_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,for_var%og_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,for_var%bns_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,for_var%og_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,for_var%uni,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,for_var%uni_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,for_var%crystalsys,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,for_var%mag_pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,for_var%setting,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,for_var%mat2std,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,for_var%mat2std_shu,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,for_var%matfrom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,for_var%generators_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,for_var%centre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,for_var%anticentre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,for_var%lat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,for_var%alat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,for_var%nk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,for_var%nq,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,for_var%ssg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,for_var%ssg_bravais,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,for_var%ssg_nlabel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,for_var%kv,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,for_var%kv_std,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,for_var%sintlim,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,for_var%nharm,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,for_var%q_coeff,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,for_var%rot,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,for_var%m,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,for_var%ep,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,for_var%t,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,for_var%ti,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_superspacegroup_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_superspacegroup_type_no_alloc

    Module Subroutine List_to_class_array1d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(superspacegroup_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_superspacegroup_type: Cannot determine fortran type'
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_superspacegroup_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_superspacegroup_type

    Module Subroutine List_to_class_array2d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(superspacegroup_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_superspacegroup_type: Cannot determine fortran type'
                    else if (fortran_type == 'superspacegroup_type') then
                        allocate(superspacegroup_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_superspacegroup_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_superspacegroup_type

    Module Subroutine Unwrap_class_superspacegroup_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(superspacegroup_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
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
            err_cfml%msg  = 'Unwrap_superspacegroup_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'superspacegroup_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_superspacegroup_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,for_var%standard_setting,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,for_var%numspg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,for_var%numshu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,for_var%anticentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,for_var%mag_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,for_var%num_alat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,for_var%bravais_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,for_var%spg_lat,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
        if (ierror == 0) call list_to_no_alloc_array_primitive(my_list,for_var%shu_lat,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','init_label',py_var,for_var%init_label,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','tfrom_parent',py_var,for_var%tfrom_parent,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','centre',py_var,for_var%centre,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','spg_symb',py_var,for_var%spg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_num',py_var,for_var%bns_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_num',py_var,for_var%og_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','bns_symb',py_var,for_var%bns_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','og_symb',py_var,for_var%og_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni',py_var,for_var%uni,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','uni_num',py_var,for_var%uni_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','crystalsys',py_var,for_var%crystalsys,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mag_pg',py_var,for_var%mag_pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','setting',py_var,for_var%setting,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std',py_var,for_var%mat2std,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','mat2std_shu',py_var,for_var%mat2std_shu,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','matfrom',py_var,for_var%matfrom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spg_type','generators_list',py_var,for_var%generators_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','centre_coord',p_real_1d,for_var%centre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','anticentre_coord',p_real_1d,for_var%anticentre_coord,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','lat_tr',p_real_2d,for_var%lat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spg_type','alat_tr',p_real_2d,for_var%alat_tr,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,for_var%nk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,for_var%nq,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_symb',py_var,for_var%ssg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_bravais',py_var,for_var%ssg_bravais,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_superspacegroup_type','ssg_nlabel',py_var,for_var%ssg_nlabel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv',p_real_2d,for_var%kv,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','kv_std',p_real_2d,for_var%kv_std,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','sintlim',p_real_1d,for_var%sintlim,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','nharm',p_int_1d,for_var%nharm,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','q_coeff',p_int_2d,for_var%q_coeff,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','rot',p_int_3d,for_var%rot,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','m',p_int_3d,for_var%m,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ep',p_int_3d,for_var%ep,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','t',p_real_2d,for_var%t,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_superspacegroup_type','ti',p_real_2d,for_var%ti,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_superspacegroup_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_superspacegroup_type

    Module Subroutine list_to_type_array1d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(superspacegroup_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_superspacegroup_type

    Module Subroutine list_to_type_array1d_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(superspacegroup_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_superspacegroup_type_no_alloc

    Module Subroutine list_to_type_array2d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(superspacegroup_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_superspacegroup_type

    Module Subroutine list_to_type_array2d_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(superspacegroup_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_superspacegroup_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_superspacegroup_type_no_alloc

    Module Subroutine Wrap_spin_operator_type(for_var,py_var,ierror)

        ! Arguments
        type(spin_operator_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_umat,nd_rot,nd_tr

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','spin_operator_type')
        if (ierror == 0) ierror = py_var%setitem('time_inv',for_var%time_inv)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (ierror == 0) ierror = ndarray_create(nd_umat,for_var%umat)
        if (ierror == 0) ierror = py_var%setitem('umat',nd_umat)
        if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        if (ierror == 0) ierror = ndarray_create(nd_tr,for_var%tr)
        if (ierror == 0) ierror = py_var%setitem('tr',nd_tr)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_spin_operator_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_spin_operator_type

    Module Subroutine Unwrap_type_spin_operator_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(spin_operator_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
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
            err_cfml%msg  = 'Unwrap_spin_operator_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'spin_operator_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_spin_operator_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_operator_type','time_inv',py_var,for_var%time_inv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_operator_type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_operator_type','umat',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_spin_operator_type','umat',p_real_2d,for_var%umat,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_operator_type','rot',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_spin_operator_type','rot',p_int_2d,for_var%rot,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_operator_type','tr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_spin_operator_type','tr',p_real_1d,for_var%tr,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_spin_operator_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_spin_operator_type

    Module Subroutine list_to_type_array1d_spin_operator_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_operator_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_spin_operator_type

    Module Subroutine list_to_type_array1d_spin_operator_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_operator_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_spin_operator_type_no_alloc

    Module Subroutine list_to_type_array2d_spin_operator_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_operator_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_spin_operator_type

    Module Subroutine list_to_type_array2d_spin_operator_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_operator_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_spin_operator_type_no_alloc

    Module Subroutine Wrap_spin_group_type(for_var,py_var,ierror)

        ! Arguments
        type(spin_group_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_op,di_symb_op
        type(list) :: li_op,li_symb_op
        type(ndarray) :: nd_inv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','spin_group_type')
        if (ierror == 0) then
            ierror = py_var%setitem('basis',for_var%basis)
            if (ierror /= 0) then
                for_var%basis = ''
                ierror = py_var%setitem('basis',for_var%basis)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (allocated(for_var%inv)) then
            if (ierror == 0) ierror = ndarray_create(nd_inv,for_var%inv)
            if (ierror == 0) ierror = py_var%setitem('inv',nd_inv)
        end if
        if (allocated(for_var%op)) then
            if (ierror == 0) ierror = list_create(li_op)
            if (ierror == 0) then
                do i = 1 , size(for_var%op)
                    ierror = dict_create(di_op)
                    if (ierror == 0) call wrap_type(for_var%op(i),di_op,ierror)
                    if (ierror == 0) ierror = li_op%append(di_op)
                    if (ierror == 0) call di_op%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('op',li_op)
        end if
        if (allocated(for_var%symb_op)) then
            if (ierror == 0) ierror = list_create(li_symb_op)
            if (ierror == 0) then
                do i = 1 , size(for_var%symb_op)
                    if (ierror == 0) then
                        ierror = li_symb_op%append(for_var%symb_op(i))
                        if (ierror /= 0) then
                            for_var%symb_op(i) = ''
                            ierror = li_symb_op%append(for_var%symb_op(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('symb_op',li_symb_op)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_spin_group_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_spin_group_type

    Module Subroutine Unwrap_type_spin_group_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(spin_group_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_spin_group_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'spin_group_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_spin_group_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_spin_group_type','basis',py_var,for_var%basis,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_group_type','inv',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_spin_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_group_type','op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_spin_group_type','op',my_list,for_var%op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_spin_group_type','symb_op',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%symb_op,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_spin_group_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_spin_group_type

    Module Subroutine list_to_type_array1d_spin_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_group_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_spin_group_type

    Module Subroutine list_to_type_array1d_spin_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_group_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_spin_group_type_no_alloc

    Module Subroutine list_to_type_array2d_spin_group_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_group_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_spin_group_type

    Module Subroutine list_to_type_array2d_spin_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(spin_group_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_spin_group_type_no_alloc

    Module Subroutine Wrap_kvect_info_type(for_var,py_var,ierror)

        ! Arguments
        type(kvect_info_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_kv,nd_kv_std,nd_sintlim,nd_nharm,nd_q_coeff

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','kvect_info_type')
        if (ierror == 0) ierror = py_var%setitem('nk',for_var%nk)
        if (allocated(for_var%kv)) then
            if (ierror == 0) ierror = ndarray_create(nd_kv,for_var%kv)
            if (ierror == 0) ierror = py_var%setitem('kv',nd_kv)
        end if
        if (allocated(for_var%kv_std)) then
            if (ierror == 0) ierror = ndarray_create(nd_kv_std,for_var%kv_std)
            if (ierror == 0) ierror = py_var%setitem('kv_std',nd_kv_std)
        end if
        if (allocated(for_var%sintlim)) then
            if (ierror == 0) ierror = ndarray_create(nd_sintlim,for_var%sintlim)
            if (ierror == 0) ierror = py_var%setitem('sintlim',nd_sintlim)
        end if
        if (allocated(for_var%nharm)) then
            if (ierror == 0) ierror = ndarray_create(nd_nharm,for_var%nharm)
            if (ierror == 0) ierror = py_var%setitem('nharm',nd_nharm)
        end if
        if (ierror == 0) ierror = py_var%setitem('nq',for_var%nq)
        if (allocated(for_var%q_coeff)) then
            if (ierror == 0) ierror = ndarray_create(nd_q_coeff,for_var%q_coeff)
            if (ierror == 0) ierror = py_var%setitem('q_coeff',nd_q_coeff)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_kvect_info_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_kvect_info_type

    Module Subroutine Unwrap_type_kvect_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(kvect_info_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_kvect_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'kvect_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_kvect_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nk',py_var,for_var%nk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','kv',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_kvect_info_type','kv',p_real_2d,for_var%kv,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','kv_std',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_kvect_info_type','kv_std',p_real_2d,for_var%kv_std,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','sintlim',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_kvect_info_type','sintlim',p_real_1d,for_var%sintlim,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nharm',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_kvect_info_type','nharm',p_int_1d,for_var%nharm,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nq',py_var,for_var%nq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','q_coeff',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_kvect_info_type','q_coeff',p_int_2d,for_var%q_coeff,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_kvect_info_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_kvect_info_type

    Module Subroutine list_to_type_array1d_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(kvect_info_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_kvect_info_type

    Module Subroutine list_to_type_array1d_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(kvect_info_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_kvect_info_type_no_alloc

    Module Subroutine list_to_type_array2d_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(kvect_info_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_kvect_info_type

    Module Subroutine list_to_type_array2d_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(kvect_info_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_kvect_info_type_no_alloc

    Module Subroutine Wrap_point_orbit(for_var,py_var,ierror)

        ! Arguments
        class(point_orbit), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_lab
        type(list) :: li_lab
        type(ndarray) :: nd_pos,nd_pos_c,nd_mom,nd_mom_c,nd_pts,nd_lat,nd_ls,nd_latt

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','point_orbit')
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (allocated(for_var%pos)) then
            if (ierror == 0) ierror = ndarray_create(nd_pos,for_var%pos)
            if (ierror == 0) ierror = py_var%setitem('pos',nd_pos)
        end if
        if (allocated(for_var%pos_c)) then
            if (ierror == 0) ierror = ndarray_create(nd_pos_c,for_var%pos_c)
            if (ierror == 0) ierror = py_var%setitem('pos_c',nd_pos_c)
        end if
        if (allocated(for_var%mom)) then
            if (ierror == 0) ierror = ndarray_create(nd_mom,for_var%mom)
            if (ierror == 0) ierror = py_var%setitem('mom',nd_mom)
        end if
        if (allocated(for_var%mom_c)) then
            if (ierror == 0) ierror = ndarray_create(nd_mom_c,for_var%mom_c)
            if (ierror == 0) ierror = py_var%setitem('mom_c',nd_mom_c)
        end if
        if (allocated(for_var%pts)) then
            if (ierror == 0) ierror = ndarray_create(nd_pts,for_var%pts)
            if (ierror == 0) ierror = py_var%setitem('pts',nd_pts)
        end if
        if (allocated(for_var%lat)) then
            if (ierror == 0) ierror = ndarray_create(nd_lat,for_var%lat)
            if (ierror == 0) ierror = py_var%setitem('lat',nd_lat)
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (orbit_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','orbit_type')
                    if (ierror == 0) then
                        ierror = py_var%setitem('chemsymb',A%chemsymb)
                        if (ierror /= 0) then
                            A%chemsymb = ''
                            ierror = py_var%setitem('chemsymb',A%chemsymb)
                        end if
                    end if
                    if (allocated(A%lab)) then
                        if (ierror == 0) ierror = list_create(li_lab)
                        if (ierror == 0) then
                            do i = 1 , size(A%lab)
                                if (ierror == 0) then
                                    ierror = li_lab%append(A%lab(i))
                                    if (ierror /= 0) then
                                        A%lab(i) = ''
                                        ierror = li_lab%append(A%lab(i))
                                    end if
                                end if
                            end do
                        end if
                        if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
                    end if
                    if (allocated(A%ls)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ls,A%ls)
                        if (ierror == 0) ierror = py_var%setitem('ls',nd_ls)
                    end if
                    if (allocated(A%latt)) then
                        if (ierror == 0) ierror = ndarray_create(nd_latt,A%latt)
                        if (ierror == 0) ierror = py_var%setitem('latt',nd_latt)
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_point_orbit: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_point_orbit

    Module Subroutine Unwrap_class_point_orbit_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(point_orbit), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_orbit_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'point_orbit' &
                .and. fortran_type /= 'orbit_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_orbit_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos_c',p_real_2d,for_var%pos_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom_c',p_real_2d,for_var%mom_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','lat',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','lat',p_int_2d,for_var%lat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (orbit_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','chemsymb',py_var,A%chemsymb,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','lab',py_var,my_list,ierror2)
                    if (ierror2 == 0) then
                        if (ierror == 0) call list_to_alloc_array_primitive(my_list,A%lab,ierror)
                    else
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','ls',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','ls',p_int_1d,A%ls,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','latt',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','latt',p_int_2d,A%latt,ierror,order)
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
                err_cfml%msg  = 'Unwrap_point_orbit_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_point_orbit_no_alloc

    Module Subroutine List_to_class_array1d_point_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(point_orbit), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_point_orbit: Cannot determine fortran type'
            else if (fortran_type == 'point_orbit') then
                allocate(point_orbit :: arr(n))
            else if (fortran_type == 'orbit_type') then
                allocate(orbit_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_point_orbit: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_point_orbit

    Module Subroutine List_to_class_array2d_point_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(point_orbit), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_point_orbit: Cannot determine fortran type'
                    else if (fortran_type == 'point_orbit') then
                        allocate(point_orbit :: arr(n,m))
                    else if (fortran_type == 'orbit_type') then
                        allocate(orbit_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_point_orbit: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_point_orbit

    Module Subroutine Unwrap_class_point_orbit(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(point_orbit), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_orbit: Cannot determine fortran type'
        else
            if (fortran_type == 'point_orbit') then
                allocate(point_orbit :: for_var)
            else if (fortran_type == 'orbit_type') then
                allocate(orbit_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_orbit: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos_c',p_real_2d,for_var%pos_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom_c',p_real_2d,for_var%mom_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','lat',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','lat',p_int_2d,for_var%lat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (orbit_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','chemsymb',py_var,A%chemsymb,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','lab',py_var,my_list,ierror2)
                    if (ierror2 == 0) then
                        if (ierror == 0) call list_to_alloc_array_primitive(my_list,A%lab,ierror)
                    else
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','ls',py_var,p_int_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','ls',p_int_1d,A%ls,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','latt',py_var,p_int_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','latt',p_int_2d,A%latt,ierror,order)
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
                err_cfml%msg  = 'Unwrap_point_orbit: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_point_orbit

    Module Subroutine list_to_type_array1d_point_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_point_orbit

    Module Subroutine list_to_type_array1d_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_point_orbit_no_alloc

    Module Subroutine list_to_type_array2d_point_orbit(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_point_orbit

    Module Subroutine list_to_type_array2d_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_orbit), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_point_orbit_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_point_orbit_no_alloc

    Module Subroutine Unwrap_class_orbit_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(orbit_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_orbit_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'orbit_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_orbit_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos_c',p_real_2d,for_var%pos_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom_c',p_real_2d,for_var%mom_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','lat',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','lat',p_int_2d,for_var%lat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','lab',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%lab,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','ls',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','ls',p_int_1d,for_var%ls,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_orbit_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_orbit_type_no_alloc

    Module Subroutine List_to_class_array1d_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(orbit_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_orbit_type: Cannot determine fortran type'
            else if (fortran_type == 'orbit_type') then
                allocate(orbit_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_orbit_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_orbit_type

    Module Subroutine List_to_class_array2d_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(orbit_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_orbit_type: Cannot determine fortran type'
                    else if (fortran_type == 'orbit_type') then
                        allocate(orbit_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_orbit_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_orbit_type

    Module Subroutine Unwrap_class_orbit_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(orbit_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_orbit_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'orbit_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_orbit_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pos_c',p_real_2d,for_var%pos_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom_c',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','mom_c',p_real_2d,for_var%mom_c,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pts',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','pts',p_int_1d,for_var%pts,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','lat',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_point_orbit','lat',p_int_2d,for_var%lat,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','lab',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%lab,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','ls',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','ls',p_int_1d,for_var%ls,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_type','latt',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_orbit_type','latt',p_int_2d,for_var%latt,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_orbit_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_orbit_type

    Module Subroutine list_to_type_array1d_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_orbit_type

    Module Subroutine list_to_type_array1d_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_orbit_type_no_alloc

    Module Subroutine list_to_type_array2d_orbit_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_orbit_type

    Module Subroutine list_to_type_array2d_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_orbit_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_orbit_type_no_alloc

    Module Subroutine Wrap_orbit_list(for_var,py_var,ierror)

        ! Arguments
        type(orbit_list), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_orbit
        type(list) :: li_orbit

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','orbit_list')
        if (ierror == 0) ierror = py_var%setitem('num_orbs',for_var%num_orbs)
        if (allocated(for_var%orbit)) then
            if (ierror == 0) ierror = list_create(li_orbit)
            if (ierror == 0) then
                do i = 1 , size(for_var%orbit)
                    ierror = dict_create(di_orbit)
                    if (ierror == 0) call wrap_type(for_var%orbit(i),di_orbit,ierror)
                    if (ierror == 0) ierror = li_orbit%append(di_orbit)
                    if (ierror == 0) call di_orbit%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('orbit',li_orbit)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_orbit_list: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_orbit_list

    Module Subroutine Unwrap_type_orbit_list(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(orbit_list), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_orbit_list: Cannot determine fortran type'
        else
            if (fortran_type /= 'orbit_list') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_orbit_list: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_list','num_orbs',py_var,for_var%num_orbs,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_orbit_list','orbit',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_orbit_list','orbit',my_list,for_var%orbit,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_orbit_list: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_orbit_list

    Module Subroutine list_to_type_array1d_orbit_list(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_list), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_orbit_list

    Module Subroutine list_to_type_array1d_orbit_list_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_list), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_orbit_list_no_alloc

    Module Subroutine list_to_type_array2d_orbit_list(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_list), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_orbit_list

    Module Subroutine list_to_type_array2d_orbit_list_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(orbit_list), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_orbit_list_no_alloc

end submodule