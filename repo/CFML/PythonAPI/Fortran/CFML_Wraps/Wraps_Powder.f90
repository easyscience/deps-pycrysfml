submodule (CFML_Wraps) Wraps_Powder

    implicit none
    contains

    Module Subroutine Wrap_irf_type(for_var,py_var,ierror)

        ! Arguments
        class(irf_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_shift,nd_corr_int,nd_lambda,nd_u_i,nd_v_i,nd_w_i,nd_x_i,nd_y_i,nd_z_i,nd_ttheta,nd_hg,nd_hl,nd_tof,nd_sigma,nd_gamma,nd_alpha,nd_beta

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','irf_type')
        if (ierror == 0) then
            ierror = py_var%setitem('nam',for_var%nam)
            if (ierror /= 0) then
                for_var%nam = ''
                ierror = py_var%setitem('nam',for_var%nam)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('geom',for_var%geom)
            if (ierror /= 0) then
                for_var%geom = ''
                ierror = py_var%setitem('geom',for_var%geom)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('file_nam',for_var%file_nam)
            if (ierror /= 0) then
                for_var%file_nam = ''
                ierror = py_var%setitem('file_nam',for_var%file_nam)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('jobt',for_var%jobt)
            if (ierror /= 0) then
                for_var%jobt = ''
                ierror = py_var%setitem('jobt',for_var%jobt)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('irftype',for_var%irftype)
            if (ierror /= 0) then
                for_var%irftype = ''
                ierror = py_var%setitem('irftype',for_var%irftype)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('tmin',for_var%tmin)
        if (ierror == 0) ierror = py_var%setitem('step',for_var%step)
        if (ierror == 0) ierror = py_var%setitem('tmax',for_var%tmax)
        if (ierror == 0) ierror = py_var%setitem('num_patt',for_var%num_patt)
        if (ierror == 0) ierror = py_var%setitem('n_points',for_var%n_points)
        if (ierror == 0) ierror = py_var%setitem('n_items',for_var%n_items)
        if (allocated(for_var%shift)) then
            if (ierror == 0) ierror = ndarray_create(nd_shift,for_var%shift)
            if (ierror == 0) ierror = py_var%setitem('shift',nd_shift)
        end if
        if (allocated(for_var%corr_int)) then
            if (ierror == 0) ierror = ndarray_create(nd_corr_int,for_var%corr_int)
            if (ierror == 0) ierror = py_var%setitem('corr_int',nd_corr_int)
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (irf_cw_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','irf_cw_type')
                    if (ierror == 0) ierror = ndarray_create(nd_lambda,A%lambda)
                    if (ierror == 0) ierror = py_var%setitem('lambda',nd_lambda)
                    if (ierror == 0) ierror = py_var%setitem('ratio',A%ratio)
                    if (ierror == 0) ierror = py_var%setitem('rkk',A%rkk)
                    if (ierror == 0) ierror = py_var%setitem('alpsd',A%alpsd)
                    if (ierror == 0) ierror = py_var%setitem('cthm',A%cthm)
                    if (ierror == 0) ierror = ndarray_create(nd_u_i,A%u_i)
                    if (ierror == 0) ierror = py_var%setitem('u_i',nd_u_i)
                    if (ierror == 0) ierror = ndarray_create(nd_v_i,A%v_i)
                    if (ierror == 0) ierror = py_var%setitem('v_i',nd_v_i)
                    if (ierror == 0) ierror = ndarray_create(nd_w_i,A%w_i)
                    if (ierror == 0) ierror = py_var%setitem('w_i',nd_w_i)
                    if (ierror == 0) ierror = ndarray_create(nd_x_i,A%x_i)
                    if (ierror == 0) ierror = py_var%setitem('x_i',nd_x_i)
                    if (ierror == 0) ierror = ndarray_create(nd_y_i,A%y_i)
                    if (ierror == 0) ierror = py_var%setitem('y_i',nd_y_i)
                    if (ierror == 0) ierror = ndarray_create(nd_z_i,A%z_i)
                    if (ierror == 0) ierror = py_var%setitem('z_i',nd_z_i)
                    if (ierror == 0) ierror = py_var%setitem('sl_i',A%sl_i)
                    if (ierror == 0) ierror = py_var%setitem('dl_i',A%dl_i)
                    if (allocated(A%ttheta)) then
                        if (ierror == 0) ierror = ndarray_create(nd_ttheta,A%ttheta)
                        if (ierror == 0) ierror = py_var%setitem('ttheta',nd_ttheta)
                    end if
                    if (allocated(A%hg)) then
                        if (ierror == 0) ierror = ndarray_create(nd_hg,A%hg)
                        if (ierror == 0) ierror = py_var%setitem('hg',nd_hg)
                    end if
                    if (allocated(A%hl)) then
                        if (ierror == 0) ierror = ndarray_create(nd_hl,A%hl)
                        if (ierror == 0) ierror = py_var%setitem('hl',nd_hl)
                    end if
                class is (irf_tof_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','irf_tof_type')
                    if (ierror == 0) ierror = py_var%setitem('toftet_i',A%toftet_i)
                    if (ierror == 0) ierror = py_var%setitem('zero_i',A%zero_i)
                    if (ierror == 0) ierror = py_var%setitem('zerot_i',A%zerot_i)
                    if (ierror == 0) ierror = py_var%setitem('dtt1_i',A%dtt1_i)
                    if (ierror == 0) ierror = py_var%setitem('dtt2_i',A%dtt2_i)
                    if (ierror == 0) ierror = py_var%setitem('dtt_1overd_i',A%dtt_1overd_i)
                    if (ierror == 0) ierror = py_var%setitem('dtt1t_i',A%dtt1t_i)
                    if (ierror == 0) ierror = py_var%setitem('dtt2t_i',A%dtt2t_i)
                    if (ierror == 0) ierror = py_var%setitem('alfa0_i',A%alfa0_i)
                    if (ierror == 0) ierror = py_var%setitem('alfa1_i',A%alfa1_i)
                    if (ierror == 0) ierror = py_var%setitem('alfaq_i',A%alfaq_i)
                    if (ierror == 0) ierror = py_var%setitem('beta0_i',A%beta0_i)
                    if (ierror == 0) ierror = py_var%setitem('betaq_i',A%betaq_i)
                    if (ierror == 0) ierror = py_var%setitem('beta1_i',A%beta1_i)
                    if (ierror == 0) ierror = py_var%setitem('alfa0t_i',A%alfa0t_i)
                    if (ierror == 0) ierror = py_var%setitem('alfa1t_i',A%alfa1t_i)
                    if (ierror == 0) ierror = py_var%setitem('beta0t_i',A%beta0t_i)
                    if (ierror == 0) ierror = py_var%setitem('beta1t_i',A%beta1t_i)
                    if (ierror == 0) ierror = py_var%setitem('xcross_i',A%xcross_i)
                    if (ierror == 0) ierror = py_var%setitem('wcross_i',A%wcross_i)
                    if (ierror == 0) ierror = py_var%setitem('sig0_i',A%sig0_i)
                    if (ierror == 0) ierror = py_var%setitem('sig1_i',A%sig1_i)
                    if (ierror == 0) ierror = py_var%setitem('sig2_i',A%sig2_i)
                    if (ierror == 0) ierror = py_var%setitem('sigq_i',A%sigq_i)
                    if (ierror == 0) ierror = py_var%setitem('gamma0_i',A%gamma0_i)
                    if (ierror == 0) ierror = py_var%setitem('gamma1_i',A%gamma1_i)
                    if (ierror == 0) ierror = py_var%setitem('gamma2_i',A%gamma2_i)
                    if (allocated(A%tof)) then
                        if (ierror == 0) ierror = ndarray_create(nd_tof,A%tof)
                        if (ierror == 0) ierror = py_var%setitem('tof',nd_tof)
                    end if
                    if (allocated(A%sigma)) then
                        if (ierror == 0) ierror = ndarray_create(nd_sigma,A%sigma)
                        if (ierror == 0) ierror = py_var%setitem('sigma',nd_sigma)
                    end if
                    if (allocated(A%gamma)) then
                        if (ierror == 0) ierror = ndarray_create(nd_gamma,A%gamma)
                        if (ierror == 0) ierror = py_var%setitem('gamma',nd_gamma)
                    end if
                    if (allocated(A%alpha)) then
                        if (ierror == 0) ierror = ndarray_create(nd_alpha,A%alpha)
                        if (ierror == 0) ierror = py_var%setitem('alpha',nd_alpha)
                    end if
                    if (allocated(A%beta)) then
                        if (ierror == 0) ierror = ndarray_create(nd_beta,A%beta)
                        if (ierror == 0) ierror = py_var%setitem('beta',nd_beta)
                    end if
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_irf_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_irf_type

    Module Subroutine Unwrap_class_irf_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'irf_type' &
                .and. fortran_type /= 'irf_cw_type' &
                .and. fortran_type /= 'irf_tof_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (irf_cw_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','lambda',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','lambda',p_real_1d,A%lambda,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ratio',py_var,A%ratio,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','rkk',py_var,A%rkk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','alpsd',py_var,A%alpsd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','cthm',py_var,A%cthm,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','u_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','u_i',p_real_1d,A%u_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','v_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','v_i',p_real_1d,A%v_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','w_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','w_i',p_real_1d,A%w_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','x_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','x_i',p_real_1d,A%x_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','y_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','y_i',p_real_1d,A%y_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','z_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','z_i',p_real_1d,A%z_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','sl_i',py_var,A%sl_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','dl_i',py_var,A%dl_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ttheta',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','ttheta',p_real_1d,A%ttheta,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hg',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hg',p_real_1d,A%hg,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hl',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hl',p_real_1d,A%hl,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (irf_tof_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','toftet_i',py_var,A%toftet_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zero_i',py_var,A%zero_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zerot_i',py_var,A%zerot_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1_i',py_var,A%dtt1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2_i',py_var,A%dtt2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt_1overd_i',py_var,A%dtt_1overd_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1t_i',py_var,A%dtt1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2t_i',py_var,A%dtt2t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0_i',py_var,A%alfa0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1_i',py_var,A%alfa1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfaq_i',py_var,A%alfaq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0_i',py_var,A%beta0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','betaq_i',py_var,A%betaq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1_i',py_var,A%beta1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0t_i',py_var,A%alfa0t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1t_i',py_var,A%alfa1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0t_i',py_var,A%beta0t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1t_i',py_var,A%beta1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','xcross_i',py_var,A%xcross_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','wcross_i',py_var,A%wcross_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig0_i',py_var,A%sig0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig1_i',py_var,A%sig1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig2_i',py_var,A%sig2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigq_i',py_var,A%sigq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma0_i',py_var,A%gamma0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma1_i',py_var,A%gamma1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma2_i',py_var,A%gamma2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','tof',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','tof',p_real_1d,A%tof,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigma',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','sigma',p_real_1d,A%sigma,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','gamma',p_real_1d,A%gamma,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alpha',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','alpha',p_real_1d,A%alpha,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','beta',p_real_1d,A%beta,ierror)
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
                err_cfml%msg  = 'Unwrap_irf_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_type_no_alloc

    Module Subroutine List_to_class_array1d_irf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_irf_type: Cannot determine fortran type'
            else if (fortran_type == 'irf_type') then
                allocate(irf_type :: arr(n))
            else if (fortran_type == 'irf_cw_type') then
                allocate(irf_cw_type :: arr(n))
            else if (fortran_type == 'irf_tof_type') then
                allocate(irf_tof_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_irf_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_irf_type

    Module Subroutine List_to_class_array2d_irf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_irf_type: Cannot determine fortran type'
                    else if (fortran_type == 'irf_type') then
                        allocate(irf_type :: arr(n,m))
                    else if (fortran_type == 'irf_cw_type') then
                        allocate(irf_cw_type :: arr(n,m))
                    else if (fortran_type == 'irf_tof_type') then
                        allocate(irf_tof_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_irf_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_irf_type

    Module Subroutine Unwrap_class_irf_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_type: Cannot determine fortran type'
        else
            if (fortran_type == 'irf_type') then
                allocate(irf_type :: for_var)
            else if (fortran_type == 'irf_cw_type') then
                allocate(irf_cw_type :: for_var)
            else if (fortran_type == 'irf_tof_type') then
                allocate(irf_tof_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (irf_cw_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','lambda',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','lambda',p_real_1d,A%lambda,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ratio',py_var,A%ratio,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','rkk',py_var,A%rkk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','alpsd',py_var,A%alpsd,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','cthm',py_var,A%cthm,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','u_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','u_i',p_real_1d,A%u_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','v_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','v_i',p_real_1d,A%v_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','w_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','w_i',p_real_1d,A%w_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','x_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','x_i',p_real_1d,A%x_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','y_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','y_i',p_real_1d,A%y_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','z_i',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','z_i',p_real_1d,A%z_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','sl_i',py_var,A%sl_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','dl_i',py_var,A%dl_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ttheta',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','ttheta',p_real_1d,A%ttheta,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hg',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hg',p_real_1d,A%hg,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hl',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hl',p_real_1d,A%hl,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (irf_tof_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','toftet_i',py_var,A%toftet_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zero_i',py_var,A%zero_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zerot_i',py_var,A%zerot_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1_i',py_var,A%dtt1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2_i',py_var,A%dtt2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt_1overd_i',py_var,A%dtt_1overd_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1t_i',py_var,A%dtt1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2t_i',py_var,A%dtt2t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0_i',py_var,A%alfa0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1_i',py_var,A%alfa1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfaq_i',py_var,A%alfaq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0_i',py_var,A%beta0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','betaq_i',py_var,A%betaq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1_i',py_var,A%beta1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0t_i',py_var,A%alfa0t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1t_i',py_var,A%alfa1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0t_i',py_var,A%beta0t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1t_i',py_var,A%beta1t_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','xcross_i',py_var,A%xcross_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','wcross_i',py_var,A%wcross_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig0_i',py_var,A%sig0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig1_i',py_var,A%sig1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig2_i',py_var,A%sig2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigq_i',py_var,A%sigq_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma0_i',py_var,A%gamma0_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma1_i',py_var,A%gamma1_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma2_i',py_var,A%gamma2_i,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','tof',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','tof',p_real_1d,A%tof,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigma',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','sigma',p_real_1d,A%sigma,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','gamma',p_real_1d,A%gamma,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alpha',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','alpha',p_real_1d,A%alpha,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','beta',p_real_1d,A%beta,ierror)
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
                err_cfml%msg  = 'Unwrap_irf_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_type

    Module Subroutine list_to_type_array1d_irf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_type

    Module Subroutine list_to_type_array1d_irf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_type_no_alloc

    Module Subroutine list_to_type_array2d_irf_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_type

    Module Subroutine list_to_type_array2d_irf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_type_no_alloc

    Module Subroutine Unwrap_class_irf_cw_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_cw_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_cw_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'irf_cw_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_cw_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','lambda',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','lambda',p_real_1d,for_var%lambda,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ratio',py_var,for_var%ratio,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','rkk',py_var,for_var%rkk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','alpsd',py_var,for_var%alpsd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','cthm',py_var,for_var%cthm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','u_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','u_i',p_real_1d,for_var%u_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','v_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','v_i',p_real_1d,for_var%v_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','w_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','w_i',p_real_1d,for_var%w_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','x_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','x_i',p_real_1d,for_var%x_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','y_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','y_i',p_real_1d,for_var%y_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','z_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','z_i',p_real_1d,for_var%z_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','sl_i',py_var,for_var%sl_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','dl_i',py_var,for_var%dl_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ttheta',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','ttheta',p_real_1d,for_var%ttheta,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hg',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hg',p_real_1d,for_var%hg,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hl',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hl',p_real_1d,for_var%hl,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_irf_cw_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_cw_type_no_alloc

    Module Subroutine List_to_class_array1d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_cw_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_irf_cw_type: Cannot determine fortran type'
            else if (fortran_type == 'irf_cw_type') then
                allocate(irf_cw_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_irf_cw_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_irf_cw_type

    Module Subroutine List_to_class_array2d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_cw_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_irf_cw_type: Cannot determine fortran type'
                    else if (fortran_type == 'irf_cw_type') then
                        allocate(irf_cw_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_irf_cw_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_irf_cw_type

    Module Subroutine Unwrap_class_irf_cw_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_cw_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_cw_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'irf_cw_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_cw_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','lambda',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','lambda',p_real_1d,for_var%lambda,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ratio',py_var,for_var%ratio,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','rkk',py_var,for_var%rkk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','alpsd',py_var,for_var%alpsd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','cthm',py_var,for_var%cthm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','u_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','u_i',p_real_1d,for_var%u_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','v_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','v_i',p_real_1d,for_var%v_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','w_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','w_i',p_real_1d,for_var%w_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','x_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','x_i',p_real_1d,for_var%x_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','y_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','y_i',p_real_1d,for_var%y_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','z_i',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_irf_cw_type','z_i',p_real_1d,for_var%z_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','sl_i',py_var,for_var%sl_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','dl_i',py_var,for_var%dl_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','ttheta',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','ttheta',p_real_1d,for_var%ttheta,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hg',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hg',p_real_1d,for_var%hg,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_cw_type','hl',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_cw_type','hl',p_real_1d,for_var%hl,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_irf_cw_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_cw_type

    Module Subroutine list_to_type_array1d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_cw_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_cw_type

    Module Subroutine list_to_type_array1d_irf_cw_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_cw_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_cw_type_no_alloc

    Module Subroutine list_to_type_array2d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_cw_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_cw_type

    Module Subroutine list_to_type_array2d_irf_cw_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_cw_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_cw_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_cw_type_no_alloc

    Module Subroutine Unwrap_class_irf_tof_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_tof_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_tof_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'irf_tof_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_tof_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','toftet_i',py_var,for_var%toftet_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zero_i',py_var,for_var%zero_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zerot_i',py_var,for_var%zerot_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1_i',py_var,for_var%dtt1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2_i',py_var,for_var%dtt2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt_1overd_i',py_var,for_var%dtt_1overd_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1t_i',py_var,for_var%dtt1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2t_i',py_var,for_var%dtt2t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0_i',py_var,for_var%alfa0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1_i',py_var,for_var%alfa1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfaq_i',py_var,for_var%alfaq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0_i',py_var,for_var%beta0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','betaq_i',py_var,for_var%betaq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1_i',py_var,for_var%beta1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0t_i',py_var,for_var%alfa0t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1t_i',py_var,for_var%alfa1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0t_i',py_var,for_var%beta0t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1t_i',py_var,for_var%beta1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','xcross_i',py_var,for_var%xcross_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','wcross_i',py_var,for_var%wcross_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig0_i',py_var,for_var%sig0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig1_i',py_var,for_var%sig1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig2_i',py_var,for_var%sig2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigq_i',py_var,for_var%sigq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma0_i',py_var,for_var%gamma0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma1_i',py_var,for_var%gamma1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma2_i',py_var,for_var%gamma2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','tof',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','tof',p_real_1d,for_var%tof,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','gamma',p_real_1d,for_var%gamma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alpha',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','alpha',p_real_1d,for_var%alpha,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','beta',p_real_1d,for_var%beta,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_irf_tof_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_tof_type_no_alloc

    Module Subroutine List_to_class_array1d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_tof_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_irf_tof_type: Cannot determine fortran type'
            else if (fortran_type == 'irf_tof_type') then
                allocate(irf_tof_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_irf_tof_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_irf_tof_type

    Module Subroutine List_to_class_array2d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(irf_tof_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_irf_tof_type: Cannot determine fortran type'
                    else if (fortran_type == 'irf_tof_type') then
                        allocate(irf_tof_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_irf_tof_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_irf_tof_type

    Module Subroutine Unwrap_class_irf_tof_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(irf_tof_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_irf_tof_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'irf_tof_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_irf_tof_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','nam',py_var,for_var%nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','geom',py_var,for_var%geom,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','file_nam',py_var,for_var%file_nam,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','jobt',py_var,for_var%jobt,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_irf_type','irftype',py_var,for_var%irftype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','num_patt',py_var,for_var%num_patt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_points',py_var,for_var%n_points,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','n_items',py_var,for_var%n_items,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','shift',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','shift',p_real_1d,for_var%shift,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_type','corr_int',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_type','corr_int',p_real_1d,for_var%corr_int,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','toftet_i',py_var,for_var%toftet_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zero_i',py_var,for_var%zero_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','zerot_i',py_var,for_var%zerot_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1_i',py_var,for_var%dtt1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2_i',py_var,for_var%dtt2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt_1overd_i',py_var,for_var%dtt_1overd_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt1t_i',py_var,for_var%dtt1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','dtt2t_i',py_var,for_var%dtt2t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0_i',py_var,for_var%alfa0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1_i',py_var,for_var%alfa1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfaq_i',py_var,for_var%alfaq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0_i',py_var,for_var%beta0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','betaq_i',py_var,for_var%betaq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1_i',py_var,for_var%beta1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa0t_i',py_var,for_var%alfa0t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alfa1t_i',py_var,for_var%alfa1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta0t_i',py_var,for_var%beta0t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta1t_i',py_var,for_var%beta1t_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','xcross_i',py_var,for_var%xcross_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','wcross_i',py_var,for_var%wcross_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig0_i',py_var,for_var%sig0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig1_i',py_var,for_var%sig1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sig2_i',py_var,for_var%sig2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigq_i',py_var,for_var%sigq_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma0_i',py_var,for_var%gamma0_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma1_i',py_var,for_var%gamma1_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma2_i',py_var,for_var%gamma2_i,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','tof',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','tof',p_real_1d,for_var%tof,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','sigma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','gamma',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','gamma',p_real_1d,for_var%gamma,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','alpha',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','alpha',p_real_1d,for_var%alpha,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_irf_tof_type','beta',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_irf_tof_type','beta',p_real_1d,for_var%beta,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_irf_tof_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_irf_tof_type

    Module Subroutine list_to_type_array1d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_tof_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_tof_type

    Module Subroutine list_to_type_array1d_irf_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_tof_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_irf_tof_type_no_alloc

    Module Subroutine list_to_type_array2d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_tof_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_tof_type

    Module Subroutine list_to_type_array2d_irf_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(irf_tof_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_irf_tof_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_irf_tof_type_no_alloc

end submodule