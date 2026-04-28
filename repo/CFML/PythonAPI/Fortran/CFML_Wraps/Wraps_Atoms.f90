submodule (CFML_Wraps) Wraps_Atoms

    implicit none
    contains

    Module Subroutine Wrap_atm_type(for_var,py_var,ierror)

        ! Arguments
        class(atm_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_x,nd_u,nd_moment,nd_ind_ff,nd_varf,nd_x_std,nd_u_std,nd_moment_std,nd_poc_q,nd_pbc_q,nd_pmc_q,nd_pdc_q,nd_puc_q,nd_ocs,nd_ocs_std,nd_bcs,nd_bcs_std,nd_mcs,nd_mcs_std,nd_dcs,nd_dcs_std,nd_ucs,nd_ucs_std,nd_xs,nd_moms,nd_us,nd_l_x,nd_l_moment,nd_l_u,nd_m_x,nd_m_moment,nd_m_u,nd_l_ocs,nd_l_bcs,nd_l_mcs,nd_l_dcs,nd_l_ucs,nd_m_ocs,nd_m_bcs,nd_m_mcs,nd_m_dcs,nd_m_ucs

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_type')
        if (ierror == 0) then
            ierror = py_var%setitem('lab',for_var%lab)
            if (ierror /= 0) then
                for_var%lab = ''
                ierror = py_var%setitem('lab',for_var%lab)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            if (ierror /= 0) then
                for_var%chemsymb = ''
                ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
            if (ierror /= 0) then
                for_var%sfacsymb = ''
                ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('charge',for_var%charge)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror == 0) ierror = py_var%setitem('u_iso',for_var%u_iso)
        if (ierror == 0) ierror = py_var%setitem('occ',for_var%occ)
        if (ierror == 0) then
            ierror = py_var%setitem('utype',for_var%utype)
            if (ierror /= 0) then
                for_var%utype = ''
                ierror = py_var%setitem('utype',for_var%utype)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('thtype',for_var%thtype)
            if (ierror /= 0) then
                for_var%thtype = ''
                ierror = py_var%setitem('thtype',for_var%thtype)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_u,for_var%u)
        if (ierror == 0) ierror = py_var%setitem('u',nd_u)
        if (ierror == 0) ierror = py_var%setitem('magnetic',for_var%magnetic)
        if (ierror == 0) ierror = py_var%setitem('mom',for_var%mom)
        if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
        if (ierror == 0) ierror = py_var%setitem('moment',nd_moment)
        if (ierror == 0) ierror = ndarray_create(nd_ind_ff,for_var%ind_ff)
        if (ierror == 0) ierror = py_var%setitem('ind_ff',nd_ind_ff)
        if (ierror == 0) then
            ierror = py_var%setitem('atminfo',for_var%atminfo)
            if (ierror /= 0) then
                for_var%atminfo = ''
                ierror = py_var%setitem('atminfo',for_var%atminfo)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('wyck',for_var%wyck)
            if (ierror /= 0) then
                for_var%wyck = ''
                ierror = py_var%setitem('wyck',for_var%wyck)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_varf,for_var%varf)
        if (ierror == 0) ierror = py_var%setitem('varf',nd_varf)
        if (ierror == 0) ierror = py_var%setitem('active',for_var%active)
        if (ierror == 0) then
            select type (A => for_var)
                class is (atm_std_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_std_type')
                    if (ierror == 0) ierror = ndarray_create(nd_x_std,A%x_std)
                    if (ierror == 0) ierror = py_var%setitem('x_std',nd_x_std)
                    if (ierror == 0) ierror = py_var%setitem('occ_std',A%occ_std)
                    if (ierror == 0) ierror = py_var%setitem('u_iso_std',A%u_iso_std)
                    if (ierror == 0) ierror = ndarray_create(nd_u_std,A%u_std)
                    if (ierror == 0) ierror = py_var%setitem('u_std',nd_u_std)
                    if (ierror == 0) ierror = ndarray_create(nd_moment_std,A%moment_std)
                    if (ierror == 0) ierror = py_var%setitem('moment_std',nd_moment_std)
            end select
            select type (A => for_var)
                class is (modatm_std_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','modatm_std_type')
                    if (ierror == 0) ierror = py_var%setitem('n_oc',A%n_oc)
                    if (ierror == 0) ierror = py_var%setitem('n_bc',A%n_bc)
                    if (ierror == 0) ierror = py_var%setitem('n_mc',A%n_mc)
                    if (ierror == 0) ierror = py_var%setitem('n_dc',A%n_dc)
                    if (ierror == 0) ierror = py_var%setitem('n_uc',A%n_uc)
                    if (ierror == 0) ierror = ndarray_create(nd_poc_q,A%poc_q)
                    if (ierror == 0) ierror = py_var%setitem('poc_q',nd_poc_q)
                    if (ierror == 0) ierror = ndarray_create(nd_pbc_q,A%pbc_q)
                    if (ierror == 0) ierror = py_var%setitem('pbc_q',nd_pbc_q)
                    if (ierror == 0) ierror = ndarray_create(nd_pmc_q,A%pmc_q)
                    if (ierror == 0) ierror = py_var%setitem('pmc_q',nd_pmc_q)
                    if (ierror == 0) ierror = ndarray_create(nd_pdc_q,A%pdc_q)
                    if (ierror == 0) ierror = py_var%setitem('pdc_q',nd_pdc_q)
                    if (ierror == 0) ierror = ndarray_create(nd_puc_q,A%puc_q)
                    if (ierror == 0) ierror = py_var%setitem('puc_q',nd_puc_q)
                    if (ierror == 0) ierror = ndarray_create(nd_ocs,A%ocs)
                    if (ierror == 0) ierror = py_var%setitem('ocs',nd_ocs)
                    if (ierror == 0) ierror = ndarray_create(nd_ocs_std,A%ocs_std)
                    if (ierror == 0) ierror = py_var%setitem('ocs_std',nd_ocs_std)
                    if (ierror == 0) ierror = ndarray_create(nd_bcs,A%bcs)
                    if (ierror == 0) ierror = py_var%setitem('bcs',nd_bcs)
                    if (ierror == 0) ierror = ndarray_create(nd_bcs_std,A%bcs_std)
                    if (ierror == 0) ierror = py_var%setitem('bcs_std',nd_bcs_std)
                    if (ierror == 0) ierror = ndarray_create(nd_mcs,A%mcs)
                    if (ierror == 0) ierror = py_var%setitem('mcs',nd_mcs)
                    if (ierror == 0) ierror = ndarray_create(nd_mcs_std,A%mcs_std)
                    if (ierror == 0) ierror = py_var%setitem('mcs_std',nd_mcs_std)
                    if (ierror == 0) ierror = ndarray_create(nd_dcs,A%dcs)
                    if (ierror == 0) ierror = py_var%setitem('dcs',nd_dcs)
                    if (ierror == 0) ierror = ndarray_create(nd_dcs_std,A%dcs_std)
                    if (ierror == 0) ierror = py_var%setitem('dcs_std',nd_dcs_std)
                    if (ierror == 0) ierror = ndarray_create(nd_ucs,A%ucs)
                    if (ierror == 0) ierror = py_var%setitem('ucs',nd_ucs)
                    if (ierror == 0) ierror = ndarray_create(nd_ucs_std,A%ucs_std)
                    if (ierror == 0) ierror = py_var%setitem('ucs_std',nd_ucs_std)
                    if (allocated(A%xs)) then
                        if (ierror == 0) ierror = ndarray_create(nd_xs,A%xs)
                        if (ierror == 0) ierror = py_var%setitem('xs',nd_xs)
                    end if
                    if (allocated(A%moms)) then
                        if (ierror == 0) ierror = ndarray_create(nd_moms,A%moms)
                        if (ierror == 0) ierror = py_var%setitem('moms',nd_moms)
                    end if
                    if (allocated(A%us)) then
                        if (ierror == 0) ierror = ndarray_create(nd_us,A%us)
                        if (ierror == 0) ierror = py_var%setitem('us',nd_us)
                    end if
                class is (atm_ref_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_ref_type')
                    if (ierror == 0) ierror = ndarray_create(nd_l_x,A%l_x)
                    if (ierror == 0) ierror = py_var%setitem('l_x',nd_l_x)
                    if (ierror == 0) ierror = py_var%setitem('l_occ',A%l_occ)
                    if (ierror == 0) ierror = py_var%setitem('l_u_iso',A%l_u_iso)
                    if (ierror == 0) ierror = ndarray_create(nd_l_moment,A%l_moment)
                    if (ierror == 0) ierror = py_var%setitem('l_moment',nd_l_moment)
                    if (ierror == 0) ierror = ndarray_create(nd_l_u,A%l_u)
                    if (ierror == 0) ierror = py_var%setitem('l_u',nd_l_u)
                    if (ierror == 0) ierror = ndarray_create(nd_m_x,A%m_x)
                    if (ierror == 0) ierror = py_var%setitem('m_x',nd_m_x)
                    if (ierror == 0) ierror = py_var%setitem('m_occ',A%m_occ)
                    if (ierror == 0) ierror = py_var%setitem('m_u_iso',A%m_u_iso)
                    if (ierror == 0) ierror = ndarray_create(nd_m_moment,A%m_moment)
                    if (ierror == 0) ierror = py_var%setitem('m_moment',nd_m_moment)
                    if (ierror == 0) ierror = ndarray_create(nd_m_u,A%m_u)
                    if (ierror == 0) ierror = py_var%setitem('m_u',nd_m_u)
            end select
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','modatm_ref_type')
                    if (ierror == 0) ierror = ndarray_create(nd_l_x,A%l_x)
                    if (ierror == 0) ierror = py_var%setitem('l_x',nd_l_x)
                    if (ierror == 0) ierror = py_var%setitem('l_occ',A%l_occ)
                    if (ierror == 0) ierror = py_var%setitem('l_u_iso',A%l_u_iso)
                    if (ierror == 0) ierror = ndarray_create(nd_l_u,A%l_u)
                    if (ierror == 0) ierror = py_var%setitem('l_u',nd_l_u)
                    if (ierror == 0) ierror = ndarray_create(nd_m_x,A%m_x)
                    if (ierror == 0) ierror = py_var%setitem('m_x',nd_m_x)
                    if (ierror == 0) ierror = py_var%setitem('m_occ',A%m_occ)
                    if (ierror == 0) ierror = py_var%setitem('m_u_iso',A%m_u_iso)
                    if (ierror == 0) ierror = ndarray_create(nd_m_u,A%m_u)
                    if (ierror == 0) ierror = py_var%setitem('m_u',nd_m_u)
                    if (ierror == 0) ierror = ndarray_create(nd_l_ocs,A%l_ocs)
                    if (ierror == 0) ierror = py_var%setitem('l_ocs',nd_l_ocs)
                    if (ierror == 0) ierror = ndarray_create(nd_l_bcs,A%l_bcs)
                    if (ierror == 0) ierror = py_var%setitem('l_bcs',nd_l_bcs)
                    if (ierror == 0) ierror = ndarray_create(nd_l_mcs,A%l_mcs)
                    if (ierror == 0) ierror = py_var%setitem('l_mcs',nd_l_mcs)
                    if (ierror == 0) ierror = ndarray_create(nd_l_dcs,A%l_dcs)
                    if (ierror == 0) ierror = py_var%setitem('l_dcs',nd_l_dcs)
                    if (ierror == 0) ierror = ndarray_create(nd_l_ucs,A%l_ucs)
                    if (ierror == 0) ierror = py_var%setitem('l_ucs',nd_l_ucs)
                    if (ierror == 0) ierror = ndarray_create(nd_m_ocs,A%m_ocs)
                    if (ierror == 0) ierror = py_var%setitem('m_ocs',nd_m_ocs)
                    if (ierror == 0) ierror = ndarray_create(nd_m_bcs,A%m_bcs)
                    if (ierror == 0) ierror = py_var%setitem('m_bcs',nd_m_bcs)
                    if (ierror == 0) ierror = ndarray_create(nd_m_mcs,A%m_mcs)
                    if (ierror == 0) ierror = py_var%setitem('m_mcs',nd_m_mcs)
                    if (ierror == 0) ierror = ndarray_create(nd_m_dcs,A%m_dcs)
                    if (ierror == 0) ierror = py_var%setitem('m_dcs',nd_m_dcs)
                    if (ierror == 0) ierror = ndarray_create(nd_m_ucs,A%m_ucs)
                    if (ierror == 0) ierror = py_var%setitem('m_ucs',nd_m_ucs)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atm_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atm_type

    Module Subroutine Unwrap_class_atm_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'atm_type' &
                .and. fortran_type /= 'atm_std_type' &
                .and. fortran_type /= 'modatm_std_type' &
                .and. fortran_type /= 'atm_ref_type' &
                .and. fortran_type /= 'modatm_ref_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (atm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,A%x_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,A%occ_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,A%u_iso_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,A%u_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,A%moment_std,ierror)
            end select
            select type (A => for_var)
                class is (modatm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,A%n_oc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,A%n_bc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,A%n_mc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,A%n_dc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,A%n_uc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,A%poc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,A%pbc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,A%pmc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,A%pdc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,A%puc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,A%ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,A%bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,A%mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,A%dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,A%ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,A%xs,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,A%moms,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,A%us,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (atm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,A%l_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,A%m_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,A%m_u,ierror)
            end select
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_type_no_alloc

    Module Subroutine List_to_class_array1d_atm_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_atm_type: Cannot determine fortran type'
            else if (fortran_type == 'atm_type') then
                allocate(atm_type :: arr(n))
            else if (fortran_type == 'atm_std_type') then
                allocate(atm_std_type :: arr(n))
            else if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: arr(n))
            else if (fortran_type == 'atm_ref_type') then
                allocate(atm_ref_type :: arr(n))
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_atm_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_atm_type

    Module Subroutine List_to_class_array2d_atm_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_atm_type: Cannot determine fortran type'
                    else if (fortran_type == 'atm_type') then
                        allocate(atm_type :: arr(n,m))
                    else if (fortran_type == 'atm_std_type') then
                        allocate(atm_std_type :: arr(n,m))
                    else if (fortran_type == 'modatm_std_type') then
                        allocate(modatm_std_type :: arr(n,m))
                    else if (fortran_type == 'atm_ref_type') then
                        allocate(atm_ref_type :: arr(n,m))
                    else if (fortran_type == 'modatm_ref_type') then
                        allocate(modatm_ref_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_atm_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_atm_type

    Module Subroutine Unwrap_class_atm_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_type: Cannot determine fortran type'
        else
            if (fortran_type == 'atm_type') then
                allocate(atm_type :: for_var)
            else if (fortran_type == 'atm_std_type') then
                allocate(atm_std_type :: for_var)
            else if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: for_var)
            else if (fortran_type == 'atm_ref_type') then
                allocate(atm_ref_type :: for_var)
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (atm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,A%x_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,A%occ_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,A%u_iso_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,A%u_std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,A%moment_std,ierror)
            end select
            select type (A => for_var)
                class is (modatm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,A%n_oc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,A%n_bc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,A%n_mc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,A%n_dc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,A%n_uc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,A%poc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,A%pbc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,A%pmc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,A%pdc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,A%puc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,A%ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,A%bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,A%mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,A%dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,A%ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,A%xs,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,A%moms,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,A%us,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (atm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,A%l_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,A%m_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,A%m_u,ierror)
            end select
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_type

    Module Subroutine list_to_type_array1d_atm_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_type

    Module Subroutine list_to_type_array1d_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_type_no_alloc

    Module Subroutine list_to_type_array2d_atm_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_type

    Module Subroutine list_to_type_array2d_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_type_no_alloc

    Module Subroutine Unwrap_class_atm_std_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_std_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_std_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'atm_std_type' &
                .and. fortran_type /= 'modatm_std_type' &
                .and. fortran_type /= 'atm_ref_type' &
                .and. fortran_type /= 'modatm_ref_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_std_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (modatm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,A%n_oc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,A%n_bc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,A%n_mc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,A%n_dc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,A%n_uc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,A%poc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,A%pbc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,A%pmc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,A%pdc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,A%puc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,A%ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,A%bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,A%mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,A%dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,A%ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,A%xs,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,A%moms,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,A%us,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (atm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,A%l_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,A%m_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,A%m_u,ierror)
            end select
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_std_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_std_type_no_alloc

    Module Subroutine List_to_class_array1d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_std_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_atm_std_type: Cannot determine fortran type'
            else if (fortran_type == 'atm_std_type') then
                allocate(atm_std_type :: arr(n))
            else if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: arr(n))
            else if (fortran_type == 'atm_ref_type') then
                allocate(atm_ref_type :: arr(n))
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_atm_std_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_atm_std_type

    Module Subroutine List_to_class_array2d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_std_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_atm_std_type: Cannot determine fortran type'
                    else if (fortran_type == 'atm_std_type') then
                        allocate(atm_std_type :: arr(n,m))
                    else if (fortran_type == 'modatm_std_type') then
                        allocate(modatm_std_type :: arr(n,m))
                    else if (fortran_type == 'atm_ref_type') then
                        allocate(atm_ref_type :: arr(n,m))
                    else if (fortran_type == 'modatm_ref_type') then
                        allocate(modatm_ref_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_atm_std_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_atm_std_type

    Module Subroutine Unwrap_class_atm_std_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_std_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_std_type: Cannot determine fortran type'
        else
            if (fortran_type == 'atm_std_type') then
                allocate(atm_std_type :: for_var)
            else if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: for_var)
            else if (fortran_type == 'atm_ref_type') then
                allocate(atm_ref_type :: for_var)
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_std_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (modatm_std_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,A%n_oc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,A%n_bc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,A%n_mc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,A%n_dc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,A%n_uc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,A%poc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,A%pbc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,A%pmc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,A%pdc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,A%puc_q,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,A%ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,A%bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,A%mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,A%dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,A%ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,A%xs,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,A%moms,ierror)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
                    if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,A%us,ierror,order)
                    if (ierror2 /= 0) then
                        call err_clear
                        call clear_error()
                    end if
                class is (atm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,A%l_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,A%m_moment,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,A%m_u,ierror)
            end select
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_std_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_std_type

    Module Subroutine list_to_type_array1d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_std_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_std_type

    Module Subroutine list_to_type_array1d_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_std_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_std_type_no_alloc

    Module Subroutine list_to_type_array2d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_std_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_std_type

    Module Subroutine list_to_type_array2d_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_std_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_std_type_no_alloc

    Module Subroutine Unwrap_class_modatm_std_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(modatm_std_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_modatm_std_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'modatm_std_type' &
                .and. fortran_type /= 'modatm_ref_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_modatm_std_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,for_var%n_oc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,for_var%n_bc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,for_var%n_mc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,for_var%n_dc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,for_var%n_uc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,for_var%poc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,for_var%pbc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,for_var%pmc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,for_var%pdc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,for_var%puc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,for_var%ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,for_var%ocs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,for_var%bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,for_var%bcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,for_var%mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,for_var%mcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,for_var%dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,for_var%dcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,for_var%ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,for_var%ucs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,for_var%xs,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,for_var%moms,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,for_var%us,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_modatm_std_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_modatm_std_type_no_alloc

    Module Subroutine List_to_class_array1d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(modatm_std_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_modatm_std_type: Cannot determine fortran type'
            else if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: arr(n))
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_modatm_std_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_modatm_std_type

    Module Subroutine List_to_class_array2d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(modatm_std_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_modatm_std_type: Cannot determine fortran type'
                    else if (fortran_type == 'modatm_std_type') then
                        allocate(modatm_std_type :: arr(n,m))
                    else if (fortran_type == 'modatm_ref_type') then
                        allocate(modatm_ref_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_modatm_std_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_modatm_std_type

    Module Subroutine Unwrap_class_modatm_std_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(modatm_std_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_modatm_std_type: Cannot determine fortran type'
        else
            if (fortran_type == 'modatm_std_type') then
                allocate(modatm_std_type :: for_var)
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_modatm_std_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,for_var%n_oc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,for_var%n_bc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,for_var%n_mc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,for_var%n_dc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,for_var%n_uc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,for_var%poc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,for_var%pbc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,for_var%pmc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,for_var%pdc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,for_var%puc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,for_var%ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,for_var%ocs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,for_var%bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,for_var%bcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,for_var%mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,for_var%mcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,for_var%dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,for_var%dcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,for_var%ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,for_var%ucs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,for_var%xs,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,for_var%moms,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,for_var%us,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) then
            select type (A => for_var)
                class is (modatm_ref_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,A%l_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,A%l_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,A%l_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,A%l_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,A%m_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,A%m_occ,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,A%m_u_iso,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,A%m_u,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_modatm_std_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_modatm_std_type

    Module Subroutine list_to_type_array1d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_std_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_modatm_std_type

    Module Subroutine list_to_type_array1d_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_std_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_modatm_std_type_no_alloc

    Module Subroutine list_to_type_array2d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_std_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_modatm_std_type

    Module Subroutine list_to_type_array2d_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_std_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_modatm_std_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_modatm_std_type_no_alloc

    Module Subroutine Unwrap_class_atm_ref_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_ref_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_ref_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'atm_ref_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_ref_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,for_var%l_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,for_var%l_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,for_var%l_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,for_var%l_moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,for_var%l_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,for_var%m_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,for_var%m_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,for_var%m_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,for_var%m_moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,for_var%m_u,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_ref_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_ref_type_no_alloc

    Module Subroutine List_to_class_array1d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_ref_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_atm_ref_type: Cannot determine fortran type'
            else if (fortran_type == 'atm_ref_type') then
                allocate(atm_ref_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_atm_ref_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_atm_ref_type

    Module Subroutine List_to_class_array2d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(atm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_atm_ref_type: Cannot determine fortran type'
                    else if (fortran_type == 'atm_ref_type') then
                        allocate(atm_ref_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_atm_ref_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_atm_ref_type

    Module Subroutine Unwrap_class_atm_ref_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(atm_ref_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atm_ref_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atm_ref_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_ref_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_x',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_x',p_int_1d,for_var%l_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_occ',py_var,for_var%l_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u_iso',py_var,for_var%l_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_moment',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_moment',p_int_1d,for_var%l_moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','l_u',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','l_u',p_int_1d,for_var%l_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_x',p_real_1d,for_var%m_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_occ',py_var,for_var%m_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u_iso',py_var,for_var%m_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_moment',p_real_1d,for_var%m_moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_ref_type','m_u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_ref_type','m_u',p_real_1d,for_var%m_u,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_ref_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_atm_ref_type

    Module Subroutine list_to_type_array1d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_ref_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_ref_type

    Module Subroutine list_to_type_array1d_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_ref_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_atm_ref_type_no_alloc

    Module Subroutine list_to_type_array2d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_ref_type

    Module Subroutine list_to_type_array2d_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_ref_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_atm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_atm_ref_type_no_alloc

    Module Subroutine Unwrap_class_modatm_ref_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(modatm_ref_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_modatm_ref_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'modatm_ref_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_modatm_ref_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,for_var%n_oc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,for_var%n_bc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,for_var%n_mc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,for_var%n_dc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,for_var%n_uc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,for_var%poc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,for_var%pbc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,for_var%pmc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,for_var%pdc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,for_var%puc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,for_var%ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,for_var%ocs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,for_var%bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,for_var%bcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,for_var%mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,for_var%mcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,for_var%dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,for_var%dcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,for_var%ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,for_var%ucs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,for_var%xs,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,for_var%moms,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,for_var%us,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,for_var%l_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,for_var%l_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,for_var%l_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,for_var%l_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,for_var%m_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,for_var%m_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,for_var%m_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,for_var%m_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,for_var%l_ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,for_var%l_bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,for_var%l_mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,for_var%l_dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,for_var%l_ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,for_var%m_ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,for_var%m_bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,for_var%m_mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,for_var%m_dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,for_var%m_ucs,ierror,order)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_modatm_ref_type_no_alloc: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_modatm_ref_type_no_alloc

    Module Subroutine List_to_class_array1d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(modatm_ref_type), dimension(:), allocatable, intent(out) :: arr
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
                err_cfml%msg  = 'List_to_class_array1d_modatm_ref_type: Cannot determine fortran type'
            else if (fortran_type == 'modatm_ref_type') then
                allocate(modatm_ref_type :: arr(n))
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'List_to_class_array1d_modatm_ref_type: Wrong fortran type'
                return
            end if
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine List_to_class_array1d_modatm_ref_type

    Module Subroutine List_to_class_array2d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(modatm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        err_cfml%msg  = 'List_to_class_array2d_modatm_ref_type: Cannot determine fortran type'
                    else if (fortran_type == 'modatm_ref_type') then
                        allocate(modatm_ref_type :: arr(n,m))
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_to_class_array2d_modatm_ref_type: Wrong fortran type'
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
                            if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine List_to_class_array2d_modatm_ref_type

    Module Subroutine Unwrap_class_modatm_ref_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(modatm_ref_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_modatm_ref_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'modatm_ref_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_modatm_ref_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_oc',py_var,for_var%n_oc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_bc',py_var,for_var%n_bc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_mc',py_var,for_var%n_mc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_dc',py_var,for_var%n_dc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','n_uc',py_var,for_var%n_uc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','poc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','poc_q',p_int_1d,for_var%poc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pbc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pbc_q',p_int_1d,for_var%pbc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pmc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pmc_q',p_int_1d,for_var%pmc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','pdc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','pdc_q',p_int_1d,for_var%pdc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','puc_q',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','puc_q',p_int_1d,for_var%puc_q,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs',p_real_2d,for_var%ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ocs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ocs_std',p_real_2d,for_var%ocs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs',p_real_2d,for_var%bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','bcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','bcs_std',p_real_2d,for_var%bcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs',p_real_2d,for_var%mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','mcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','mcs_std',p_real_2d,for_var%mcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs',p_real_2d,for_var%dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','dcs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','dcs_std',p_real_2d,for_var%dcs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs',p_real_2d,for_var%ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','ucs_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_std_type','ucs_std',p_real_2d,for_var%ucs_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','xs',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','xs',p_real_1d,for_var%xs,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','moms',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','moms',p_real_1d,for_var%moms,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_std_type','us',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_modatm_std_type','us',p_real_2d,for_var%us,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_iso_std',py_var,for_var%u_iso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_std_type','moment_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_std_type','moment_std',p_real_1d,for_var%moment_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u_iso',py_var,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','mom',py_var,for_var%mom,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','moment',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','ind_ff',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_atm_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_x',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_x',p_int_1d,for_var%l_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_occ',py_var,for_var%l_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u_iso',py_var,for_var%l_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_u',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_u',p_int_1d,for_var%l_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_x',p_real_1d,for_var%m_x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_occ',py_var,for_var%m_occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u_iso',py_var,for_var%m_u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_u',p_real_1d,for_var%m_u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ocs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ocs',p_int_2d,for_var%l_ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_bcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_bcs',p_int_2d,for_var%l_bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_mcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_mcs',p_int_2d,for_var%l_mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_dcs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_dcs',p_int_2d,for_var%l_dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','l_ucs',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','l_ucs',p_int_2d,for_var%l_ucs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ocs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ocs',p_real_2d,for_var%m_ocs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_bcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_bcs',p_real_2d,for_var%m_bcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_mcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_mcs',p_real_2d,for_var%m_mcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_dcs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_dcs',p_real_2d,for_var%m_dcs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_modatm_ref_type','m_ucs',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_modatm_ref_type','m_ucs',p_real_2d,for_var%m_ucs,ierror,order)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_modatm_ref_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_class_modatm_ref_type

    Module Subroutine list_to_type_array1d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_ref_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_modatm_ref_type

    Module Subroutine list_to_type_array1d_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_ref_type), dimension(*), intent(inout) :: arr
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
                if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_type_array1d_modatm_ref_type_no_alloc

    Module Subroutine list_to_type_array2d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
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
                            if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_modatm_ref_type

    Module Subroutine list_to_type_array2d_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(modatm_ref_type), dimension(:,:), intent(inout) :: arr
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
                            if (ierror == 0) call unwrap_class_modatm_ref_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                            if (ierror == 0) ierror = err_cfml%ierr
                        end do
                    end do
                end if
            end if
        end if

    End Subroutine list_to_type_array2d_modatm_ref_type_no_alloc

    Module Subroutine Wrap_atm_cell_type(for_var,py_var,ierror)

        ! Arguments
        type(atm_cell_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_lab,di_ddlab
        type(list) :: li_lab,li_ddlab
        type(ndarray) :: nd_xyz,nd_charge,nd_moment,nd_var_free,nd_neighb,nd_neighb_atom,nd_distance,nd_trans,nd_ddist

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_cell_type')
        if (ierror == 0) ierror = py_var%setitem('nat',for_var%nat)
        if (allocated(for_var%lab)) then
            if (ierror == 0) ierror = list_create(li_lab)
            if (ierror == 0) then
                do i = 1 , size(for_var%lab)
                    if (ierror == 0) then
                        ierror = li_lab%append(for_var%lab(i))
                        if (ierror /= 0) then
                            for_var%lab(i) = ''
                            ierror = li_lab%append(for_var%lab(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
        end if
        if (allocated(for_var%xyz)) then
            if (ierror == 0) ierror = ndarray_create(nd_xyz,for_var%xyz)
            if (ierror == 0) ierror = py_var%setitem('xyz',nd_xyz)
        end if
        if (allocated(for_var%charge)) then
            if (ierror == 0) ierror = ndarray_create(nd_charge,for_var%charge)
            if (ierror == 0) ierror = py_var%setitem('charge',nd_charge)
        end if
        if (allocated(for_var%moment)) then
            if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
            if (ierror == 0) ierror = py_var%setitem('moment',nd_moment)
        end if
        if (allocated(for_var%var_free)) then
            if (ierror == 0) ierror = ndarray_create(nd_var_free,for_var%var_free)
            if (ierror == 0) ierror = py_var%setitem('var_free',nd_var_free)
        end if
        if (allocated(for_var%neighb)) then
            if (ierror == 0) ierror = ndarray_create(nd_neighb,for_var%neighb)
            if (ierror == 0) ierror = py_var%setitem('neighb',nd_neighb)
        end if
        if (allocated(for_var%neighb_atom)) then
            if (ierror == 0) ierror = ndarray_create(nd_neighb_atom,for_var%neighb_atom)
            if (ierror == 0) ierror = py_var%setitem('neighb_atom',nd_neighb_atom)
        end if
        if (allocated(for_var%distance)) then
            if (ierror == 0) ierror = ndarray_create(nd_distance,for_var%distance)
            if (ierror == 0) ierror = py_var%setitem('distance',nd_distance)
        end if
        if (allocated(for_var%trans)) then
            if (ierror == 0) ierror = ndarray_create(nd_trans,for_var%trans)
            if (ierror == 0) ierror = py_var%setitem('trans',nd_trans)
        end if
        if (ierror == 0) ierror = py_var%setitem('ndist',for_var%ndist)
        if (allocated(for_var%ddist)) then
            if (ierror == 0) ierror = ndarray_create(nd_ddist,for_var%ddist)
            if (ierror == 0) ierror = py_var%setitem('ddist',nd_ddist)
        end if
        if (allocated(for_var%ddlab)) then
            if (ierror == 0) ierror = list_create(li_ddlab)
            if (ierror == 0) then
                do i = 1 , size(for_var%ddlab)
                    if (ierror == 0) then
                        ierror = li_ddlab%append(for_var%ddlab(i))
                        if (ierror /= 0) then
                            for_var%ddlab(i) = ''
                            ierror = li_ddlab%append(for_var%ddlab(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('ddlab',li_ddlab)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atm_cell_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atm_cell_type

    Module Subroutine Unwrap_type_atm_cell_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atm_cell_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_atm_cell_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atm_cell_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atm_cell_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','nat',py_var,for_var%nat,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','lab',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%lab,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','xyz',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','xyz',p_real_2d,for_var%xyz,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','charge',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','charge',p_real_1d,for_var%charge,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','moment',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','moment',p_real_1d,for_var%moment,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','var_free',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','var_free',p_real_2d,for_var%var_free,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','neighb',py_var,p_int_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','neighb',p_int_1d,for_var%neighb,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','neighb_atom',py_var,p_int_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','neighb_atom',p_int_2d,for_var%neighb_atom,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','distance',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','distance',p_real_2d,for_var%distance,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','trans',py_var,p_real_3d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','trans',p_real_3d,for_var%trans,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','ndist',py_var,for_var%ndist,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','ddist',py_var,p_real_1d,ierror2)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atm_cell_type','ddist',p_real_1d,for_var%ddist,ierror)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atm_cell_type','ddlab',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%ddlab,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atm_cell_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_atm_cell_type

    Module Subroutine list_to_type_array1d_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_cell_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_atm_cell_type

    Module Subroutine list_to_type_array1d_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_cell_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_atm_cell_type_no_alloc

    Module Subroutine list_to_type_array2d_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_cell_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_atm_cell_type

    Module Subroutine list_to_type_array2d_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atm_cell_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_atm_cell_type_no_alloc

    Module Subroutine Wrap_atom_equiv_type(for_var,py_var,ierror)

        ! Arguments
        type(atom_equiv_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_lab
        type(list) :: li_lab
        type(ndarray) :: nd_x

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atom_equiv_type')
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) then
            ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            if (ierror /= 0) then
                for_var%chemsymb = ''
                ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            end if
        end if
        if (allocated(for_var%lab)) then
            if (ierror == 0) ierror = list_create(li_lab)
            if (ierror == 0) then
                do i = 1 , size(for_var%lab)
                    if (ierror == 0) then
                        ierror = li_lab%append(for_var%lab(i))
                        if (ierror /= 0) then
                            for_var%lab(i) = ''
                            ierror = li_lab%append(for_var%lab(i))
                        end if
                    end if
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
        end if
        if (allocated(for_var%x)) then
            if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
            if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atom_equiv_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atom_equiv_type

    Module Subroutine Unwrap_type_atom_equiv_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atom_equiv_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: ierror2
        character(len=:), allocatable :: fortran_type
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror2 = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_atom_equiv_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atom_equiv_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atom_equiv_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_type','lab',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%lab,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_type','x',py_var,p_real_2d,ierror2,order)
        if (ierror2 == 0) call pointer_to_alloc_array('Unwrap_atom_equiv_type','x',p_real_2d,for_var%x,ierror,order)
        if (ierror2 /= 0) then
            call err_clear
            call clear_error()
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atom_equiv_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_atom_equiv_type

    Module Subroutine list_to_type_array1d_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_atom_equiv_type

    Module Subroutine list_to_type_array1d_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_atom_equiv_type_no_alloc

    Module Subroutine list_to_type_array2d_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_atom_equiv_type

    Module Subroutine list_to_type_array2d_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_atom_equiv_type_no_alloc

    Module Subroutine Wrap_atom_equiv_list_type(for_var,py_var,ierror)

        ! Arguments
        type(atom_equiv_list_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_atm
        type(list) :: li_atm

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atom_equiv_list_type')
        if (ierror == 0) ierror = py_var%setitem('nauas',for_var%nauas)
        if (allocated(for_var%atm)) then
            if (ierror == 0) ierror = list_create(li_atm)
            if (ierror == 0) then
                do i = 1 , size(for_var%atm)
                    ierror = dict_create(di_atm)
                    if (ierror == 0) call wrap_type(for_var%atm(i),di_atm,ierror)
                    if (ierror == 0) ierror = li_atm%append(di_atm)
                    if (ierror == 0) call di_atm%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('atm',li_atm)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atom_equiv_list_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atom_equiv_list_type

    Module Subroutine Unwrap_type_atom_equiv_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atom_equiv_list_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atom_equiv_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atom_equiv_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atom_equiv_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_list_type','nauas',py_var,for_var%nauas,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atom_equiv_list_type','atm',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_atom_equiv_list_type','atm',my_list,for_var%atm,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atom_equiv_list_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_atom_equiv_list_type

    Module Subroutine list_to_type_array1d_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_list_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_atom_equiv_list_type

    Module Subroutine list_to_type_array1d_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_list_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_atom_equiv_list_type_no_alloc

    Module Subroutine list_to_type_array2d_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_list_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_atom_equiv_list_type

    Module Subroutine list_to_type_array2d_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atom_equiv_list_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_atom_equiv_list_type_no_alloc

    Module Subroutine Wrap_atlist_type(for_var,py_var,ierror)

        ! Arguments
        type(atlist_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_active,di_atom
        type(list) :: li_active,li_atom

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','atlist_type')
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('iph',for_var%iph)
        if (ierror == 0) then
            ierror = py_var%setitem('mcomp',for_var%mcomp)
            if (ierror /= 0) then
                for_var%mcomp = ''
                ierror = py_var%setitem('mcomp',for_var%mcomp)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('symm_checked',for_var%symm_checked)
        if (allocated(for_var%active)) then
            if (ierror == 0) ierror = list_create(li_active)
            if (ierror == 0) then
                do i = 1 , size(for_var%active)
                    if (ierror == 0) ierror = li_active%append(for_var%active(i))
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('active',li_active)
        end if
        if (allocated(for_var%atom)) then
            if (ierror == 0) ierror = list_create(li_atom)
            if (ierror == 0) then
                do i = 1 , size(for_var%atom)
                    ierror = dict_create(di_atom)
                    if (ierror == 0) call wrap_type(for_var%atom(i),di_atom,ierror)
                    if (ierror == 0) ierror = li_atom%append(di_atom)
                    if (ierror == 0) call di_atom%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('atom',li_atom)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_atlist_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_atlist_type

    Module Subroutine Unwrap_type_atlist_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atlist_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_atlist_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atlist_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atlist_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','iph',py_var,for_var%iph,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','mcomp',py_var,for_var%mcomp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','symm_checked',py_var,for_var%symm_checked,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','active',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_alloc_array_primitive(my_list,for_var%active,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atlist_type','atom',py_var,my_list,ierror2)
        if (ierror == 0) call list_to_class_array_atm_type('Unwrap_atlist_type','atom',my_list,for_var%atom,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_atlist_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_atlist_type

    Module Subroutine list_to_type_array1d_atlist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atlist_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_atlist_type

    Module Subroutine list_to_type_array1d_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atlist_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_atlist_type_no_alloc

    Module Subroutine list_to_type_array2d_atlist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atlist_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_atlist_type

    Module Subroutine list_to_type_array2d_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atlist_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_atlist_type_no_alloc

    Module Subroutine Wrap_matom_type(for_var,py_var,ierror)

        ! Arguments
        type(matom_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_x,nd_x_std,nd_mx,nd_lx,nd_u,nd_u_std,nd_mu,nd_lu,nd_ind,nd_varf,nd_mvarf,nd_lvarf,nd_imat,nd_skr,nd_skr_std,nd_spher_skr,nd_spher_skr_std,nd_mskr,nd_lskr,nd_ski,nd_ski_std,nd_spher_ski,nd_spher_ski_std,nd_mski,nd_lski,nd_mphas,nd_mphas_std,nd_mmphas,nd_lmphas,nd_cbas,nd_cbas_std,nd_mbas,nd_lbas,nd_chi,nd_chi_std,nd_mchi,nd_lchi

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','matom_type')
        if (ierror == 0) then
            ierror = py_var%setitem('lab',for_var%lab)
            if (ierror /= 0) then
                for_var%lab = ''
                ierror = py_var%setitem('lab',for_var%lab)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            if (ierror /= 0) then
                for_var%chemsymb = ''
                ierror = py_var%setitem('chemsymb',for_var%chemsymb)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
            if (ierror /= 0) then
                for_var%sfacsymb = ''
                ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('wyck',for_var%wyck)
            if (ierror /= 0) then
                for_var%wyck = ''
                ierror = py_var%setitem('wyck',for_var%wyck)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('active',for_var%active)
        if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror == 0) ierror = ndarray_create(nd_x_std,for_var%x_std)
        if (ierror == 0) ierror = py_var%setitem('x_std',nd_x_std)
        if (ierror == 0) ierror = ndarray_create(nd_mx,for_var%mx)
        if (ierror == 0) ierror = py_var%setitem('mx',nd_mx)
        if (ierror == 0) ierror = ndarray_create(nd_lx,for_var%lx)
        if (ierror == 0) ierror = py_var%setitem('lx',nd_lx)
        if (ierror == 0) ierror = py_var%setitem('occ',for_var%occ)
        if (ierror == 0) ierror = py_var%setitem('occ_std',for_var%occ_std)
        if (ierror == 0) ierror = py_var%setitem('mocc',for_var%mocc)
        if (ierror == 0) ierror = py_var%setitem('locc',for_var%locc)
        if (ierror == 0) ierror = py_var%setitem('biso',for_var%biso)
        if (ierror == 0) ierror = py_var%setitem('biso_std',for_var%biso_std)
        if (ierror == 0) ierror = py_var%setitem('mbiso',for_var%mbiso)
        if (ierror == 0) ierror = py_var%setitem('lbiso',for_var%lbiso)
        if (ierror == 0) then
            ierror = py_var%setitem('utype',for_var%utype)
            if (ierror /= 0) then
                for_var%utype = ''
                ierror = py_var%setitem('utype',for_var%utype)
            end if
        end if
        if (ierror == 0) then
            ierror = py_var%setitem('thtype',for_var%thtype)
            if (ierror /= 0) then
                for_var%thtype = ''
                ierror = py_var%setitem('thtype',for_var%thtype)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_u,for_var%u)
        if (ierror == 0) ierror = py_var%setitem('u',nd_u)
        if (ierror == 0) ierror = ndarray_create(nd_u_std,for_var%u_std)
        if (ierror == 0) ierror = py_var%setitem('u_std',nd_u_std)
        if (ierror == 0) ierror = py_var%setitem('ueq',for_var%ueq)
        if (ierror == 0) ierror = ndarray_create(nd_mu,for_var%mu)
        if (ierror == 0) ierror = py_var%setitem('mu',nd_mu)
        if (ierror == 0) ierror = ndarray_create(nd_lu,for_var%lu)
        if (ierror == 0) ierror = py_var%setitem('lu',nd_lu)
        if (ierror == 0) ierror = py_var%setitem('charge',for_var%charge)
        if (ierror == 0) ierror = py_var%setitem('moment',for_var%moment)
        if (ierror == 0) ierror = ndarray_create(nd_ind,for_var%ind)
        if (ierror == 0) ierror = py_var%setitem('ind',nd_ind)
        if (ierror == 0) ierror = py_var%setitem('nvar',for_var%nvar)
        if (ierror == 0) ierror = ndarray_create(nd_varf,for_var%varf)
        if (ierror == 0) ierror = py_var%setitem('varf',nd_varf)
        if (ierror == 0) ierror = ndarray_create(nd_mvarf,for_var%mvarf)
        if (ierror == 0) ierror = py_var%setitem('mvarf',nd_mvarf)
        if (ierror == 0) ierror = ndarray_create(nd_lvarf,for_var%lvarf)
        if (ierror == 0) ierror = py_var%setitem('lvarf',nd_lvarf)
        if (ierror == 0) then
            ierror = py_var%setitem('atminfo',for_var%atminfo)
            if (ierror /= 0) then
                for_var%atminfo = ''
                ierror = py_var%setitem('atminfo',for_var%atminfo)
            end if
        end if
        if (ierror == 0) ierror = py_var%setitem('nvk',for_var%nvk)
        if (ierror == 0) ierror = ndarray_create(nd_imat,for_var%imat)
        if (ierror == 0) ierror = py_var%setitem('imat',nd_imat)
        if (ierror == 0) ierror = ndarray_create(nd_skr,for_var%skr)
        if (ierror == 0) ierror = py_var%setitem('skr',nd_skr)
        if (ierror == 0) ierror = ndarray_create(nd_skr_std,for_var%skr_std)
        if (ierror == 0) ierror = py_var%setitem('skr_std',nd_skr_std)
        if (ierror == 0) ierror = ndarray_create(nd_spher_skr,for_var%spher_skr)
        if (ierror == 0) ierror = py_var%setitem('spher_skr',nd_spher_skr)
        if (ierror == 0) ierror = ndarray_create(nd_spher_skr_std,for_var%spher_skr_std)
        if (ierror == 0) ierror = py_var%setitem('spher_skr_std',nd_spher_skr_std)
        if (ierror == 0) ierror = ndarray_create(nd_mskr,for_var%mskr)
        if (ierror == 0) ierror = py_var%setitem('mskr',nd_mskr)
        if (ierror == 0) ierror = ndarray_create(nd_lskr,for_var%lskr)
        if (ierror == 0) ierror = py_var%setitem('lskr',nd_lskr)
        if (ierror == 0) ierror = ndarray_create(nd_ski,for_var%ski)
        if (ierror == 0) ierror = py_var%setitem('ski',nd_ski)
        if (ierror == 0) ierror = ndarray_create(nd_ski_std,for_var%ski_std)
        if (ierror == 0) ierror = py_var%setitem('ski_std',nd_ski_std)
        if (ierror == 0) ierror = ndarray_create(nd_spher_ski,for_var%spher_ski)
        if (ierror == 0) ierror = py_var%setitem('spher_ski',nd_spher_ski)
        if (ierror == 0) ierror = ndarray_create(nd_spher_ski_std,for_var%spher_ski_std)
        if (ierror == 0) ierror = py_var%setitem('spher_ski_std',nd_spher_ski_std)
        if (ierror == 0) ierror = ndarray_create(nd_mski,for_var%mski)
        if (ierror == 0) ierror = py_var%setitem('mski',nd_mski)
        if (ierror == 0) ierror = ndarray_create(nd_lski,for_var%lski)
        if (ierror == 0) ierror = py_var%setitem('lski',nd_lski)
        if (ierror == 0) ierror = ndarray_create(nd_mphas,for_var%mphas)
        if (ierror == 0) ierror = py_var%setitem('mphas',nd_mphas)
        if (ierror == 0) ierror = ndarray_create(nd_mphas_std,for_var%mphas_std)
        if (ierror == 0) ierror = py_var%setitem('mphas_std',nd_mphas_std)
        if (ierror == 0) ierror = ndarray_create(nd_mmphas,for_var%mmphas)
        if (ierror == 0) ierror = py_var%setitem('mmphas',nd_mmphas)
        if (ierror == 0) ierror = ndarray_create(nd_lmphas,for_var%lmphas)
        if (ierror == 0) ierror = py_var%setitem('lmphas',nd_lmphas)
        if (ierror == 0) ierror = ndarray_create(nd_cbas,for_var%cbas)
        if (ierror == 0) ierror = py_var%setitem('cbas',nd_cbas)
        if (ierror == 0) ierror = ndarray_create(nd_cbas_std,for_var%cbas_std)
        if (ierror == 0) ierror = py_var%setitem('cbas_std',nd_cbas_std)
        if (ierror == 0) ierror = ndarray_create(nd_mbas,for_var%mbas)
        if (ierror == 0) ierror = py_var%setitem('mbas',nd_mbas)
        if (ierror == 0) ierror = ndarray_create(nd_lbas,for_var%lbas)
        if (ierror == 0) ierror = py_var%setitem('lbas',nd_lbas)
        if (ierror == 0) then
            ierror = py_var%setitem('chitype',for_var%chitype)
            if (ierror /= 0) then
                for_var%chitype = ''
                ierror = py_var%setitem('chitype',for_var%chitype)
            end if
        end if
        if (ierror == 0) ierror = ndarray_create(nd_chi,for_var%chi)
        if (ierror == 0) ierror = py_var%setitem('chi',nd_chi)
        if (ierror == 0) ierror = ndarray_create(nd_chi_std,for_var%chi_std)
        if (ierror == 0) ierror = py_var%setitem('chi_std',nd_chi_std)
        if (ierror == 0) ierror = py_var%setitem('chieq',for_var%chieq)
        if (ierror == 0) ierror = ndarray_create(nd_mchi,for_var%mchi)
        if (ierror == 0) ierror = py_var%setitem('mchi',nd_mchi)
        if (ierror == 0) ierror = ndarray_create(nd_lchi,for_var%lchi)
        if (ierror == 0) ierror = py_var%setitem('lchi',nd_lchi)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_matom_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_matom_type

    Module Subroutine Unwrap_type_matom_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(matom_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_matom_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'matom_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_matom_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lab',py_var,for_var%lab,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','chemsymb',py_var,for_var%chemsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','sfacsymb',py_var,for_var%sfacsymb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','wyck',py_var,for_var%wyck,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','active',py_var,for_var%active,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','x_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','x_std',p_real_1d,for_var%x_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mx',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mx',p_real_1d,for_var%mx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lx',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lx',p_int_1d,for_var%lx,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','occ',py_var,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','occ_std',py_var,for_var%occ_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mocc',py_var,for_var%mocc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','locc',py_var,for_var%locc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','biso',py_var,for_var%biso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','biso_std',py_var,for_var%biso_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mbiso',py_var,for_var%mbiso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lbiso',py_var,for_var%lbiso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','utype',py_var,for_var%utype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','thtype',py_var,for_var%thtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','u',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','u',p_real_1d,for_var%u,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','u_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','u_std',p_real_1d,for_var%u_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','ueq',py_var,for_var%ueq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mu',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mu',p_real_1d,for_var%mu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lu',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lu',p_int_1d,for_var%lu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','charge',py_var,for_var%charge,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','moment',py_var,for_var%moment,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','ind',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','ind',p_int_1d,for_var%ind,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','nvar',py_var,for_var%nvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','varf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','varf',p_real_1d,for_var%varf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mvarf',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mvarf',p_real_1d,for_var%mvarf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lvarf',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lvarf',p_int_1d,for_var%lvarf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','atminfo',py_var,for_var%atminfo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','nvk',py_var,for_var%nvk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','imat',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','imat',p_int_1d,for_var%imat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','skr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','skr',p_real_2d,for_var%skr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','skr_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','skr_std',p_real_2d,for_var%skr_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','spher_skr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','spher_skr',p_real_2d,for_var%spher_skr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','spher_skr_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','spher_skr_std',p_real_2d,for_var%spher_skr_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mskr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mskr',p_real_2d,for_var%mskr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lskr',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lskr',p_int_2d,for_var%lskr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','ski',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','ski',p_real_2d,for_var%ski,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','ski_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','ski_std',p_real_2d,for_var%ski_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','spher_ski',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','spher_ski',p_real_2d,for_var%spher_ski,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','spher_ski_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','spher_ski_std',p_real_2d,for_var%spher_ski_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mski',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mski',p_real_2d,for_var%mski,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lski',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lski',p_int_2d,for_var%lski,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mphas',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mphas',p_real_1d,for_var%mphas,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mphas_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mphas_std',p_real_1d,for_var%mphas_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mmphas',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mmphas',p_real_1d,for_var%mmphas,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lmphas',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lmphas',p_int_1d,for_var%lmphas,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','cbas',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','cbas',p_real_2d,for_var%cbas,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','cbas_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','cbas_std',p_real_2d,for_var%cbas_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mbas',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mbas',p_real_2d,for_var%mbas,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lbas',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lbas',p_int_2d,for_var%lbas,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','chitype',py_var,for_var%chitype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','chi',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','chi',p_real_1d,for_var%chi,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','chi_std',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','chi_std',p_real_1d,for_var%chi_std,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','chieq',py_var,for_var%chieq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','mchi',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','mchi',p_real_1d,for_var%mchi,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_type','lchi',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_type','lchi',p_real_1d,for_var%lchi,ierror)
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_matom_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_matom_type

    Module Subroutine list_to_type_array1d_matom_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_matom_type

    Module Subroutine list_to_type_array1d_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_matom_type_no_alloc

    Module Subroutine list_to_type_array2d_matom_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_matom_type

    Module Subroutine list_to_type_array2d_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_matom_type_no_alloc

    Module Subroutine Wrap_matom_list_type(for_var,py_var,ierror)

        ! Arguments
        type(matom_list_type), intent(inout) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(dict) :: di_atom
        type(list) :: li_atom
        type(ndarray) :: nd_dir_mfield

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','matom_list_type')
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('suscept',for_var%suscept)
        if (ierror == 0) ierror = py_var%setitem('magfield',for_var%magfield)
        if (ierror == 0) ierror = ndarray_create(nd_dir_mfield,for_var%dir_mfield)
        if (ierror == 0) ierror = py_var%setitem('dir_mfield',nd_dir_mfield)
        if (allocated(for_var%atom)) then
            if (ierror == 0) ierror = list_create(li_atom)
            if (ierror == 0) then
                do i = 1 , size(for_var%atom)
                    ierror = dict_create(di_atom)
                    if (ierror == 0) call wrap_type(for_var%atom(i),di_atom,ierror)
                    if (ierror == 0) ierror = li_atom%append(di_atom)
                    if (ierror == 0) call di_atom%destroy
                end do
            end if
            if (ierror == 0) ierror = py_var%setitem('atom',li_atom)
        end if
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Wrap_matom_list_type: Wrapping failed'
            end if
        end if

    End Subroutine Wrap_matom_list_type

    Module Subroutine Unwrap_type_matom_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(matom_list_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_matom_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'matom_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_matom_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
                return
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_list_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_list_type','suscept',py_var,for_var%suscept,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_list_type','magfield',py_var,for_var%magfield,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_list_type','dir_mfield',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_matom_list_type','dir_mfield',p_real_1d,for_var%dir_mfield,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_matom_list_type','atom',py_var,my_list,ierror2)
        if (ierror2 == 0) then
            if (ierror == 0) call list_to_type_array('Unwrap_matom_list_type','atom',my_list,for_var%atom,ierror)
        else
            call err_clear
            call clear_error()
        end if
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            if (err_cfml%ierr == 0) then
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_matom_list_type: Unwrapping failed'
            end if
        end if

    End Subroutine Unwrap_type_matom_list_type

    Module Subroutine list_to_type_array1d_matom_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_list_type), dimension(:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array1d_matom_list_type

    Module Subroutine list_to_type_array1d_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_list_type), dimension(*), intent(inout) :: arr
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

    End Subroutine list_to_type_array1d_matom_list_type_no_alloc

    Module Subroutine list_to_type_array2d_matom_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_list_type), dimension(:,:), allocatable, intent(out) :: arr
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

    End Subroutine list_to_type_array2d_matom_list_type

    Module Subroutine list_to_type_array2d_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(matom_list_type), dimension(:,:), intent(inout) :: arr
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

    End Subroutine list_to_type_array2d_matom_list_type_no_alloc

end submodule