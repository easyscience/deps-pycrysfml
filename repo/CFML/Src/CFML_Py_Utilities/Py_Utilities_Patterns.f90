submodule (CFML_Py_Utilities) Py_Utilities_Patterns

implicit none

contains

    subroutine atoms_from_dict(di_ph,a)
        !> Get unit cell from a dictionary.

        ! Arguments
        type(dict),                      intent(inout) :: di_ph !> phase info
        type(atlist_type),               intent(out)   :: a     !> list of atoms

        ! Local variables
        integer :: ierror,i,nat
        character(len=:), allocatable :: mystr
        type(dict) :: di_at
        type(list) :: li_at
        type(object) :: item

        ! Get the number of atoms
        ierror = di_ph%getitem(item,'_atom_site')
        if (ierror == 0) ierror = cast(li_at,item)
        if (ierror == 0) ierror = li_at%len(nat)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'atoms_from_dict: unable to get number of atoms'
            return
        end if
        call allocate_atom_list(nat,a,'atm_type',0)
        do i = 1 , nat
            if (ierror == 0) ierror = li_at%getitem(item,i-1)
            if (ierror == 0) ierror = cast(di_at,item)
            if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(1),'_fract_x')
            if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(2),'_fract_y')
            if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(3),'_fract_z')
            if (ierror == 0) ierror = di_at%getitem(a%atom(i)%occ,'_occupancy')
            if (ierror == 0) ierror = di_at%getitem(item,'_label')
            if (ierror == 0) ierror = cast(mystr,item)
            if (ierror == 0) a%atom(i)%lab = mystr
            if (ierror == 0) ierror = di_at%getitem(item,'_type_symbol')
            if (ierror == 0) ierror = cast(mystr,item)
            if (ierror == 0) a%atom(i)%chemsymb = mystr
            if (ierror == 0) a%atom(i)%sfacsymb = mystr
            if (ierror == 0) ierror = di_at%getitem(a%atom(i)%u_iso,'_B_iso_or_equiv')
        end do
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'atoms_from_dict: unable to get atoms'
        end if

    end subroutine atoms_from_dict

    subroutine cell_from_dict(di_ph,cell)
        !> Get unit cell from a dictionary.

        ! Arguments
        type(dict),                      intent(inout) :: di_ph !> phase info
        class(cell_g_type), allocatable, intent(out)   :: cell  !> unit cell

        ! Local variables
        integer :: ierror
        real(kind=cp), dimension(3) :: vcell,vang

        ierror = di_ph%getitem(vcell(1),'_cell_length_a')
        if (ierror == 0) ierror = di_ph%getitem(vcell(2),'_cell_length_b')
        if (ierror == 0) ierror = di_ph%getitem(vcell(3),'_cell_length_c')
        if (ierror == 0) ierror = di_ph%getitem(vang(1),'_cell_angle_alpha')
        if (ierror == 0) ierror = di_ph%getitem(vang(2),'_cell_angle_beta')
        if (ierror == 0) ierror = di_ph%getitem(vang(3),'_cell_angle_gamma')
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'cell_from_dict: unable to '//&
            'get cell parameters'
        else
            allocate(cell_g_type :: cell)
            call set_crystal_cell(vcell,vang,cell)
        end if

    end subroutine cell_from_dict

    subroutine cw_experimental_conditions_from_dict(json,ppc)
        !> Read experimental conditions from a json file

        ! Arguments
        type(dict),                       intent(inout) :: json !> json file content
        type(powpatt_cw_conditions_type), intent(out)   :: ppc  !> experimental conditions

        ! Local variables
        integer :: ierror,i,ntth
        character(len=:), allocatable :: ex_name,r_probe
        type(dict) :: di_exs,di_ex
        type(list) :: li_ex,li_keys,li_tth
        type(object) :: item

        ! Default values
        ppc%is_asym      = .false.
        ppc%is_tth       = .false.
        ppc%job          = 0
        ppc%lambda       = 1.540560
        ppc%scale_factor = 1.0
        ppc%step         = 0.02
        ppc%tthmin       = 10.0
        ppc%tthmax       = 150.0
        ppc%title        = "Powder Pattern"
        ppc%u            = 0.004133
        ppc%v            =-0.007618
        ppc%w            = 0.006255
        ppc%x            = 0.018961
        ppc%y            = 0.000000
        ppc%zero         = 0.0
        if (allocated(ppc%tth)) deallocate(ppc%tth)

        ! Data from json
        ierror = json%getitem(item,'experiments')
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg  = 'cw_experimental_conditions_from_dict: '//&
            'experiments key not found in json file'
            return
        end if
        if (ierror == 0) ierror = cast(li_ex,item)
        if (ierror == 0) ierror = li_ex%getitem(item,0)
        if (ierror == 0) ierror = cast(di_exs,item)
        if (ierror == 0) ierror = di_exs%keys(li_keys)
        if (ierror == 0) ierror = li_keys%getitem(item,0)
        if (ierror == 0) ierror = cast(ex_name,item)
        if (ierror == 0) ierror = di_exs%getitem(item,ex_name)
        if (ierror == 0) ierror = cast(di_ex,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg  = 'cw_experimental_conditions_from_dict: '//&
            'error reading experimental section'
            return
        end if
        ierror = di_ex%getitem(item,'_diffrn_radiation_probe')
        if (ierror == 0) ierror = cast(r_probe,item)
        if (ierror == 0) then
            if (r_probe == 'neutron') then
                ppc%job = 1
            else if (r_probe == 'x-ray') then
                ppc%job = 0
            end if
        end if
        ierror = di_ex%getitem(ppc%lambda(1),'_diffrn_radiation_wavelength')
        ierror = di_ex%getitem(ppc%tthmin,'_pd_meas_2theta_range_min')
        ierror = di_ex%getitem(ppc%tthmax,'_pd_meas_2theta_range_max')
        ierror = di_ex%getitem(ppc%scale_factor,'_pd_meas_scale_factor')
        ierror = di_ex%getitem(ppc%step,'_pd_meas_2theta_range_inc')
        ierror = di_ex%getitem(ppc%zero,'_pd_meas_2theta_offset')
        ierror = di_ex%getitem(ppc%u,'_pd_instr_resolution_u')
        ierror = di_ex%getitem(ppc%v,'_pd_instr_resolution_v')
        ierror = di_ex%getitem(ppc%w,'_pd_instr_resolution_w')
        ierror = di_ex%getitem(ppc%x,'_pd_instr_resolution_x')
        ierror = di_ex%getitem(ppc%y,'_pd_instr_resolution_y')
        ierror = 0
        ierror = di_ex%getitem(ppc%asym1,'_pd_instr_reflex_s_l')
        if (ierror == 0) ppc%is_asym = .true.
        ierror = 0
        ierror = di_ex%getitem(ppc%asym2,'_pd_instr_reflex_d_l')
        if (ierror == 0) ppc%is_asym = .true.
        ierror = 0
        ierror = di_ex%getitem(item,'_pd_meas_2theta_scan')
        if (ierror == 0) ierror = cast(li_tth,item)
        if (ierror == 0) then
            ierror = li_tth%len(ntth)
            if (ierror == 0 .and. ntth > 0) then
                allocate(ppc%tth(ntth))
                do i = 0 , ntth - 1
                    if (ierror == 0) ierror = li_tth%getitem(item,i)
                    if (ierror == 0) ierror = cast(ppc%tth(i+1),item)
                end do
                if (ierror == 0) then 
                    ppc%is_tth = .true.
                    ppc%tthmax = maxval(ppc%tth(1:ntth))
                end if
            end if
        end if

        ! Clear any pending Python exceptions from failed optional getitem calls.
        ! On Python 3.13+, a pending exception causes SystemError when later C API
        ! calls return successfully.
        call err_clear()

    end subroutine cw_experimental_conditions_from_dict

    module subroutine cw_powder_pattern_from_dict(json,xc,yc)
        !> Computes a powder diffraction pattern from information provided
        !> by a json file.

        ! Arguments
        type(dict),                                 intent(inout) :: json !> json file content
        real(kind=cp),   dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
        real(kind=cp),   dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity

        ! Local variables
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: a
        type(powpatt_cw_conditions_type) :: ppc

        call clear_error()
        call cw_read_json(json,cell,spg,a,ppc)
        if (err_cfml%ierr /= 0) return
        if (ppc%is_tth) then
            call cw_powder_pattern(cell,spg,a,ppc,xc,yc,ppc%tth)
        else
            call cw_powder_pattern(cell,spg,a,ppc,xc,yc)
        end if

    end subroutine cw_powder_pattern_from_dict

    subroutine cw_read_json(json,cell,spg,a,ppc)
        !> Read content of a json file. The content of the json file
        !> must be put in a dictionary before calling this function.

        ! Arguments
        type(dict),                       intent(inout) :: json !> json file content
        class(cell_g_type), allocatable,  intent(out)   :: cell !> unit cell
        class(spg_type),    allocatable,  intent(out)   :: spg  !> space group
        type(atlist_type),                intent(out)   :: a    !> list of atoms
        type(powpatt_cw_conditions_type), intent(out)   :: ppc  !> experimental conditions

        ! Local variables
        integer :: ierror
        character(len=:), allocatable :: cif
        type(object) :: item

        ierror = json%getitem(item,'cif')
        if (ierror == 0) then
            ! Structural data in a CIF file
            if (ierror == 0) ierror = cast(cif,item)
            if (ierror == 0) call read_xtal_structure(cif,cell,spg,a)
            if (ierror /= 0) then
                err_cfml%ierr = -1
                err_cfml%msg = 'cw_read_json: unable to cast cif'
            end if
            if (err_cfml%ierr /= 0) return
        else
            ! Structural data in the JSON file
            call xtal_structure_from_dict(json,0,cell,spg,a)
            if (err_cfml%ierr /= 0) return
        end if
        call cw_experimental_conditions_from_dict(json,ppc)
        if (err_cfml%ierr /= 0) return

    end subroutine cw_read_json

    subroutine set_fullprof_occupancies(a,spg)

        ! Arguments
        type(atlist_type), intent(inout) :: a    !> list of atoms
        class(spg_type),   intent(in)    :: spg  !> space group

        ! Local variables
        integer :: i,mpos

        do i = 1 , a%natoms
            mpos = get_multip_pos(a%atom(i)%x,spg)
            a%atom(i)%occ = a%atom(i)%occ * mpos / (1.0*spg%multip)
        end do

    end subroutine set_fullprof_occupancies

    subroutine spg_from_dict(di_ph,spg)
        !> Get space group from a dictionary.

        ! Arguments
        type(dict),                      intent(inout) :: di_ph !> phase info
        class(spg_type), allocatable,    intent(out)   :: spg   !> space group

        ! Local variables
        integer :: ierror
        character(len=:), allocatable :: spg_name
        type(object) :: item

        ierror = di_ph%getitem(item,'_space_group_name_H-M_alt')
        if (ierror == 0) ierror = cast(spg_name,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'spg_from_dict: unable to '//&
            'get space group'
        else
            call set_spacegroup(spg_name,spg)
        end if

    end subroutine spg_from_dict

    subroutine tof_experimental_conditions_from_dict(json,ppc)
        !> Read experimental conditions from a json file

        ! Arguments
        type(dict),                        intent(inout) :: json !> json file content
        type(powpatt_tof_conditions_type), intent(out)   :: ppc  !> experimental conditions

        ! Local variables
        integer :: ierror,i,ntof
        character(len=:), allocatable :: ex_name
        type(dict) :: di_exs,di_ex
        type(list) :: li_ex,li_keys,li_tof
        type(object) :: item

        ! Default values
        lorcomp          = .false.
        ppc%is_tof       = .false.
        ppc%alpha0       = 0.000000
        ppc%alpha1       = 0.716993
        ppc%bank_angle   = 90.0
        ppc%beta0        = 0.050835
        ppc%beta1        = 0.010232
        ppc%dtt1         = 5000.0
        ppc%dtt2         = 0.0
        ppc%gamma0       = 0.000000
        ppc%gamma1       = 4.695000
        ppc%gamma2       = 0.000000
        ppc%scale_factor = 1.0
        ppc%sigma0       = 3.519000
        ppc%sigma1       = 63.385000
        ppc%sigma2       = 1.588000
        ppc%step         = 4.0
        ppc%title        = "Powder Pattern"
        ppc%tof_max      = 20000.0
        ppc%tof_min      = 2000.0
        ppc%zero         = 0.0
        if (allocated(ppc%tof)) deallocate(ppc%tof)

        ! Data from json
        ierror = json%getitem(item,'experiments')
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg  = 'tof_experimental_conditions_from_dict: '//&
            'experiments key not found in json file'
            return
        end if
        if (ierror == 0) ierror = cast(li_ex,item)
        if (ierror == 0) ierror = li_ex%getitem(item,0)
        if (ierror == 0) ierror = cast(di_exs,item)
        if (ierror == 0) ierror = di_exs%keys(li_keys)
        if (ierror == 0) ierror = li_keys%getitem(item,0)
        if (ierror == 0) ierror = cast(ex_name,item)
        if (ierror == 0) ierror = di_exs%getitem(item,ex_name)
        if (ierror == 0) ierror = cast(di_ex,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg  = 'tof_experimental_conditions_from_dict: '//&
            'error reading experimental section'
            return
        end if
        ierror = di_ex%getitem(ppc%alpha0,'_pd_jorg_vondreele_alpha0')
        ierror = di_ex%getitem(ppc%alpha1,'_pd_jorg_vondreele_alpha1')
        ierror = di_ex%getitem(ppc%beta0,'_pd_jorg_vondreele_beta0')
        ierror = di_ex%getitem(ppc%beta1,'_pd_jorg_vondreele_beta1')
        ierror = di_ex%getitem(ppc%gamma0,'_pd_jorg_vondreele_gamma0')
        ierror = di_ex%getitem(ppc%gamma1,'_pd_jorg_vondreele_gamma1')
        ierror = di_ex%getitem(ppc%gamma2,'_pd_jorg_vondreele_gamma2')
        ierror = di_ex%getitem(ppc%sigma0,'_pd_jorg_vondreele_sigma0')
        ierror = di_ex%getitem(ppc%sigma1,'_pd_jorg_vondreele_sigma1')
        ierror = di_ex%getitem(ppc%sigma2,'_pd_jorg_vondreele_sigma2')
        ierror = di_ex%getitem(ppc%bank_angle,'_pd_meas_tof_bank_angle')
        ierror = di_ex%getitem(ppc%zero,'_pd_meas_tof_offset')
        ierror = di_ex%getitem(ppc%dtt1,'_pd_meas_tof_dtt1')
        ierror = di_ex%getitem(ppc%dtt2,'_pd_meas_tof_dtt2')
        ierror = di_ex%getitem(ppc%step,'_pd_meas_tof_range_inc')
        ierror = di_ex%getitem(ppc%tof_min,'_pd_meas_tof_range_min')
        ierror = di_ex%getitem(ppc%tof_max,'_pd_meas_tof_range_max')
        ierror = 0
        ierror = di_ex%getitem(item,'_pd_meas_time_of_flight')
        if (ierror == 0) ierror = cast(li_tof,item)
        if (ierror == 0) then
            ierror = li_tof%len(ntof)
            if (ierror == 0 .and. ntof > 0) then
                allocate(ppc%tof(ntof))
                do i = 0 , ntof - 1
                    if (ierror == 0) ierror = li_tof%getitem(item,i)
                    if (ierror == 0) ierror = cast(ppc%tof(i+1),item)
                end do
                if (ierror == 0) ppc%is_tof = .true.
            end if
        end if
        if (ppc%sigma1 > 0.000001) lorcomp = .true.

        ! Clear any pending Python exceptions from failed optional getitem calls.
        ! On Python 3.13+, a pending exception causes SystemError when later C API
        ! calls return successfully.
        call err_clear()

    end subroutine tof_experimental_conditions_from_dict

    module subroutine tof_powder_pattern_from_dict(json,xc,yc)
        !> Computes a powder diffraction pattern from information provided
        !> by a json file.

        ! Arguments
        type(dict),                                 intent(inout) :: json !> json file content
        real(kind=cp),   dimension(:), allocatable, intent(out)   :: xc   !> two theta angle
        real(kind=cp),   dimension(:), allocatable, intent(out)   :: yc   !> calculated intensity

        ! Local variables
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: a
        type(powpatt_tof_conditions_type) :: ppc

        call clear_error()
        call tof_read_json(json,cell,spg,a,ppc)
        if (err_cfml%ierr /= 0) return
        if (ppc%is_tof) then
            call tof_powder_pattern(cell,spg,a,ppc,xc,yc,ppc%tof)
        else
            call tof_powder_pattern(cell,spg,a,ppc,xc,yc)
        end if

    end subroutine tof_powder_pattern_from_dict

    subroutine tof_read_json(json,cell,spg,a,ppc)
        !> Read content of a json file. The content of the json file
        !> must be put in a dictionary before calling this function.

        ! Arguments
        type(dict),                        intent(inout) :: json !> json file content
        class(cell_g_type), allocatable,   intent(out)   :: cell !> unit cell
        class(spg_type),    allocatable,   intent(out)   :: spg  !> space group
        type(atlist_type),                 intent(out)   :: a    !> list of atoms
        type(powpatt_tof_conditions_type), intent(out)   :: ppc  !> experimental conditions

        ! Local variables
        integer :: ierror
        character(len=:), allocatable :: cif
        type(object) :: item

        ierror = json%getitem(item,'cif')
        if (ierror == 0) then
            ! Structural data in a CIF file
            if (ierror == 0) ierror = cast(cif,item)
            if (ierror == 0) call read_xtal_structure(cif,cell,spg,a)
            if (ierror /= 0) then
                err_cfml%ierr = -1
                err_cfml%msg = 'tof_read_json: unable to cast cif'
            end if
            if (err_cfml%ierr /= 0) return
        else
            ! Structural data in the JSON file
            call xtal_structure_from_dict(json,0,cell,spg,a)
            if (err_cfml%ierr /= 0) return
        end if
        call tof_experimental_conditions_from_dict(json,ppc)
        if (err_cfml%ierr /= 0) return

    end subroutine tof_read_json

    subroutine xtal_structure_from_dict(json,iph,cell,spg,a)
        !> Read crystal structure data from content of a json file

        ! Arguments
        type(dict),                      intent(inout) :: json !> json file content
        integer,                         intent(in)    :: iph  !> phase number
        class(cell_g_type), allocatable, intent(out)   :: cell !> unit cell
        class(spg_type), allocatable,    intent(out)   :: spg  !> space group
        type(atlist_type),               intent(out)   :: a    !> list of atoms

        ! Local variables
        integer :: ierror
        character(len=:), allocatable :: ph_name
        type(dict) :: di_phs,di_ph
        type(list) :: li_keys,li_ph
        type(object) :: item

        ierror = json%getitem(item,'phases')
        if (ierror == 0) ierror = cast(li_ph,item)
        if (ierror == 0) ierror = li_ph%getitem(item,0)
        if (ierror == 0) ierror = cast(di_phs,item)
        if (ierror == 0) ierror = di_phs%keys(li_keys)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'xtal_structure_from_dict: unable to '//&
            'get dictionary containing phases'
            return
        end if

        ! Get the first phase
        if (ierror == 0) ierror = li_keys%getitem(item,iph)
        if (ierror == 0) ierror = cast(ph_name,item)
        if (ierror == 0) ierror = di_phs%getitem(item,ph_name)
        if (ierror == 0) ierror = cast(di_ph,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'xtal_structure_from_dict: unable to '//&
            'get first phase'
            return
        end if

        call cell_from_dict(di_ph,cell)
        if (err_cfml%ierr /= 0) return

        call spg_from_dict(di_ph,spg)
        if (err_cfml%ierr /= 0) return

        call atoms_from_dict(di_ph,a)
        if (err_cfml%ierr /= 0) return
        call set_fullprof_occupancies(a,spg)

    end subroutine xtal_structure_from_dict

end submodule Py_Utilities_Patterns