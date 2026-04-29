module crysfml08lib

    use forpy_mod, str_forpy => str
    use iso_c_binding
    use cfml_atoms
    use cfml_bckpeaks
    use cfml_bonds_tables
    use cfml_bvs_tables
    use cfml_diffpatt
    use cfml_enbvs
    use cfml_export_vtk
    use cfml_extincorr
    use cfml_geom
    use cfml_globaldeps
    use cfml_gspacegroups
    use cfml_ill_instrm_data
    use cfml_ioform
    use cfml_kvec_symmetry
    use cfml_laue
    use cfml_magnetic_database
    use cfml_maths
    use cfml_messages
    use cfml_metrics
    use cfml_molecules
    use cfml_powder
    use cfml_profiles
    use cfml_propagation_vectors
    use cfml_py_utilities
    use cfml_rational
    use cfml_reflections
    use cfml_scattering_tables
    use cfml_simulated_annealing
    use cfml_strings
    use cfml_structure_factors
    use cfml_superspace_database
    use cfml_sxtal_geom
    use cfml_symmetry_tables
    use cfml_utilities
    use cfml_wraps_utils
    use ieee_arithmetic
    use cfml_wraps
    use cfml_wraps_utils

    implicit none

    type(PythonModule), save :: mod_crysfml08lib
    type(PythonMethodTable), save :: table_crysfml08lib

    contains

    function PyInit_crysfml08lib() bind(c,name='PyInit_crysfml08lib') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml08lib

        type(c_ptr) :: m

        m = Init()

    end function PyInit_crysfml08lib

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_crysfml08lib%init(43)
        call table_crysfml08lib%add_method('f_automatic_peak_background_search','wrapper of function cfml_bckpeaks.automatic_peak_background_search', &
            METH_VARARGS,c_funloc(f_automatic_peak_background_search))
        call table_crysfml08lib%add_method('f_get_pkb_conditions','wrapper of function cfml_bckpeaks.get_pkb_conditions', &
            METH_VARARGS,c_funloc(f_get_pkb_conditions))
        call table_crysfml08lib%add_method('f_set_pkb_conditions','wrapper of function cfml_bckpeaks.set_pkb_conditions', &
            METH_VARARGS,c_funloc(f_set_pkb_conditions))
        call table_crysfml08lib%add_method('f_get_abs_xs','wrapper of function cfml_scattering_tables.get_abs_xs', &
            METH_VARARGS,c_funloc(f_get_abs_xs))
        call table_crysfml08lib%add_method('f_get_anomalous_scfac','wrapper of function cfml_scattering_tables.get_anomalous_scfac', &
            METH_VARARGS,c_funloc(f_get_anomalous_scfac))
        call table_crysfml08lib%add_method('f_get_atomic_mass','wrapper of function cfml_scattering_tables.get_atomic_mass', &
            METH_VARARGS,c_funloc(f_get_atomic_mass))
        call table_crysfml08lib%add_method('f_get_atomic_vol','wrapper of function cfml_scattering_tables.get_atomic_vol', &
            METH_VARARGS,c_funloc(f_get_atomic_vol))
        call table_crysfml08lib%add_method('f_get_chem_info','wrapper of function cfml_scattering_tables.get_chem_info', &
            METH_VARARGS,c_funloc(f_get_chem_info))
        call table_crysfml08lib%add_method('f_get_chem_symb','wrapper of function cfml_scattering_tables.get_chem_symb', &
            METH_VARARGS,c_funloc(f_get_chem_symb))
        call table_crysfml08lib%add_method('f_get_covalent_radius','wrapper of function cfml_scattering_tables.get_covalent_radius', &
            METH_VARARGS,c_funloc(f_get_covalent_radius))
        call table_crysfml08lib%add_method('f_get_fermi_length','wrapper of function cfml_scattering_tables.get_fermi_length', &
            METH_VARARGS,c_funloc(f_get_fermi_length))
        call table_crysfml08lib%add_method('f_get_inc_xs','wrapper of function cfml_scattering_tables.get_inc_xs', &
            METH_VARARGS,c_funloc(f_get_inc_xs))
        call table_crysfml08lib%add_method('f_get_ionic_radius','wrapper of function cfml_scattering_tables.get_ionic_radius', &
            METH_VARARGS,c_funloc(f_get_ionic_radius))
        call table_crysfml08lib%add_method('f_get_magnetic_form','wrapper of function cfml_scattering_tables.get_magnetic_form', &
            METH_VARARGS,c_funloc(f_get_magnetic_form))
        call table_crysfml08lib%add_method('f_get_magnetic_j2','wrapper of function cfml_scattering_tables.get_magnetic_j2', &
            METH_VARARGS,c_funloc(f_get_magnetic_j2))
        call table_crysfml08lib%add_method('f_get_magnetic_j4','wrapper of function cfml_scattering_tables.get_magnetic_j4', &
            METH_VARARGS,c_funloc(f_get_magnetic_j4))
        call table_crysfml08lib%add_method('f_get_magnetic_j6','wrapper of function cfml_scattering_tables.get_magnetic_j6', &
            METH_VARARGS,c_funloc(f_get_magnetic_j6))
        call table_crysfml08lib%add_method('f_get_z_symb','wrapper of function cfml_scattering_tables.get_z_symb', &
            METH_VARARGS,c_funloc(f_get_z_symb))
        call table_crysfml08lib%add_method('f_get_xray_form','wrapper of function cfml_scattering_tables.get_xray_form', &
            METH_VARARGS,c_funloc(f_get_xray_form))
        call table_crysfml08lib%add_method('f_get_xray_wavelengths','wrapper of function cfml_scattering_tables.get_xray_wavelengths', &
            METH_VARARGS,c_funloc(f_get_xray_wavelengths))
        call table_crysfml08lib%add_method('f_set_cell','wrapper of function cfml_metrics.set_cell', &
            METH_VARARGS,c_funloc(f_set_cell))
        call table_crysfml08lib%add_method('f_cw_powder_pattern_from_dict','wrapper of function cfml_py_utilities.cw_powder_pattern_from_dict', &
            METH_VARARGS,c_funloc(f_cw_powder_pattern_from_dict))
        call table_crysfml08lib%add_method('f_read_crystal_structure','wrapper of function cfml_py_utilities.read_crystal_structure', &
            METH_VARARGS,c_funloc(f_read_crystal_structure))
        call table_crysfml08lib%add_method('f_magnetic_structure_factors_from_mcif','wrapper of function cfml_py_utilities.magnetic_structure_factors_from_mcif', &
            METH_VARARGS,c_funloc(f_magnetic_structure_factors_from_mcif))
        call table_crysfml08lib%add_method('f_set_boundary','wrapper of function cfml_py_utilities.set_boundary', &
            METH_VARARGS,c_funloc(f_set_boundary))
        call table_crysfml08lib%add_method('f_set_crystal_coordination','wrapper of function cfml_py_utilities.set_crystal_coordination', &
            METH_VARARGS,c_funloc(f_set_crystal_coordination))
        call table_crysfml08lib%add_method('f_set_mask_atoms','wrapper of function cfml_py_utilities.set_mask_atoms', &
            METH_VARARGS,c_funloc(f_set_mask_atoms))
        call table_crysfml08lib%add_method('f_structure_factors_from_cif','wrapper of function cfml_py_utilities.structure_factors_from_cif', &
            METH_VARARGS,c_funloc(f_structure_factors_from_cif))
        call table_crysfml08lib%add_method('f_tof_powder_pattern_from_dict','wrapper of function cfml_py_utilities.tof_powder_pattern_from_dict', &
            METH_VARARGS,c_funloc(f_tof_powder_pattern_from_dict))
        call table_crysfml08lib%add_method('f_update_global_phase','wrapper of function cfml_py_utilities.update_global_phase', &
            METH_VARARGS,c_funloc(f_update_global_phase))
        call table_crysfml08lib%add_method('f_calculate_laue_image','wrapper of function cfml_py_utilities.calculate_laue_image', &
            METH_VARARGS,c_funloc(f_calculate_laue_image))
        call table_crysfml08lib%add_method('f_set_instrument_esmeralda','wrapper of function cfml_py_utilities.set_instrument_esmeralda', &
            METH_VARARGS,c_funloc(f_set_instrument_esmeralda))
        call table_crysfml08lib%add_method('f_set_spg_esmeralda','wrapper of function cfml_py_utilities.set_spg_esmeralda', &
            METH_VARARGS,c_funloc(f_set_spg_esmeralda))
        call table_crysfml08lib%add_method('f_set_ub_esmeralda','wrapper of function cfml_py_utilities.set_ub_esmeralda', &
            METH_VARARGS,c_funloc(f_set_ub_esmeralda))
        call table_crysfml08lib%add_method('f_get_shubnikov_info','wrapper of function cfml_symmetry_tables.get_shubnikov_info', &
            METH_VARARGS,c_funloc(f_get_shubnikov_info))
        call table_crysfml08lib%add_method('f_get_spgr_info','wrapper of function cfml_symmetry_tables.get_spgr_info', &
            METH_VARARGS,c_funloc(f_get_spgr_info))
        call table_crysfml08lib%add_method('f_load_pattern','wrapper of function cfml_diffpatt.load_pattern', &
            METH_VARARGS,c_funloc(f_load_pattern))
        call table_crysfml08lib%add_method('f_read_xtal_structure','wrapper of function cfml_ioform.read_xtal_structure', &
            METH_VARARGS,c_funloc(f_read_xtal_structure))
        call table_crysfml08lib%add_method('f_get_maxnumref','wrapper of function cfml_reflections.get_maxnumref', &
            METH_VARARGS,c_funloc(f_get_maxnumref))
        call table_crysfml08lib%add_method('f_generate_reflections','wrapper of function cfml_reflections.generate_reflections', &
            METH_VARARGS,c_funloc(f_generate_reflections))
        call table_crysfml08lib%add_method('f_init_reflist','wrapper of function cfml_reflections.init_reflist', &
            METH_VARARGS,c_funloc(f_init_reflist))
        call table_crysfml08lib%add_method('f_set_spacegroup_from_dbase','wrapper of function cfml_gspacegroups.set_spacegroup_from_dbase', &
            METH_VARARGS,c_funloc(f_set_spacegroup_from_dbase))
        call table_crysfml08lib%add_method('f_set_spacegroup_from_generators','wrapper of function cfml_gspacegroups.set_spacegroup_from_generators', &
            METH_VARARGS,c_funloc(f_set_spacegroup_from_generators))

        m = mod_crysfml08lib%init('crysfml08lib','A Python API for CrysFML08',table_crysfml08lib)

    end function Init

    function f_automatic_peak_background_search(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure automatic_peak_background_search of module CFML_BckPeaks

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_pat
        real(kind=cp) :: x1
        real(kind=cp) :: x2
        character(len=:), allocatable :: mode

        ! Variables in returned tuple
        type(dict) :: di_pkb

        ! Unwrapped variables
        type(diffpat_e_type) :: pat
        type(pkb_type) :: pkb

        ! Local parameters
        integer, parameter :: NMANDATORY = 4

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_automatic_peak_background_search','pat',item,di_pat,ierror)
        if (ierror == 0) call unwrap_class_diffpat_e_type_no_alloc(di_pat,pat,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_automatic_peak_background_search','x1',item,x1,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('f_automatic_peak_background_search','x2',item,x2,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('f_automatic_peak_background_search','mode',item,mode,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_automatic_peak_background_search: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_automatic_peak_background_search: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            pkb = automatic_peak_background_search(pat,x1,x2,mode)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_automatic_peak_background_search: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_pkb)
            if (ierror == 0) call wrap_type(pkb,di_pkb,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_automatic_peak_background_search: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_pkb)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_automatic_peak_background_search

    function f_get_pkb_conditions(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_pkb_conditions of module CFML_BckPeaks

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr

        ! Variables in returned tuple
        type(dict) :: di_pkbc

        ! Unwrapped variables
        type(peak_search_cond_type) :: pkbc

        ! Local parameters
        integer, parameter :: NMANDATORY = 0

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_pkb_conditions: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_pkb_conditions: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            pkbc = get_pkb_conditions()
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_pkb_conditions: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_pkbc)
            if (ierror == 0) call wrap_type(pkbc,di_pkbc,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_pkb_conditions: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_pkbc)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_pkb_conditions

    function f_set_pkb_conditions(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_pkb_conditions of module CFML_BckPeaks

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real(kind=cp), target :: pk_th
        real(kind=cp), pointer :: ptr_pk_th => null()
        real(kind=cp), target :: sh_th
        real(kind=cp), pointer :: ptr_sh_th => null()
        real(kind=cp), target :: bg_th
        real(kind=cp), pointer :: ptr_bg_th => null()
        integer, target :: peak_kind
        integer, pointer :: ptr_peak_kind => null()
        integer, target :: iter
        integer, pointer :: ptr_iter => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 0

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_pk_th => null()
            ptr_sh_th => null()
            ptr_bg_th => null()
            ptr_peak_kind => null()
            ptr_iter => null()
        end if
        if (ierror == 0 .and. nargs > 0) then
            ! Optional arguments
            ierror2 = args%getitem(item,0)
            if (ierror2 == 0) call get_var_from_item('f_set_pkb_conditions','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'pk_th')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_pkb_conditions','pk_th',item,pk_th,ierror)
                    if (ierror == 0) ptr_pk_th => pk_th
                else
                    ptr_pk_th => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sh_th')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_pkb_conditions','sh_th',item,sh_th,ierror)
                    if (ierror == 0) ptr_sh_th => sh_th
                else
                    ptr_sh_th => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'bg_th')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_pkb_conditions','bg_th',item,bg_th,ierror)
                    if (ierror == 0) ptr_bg_th => bg_th
                else
                    ptr_bg_th => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'peak_kind')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_pkb_conditions','peak_kind',item,peak_kind,ierror)
                    if (ierror == 0) ptr_peak_kind => peak_kind
                else
                    ptr_peak_kind => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'iter')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_pkb_conditions','iter',item,iter,ierror)
                    if (ierror == 0) ptr_iter => iter
                else
                    ptr_iter => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_pkb_conditions: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_pkb_conditions: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call set_pkb_conditions(ptr_pk_th,ptr_sh_th,ptr_bg_th,ptr_peak_kind,ptr_iter)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_pkb_conditions: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        ierror = nonetype_create(nret)
        resul = nret%get_c_ptr()

    end function f_set_pkb_conditions

    function f_get_abs_xs(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_abs_xs of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: u

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_abs_xs','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_abs_xs: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_abs_xs: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            u = get_abs_xs(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_abs_xs: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,u)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_abs_xs

    function f_get_anomalous_scfac(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_anomalous_scfac of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(anomalous_sc_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_anomalous_scfac','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_anomalous_scfac: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_anomalous_scfac: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_anomalous_scfac(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_anomalous_scfac: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_anomalous_scfac: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_anomalous_scfac

    function f_get_atomic_mass(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_atomic_mass of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: mass

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_atomic_mass','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_atomic_mass: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_atomic_mass: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            mass = get_atomic_mass(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_atomic_mass: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,mass)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_atomic_mass

    function f_get_atomic_vol(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_atomic_vol of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: vol

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_atomic_vol','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_atomic_vol: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_atomic_vol: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            vol = get_atomic_vol(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_atomic_vol: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,vol)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_atomic_vol

    function f_get_chem_info(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_chem_info of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(chem_info_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_chem_info','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_chem_info: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_chem_info: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_chem_info(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_chem_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_chem_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_chem_info

    function f_get_chem_symb(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_chem_symb of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: label

        ! Variables in returned tuple
        character(len=:), allocatable :: symb

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_chem_symb','label',item,label,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_chem_symb: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_chem_symb: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            symb = get_chem_symb(label)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_chem_symb: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,symb)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_chem_symb

    function f_get_covalent_radius(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_covalent_radius of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: rad

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_covalent_radius','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_covalent_radius: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_covalent_radius: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            rad = get_covalent_radius(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_covalent_radius: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,rad)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_covalent_radius

    function f_get_fermi_length(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_fermi_length of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: b

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_fermi_length','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_fermi_length: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_fermi_length: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            b = get_fermi_length(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_fermi_length: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,b)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_fermi_length

    function f_get_inc_xs(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_inc_xs of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        real(kind=cp) :: u

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_inc_xs','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_inc_xs: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_inc_xs: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            u = get_inc_xs(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_inc_xs: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,u)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_inc_xs

    function f_get_ionic_radius(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_ionic_radius of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb
        integer, target :: valence

        ! Variables in returned tuple
        real(kind=cp) :: rad

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_ionic_radius','symb',item,symb,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_get_ionic_radius','valence',item,valence,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_ionic_radius: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_ionic_radius: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            rad = get_ionic_radius(symb,valence)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_ionic_radius: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,rad)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_ionic_radius

    function f_get_magnetic_form(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_magnetic_form of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(magnetic_form_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_magnetic_form','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_magnetic_form: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_magnetic_form: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_magnetic_form(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_form: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_form: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_magnetic_form

    function f_get_magnetic_j2(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_magnetic_j2 of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(magnetic_form_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_magnetic_j2','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_magnetic_j2: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_magnetic_j2: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_magnetic_j2(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j2: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j2: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_magnetic_j2

    function f_get_magnetic_j4(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_magnetic_j4 of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(magnetic_form_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_magnetic_j4','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_magnetic_j4: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_magnetic_j4: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_magnetic_j4(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j4: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j4: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_magnetic_j4

    function f_get_magnetic_j6(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_magnetic_j6 of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(magnetic_form_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_magnetic_j6','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_magnetic_j6: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_magnetic_j6: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_magnetic_j6(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j6: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_magnetic_j6: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_magnetic_j6

    function f_get_z_symb(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_z_symb of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb

        ! Variables in returned tuple
        integer :: z

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_z_symb','symb',item,symb,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_z_symb: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_z_symb: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            z = get_z_symb(symb)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_z_symb: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,z)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_z_symb

    function f_get_xray_form(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_xray_form of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(xray_form_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_xray_form','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_xray_form: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_xray_form: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_xray_form(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_xray_form: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_xray_form: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_xray_form

    function f_get_xray_wavelengths(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_xray_wavelengths of module CFML_Scattering_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(xray_wavelength_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_xray_wavelengths','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_xray_wavelengths: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_xray_wavelengths: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_xray_wavelengths(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_xray_wavelengths: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_xray_wavelengths: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_xray_wavelengths

    function f_set_cell(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_cell of module CFML_Metrics

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_vcell
        type(ndarray) :: nd_vang
        character(len=:), allocatable, target :: carttype
        character(len=:), pointer :: ptr_carttype => null()
        type(ndarray) :: nd_vscell
        real(kind=cp), dimension(:), pointer :: ptr_vscell => null()
        type(ndarray) :: nd_vsang
        real(kind=cp), dimension(:), pointer :: ptr_vsang => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_cell

        ! Unwrapped variables
        real(kind=cp), dimension(3) :: vcell
        real(kind=cp), dimension(:), pointer :: p_vcell
        real(kind=cp), dimension(3) :: vang
        real(kind=cp), dimension(:), pointer :: p_vang
        real(kind=cp), dimension(3), target :: vscell
        real(kind=cp), dimension(:), pointer :: p_vscell
        real(kind=cp), dimension(3), target :: vsang
        real(kind=cp), dimension(:), pointer :: p_vsang
        type(cell_g_type) :: cell

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_carttype => null()
            ptr_vscell => null()
            ptr_vsang => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_cell','vcell',item,nd_vcell,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_cell','vcell',nd_vcell,p_vcell,ierror)
        if (ierror == 0) call pointer_to_array('set_cell','vcell',p_vcell,vcell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_set_cell','vang',item,nd_vang,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_cell','vang',nd_vang,p_vang,ierror)
        if (ierror == 0) call pointer_to_array('set_cell','vang',p_vang,vang,ierror)
        if (ierror == 0 .and. nargs > 2) then
            ! Optional arguments
            ierror2 = args%getitem(item,2)
            if (ierror2 == 0) call get_var_from_item('f_set_cell','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'carttype')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_cell','carttype',item,carttype,ierror)
                    if (ierror == 0) ptr_carttype => carttype
                else
                    ptr_carttype => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'vscell')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_cell','vscell',item,nd_vscell,ierror)
                    if (ierror == 0) call ndarray_to_pointer('set_cell','vscell',nd_vscell,p_vscell,ierror)
                    if (ierror == 0) call pointer_to_array('set_cell','vscell',p_vscell,vscell,ierror)
                    if (ierror == 0) ptr_vscell => vscell
                else
                    ptr_vscell => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'vsang')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_cell','vsang',item,nd_vsang,ierror)
                    if (ierror == 0) call ndarray_to_pointer('set_cell','vsang',nd_vsang,p_vsang,ierror)
                    if (ierror == 0) call pointer_to_array('set_cell','vsang',p_vsang,vsang,ierror)
                    if (ierror == 0) ptr_vsang => vsang
                else
                    ptr_vsang => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_cell: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_cell: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            cell = set_cell(vcell,vang,ptr_carttype,ptr_vscell,ptr_vsang)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_cell: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_cell)
            if (ierror == 0) call wrap_type(cell,di_cell,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_cell: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_cell)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_cell

    function f_cw_powder_pattern_from_dict(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure cw_powder_pattern_from_dict of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: json

        ! Variables in returned tuple
        type(ndarray) :: nd_xc
        type(ndarray) :: nd_yc

        ! Unwrapped variables
        real(kind=cp), dimension(:), allocatable :: xc
        real(kind=cp), dimension(:), allocatable :: yc

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_cw_powder_pattern_from_dict','json',item,json,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_cw_powder_pattern_from_dict: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_cw_powder_pattern_from_dict: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call cw_powder_pattern_from_dict(json,xc,yc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_cw_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = ndarray_create(nd_xc,xc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_cw_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            ierror = ndarray_create(nd_yc,yc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_cw_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,nd_xc)
            ierror = ret%setitem(1,nd_yc)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_cw_powder_pattern_from_dict

    function f_read_crystal_structure(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure read_crystal_structure of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: filename
        character(len=:), allocatable, target :: database_path
        character(len=:), pointer :: ptr_database_path => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_crystal

        ! Unwrapped variables
        type(crystal_type) :: crystal

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_database_path => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_read_crystal_structure','filename',item,filename,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_read_crystal_structure','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'database_path')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_read_crystal_structure','database_path',item,database_path,ierror)
                    if (ierror == 0) ptr_database_path => database_path
                else
                    ptr_database_path => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_read_crystal_structure: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_read_crystal_structure: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            crystal = read_crystal_structure(filename,ptr_database_path)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_crystal_structure: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_crystal)
            if (ierror == 0) call wrap_type(crystal,di_crystal,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_crystal_structure: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_crystal)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_read_crystal_structure

    function f_magnetic_structure_factors_from_mcif(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure magnetic_structure_factors_from_mcif of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: mcif_file
        type(list) :: li_sfacsymb
        character(len=:), allocatable, target :: experiment
        character(len=:), pointer :: ptr_experiment => null()
        real(kind=cp), target :: sintlmin
        real(kind=cp), pointer :: ptr_sintlmin => null()
        real(kind=cp), target :: sintlmax
        real(kind=cp), pointer :: ptr_sintlmax => null()
        logical, target :: unique
        logical, pointer :: ptr_unique => null()
        logical, target :: mag_only
        logical, pointer :: ptr_mag_only => null()
        logical, target :: mag_ext
        logical, pointer :: ptr_mag_ext => null()
        logical, target :: friedel
        logical, pointer :: ptr_friedel => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_hkl

        ! Unwrapped variables
        character(len=4), dimension(:), allocatable :: sfacsymb
        type(reflist_type) :: hkl

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_experiment => null()
            ptr_sintlmin => null()
            ptr_sintlmax => null()
            ptr_unique => null()
            ptr_mag_only => null()
            ptr_mag_ext => null()
            ptr_friedel => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','mcif_file',item,mcif_file,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','sfacsymb',item,li_sfacsymb,ierror)
        if (ierror == 0) call List_to_Alloc_Array_Primitive(li_sfacsymb,sfacsymb,ierror)
        if (ierror == 0 .and. nargs > 2) then
            ! Optional arguments
            ierror2 = args%getitem(item,2)
            if (ierror2 == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'experiment')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','experiment',item,experiment,ierror)
                    if (ierror == 0) ptr_experiment => experiment
                else
                    ptr_experiment => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sintlmin')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','sintlmin',item,sintlmin,ierror)
                    if (ierror == 0) ptr_sintlmin => sintlmin
                else
                    ptr_sintlmin => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sintlmax')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','sintlmax',item,sintlmax,ierror)
                    if (ierror == 0) ptr_sintlmax => sintlmax
                else
                    ptr_sintlmax => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'unique')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','unique',item,unique,ierror)
                    if (ierror == 0) ptr_unique => unique
                else
                    ptr_unique => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mag_only')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','mag_only',item,mag_only,ierror)
                    if (ierror == 0) ptr_mag_only => mag_only
                else
                    ptr_mag_only => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mag_ext')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','mag_ext',item,mag_ext,ierror)
                    if (ierror == 0) ptr_mag_ext => mag_ext
                else
                    ptr_mag_ext => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'friedel')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_magnetic_structure_factors_from_mcif','friedel',item,friedel,ierror)
                    if (ierror == 0) ptr_friedel => friedel
                else
                    ptr_friedel => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_magnetic_structure_factors_from_mcif: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_magnetic_structure_factors_from_mcif: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            hkl = magnetic_structure_factors_from_mcif(mcif_file,sfacsymb,ptr_experiment,ptr_sintlmin,ptr_sintlmax,ptr_unique,ptr_mag_only,ptr_mag_ext,ptr_friedel)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_magnetic_structure_factors_from_mcif: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_hkl)
            if (ierror == 0) call wrap_type(hkl,di_hkl,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_magnetic_structure_factors_from_mcif: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_hkl)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_magnetic_structure_factors_from_mcif

    function f_set_boundary(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_boundary of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_crystal
        type(dict) :: di_coor
        type(ndarray) :: nd_limits
        logical :: boundary_coordination

        ! Variables in returned tuple
        type(dict) :: di_c

        ! Unwrapped variables
        type(crystal_type) :: crystal
        type(coordination_crystal_type) :: coor
        real, dimension(2,3) :: limits
        real, dimension(:,:), pointer :: p_limits
        type(graphical_crystal_type) :: c

        ! Local parameters
        integer, parameter :: NMANDATORY = 4

        ! Local variables
        integer :: nargs
        integer :: ierror
        character(len=1) :: array_order
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_boundary','crystal',item,di_crystal,ierror)
        if (ierror == 0) call unwrap_type(di_crystal,crystal,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_set_boundary','coor',item,di_coor,ierror)
        if (ierror == 0) call unwrap_type(di_coor,coor,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('f_set_boundary','limits',item,nd_limits,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_boundary','limits',nd_limits,p_limits,ierror,array_order)
        if (ierror == 0) call pointer_to_array('set_boundary','limits',p_limits,limits,ierror,array_order)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('f_set_boundary','boundary_coordination',item,boundary_coordination,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_boundary: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_boundary: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            c = set_boundary(crystal,coor,limits,boundary_coordination)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_boundary: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_c)
            if (ierror == 0) call wrap_type(c,di_c,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_boundary: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            if (ierror == 0) call wrap_type(crystal,di_crystal,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_boundary: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_c)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_boundary

    function f_set_crystal_coordination(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_crystal_coordination of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_crystal
        type(ndarray) :: nd_dmin
        real(kind=cp), dimension(:,:), pointer :: ptr_dmin => null()
        type(ndarray) :: nd_dmax
        real(kind=cp), dimension(:,:), pointer :: ptr_dmax => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_coor

        ! Unwrapped variables
        type(crystal_type) :: crystal
        real(kind=cp), dimension(:,:), allocatable, target :: dmin
        real(kind=cp), dimension(:,:), pointer :: p_dmin
        real(kind=cp), dimension(:,:), allocatable, target :: dmax
        real(kind=cp), dimension(:,:), pointer :: p_dmax
        type(coordination_crystal_type) :: coor

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        character(len=1) :: array_order
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_dmin => null()
            ptr_dmax => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_crystal_coordination','crystal',item,di_crystal,ierror)
        if (ierror == 0) call unwrap_type(di_crystal,crystal,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_set_crystal_coordination','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'dmin')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_crystal_coordination','dmin',item,nd_dmin,ierror)
                    if (ierror == 0) call ndarray_to_pointer('set_crystal_coordination','dmin',nd_dmin,p_dmin,ierror,array_order)
                    if (ierror == 0) call pointer_to_alloc_array('set_crystal_coordination','dmin',p_dmin,dmin,ierror,array_order)
                    if (ierror == 0) ptr_dmin => dmin
                else
                    ptr_dmin => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'dmax')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_crystal_coordination','dmax',item,nd_dmax,ierror)
                    if (ierror == 0) call ndarray_to_pointer('set_crystal_coordination','dmax',nd_dmax,p_dmax,ierror,array_order)
                    if (ierror == 0) call pointer_to_alloc_array('set_crystal_coordination','dmax',p_dmax,dmax,ierror,array_order)
                    if (ierror == 0) ptr_dmax => dmax
                else
                    ptr_dmax => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_crystal_coordination: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_crystal_coordination: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            coor = set_crystal_coordination(crystal,ptr_dmin,ptr_dmax)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_crystal_coordination: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_coor)
            if (ierror == 0) call wrap_type(coor,di_coor,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_crystal_coordination: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            if (ierror == 0) call wrap_type(crystal,di_crystal,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_crystal_coordination: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_coor)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_crystal_coordination

    function f_set_mask_atoms(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_mask_atoms of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_id_atom
        type(ndarray) :: nd_id_to_hide

        ! Variables in returned tuple
        type(ndarray) :: nd_mask_array

        ! Unwrapped variables
        integer, dimension(:), allocatable :: id_atom
        integer, dimension(:), pointer :: p_id_atom
        integer, dimension(:), allocatable :: id_to_hide
        integer, dimension(:), pointer :: p_id_to_hide
        integer, dimension(:), allocatable :: mask_array
        integer, dimension(:), pointer :: p_mask_array

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_mask_atoms','id_atom',item,nd_id_atom,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_mask_atoms','id_atom',nd_id_atom,p_id_atom,ierror)
        if (ierror == 0) call pointer_to_alloc_array('set_mask_atoms','id_atom',p_id_atom,id_atom,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_set_mask_atoms','id_to_hide',item,nd_id_to_hide,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_mask_atoms','id_to_hide',nd_id_to_hide,p_id_to_hide,ierror)
        if (ierror == 0) call pointer_to_alloc_array('set_mask_atoms','id_to_hide',p_id_to_hide,id_to_hide,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_mask_atoms: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_mask_atoms: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            mask_array = set_mask_atoms(id_atom,id_to_hide)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_mask_atoms: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = ndarray_create(nd_mask_array,mask_array)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_mask_atoms: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,nd_mask_array)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_mask_atoms

    function f_structure_factors_from_cif(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure structure_factors_from_cif of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: cif_file
        character(len=:), allocatable, target :: radiation
        character(len=:), pointer :: ptr_radiation => null()
        real(kind=cp), target :: lambda
        real(kind=cp), pointer :: ptr_lambda => null()
        real(kind=cp), target :: sintlmin
        real(kind=cp), pointer :: ptr_sintlmin => null()
        real(kind=cp), target :: sintlmax
        real(kind=cp), pointer :: ptr_sintlmax => null()
        logical, target :: unique
        logical, pointer :: ptr_unique => null()
        logical, target :: friedel
        logical, pointer :: ptr_friedel => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_hkl

        ! Unwrapped variables
        type(reflist_type) :: hkl

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_radiation => null()
            ptr_lambda => null()
            ptr_sintlmin => null()
            ptr_sintlmax => null()
            ptr_unique => null()
            ptr_friedel => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','cif_file',item,cif_file,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_structure_factors_from_cif','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'radiation')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','radiation',item,radiation,ierror)
                    if (ierror == 0) ptr_radiation => radiation
                else
                    ptr_radiation => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'lambda')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','lambda',item,lambda,ierror)
                    if (ierror == 0) ptr_lambda => lambda
                else
                    ptr_lambda => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sintlmin')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','sintlmin',item,sintlmin,ierror)
                    if (ierror == 0) ptr_sintlmin => sintlmin
                else
                    ptr_sintlmin => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sintlmax')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','sintlmax',item,sintlmax,ierror)
                    if (ierror == 0) ptr_sintlmax => sintlmax
                else
                    ptr_sintlmax => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'unique')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','unique',item,unique,ierror)
                    if (ierror == 0) ptr_unique => unique
                else
                    ptr_unique => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'friedel')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_structure_factors_from_cif','friedel',item,friedel,ierror)
                    if (ierror == 0) ptr_friedel => friedel
                else
                    ptr_friedel => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_structure_factors_from_cif: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_structure_factors_from_cif: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            hkl = structure_factors_from_cif(cif_file,ptr_radiation,ptr_lambda,ptr_sintlmin,ptr_sintlmax,ptr_unique,ptr_friedel)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_structure_factors_from_cif: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_hkl)
            if (ierror == 0) call wrap_type(hkl,di_hkl,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_structure_factors_from_cif: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_hkl)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_structure_factors_from_cif

    function f_tof_powder_pattern_from_dict(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure tof_powder_pattern_from_dict of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: json

        ! Variables in returned tuple
        type(ndarray) :: nd_xc
        type(ndarray) :: nd_yc

        ! Unwrapped variables
        real(kind=cp), dimension(:), allocatable :: xc
        real(kind=cp), dimension(:), allocatable :: yc

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_tof_powder_pattern_from_dict','json',item,json,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_tof_powder_pattern_from_dict: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_tof_powder_pattern_from_dict: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call tof_powder_pattern_from_dict(json,xc,yc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_tof_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = ndarray_create(nd_xc,xc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_tof_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            ierror = ndarray_create(nd_yc,yc)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_tof_powder_pattern_from_dict: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,nd_xc)
            ierror = ret%setitem(1,nd_yc)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_tof_powder_pattern_from_dict

    function f_update_global_phase(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure update_global_phase of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: global_phase
        type(dict) :: di_crystal
        type(dict) :: di_c

        ! Variables in returned tuple

        ! Unwrapped variables
        type(crystal_type) :: crystal
        type(graphical_crystal_type) :: c

        ! Local parameters
        integer, parameter :: NMANDATORY = 3

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_update_global_phase','global_phase',item,global_phase,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_update_global_phase','crystal',item,di_crystal,ierror)
        if (ierror == 0) call unwrap_type(di_crystal,crystal,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('f_update_global_phase','c',item,di_c,ierror)
        if (ierror == 0) call unwrap_type(di_c,c,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_update_global_phase: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_update_global_phase: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call update_global_phase(global_phase,crystal,c)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_update_global_phase: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            if (ierror == 0) call wrap_type(c,di_c,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_update_global_phase: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        ierror = nonetype_create(nret)
        resul = nret%get_c_ptr()

    end function f_update_global_phase

    function f_calculate_laue_image(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure calculate_laue_image of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr

        ! Variables in returned tuple
        type(dict) :: di_laue_img

        ! Unwrapped variables
        type(esmeralda_laue_image_type) :: laue_img

        ! Local parameters
        integer, parameter :: NMANDATORY = 0

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_calculate_laue_image: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_calculate_laue_image: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            laue_img = calculate_laue_image()
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_calculate_laue_image: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_laue_img)
            if (ierror == 0) call wrap_type(laue_img,di_laue_img,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_calculate_laue_image: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_laue_img)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_calculate_laue_image

    function f_set_instrument_esmeralda(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_instrument_esmeralda of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: name
        character(len=:), allocatable :: dtype
        real(kind=4) :: d
        real(kind=4) :: h
        real(kind=4) :: v
        integer, target :: np_h
        integer, target :: np_v
        real(kind=4) :: xo
        real(kind=4) :: zo
        real(kind=4) :: ga_d
        real(kind=4) :: nu_d
        real(kind=4) :: l_min
        real(kind=4) :: l_max
        real(kind=4) :: x_min
        real(kind=4) :: x_max
        real(kind=4) :: z_min
        real(kind=4) :: z_max
        real(kind=4) :: gan_min
        real(kind=4) :: gan_max
        real(kind=4) :: gap_min
        real(kind=4) :: gap_max
        real(kind=4) :: nu_min
        real(kind=4) :: nu_max

        ! Variables in returned tuple

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 23

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','name',item,name,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','dtype',item,dtype,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','d',item,d,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','h',item,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','v',item,v,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','np_h',item,np_h,ierror)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','np_v',item,np_v,ierror)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','xo',item,xo,ierror)
        if (ierror == 0) ierror = args%getitem(item,8)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','zo',item,zo,ierror)
        if (ierror == 0) ierror = args%getitem(item,9)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','ga_d',item,ga_d,ierror)
        if (ierror == 0) ierror = args%getitem(item,10)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','nu_d',item,nu_d,ierror)
        if (ierror == 0) ierror = args%getitem(item,11)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','l_min',item,l_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,12)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','l_max',item,l_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,13)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','x_min',item,x_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,14)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','x_max',item,x_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,15)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','z_min',item,z_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,16)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','z_max',item,z_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,17)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','gan_min',item,gan_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,18)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','gan_max',item,gan_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,19)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','gap_min',item,gap_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,20)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','gap_max',item,gap_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,21)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','nu_min',item,nu_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,22)
        if (ierror == 0) call get_var_from_item('f_set_instrument_esmeralda','nu_max',item,nu_max,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_instrument_esmeralda: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_instrument_esmeralda: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call set_instrument_esmeralda(name,dtype,d,h,v,np_h,np_v,xo,zo,ga_d,nu_d,l_min,l_max,x_min,x_max,z_min,z_max,gan_min,gan_max,gap_min,gap_max,nu_min,nu_max)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_instrument_esmeralda: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        ierror = nonetype_create(nret)
        resul = nret%get_c_ptr()

    end function f_set_instrument_esmeralda

    function f_set_spg_esmeralda(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_spg_esmeralda of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: spg_id

        ! Variables in returned tuple

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_spg_esmeralda','spg_id',item,spg_id,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_spg_esmeralda: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_spg_esmeralda: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call set_spg_esmeralda(spg_id)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_spg_esmeralda: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        ierror = nonetype_create(nret)
        resul = nret%get_c_ptr()

    end function f_set_spg_esmeralda

    function f_set_ub_esmeralda(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_ub_esmeralda of module CFML_Py_Utilities

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_ub

        ! Variables in returned tuple

        ! Unwrapped variables
        real(kind=4), dimension(3,3) :: ub
        real(kind=4), dimension(:,:), pointer :: p_ub

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        character(len=1) :: array_order
        type(object) :: item
        type(tuple) :: args
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_ub_esmeralda','ub',item,nd_ub,ierror)
        if (ierror == 0) call ndarray_to_pointer('set_ub_esmeralda','ub',nd_ub,p_ub,ierror,array_order)
        if (ierror == 0) call pointer_to_array('set_ub_esmeralda','ub',p_ub,ub,ierror,array_order)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_ub_esmeralda: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_ub_esmeralda: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call set_ub_esmeralda(ub)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_ub_esmeralda: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        ierror = nonetype_create(nret)
        resul = nret%get_c_ptr()

    end function f_set_ub_esmeralda

    function f_get_shubnikov_info(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_shubnikov_info of module CFML_Symmetry_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(shub_spgr_info_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_shubnikov_info','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_shubnikov_info: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_shubnikov_info: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_shubnikov_info(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_shubnikov_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_shubnikov_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_shubnikov_info

    function f_get_spgr_info(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_spgr_info of module CFML_Symmetry_Tables

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: r

        ! Variables in returned tuple
        type(dict) :: di_info

        ! Unwrapped variables
        type(spgr_info_type) :: info

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_spgr_info','r',item,r,ierror)

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_spgr_info: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_spgr_info: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            info = get_spgr_info(r)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_spgr_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_info)
            if (ierror == 0) call wrap_type(info,di_info,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_spgr_info: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_info)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_spgr_info

    function f_load_pattern(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure load_pattern of module CFML_DiffPatt

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: filename
        character(len=:), allocatable, target :: mode
        character(len=:), pointer :: ptr_mode => null()
        logical, target :: sig
        logical, pointer :: ptr_sig => null()
        character(len=:), allocatable, target :: header
        character(len=:), pointer :: ptr_header => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_pat

        ! Unwrapped variables
        type(diffpat_e_type) :: pat

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_mode => null()
            ptr_sig => null()
            ptr_header => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_load_pattern','filename',item,filename,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_load_pattern','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'mode')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_load_pattern','mode',item,mode,ierror)
                    if (ierror == 0) ptr_mode => mode
                else
                    ptr_mode => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'sig')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_load_pattern','sig',item,sig,ierror)
                    if (ierror == 0) ptr_sig => sig
                else
                    ptr_sig => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'header')
                if (ierror2 == 0) then
                    ptr_header => header
                else
                    ptr_header => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_load_pattern: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_load_pattern: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            pat = load_pattern(filename,ptr_mode,ptr_sig,ptr_header)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_load_pattern: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_pat)
            if (ierror == 0) call wrap_type(pat,di_pat,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_load_pattern: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_pat)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_load_pattern

    function f_read_xtal_structure(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure read_xtal_structure of module CFML_IOForm

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: filenam
        character(len=:), allocatable, target :: atm_typ
        character(len=:), pointer :: ptr_atm_typ => null()
        type(dict) :: di_mgp
        type(magsymm_k_type), pointer :: ptr_mgp => null()
        type(dict) :: di_matm
        type(matom_list_type), pointer :: ptr_matm => null()
        type(dict) :: di_mag_dom
        type(magnetic_domain_type), pointer :: ptr_mag_dom => null()
        integer, target :: iphase
        integer, pointer :: ptr_iphase => null()
        type(dict) :: di_ftype
        type(file_type), pointer :: ptr_ftype => null()
        type(dict) :: di_filelist
        type(file_list_type), pointer :: ptr_filelist => null()
        character(len=:), allocatable, target :: database_path
        character(len=:), pointer :: ptr_database_path => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_cell
        type(dict) :: di_spg
        type(dict) :: di_atm

        ! Unwrapped variables
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atm
        type(magsymm_k_type), target :: mgp
        type(matom_list_type), target :: matm
        type(magnetic_domain_type), target :: mag_dom
        type(file_type), target :: ftype
        type(file_list_type), target :: filelist

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_atm_typ => null()
            ptr_mgp => null()
            ptr_matm => null()
            ptr_mag_dom => null()
            ptr_iphase => null()
            ptr_ftype => null()
            ptr_filelist => null()
            ptr_database_path => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_read_xtal_structure','filenam',item,filenam,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_read_xtal_structure','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'atm_typ')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_read_xtal_structure','atm_typ',item,atm_typ,ierror)
                    if (ierror == 0) ptr_atm_typ => atm_typ
                else
                    ptr_atm_typ => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mgp')
                if (ierror2 == 0) then
                    ptr_mgp => mgp
                else
                    ptr_mgp => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'matm')
                if (ierror2 == 0) then
                    ptr_matm => matm
                else
                    ptr_matm => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mag_dom')
                if (ierror2 == 0) then
                    ptr_mag_dom => mag_dom
                else
                    ptr_mag_dom => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'iphase')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_read_xtal_structure','iphase',item,iphase,ierror)
                    if (ierror == 0) ptr_iphase => iphase
                else
                    ptr_iphase => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'ftype')
                if (ierror2 == 0) then
                    ptr_ftype => ftype
                else
                    ptr_ftype => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'filelist')
                if (ierror2 == 0) then
                    ptr_filelist => filelist
                else
                    ptr_filelist => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'database_path')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_read_xtal_structure','database_path',item,database_path,ierror)
                    if (ierror == 0) ptr_database_path => database_path
                else
                    ptr_database_path => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            call read_xtal_structure(filenam,cell,spg,atm,ptr_atm_typ,ptr_mgp,ptr_matm,ptr_mag_dom,ptr_iphase,ptr_ftype,ptr_filelist,ptr_database_path)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_cell)
            if (ierror == 0) call wrap_type(cell,di_cell,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            ierror = dict_create(di_spg)
            if (ierror == 0) call wrap_type(spg,di_spg,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (ierror == 0) then
            ierror = dict_create(di_atm)
            if (ierror == 0) call wrap_type(atm,di_atm,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (associated(ptr_mgp)) then
            if (ierror == 0) call wrap_type(mgp,di_mgp,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (associated(ptr_matm)) then
            if (ierror == 0) call wrap_type(matm,di_matm,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (associated(ptr_mag_dom)) then
            if (ierror == 0) call wrap_type(mag_dom,di_mag_dom,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (associated(ptr_ftype)) then
            if (ierror == 0) call wrap_type(ftype,di_ftype,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if
        if (associated(ptr_filelist)) then
            if (ierror == 0) call wrap_type(filelist,di_filelist,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_read_xtal_structure: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,di_cell)
            ierror = ret%setitem(1,di_spg)
            ierror = ret%setitem(2,di_atm)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_read_xtal_structure

    function f_get_maxnumref(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure get_maxnumref of module CFML_Reflections

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real(kind=cp) :: sintlmax
        real(kind=cp) :: volcell
        real(kind=cp), target :: sintlmin
        real(kind=cp), pointer :: ptr_sintlmin => null()
        integer, target :: mult
        integer, pointer :: ptr_mult => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        integer :: numref

        ! Unwrapped variables

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_sintlmin => null()
            ptr_mult => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_get_maxnumref','sintlmax',item,sintlmax,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_get_maxnumref','volcell',item,volcell,ierror)
        if (ierror == 0 .and. nargs > 2) then
            ! Optional arguments
            ierror2 = args%getitem(item,2)
            if (ierror2 == 0) call get_var_from_item('f_get_maxnumref','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'sintlmin')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_get_maxnumref','sintlmin',item,sintlmin,ierror)
                    if (ierror == 0) ptr_sintlmin => sintlmin
                else
                    ptr_sintlmin => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mult')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_get_maxnumref','mult',item,mult,ierror)
                    if (ierror == 0) ptr_mult => mult
                else
                    ptr_mult => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_get_maxnumref: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_get_maxnumref: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            numref = get_maxnumref(sintlmax,volcell,ptr_sintlmin,multip=ptr_mult)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_get_maxnumref: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,numref)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_get_maxnumref

    function f_generate_reflections(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure generate_reflections of module CFML_Reflections

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_cell
        real(kind=cp) :: slmin
        real(kind=cp) :: slmax
        type(dict) :: di_spg
        logical, target :: magext
        logical, pointer :: ptr_magext => null()
        type(dict) :: di_kinfo
        type(kvect_info_type), pointer :: ptr_kinfo => null()
        logical, target :: order
        logical, pointer :: ptr_order => null()
        logical, target :: unique
        logical, pointer :: ptr_unique => null()
        type(ndarray) :: nd_seqindx
        integer, dimension(:), pointer :: ptr_seqindx => null()
        type(ndarray) :: nd_hlim
        integer, dimension(:,:), pointer :: ptr_hlim => null()
        logical, target :: mag_only
        logical, pointer :: ptr_mag_only => null()
        logical, target :: friedel
        logical, pointer :: ptr_friedel => null()
        character(len=:), allocatable, target :: ref_typ
        character(len=:), pointer :: ptr_ref_typ => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_reflex

        ! Unwrapped variables
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(kvect_info_type), target :: kinfo
        integer, dimension(3), target :: seqindx
        integer, dimension(:), pointer :: p_seqindx
        integer, dimension(3,2), target :: hlim
        integer, dimension(:,:), pointer :: p_hlim
        type(reflist_type) :: reflex

        ! Local parameters
        integer, parameter :: NMANDATORY = 4

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        character(len=1) :: array_order
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_magext => null()
            ptr_kinfo => null()
            ptr_order => null()
            ptr_unique => null()
            ptr_seqindx => null()
            ptr_hlim => null()
            ptr_mag_only => null()
            ptr_friedel => null()
            ptr_ref_typ => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_generate_reflections','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_class_cell_g_type(di_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_generate_reflections','slmin',item,slmin,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('f_generate_reflections','slmax',item,slmax,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('f_generate_reflections','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_class_spg_type(di_spg,spg,ierror)
        if (ierror == 0 .and. nargs > 4) then
            ! Optional arguments
            ierror2 = args%getitem(item,4)
            if (ierror2 == 0) call get_var_from_item('f_generate_reflections','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'magext')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','magext',item,magext,ierror)
                    if (ierror == 0) ptr_magext => magext
                else
                    ptr_magext => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'kinfo')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','kinfo',item,di_kinfo,ierror)
                    if (ierror == 0) call unwrap_type(di_kinfo,kinfo,ierror)
                    if (ierror == 0) ptr_kinfo => kinfo
                else
                    ptr_kinfo => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'order')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','order',item,order,ierror)
                    if (ierror == 0) ptr_order => order
                else
                    ptr_order => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'unique')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','unique',item,unique,ierror)
                    if (ierror == 0) ptr_unique => unique
                else
                    ptr_unique => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'seqindx')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','seqindx',item,nd_seqindx,ierror)
                    if (ierror == 0) call ndarray_to_pointer('generate_reflections','seqindx',nd_seqindx,p_seqindx,ierror)
                    if (ierror == 0) call pointer_to_array('generate_reflections','seqindx',p_seqindx,seqindx,ierror)
                    if (ierror == 0) ptr_seqindx => seqindx
                else
                    ptr_seqindx => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'hlim')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','hlim',item,nd_hlim,ierror)
                    if (ierror == 0) call ndarray_to_pointer('generate_reflections','hlim',nd_hlim,p_hlim,ierror,array_order)
                    if (ierror == 0) call pointer_to_array('generate_reflections','hlim',p_hlim,hlim,ierror,array_order)
                    if (ierror == 0) ptr_hlim => hlim
                else
                    ptr_hlim => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'mag_only')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','mag_only',item,mag_only,ierror)
                    if (ierror == 0) ptr_mag_only => mag_only
                else
                    ptr_mag_only => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'friedel')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','friedel',item,friedel,ierror)
                    if (ierror == 0) ptr_friedel => friedel
                else
                    ptr_friedel => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'ref_typ')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_generate_reflections','ref_typ',item,ref_typ,ierror)
                    if (ierror == 0) ptr_ref_typ => ref_typ
                else
                    ptr_ref_typ => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_generate_reflections: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_generate_reflections: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            reflex = generate_reflections(cell,slmin,slmax,spg,ptr_magext,ptr_kinfo,ptr_order,ptr_unique,ptr_seqindx,ptr_hlim,ptr_mag_only,ptr_friedel,ptr_ref_typ)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_generate_reflections: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_reflex)
            if (ierror == 0) call wrap_type(reflex,di_reflex,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_generate_reflections: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_reflex)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_generate_reflections

    function f_init_reflist(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure init_reflist of module CFML_Reflections

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer, target :: n
        character(len=:), allocatable :: ctype
        integer, target :: d
        integer, pointer :: ptr_d => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_reflex

        ! Unwrapped variables
        class(reflist_type), allocatable :: reflex

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_d => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_init_reflist','n',item,n,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_init_reflist','ctype',item,ctype,ierror)
        if (ierror == 0 .and. nargs > 2) then
            ! Optional arguments
            ierror2 = args%getitem(item,2)
            if (ierror2 == 0) call get_var_from_item('f_init_reflist','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'d')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_init_reflist','d',item,d,ierror)
                    if (ierror == 0) ptr_d => d
                else
                    ptr_d => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_init_reflist: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_init_reflist: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            reflex = init_reflist(n,ctype,ptr_d)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_init_reflist: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_reflex)
            if (ierror == 0) call wrap_type(reflex,di_reflex,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_init_reflist: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_reflex)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_init_reflist

    function f_set_spacegroup_from_dbase(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_spacegroup_from_dbase of module CFML_gSpaceGroups

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: str
        character(len=:), allocatable :: mode
        character(len=:), allocatable, target :: xyz_type
        character(len=:), pointer :: ptr_xyz_type => null()
        character(len=:), allocatable, target :: setting
        character(len=:), pointer :: ptr_setting => null()
        logical, target :: keepdb
        logical, pointer :: ptr_keepdb => null()
        character(len=:), allocatable, target :: parent
        character(len=:), pointer :: ptr_parent => null()
        character(len=:), allocatable, target :: database_path
        character(len=:), pointer :: ptr_database_path => null()
        logical, target :: trn_to
        logical, pointer :: ptr_trn_to => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_spaceg

        ! Unwrapped variables
        class(spg_type), allocatable :: spaceg

        ! Local parameters
        integer, parameter :: NMANDATORY = 2

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_xyz_type => null()
            ptr_setting => null()
            ptr_keepdb => null()
            ptr_parent => null()
            ptr_database_path => null()
            ptr_trn_to => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','str',item,str,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','mode',item,mode,ierror)
        if (ierror == 0 .and. nargs > 2) then
            ! Optional arguments
            ierror2 = args%getitem(item,2)
            if (ierror2 == 0) call get_var_from_item('f_set_spacegroup_from_dbase','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'xyz_type')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','xyz_type',item,xyz_type,ierror)
                    if (ierror == 0) ptr_xyz_type => xyz_type
                else
                    ptr_xyz_type => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'setting')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','setting',item,setting,ierror)
                    if (ierror == 0) ptr_setting => setting
                else
                    ptr_setting => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'keepdb')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','keepdb',item,keepdb,ierror)
                    if (ierror == 0) ptr_keepdb => keepdb
                else
                    ptr_keepdb => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'parent')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','parent',item,parent,ierror)
                    if (ierror == 0) ptr_parent => parent
                else
                    ptr_parent => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'database_path')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','database_path',item,database_path,ierror)
                    if (ierror == 0) ptr_database_path => database_path
                else
                    ptr_database_path => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'trn_to')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_dbase','trn_to',item,trn_to,ierror)
                    if (ierror == 0) ptr_trn_to => trn_to
                else
                    ptr_trn_to => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_spacegroup_from_dbase: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_spacegroup_from_dbase: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            spaceg = set_spacegroup_from_dbase(str,mode,ptr_xyz_type,ptr_setting,ptr_keepdb,ptr_parent,ptr_database_path,ptr_trn_to)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_spacegroup_from_dbase: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_spaceg)
            if (ierror == 0) call wrap_type(spaceg,di_spaceg,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_spacegroup_from_dbase: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_spaceg)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_spacegroup_from_dbase

    function f_set_spacegroup_from_generators(self_ptr,args_ptr) result(resul) bind(c)
        ! Wrapper for procedure set_spacegroup_from_generators of module CFML_gSpaceGroups

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: str
        logical, target :: set_inv
        logical, pointer :: ptr_set_inv => null()
        character(len=:), allocatable, target :: database_path
        character(len=:), pointer :: ptr_database_path => null()
        type(dict) :: di_kwargs

        ! Variables in returned tuple
        type(dict) :: di_spaceg

        ! Unwrapped variables
        class(spg_type), allocatable :: spaceg

        ! Local parameters
        integer, parameter :: NMANDATORY = 1

        ! Local variables
        integer :: nargs
        integer :: ierror,ierror2
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ierror = 0
        call err_clear ! Reset Python error
        call clear_error() ! Reset CrysFML08 error

        ! Use unsafe_cast_from_c_ptr to castfrom c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%len(nargs)
        if (ierror == 0) then
            if (nargs < NMANDATORY) then
                ierror = 1
                err_cfml%ierr = 1
                err_cfml%msg = 'Wrong number of arguments'
            end if
        end if

        ! Unwrapping
        if (ierror == 0 .and. nargs == NMANDATORY) then
            ptr_set_inv => null()
            ptr_database_path => null()
        end if
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_generators','str',item,str,ierror)
        if (ierror == 0 .and. nargs > 1) then
            ! Optional arguments
            ierror2 = args%getitem(item,1)
            if (ierror2 == 0) call get_var_from_item('f_set_spacegroup_from_generators','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                ierror2 = di_kwargs%getitem(item,'set_inv')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_generators','set_inv',item,set_inv,ierror)
                    if (ierror == 0) ptr_set_inv => set_inv
                else
                    ptr_set_inv => null()
                    call err_clear
                end if
                ierror2 = di_kwargs%getitem(item,'database_path')
                if (ierror2 == 0) then
                    if (ierror == 0) call get_var_from_item('f_set_spacegroup_from_generators','database_path',item,database_path,ierror)
                    if (ierror == 0) ptr_database_path => database_path
                else
                    ptr_database_path => null()
                    call err_clear
                end if
            end if
            if (ierror2 == 0) call err_clear
        end if

        ! Check errors
        if (ierror /= 0 .or. err_cfml%ierr /= 0) then
            ierror = EXCEPTION_ERROR
            if (err_cfml%ierr /= 0) then
                call raise_exception(RuntimeError,'f_set_spacegroup_from_generators: '//trim(err_cfml%msg))
            else
                call raise_exception(RuntimeError,'f_set_spacegroup_from_generators: '//'error parsing arguments')
            end if
        end if

        ! Call to CrysFML08 procedure
        if (ierror == 0) then
            spaceg = set_spacegroup_from_generators(str,ptr_set_inv,ptr_database_path)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_spacegroup_from_generators: '//trim(err_cfml%msg))
            end if
        end if

        ! Wrapping
        if (ierror == 0) then
            ierror = dict_create(di_spaceg)
            if (ierror == 0) call wrap_type(spaceg,di_spaceg,ierror)
            if (err_cfml%ierr /= 0) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'f_set_spacegroup_from_generators: '//trim(err_cfml%msg))
            end if
        end if

        ! Return
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,di_spaceg)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function f_set_spacegroup_from_generators

end module crysfml08lib