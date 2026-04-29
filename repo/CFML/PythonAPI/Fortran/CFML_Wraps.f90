
Module CFML_Wraps

    use forpy_mod
    use CFML_GlobalDeps
    use CFML_Atoms, only: atm_type,atm_std_type,modatm_std_type,atm_ref_type,modatm_ref_type,atm_cell_type,atom_equiv_type,atom_equiv_list_type,atlist_type,matom_type,matom_list_type
    use CFML_Strings, only: file_list_type,string_array_type,file_type
    use CFML_BckPeaks, only: pkb_type,peak_search_cond_type
    use CFML_Scattering_Tables, only: anomalous_sc_type,chem_info_type,magnetic_form_type,xray_form_type,xray_wavelength_type
    use CFML_Structure_Factors, only: scattering_species_type,strf_type,strflist_type
    use CFML_Geom, only: coordination_type,point_list_type
    use CFML_Metrics, only: cell_type,cell_g_type,cell_ls_type,cell_gls_type,twofold_axes_type,zone_axis_type,strain_tensor_type
    use CFML_Py_Utilities, only: esmeralda_laue_image_type,coordination_asu_type,coordination_crystal_type,coordination_orbit_type,crystal_bond_type,crystal_type,envelope_type,envelope_list_type,graphical_crystal_type
    use CFML_Symmetry_Tables, only: shub_spgr_info_type,spgr_info_type,table_equiv_type,wyck_info_type
    use CFML_DiffPatt, only: diffpat_type,diffpat_e_type,diffpat_g_type,diffpatt_conditions_type,powpatt_cw_conditions_type,powpatt_tof_conditions_type,bck_type,interval_type,excl_reg_type,pattern_type
    use CFML_IOForm, only: job_info_type,blockinfo_type,genvec_type,sxtal_attributes_type,powder_attributes_type,phase_type
    use CFML_Molecules, only: molecule_type,molcrystal_type
    use CFML_Propagation_Vectors, only: group_k_type
    use CFML_ILL_Instrm_Data, only: basic_numc_type,basic_numi_type,basic_numr_type,calibration_detector_type,diffractometer_type,generic_numor_type,ill_data_record_type,powder_numor_type,sxtal_numor_type,sxtal_orient_type
    use CFML_Reflections, only: refl_type,srefl_type,mrefl_type,reflist_type,refp_type
    use CFML_BVS_Tables, only: atomic_properties_type,bvel_par_type,bvs_par_type,sbvs_par_type
    use CFML_Powder, only: irf_type,irf_cw_type,irf_tof_type
    use CFML_EnBVS, only: atoms_conf_list_type
    use CFML_SXTAL_Geom, only: psd_val_type,sxd_val_type,twin_type
    use CFML_Simulated_Annealing, only: multistate_vector_type,simann_conditions_type,state_vector_type
    use CFML_Profiles, only: deriv_tof_type
    use CFML_Laue, only: excluded_regions_type,header_type,image_conditions,laue_instrument_type,laue_ref_type,laue_ref_list_type,peakfind_parameters_type,peak_type,peak_list_type
    use CFML_Rational
    use CFML_gSpaceGroups, only: symm_oper_type,group_type,rot_mat_type,spg_type,superspacegroup_type,spin_operator_type,spin_group_type,kvect_info_type,point_orbit,orbit_type,orbit_list
    use CFML_kvec_Symmetry, only: sym_oper_type,msym_oper_type,magnetic_domain_type,magsymm_k_type,point_orbit_kv,mod_orbit
    use CFML_Wraps_Utils

    implicit none

    private
    public :: list_to_class_array_atm_type
    public :: list_to_class_array_atm_std_type
    public :: list_to_class_array_modatm_std_type
    public :: list_to_class_array_atm_ref_type
    public :: list_to_class_array_modatm_ref_type
    public :: list_to_class_array_cell_type
    public :: list_to_class_array_cell_g_type
    public :: list_to_class_array_cell_ls_type
    public :: list_to_class_array_cell_gls_type
    public :: list_to_class_array_diffpat_type
    public :: list_to_class_array_diffpat_e_type
    public :: list_to_class_array_diffpat_g_type
    public :: list_to_class_array_diffpatt_conditions_type
    public :: list_to_class_array_powpatt_cw_conditions_type
    public :: list_to_class_array_powpatt_tof_conditions_type
    public :: list_to_class_array_refl_type
    public :: list_to_class_array_srefl_type
    public :: list_to_class_array_mrefl_type
    public :: list_to_class_array_reflist_type
    public :: list_to_class_array_refp_type
    public :: list_to_class_array_irf_type
    public :: list_to_class_array_irf_cw_type
    public :: list_to_class_array_irf_tof_type
    public :: list_to_class_array_group_type
    public :: list_to_class_array_spg_type
    public :: list_to_class_array_superspacegroup_type
    public :: list_to_class_array_point_orbit
    public :: list_to_class_array_orbit_type
    public :: list_to_class_array_point_orbit_kv
    public :: list_to_class_array_mod_orbit
    public :: list_to_type_array
    public :: list_to_type_array_no_alloc
    public :: unwrap_class_atm_type
    public :: unwrap_class_atm_std_type
    public :: unwrap_class_modatm_std_type
    public :: unwrap_class_atm_ref_type
    public :: unwrap_class_modatm_ref_type
    public :: unwrap_class_cell_type
    public :: unwrap_class_cell_g_type
    public :: unwrap_class_cell_ls_type
    public :: unwrap_class_cell_gls_type
    public :: unwrap_class_diffpat_type
    public :: unwrap_class_diffpat_e_type
    public :: unwrap_class_diffpat_g_type
    public :: unwrap_class_diffpatt_conditions_type
    public :: unwrap_class_powpatt_cw_conditions_type
    public :: unwrap_class_powpatt_tof_conditions_type
    public :: unwrap_class_refl_type
    public :: unwrap_class_srefl_type
    public :: unwrap_class_mrefl_type
    public :: unwrap_class_reflist_type
    public :: unwrap_class_refp_type
    public :: unwrap_class_irf_type
    public :: unwrap_class_irf_cw_type
    public :: unwrap_class_irf_tof_type
    public :: unwrap_class_group_type
    public :: unwrap_class_spg_type
    public :: unwrap_class_superspacegroup_type
    public :: unwrap_class_point_orbit
    public :: unwrap_class_orbit_type
    public :: unwrap_class_point_orbit_kv
    public :: unwrap_class_mod_orbit
    public :: unwrap_class_atm_type_no_alloc
    public :: unwrap_class_atm_std_type_no_alloc
    public :: unwrap_class_modatm_std_type_no_alloc
    public :: unwrap_class_atm_ref_type_no_alloc
    public :: unwrap_class_modatm_ref_type_no_alloc
    public :: unwrap_class_cell_type_no_alloc
    public :: unwrap_class_cell_g_type_no_alloc
    public :: unwrap_class_cell_ls_type_no_alloc
    public :: unwrap_class_cell_gls_type_no_alloc
    public :: unwrap_class_diffpat_type_no_alloc
    public :: unwrap_class_diffpat_e_type_no_alloc
    public :: unwrap_class_diffpat_g_type_no_alloc
    public :: unwrap_class_diffpatt_conditions_type_no_alloc
    public :: unwrap_class_powpatt_cw_conditions_type_no_alloc
    public :: unwrap_class_powpatt_tof_conditions_type_no_alloc
    public :: unwrap_class_refl_type_no_alloc
    public :: unwrap_class_srefl_type_no_alloc
    public :: unwrap_class_mrefl_type_no_alloc
    public :: unwrap_class_reflist_type_no_alloc
    public :: unwrap_class_refp_type_no_alloc
    public :: unwrap_class_irf_type_no_alloc
    public :: unwrap_class_irf_cw_type_no_alloc
    public :: unwrap_class_irf_tof_type_no_alloc
    public :: unwrap_class_group_type_no_alloc
    public :: unwrap_class_spg_type_no_alloc
    public :: unwrap_class_superspacegroup_type_no_alloc
    public :: unwrap_class_point_orbit_no_alloc
    public :: unwrap_class_orbit_type_no_alloc
    public :: unwrap_class_point_orbit_kv_no_alloc
    public :: unwrap_class_mod_orbit_no_alloc
    public :: unwrap_type
    public :: wrap_type

    interface list_to_class_array_atm_type
        module procedure list_to_class_array1d_atm_type
        module procedure list_to_class_array2d_atm_type
    end interface

    interface list_to_class_array_atm_std_type
        module procedure list_to_class_array1d_atm_std_type
        module procedure list_to_class_array2d_atm_std_type
    end interface

    interface list_to_class_array_modatm_std_type
        module procedure list_to_class_array1d_modatm_std_type
        module procedure list_to_class_array2d_modatm_std_type
    end interface

    interface list_to_class_array_atm_ref_type
        module procedure list_to_class_array1d_atm_ref_type
        module procedure list_to_class_array2d_atm_ref_type
    end interface

    interface list_to_class_array_modatm_ref_type
        module procedure list_to_class_array1d_modatm_ref_type
        module procedure list_to_class_array2d_modatm_ref_type
    end interface

    interface list_to_class_array_cell_type
        module procedure list_to_class_array1d_cell_type
        module procedure list_to_class_array2d_cell_type
    end interface

    interface list_to_class_array_cell_g_type
        module procedure list_to_class_array1d_cell_g_type
        module procedure list_to_class_array2d_cell_g_type
    end interface

    interface list_to_class_array_cell_ls_type
        module procedure list_to_class_array1d_cell_ls_type
        module procedure list_to_class_array2d_cell_ls_type
    end interface

    interface list_to_class_array_cell_gls_type
        module procedure list_to_class_array1d_cell_gls_type
        module procedure list_to_class_array2d_cell_gls_type
    end interface

    interface list_to_class_array_diffpat_type
        module procedure list_to_class_array1d_diffpat_type
        module procedure list_to_class_array2d_diffpat_type
    end interface

    interface list_to_class_array_diffpat_e_type
        module procedure list_to_class_array1d_diffpat_e_type
        module procedure list_to_class_array2d_diffpat_e_type
    end interface

    interface list_to_class_array_diffpat_g_type
        module procedure list_to_class_array1d_diffpat_g_type
        module procedure list_to_class_array2d_diffpat_g_type
    end interface

    interface list_to_class_array_diffpatt_conditions_type
        module procedure list_to_class_array1d_diffpatt_conditions_type
        module procedure list_to_class_array2d_diffpatt_conditions_type
    end interface

    interface list_to_class_array_powpatt_cw_conditions_type
        module procedure list_to_class_array1d_powpatt_cw_conditions_type
        module procedure list_to_class_array2d_powpatt_cw_conditions_type
    end interface

    interface list_to_class_array_powpatt_tof_conditions_type
        module procedure list_to_class_array1d_powpatt_tof_conditions_type
        module procedure list_to_class_array2d_powpatt_tof_conditions_type
    end interface

    interface list_to_class_array_refl_type
        module procedure list_to_class_array1d_refl_type
        module procedure list_to_class_array2d_refl_type
    end interface

    interface list_to_class_array_srefl_type
        module procedure list_to_class_array1d_srefl_type
        module procedure list_to_class_array2d_srefl_type
    end interface

    interface list_to_class_array_mrefl_type
        module procedure list_to_class_array1d_mrefl_type
        module procedure list_to_class_array2d_mrefl_type
    end interface

    interface list_to_class_array_reflist_type
        module procedure list_to_class_array1d_reflist_type
        module procedure list_to_class_array2d_reflist_type
    end interface

    interface list_to_class_array_refp_type
        module procedure list_to_class_array1d_refp_type
        module procedure list_to_class_array2d_refp_type
    end interface

    interface list_to_class_array_irf_type
        module procedure list_to_class_array1d_irf_type
        module procedure list_to_class_array2d_irf_type
    end interface

    interface list_to_class_array_irf_cw_type
        module procedure list_to_class_array1d_irf_cw_type
        module procedure list_to_class_array2d_irf_cw_type
    end interface

    interface list_to_class_array_irf_tof_type
        module procedure list_to_class_array1d_irf_tof_type
        module procedure list_to_class_array2d_irf_tof_type
    end interface

    interface list_to_class_array_group_type
        module procedure list_to_class_array1d_group_type
        module procedure list_to_class_array2d_group_type
    end interface

    interface list_to_class_array_spg_type
        module procedure list_to_class_array1d_spg_type
        module procedure list_to_class_array2d_spg_type
    end interface

    interface list_to_class_array_superspacegroup_type
        module procedure list_to_class_array1d_superspacegroup_type
        module procedure list_to_class_array2d_superspacegroup_type
    end interface

    interface list_to_class_array_point_orbit
        module procedure list_to_class_array1d_point_orbit
        module procedure list_to_class_array2d_point_orbit
    end interface

    interface list_to_class_array_orbit_type
        module procedure list_to_class_array1d_orbit_type
        module procedure list_to_class_array2d_orbit_type
    end interface

    interface list_to_class_array_point_orbit_kv
        module procedure list_to_class_array1d_point_orbit_kv
        module procedure list_to_class_array2d_point_orbit_kv
    end interface

    interface list_to_class_array_mod_orbit
        module procedure list_to_class_array1d_mod_orbit
        module procedure list_to_class_array2d_mod_orbit
    end interface

    interface list_to_type_array
        module procedure list_to_type_array1d_atm_type
        module procedure list_to_type_array2d_atm_type
        module procedure list_to_type_array1d_atm_std_type
        module procedure list_to_type_array2d_atm_std_type
        module procedure list_to_type_array1d_modatm_std_type
        module procedure list_to_type_array2d_modatm_std_type
        module procedure list_to_type_array1d_atm_ref_type
        module procedure list_to_type_array2d_atm_ref_type
        module procedure list_to_type_array1d_modatm_ref_type
        module procedure list_to_type_array2d_modatm_ref_type
        module procedure list_to_type_array1d_atm_cell_type
        module procedure list_to_type_array2d_atm_cell_type
        module procedure list_to_type_array1d_atom_equiv_type
        module procedure list_to_type_array2d_atom_equiv_type
        module procedure list_to_type_array1d_atom_equiv_list_type
        module procedure list_to_type_array2d_atom_equiv_list_type
        module procedure list_to_type_array1d_atlist_type
        module procedure list_to_type_array2d_atlist_type
        module procedure list_to_type_array1d_matom_type
        module procedure list_to_type_array2d_matom_type
        module procedure list_to_type_array1d_matom_list_type
        module procedure list_to_type_array2d_matom_list_type
        module procedure list_to_type_array1d_file_list_type
        module procedure list_to_type_array2d_file_list_type
        module procedure list_to_type_array1d_string_array_type
        module procedure list_to_type_array2d_string_array_type
        module procedure list_to_type_array1d_file_type
        module procedure list_to_type_array2d_file_type
        module procedure list_to_type_array1d_pkb_type
        module procedure list_to_type_array2d_pkb_type
        module procedure list_to_type_array1d_peak_search_cond_type
        module procedure list_to_type_array2d_peak_search_cond_type
        module procedure list_to_type_array1d_anomalous_sc_type
        module procedure list_to_type_array2d_anomalous_sc_type
        module procedure list_to_type_array1d_chem_info_type
        module procedure list_to_type_array2d_chem_info_type
        module procedure list_to_type_array1d_magnetic_form_type
        module procedure list_to_type_array2d_magnetic_form_type
        module procedure list_to_type_array1d_xray_form_type
        module procedure list_to_type_array2d_xray_form_type
        module procedure list_to_type_array1d_xray_wavelength_type
        module procedure list_to_type_array2d_xray_wavelength_type
        module procedure list_to_type_array1d_scattering_species_type
        module procedure list_to_type_array2d_scattering_species_type
        module procedure list_to_type_array1d_strf_type
        module procedure list_to_type_array2d_strf_type
        module procedure list_to_type_array1d_strflist_type
        module procedure list_to_type_array2d_strflist_type
        module procedure list_to_type_array1d_coordination_type
        module procedure list_to_type_array2d_coordination_type
        module procedure list_to_type_array1d_point_list_type
        module procedure list_to_type_array2d_point_list_type
        module procedure list_to_type_array1d_cell_type
        module procedure list_to_type_array2d_cell_type
        module procedure list_to_type_array1d_cell_g_type
        module procedure list_to_type_array2d_cell_g_type
        module procedure list_to_type_array1d_cell_ls_type
        module procedure list_to_type_array2d_cell_ls_type
        module procedure list_to_type_array1d_cell_gls_type
        module procedure list_to_type_array2d_cell_gls_type
        module procedure list_to_type_array1d_twofold_axes_type
        module procedure list_to_type_array2d_twofold_axes_type
        module procedure list_to_type_array1d_zone_axis_type
        module procedure list_to_type_array2d_zone_axis_type
        module procedure list_to_type_array1d_strain_tensor_type
        module procedure list_to_type_array2d_strain_tensor_type
        module procedure list_to_type_array1d_esmeralda_laue_image_type
        module procedure list_to_type_array2d_esmeralda_laue_image_type
        module procedure list_to_type_array1d_coordination_asu_type
        module procedure list_to_type_array2d_coordination_asu_type
        module procedure list_to_type_array1d_coordination_crystal_type
        module procedure list_to_type_array2d_coordination_crystal_type
        module procedure list_to_type_array1d_coordination_orbit_type
        module procedure list_to_type_array2d_coordination_orbit_type
        module procedure list_to_type_array1d_crystal_bond_type
        module procedure list_to_type_array2d_crystal_bond_type
        module procedure list_to_type_array1d_crystal_type
        module procedure list_to_type_array2d_crystal_type
        module procedure list_to_type_array1d_envelope_type
        module procedure list_to_type_array2d_envelope_type
        module procedure list_to_type_array1d_envelope_list_type
        module procedure list_to_type_array2d_envelope_list_type
        module procedure list_to_type_array1d_graphical_crystal_type
        module procedure list_to_type_array2d_graphical_crystal_type
        module procedure list_to_type_array1d_shub_spgr_info_type
        module procedure list_to_type_array2d_shub_spgr_info_type
        module procedure list_to_type_array1d_spgr_info_type
        module procedure list_to_type_array2d_spgr_info_type
        module procedure list_to_type_array1d_table_equiv_type
        module procedure list_to_type_array2d_table_equiv_type
        module procedure list_to_type_array1d_wyck_info_type
        module procedure list_to_type_array2d_wyck_info_type
        module procedure list_to_type_array1d_diffpat_type
        module procedure list_to_type_array2d_diffpat_type
        module procedure list_to_type_array1d_diffpat_e_type
        module procedure list_to_type_array2d_diffpat_e_type
        module procedure list_to_type_array1d_diffpat_g_type
        module procedure list_to_type_array2d_diffpat_g_type
        module procedure list_to_type_array1d_diffpatt_conditions_type
        module procedure list_to_type_array2d_diffpatt_conditions_type
        module procedure list_to_type_array1d_powpatt_cw_conditions_type
        module procedure list_to_type_array2d_powpatt_cw_conditions_type
        module procedure list_to_type_array1d_powpatt_tof_conditions_type
        module procedure list_to_type_array2d_powpatt_tof_conditions_type
        module procedure list_to_type_array1d_bck_type
        module procedure list_to_type_array2d_bck_type
        module procedure list_to_type_array1d_interval_type
        module procedure list_to_type_array2d_interval_type
        module procedure list_to_type_array1d_excl_reg_type
        module procedure list_to_type_array2d_excl_reg_type
        module procedure list_to_type_array1d_pattern_type
        module procedure list_to_type_array2d_pattern_type
        module procedure list_to_type_array1d_job_info_type
        module procedure list_to_type_array2d_job_info_type
        module procedure list_to_type_array1d_blockinfo_type
        module procedure list_to_type_array2d_blockinfo_type
        module procedure list_to_type_array1d_genvec_type
        module procedure list_to_type_array2d_genvec_type
        module procedure list_to_type_array1d_sxtal_attributes_type
        module procedure list_to_type_array2d_sxtal_attributes_type
        module procedure list_to_type_array1d_powder_attributes_type
        module procedure list_to_type_array2d_powder_attributes_type
        module procedure list_to_type_array1d_phase_type
        module procedure list_to_type_array2d_phase_type
        module procedure list_to_type_array1d_molecule_type
        module procedure list_to_type_array2d_molecule_type
        module procedure list_to_type_array1d_molcrystal_type
        module procedure list_to_type_array2d_molcrystal_type
        module procedure list_to_type_array1d_group_k_type
        module procedure list_to_type_array2d_group_k_type
        module procedure list_to_type_array1d_basic_numc_type
        module procedure list_to_type_array2d_basic_numc_type
        module procedure list_to_type_array1d_basic_numi_type
        module procedure list_to_type_array2d_basic_numi_type
        module procedure list_to_type_array1d_basic_numr_type
        module procedure list_to_type_array2d_basic_numr_type
        module procedure list_to_type_array1d_calibration_detector_type
        module procedure list_to_type_array2d_calibration_detector_type
        module procedure list_to_type_array1d_diffractometer_type
        module procedure list_to_type_array2d_diffractometer_type
        module procedure list_to_type_array1d_generic_numor_type
        module procedure list_to_type_array2d_generic_numor_type
        module procedure list_to_type_array1d_ill_data_record_type
        module procedure list_to_type_array2d_ill_data_record_type
        module procedure list_to_type_array1d_powder_numor_type
        module procedure list_to_type_array2d_powder_numor_type
        module procedure list_to_type_array1d_sxtal_numor_type
        module procedure list_to_type_array2d_sxtal_numor_type
        module procedure list_to_type_array1d_sxtal_orient_type
        module procedure list_to_type_array2d_sxtal_orient_type
        module procedure list_to_type_array1d_refl_type
        module procedure list_to_type_array2d_refl_type
        module procedure list_to_type_array1d_srefl_type
        module procedure list_to_type_array2d_srefl_type
        module procedure list_to_type_array1d_mrefl_type
        module procedure list_to_type_array2d_mrefl_type
        module procedure list_to_type_array1d_reflist_type
        module procedure list_to_type_array2d_reflist_type
        module procedure list_to_type_array1d_refp_type
        module procedure list_to_type_array2d_refp_type
        module procedure list_to_type_array1d_atomic_properties_type
        module procedure list_to_type_array2d_atomic_properties_type
        module procedure list_to_type_array1d_bvel_par_type
        module procedure list_to_type_array2d_bvel_par_type
        module procedure list_to_type_array1d_bvs_par_type
        module procedure list_to_type_array2d_bvs_par_type
        module procedure list_to_type_array1d_sbvs_par_type
        module procedure list_to_type_array2d_sbvs_par_type
        module procedure list_to_type_array1d_irf_type
        module procedure list_to_type_array2d_irf_type
        module procedure list_to_type_array1d_irf_cw_type
        module procedure list_to_type_array2d_irf_cw_type
        module procedure list_to_type_array1d_irf_tof_type
        module procedure list_to_type_array2d_irf_tof_type
        module procedure list_to_type_array1d_atoms_conf_list_type
        module procedure list_to_type_array2d_atoms_conf_list_type
        module procedure list_to_type_array1d_psd_val_type
        module procedure list_to_type_array2d_psd_val_type
        module procedure list_to_type_array1d_sxd_val_type
        module procedure list_to_type_array2d_sxd_val_type
        module procedure list_to_type_array1d_twin_type
        module procedure list_to_type_array2d_twin_type
        module procedure list_to_type_array1d_multistate_vector_type
        module procedure list_to_type_array2d_multistate_vector_type
        module procedure list_to_type_array1d_simann_conditions_type
        module procedure list_to_type_array2d_simann_conditions_type
        module procedure list_to_type_array1d_state_vector_type
        module procedure list_to_type_array2d_state_vector_type
        module procedure list_to_type_array1d_deriv_tof_type
        module procedure list_to_type_array2d_deriv_tof_type
        module procedure list_to_type_array1d_excluded_regions_type
        module procedure list_to_type_array2d_excluded_regions_type
        module procedure list_to_type_array1d_header_type
        module procedure list_to_type_array2d_header_type
        module procedure list_to_type_array1d_image_conditions
        module procedure list_to_type_array2d_image_conditions
        module procedure list_to_type_array1d_laue_instrument_type
        module procedure list_to_type_array2d_laue_instrument_type
        module procedure list_to_type_array1d_laue_ref_type
        module procedure list_to_type_array2d_laue_ref_type
        module procedure list_to_type_array1d_laue_ref_list_type
        module procedure list_to_type_array2d_laue_ref_list_type
        module procedure list_to_type_array1d_peakfind_parameters_type
        module procedure list_to_type_array2d_peakfind_parameters_type
        module procedure list_to_type_array1d_peak_type
        module procedure list_to_type_array2d_peak_type
        module procedure list_to_type_array1d_peak_list_type
        module procedure list_to_type_array2d_peak_list_type
        module procedure list_to_type_array1d_rational
        module procedure list_to_type_array2d_rational
        module procedure list_to_type_array1d_symm_oper_type
        module procedure list_to_type_array2d_symm_oper_type
        module procedure list_to_type_array1d_group_type
        module procedure list_to_type_array2d_group_type
        module procedure list_to_type_array1d_rot_mat_type
        module procedure list_to_type_array2d_rot_mat_type
        module procedure list_to_type_array1d_spg_type
        module procedure list_to_type_array2d_spg_type
        module procedure list_to_type_array1d_superspacegroup_type
        module procedure list_to_type_array2d_superspacegroup_type
        module procedure list_to_type_array1d_spin_operator_type
        module procedure list_to_type_array2d_spin_operator_type
        module procedure list_to_type_array1d_spin_group_type
        module procedure list_to_type_array2d_spin_group_type
        module procedure list_to_type_array1d_kvect_info_type
        module procedure list_to_type_array2d_kvect_info_type
        module procedure list_to_type_array1d_point_orbit
        module procedure list_to_type_array2d_point_orbit
        module procedure list_to_type_array1d_orbit_type
        module procedure list_to_type_array2d_orbit_type
        module procedure list_to_type_array1d_orbit_list
        module procedure list_to_type_array2d_orbit_list
        module procedure list_to_type_array1d_sym_oper_type
        module procedure list_to_type_array2d_sym_oper_type
        module procedure list_to_type_array1d_msym_oper_type
        module procedure list_to_type_array2d_msym_oper_type
        module procedure list_to_type_array1d_magnetic_domain_type
        module procedure list_to_type_array2d_magnetic_domain_type
        module procedure list_to_type_array1d_magsymm_k_type
        module procedure list_to_type_array2d_magsymm_k_type
        module procedure list_to_type_array1d_point_orbit_kv
        module procedure list_to_type_array2d_point_orbit_kv
        module procedure list_to_type_array1d_mod_orbit
        module procedure list_to_type_array2d_mod_orbit
    end interface

    interface list_to_type_array_no_alloc
        module procedure list_to_type_array1d_atm_type_no_alloc
        module procedure list_to_type_array2d_atm_type_no_alloc
        module procedure list_to_type_array1d_atm_std_type_no_alloc
        module procedure list_to_type_array2d_atm_std_type_no_alloc
        module procedure list_to_type_array1d_modatm_std_type_no_alloc
        module procedure list_to_type_array2d_modatm_std_type_no_alloc
        module procedure list_to_type_array1d_atm_ref_type_no_alloc
        module procedure list_to_type_array2d_atm_ref_type_no_alloc
        module procedure list_to_type_array1d_modatm_ref_type_no_alloc
        module procedure list_to_type_array2d_modatm_ref_type_no_alloc
        module procedure list_to_type_array1d_atm_cell_type_no_alloc
        module procedure list_to_type_array2d_atm_cell_type_no_alloc
        module procedure list_to_type_array1d_atom_equiv_type_no_alloc
        module procedure list_to_type_array2d_atom_equiv_type_no_alloc
        module procedure list_to_type_array1d_atom_equiv_list_type_no_alloc
        module procedure list_to_type_array2d_atom_equiv_list_type_no_alloc
        module procedure list_to_type_array1d_atlist_type_no_alloc
        module procedure list_to_type_array2d_atlist_type_no_alloc
        module procedure list_to_type_array1d_matom_type_no_alloc
        module procedure list_to_type_array2d_matom_type_no_alloc
        module procedure list_to_type_array1d_matom_list_type_no_alloc
        module procedure list_to_type_array2d_matom_list_type_no_alloc
        module procedure list_to_type_array1d_file_list_type_no_alloc
        module procedure list_to_type_array2d_file_list_type_no_alloc
        module procedure list_to_type_array1d_string_array_type_no_alloc
        module procedure list_to_type_array2d_string_array_type_no_alloc
        module procedure list_to_type_array1d_file_type_no_alloc
        module procedure list_to_type_array2d_file_type_no_alloc
        module procedure list_to_type_array1d_pkb_type_no_alloc
        module procedure list_to_type_array2d_pkb_type_no_alloc
        module procedure list_to_type_array1d_peak_search_cond_type_no_alloc
        module procedure list_to_type_array2d_peak_search_cond_type_no_alloc
        module procedure list_to_type_array1d_anomalous_sc_type_no_alloc
        module procedure list_to_type_array2d_anomalous_sc_type_no_alloc
        module procedure list_to_type_array1d_chem_info_type_no_alloc
        module procedure list_to_type_array2d_chem_info_type_no_alloc
        module procedure list_to_type_array1d_magnetic_form_type_no_alloc
        module procedure list_to_type_array2d_magnetic_form_type_no_alloc
        module procedure list_to_type_array1d_xray_form_type_no_alloc
        module procedure list_to_type_array2d_xray_form_type_no_alloc
        module procedure list_to_type_array1d_xray_wavelength_type_no_alloc
        module procedure list_to_type_array2d_xray_wavelength_type_no_alloc
        module procedure list_to_type_array1d_scattering_species_type_no_alloc
        module procedure list_to_type_array2d_scattering_species_type_no_alloc
        module procedure list_to_type_array1d_strf_type_no_alloc
        module procedure list_to_type_array2d_strf_type_no_alloc
        module procedure list_to_type_array1d_strflist_type_no_alloc
        module procedure list_to_type_array2d_strflist_type_no_alloc
        module procedure list_to_type_array1d_coordination_type_no_alloc
        module procedure list_to_type_array2d_coordination_type_no_alloc
        module procedure list_to_type_array1d_point_list_type_no_alloc
        module procedure list_to_type_array2d_point_list_type_no_alloc
        module procedure list_to_type_array1d_cell_type_no_alloc
        module procedure list_to_type_array2d_cell_type_no_alloc
        module procedure list_to_type_array1d_cell_g_type_no_alloc
        module procedure list_to_type_array2d_cell_g_type_no_alloc
        module procedure list_to_type_array1d_cell_ls_type_no_alloc
        module procedure list_to_type_array2d_cell_ls_type_no_alloc
        module procedure list_to_type_array1d_cell_gls_type_no_alloc
        module procedure list_to_type_array2d_cell_gls_type_no_alloc
        module procedure list_to_type_array1d_twofold_axes_type_no_alloc
        module procedure list_to_type_array2d_twofold_axes_type_no_alloc
        module procedure list_to_type_array1d_zone_axis_type_no_alloc
        module procedure list_to_type_array2d_zone_axis_type_no_alloc
        module procedure list_to_type_array1d_strain_tensor_type_no_alloc
        module procedure list_to_type_array2d_strain_tensor_type_no_alloc
        module procedure list_to_type_array1d_esmeralda_laue_image_type_no_alloc
        module procedure list_to_type_array2d_esmeralda_laue_image_type_no_alloc
        module procedure list_to_type_array1d_coordination_asu_type_no_alloc
        module procedure list_to_type_array2d_coordination_asu_type_no_alloc
        module procedure list_to_type_array1d_coordination_crystal_type_no_alloc
        module procedure list_to_type_array2d_coordination_crystal_type_no_alloc
        module procedure list_to_type_array1d_coordination_orbit_type_no_alloc
        module procedure list_to_type_array2d_coordination_orbit_type_no_alloc
        module procedure list_to_type_array1d_crystal_bond_type_no_alloc
        module procedure list_to_type_array2d_crystal_bond_type_no_alloc
        module procedure list_to_type_array1d_crystal_type_no_alloc
        module procedure list_to_type_array2d_crystal_type_no_alloc
        module procedure list_to_type_array1d_envelope_type_no_alloc
        module procedure list_to_type_array2d_envelope_type_no_alloc
        module procedure list_to_type_array1d_envelope_list_type_no_alloc
        module procedure list_to_type_array2d_envelope_list_type_no_alloc
        module procedure list_to_type_array1d_graphical_crystal_type_no_alloc
        module procedure list_to_type_array2d_graphical_crystal_type_no_alloc
        module procedure list_to_type_array1d_shub_spgr_info_type_no_alloc
        module procedure list_to_type_array2d_shub_spgr_info_type_no_alloc
        module procedure list_to_type_array1d_spgr_info_type_no_alloc
        module procedure list_to_type_array2d_spgr_info_type_no_alloc
        module procedure list_to_type_array1d_table_equiv_type_no_alloc
        module procedure list_to_type_array2d_table_equiv_type_no_alloc
        module procedure list_to_type_array1d_wyck_info_type_no_alloc
        module procedure list_to_type_array2d_wyck_info_type_no_alloc
        module procedure list_to_type_array1d_diffpat_type_no_alloc
        module procedure list_to_type_array2d_diffpat_type_no_alloc
        module procedure list_to_type_array1d_diffpat_e_type_no_alloc
        module procedure list_to_type_array2d_diffpat_e_type_no_alloc
        module procedure list_to_type_array1d_diffpat_g_type_no_alloc
        module procedure list_to_type_array2d_diffpat_g_type_no_alloc
        module procedure list_to_type_array1d_diffpatt_conditions_type_no_alloc
        module procedure list_to_type_array2d_diffpatt_conditions_type_no_alloc
        module procedure list_to_type_array1d_powpatt_cw_conditions_type_no_alloc
        module procedure list_to_type_array2d_powpatt_cw_conditions_type_no_alloc
        module procedure list_to_type_array1d_powpatt_tof_conditions_type_no_alloc
        module procedure list_to_type_array2d_powpatt_tof_conditions_type_no_alloc
        module procedure list_to_type_array1d_bck_type_no_alloc
        module procedure list_to_type_array2d_bck_type_no_alloc
        module procedure list_to_type_array1d_interval_type_no_alloc
        module procedure list_to_type_array2d_interval_type_no_alloc
        module procedure list_to_type_array1d_excl_reg_type_no_alloc
        module procedure list_to_type_array2d_excl_reg_type_no_alloc
        module procedure list_to_type_array1d_pattern_type_no_alloc
        module procedure list_to_type_array2d_pattern_type_no_alloc
        module procedure list_to_type_array1d_job_info_type_no_alloc
        module procedure list_to_type_array2d_job_info_type_no_alloc
        module procedure list_to_type_array1d_blockinfo_type_no_alloc
        module procedure list_to_type_array2d_blockinfo_type_no_alloc
        module procedure list_to_type_array1d_genvec_type_no_alloc
        module procedure list_to_type_array2d_genvec_type_no_alloc
        module procedure list_to_type_array1d_sxtal_attributes_type_no_alloc
        module procedure list_to_type_array2d_sxtal_attributes_type_no_alloc
        module procedure list_to_type_array1d_powder_attributes_type_no_alloc
        module procedure list_to_type_array2d_powder_attributes_type_no_alloc
        module procedure list_to_type_array1d_phase_type_no_alloc
        module procedure list_to_type_array2d_phase_type_no_alloc
        module procedure list_to_type_array1d_molecule_type_no_alloc
        module procedure list_to_type_array2d_molecule_type_no_alloc
        module procedure list_to_type_array1d_molcrystal_type_no_alloc
        module procedure list_to_type_array2d_molcrystal_type_no_alloc
        module procedure list_to_type_array1d_group_k_type_no_alloc
        module procedure list_to_type_array2d_group_k_type_no_alloc
        module procedure list_to_type_array1d_basic_numc_type_no_alloc
        module procedure list_to_type_array2d_basic_numc_type_no_alloc
        module procedure list_to_type_array1d_basic_numi_type_no_alloc
        module procedure list_to_type_array2d_basic_numi_type_no_alloc
        module procedure list_to_type_array1d_basic_numr_type_no_alloc
        module procedure list_to_type_array2d_basic_numr_type_no_alloc
        module procedure list_to_type_array1d_calibration_detector_type_no_alloc
        module procedure list_to_type_array2d_calibration_detector_type_no_alloc
        module procedure list_to_type_array1d_diffractometer_type_no_alloc
        module procedure list_to_type_array2d_diffractometer_type_no_alloc
        module procedure list_to_type_array1d_generic_numor_type_no_alloc
        module procedure list_to_type_array2d_generic_numor_type_no_alloc
        module procedure list_to_type_array1d_ill_data_record_type_no_alloc
        module procedure list_to_type_array2d_ill_data_record_type_no_alloc
        module procedure list_to_type_array1d_powder_numor_type_no_alloc
        module procedure list_to_type_array2d_powder_numor_type_no_alloc
        module procedure list_to_type_array1d_sxtal_numor_type_no_alloc
        module procedure list_to_type_array2d_sxtal_numor_type_no_alloc
        module procedure list_to_type_array1d_sxtal_orient_type_no_alloc
        module procedure list_to_type_array2d_sxtal_orient_type_no_alloc
        module procedure list_to_type_array1d_refl_type_no_alloc
        module procedure list_to_type_array2d_refl_type_no_alloc
        module procedure list_to_type_array1d_srefl_type_no_alloc
        module procedure list_to_type_array2d_srefl_type_no_alloc
        module procedure list_to_type_array1d_mrefl_type_no_alloc
        module procedure list_to_type_array2d_mrefl_type_no_alloc
        module procedure list_to_type_array1d_reflist_type_no_alloc
        module procedure list_to_type_array2d_reflist_type_no_alloc
        module procedure list_to_type_array1d_refp_type_no_alloc
        module procedure list_to_type_array2d_refp_type_no_alloc
        module procedure list_to_type_array1d_atomic_properties_type_no_alloc
        module procedure list_to_type_array2d_atomic_properties_type_no_alloc
        module procedure list_to_type_array1d_bvel_par_type_no_alloc
        module procedure list_to_type_array2d_bvel_par_type_no_alloc
        module procedure list_to_type_array1d_bvs_par_type_no_alloc
        module procedure list_to_type_array2d_bvs_par_type_no_alloc
        module procedure list_to_type_array1d_sbvs_par_type_no_alloc
        module procedure list_to_type_array2d_sbvs_par_type_no_alloc
        module procedure list_to_type_array1d_irf_type_no_alloc
        module procedure list_to_type_array2d_irf_type_no_alloc
        module procedure list_to_type_array1d_irf_cw_type_no_alloc
        module procedure list_to_type_array2d_irf_cw_type_no_alloc
        module procedure list_to_type_array1d_irf_tof_type_no_alloc
        module procedure list_to_type_array2d_irf_tof_type_no_alloc
        module procedure list_to_type_array1d_atoms_conf_list_type_no_alloc
        module procedure list_to_type_array2d_atoms_conf_list_type_no_alloc
        module procedure list_to_type_array1d_psd_val_type_no_alloc
        module procedure list_to_type_array2d_psd_val_type_no_alloc
        module procedure list_to_type_array1d_sxd_val_type_no_alloc
        module procedure list_to_type_array2d_sxd_val_type_no_alloc
        module procedure list_to_type_array1d_twin_type_no_alloc
        module procedure list_to_type_array2d_twin_type_no_alloc
        module procedure list_to_type_array1d_multistate_vector_type_no_alloc
        module procedure list_to_type_array2d_multistate_vector_type_no_alloc
        module procedure list_to_type_array1d_simann_conditions_type_no_alloc
        module procedure list_to_type_array2d_simann_conditions_type_no_alloc
        module procedure list_to_type_array1d_state_vector_type_no_alloc
        module procedure list_to_type_array2d_state_vector_type_no_alloc
        module procedure list_to_type_array1d_deriv_tof_type_no_alloc
        module procedure list_to_type_array2d_deriv_tof_type_no_alloc
        module procedure list_to_type_array1d_excluded_regions_type_no_alloc
        module procedure list_to_type_array2d_excluded_regions_type_no_alloc
        module procedure list_to_type_array1d_header_type_no_alloc
        module procedure list_to_type_array2d_header_type_no_alloc
        module procedure list_to_type_array1d_image_conditions_no_alloc
        module procedure list_to_type_array2d_image_conditions_no_alloc
        module procedure list_to_type_array1d_laue_instrument_type_no_alloc
        module procedure list_to_type_array2d_laue_instrument_type_no_alloc
        module procedure list_to_type_array1d_laue_ref_type_no_alloc
        module procedure list_to_type_array2d_laue_ref_type_no_alloc
        module procedure list_to_type_array1d_laue_ref_list_type_no_alloc
        module procedure list_to_type_array2d_laue_ref_list_type_no_alloc
        module procedure list_to_type_array1d_peakfind_parameters_type_no_alloc
        module procedure list_to_type_array2d_peakfind_parameters_type_no_alloc
        module procedure list_to_type_array1d_peak_type_no_alloc
        module procedure list_to_type_array2d_peak_type_no_alloc
        module procedure list_to_type_array1d_peak_list_type_no_alloc
        module procedure list_to_type_array2d_peak_list_type_no_alloc
        module procedure list_to_type_array1d_rational_no_alloc
        module procedure list_to_type_array2d_rational_no_alloc
        module procedure list_to_type_array1d_symm_oper_type_no_alloc
        module procedure list_to_type_array2d_symm_oper_type_no_alloc
        module procedure list_to_type_array1d_group_type_no_alloc
        module procedure list_to_type_array2d_group_type_no_alloc
        module procedure list_to_type_array1d_rot_mat_type_no_alloc
        module procedure list_to_type_array2d_rot_mat_type_no_alloc
        module procedure list_to_type_array1d_spg_type_no_alloc
        module procedure list_to_type_array2d_spg_type_no_alloc
        module procedure list_to_type_array1d_superspacegroup_type_no_alloc
        module procedure list_to_type_array2d_superspacegroup_type_no_alloc
        module procedure list_to_type_array1d_spin_operator_type_no_alloc
        module procedure list_to_type_array2d_spin_operator_type_no_alloc
        module procedure list_to_type_array1d_spin_group_type_no_alloc
        module procedure list_to_type_array2d_spin_group_type_no_alloc
        module procedure list_to_type_array1d_kvect_info_type_no_alloc
        module procedure list_to_type_array2d_kvect_info_type_no_alloc
        module procedure list_to_type_array1d_point_orbit_no_alloc
        module procedure list_to_type_array2d_point_orbit_no_alloc
        module procedure list_to_type_array1d_orbit_type_no_alloc
        module procedure list_to_type_array2d_orbit_type_no_alloc
        module procedure list_to_type_array1d_orbit_list_no_alloc
        module procedure list_to_type_array2d_orbit_list_no_alloc
        module procedure list_to_type_array1d_sym_oper_type_no_alloc
        module procedure list_to_type_array2d_sym_oper_type_no_alloc
        module procedure list_to_type_array1d_msym_oper_type_no_alloc
        module procedure list_to_type_array2d_msym_oper_type_no_alloc
        module procedure list_to_type_array1d_magnetic_domain_type_no_alloc
        module procedure list_to_type_array2d_magnetic_domain_type_no_alloc
        module procedure list_to_type_array1d_magsymm_k_type_no_alloc
        module procedure list_to_type_array2d_magsymm_k_type_no_alloc
        module procedure list_to_type_array1d_point_orbit_kv_no_alloc
        module procedure list_to_type_array2d_point_orbit_kv_no_alloc
        module procedure list_to_type_array1d_mod_orbit_no_alloc
        module procedure list_to_type_array2d_mod_orbit_no_alloc
    end interface

    interface unwrap_type
        module procedure unwrap_type_atm_cell_type
        module procedure unwrap_type_atom_equiv_type
        module procedure unwrap_type_atom_equiv_list_type
        module procedure unwrap_type_atlist_type
        module procedure unwrap_type_matom_type
        module procedure unwrap_type_matom_list_type
        module procedure unwrap_type_file_list_type
        module procedure unwrap_type_string_array_type
        module procedure unwrap_type_file_type
        module procedure unwrap_type_pkb_type
        module procedure unwrap_type_peak_search_cond_type
        module procedure unwrap_type_anomalous_sc_type
        module procedure unwrap_type_chem_info_type
        module procedure unwrap_type_magnetic_form_type
        module procedure unwrap_type_xray_form_type
        module procedure unwrap_type_xray_wavelength_type
        module procedure unwrap_type_scattering_species_type
        module procedure unwrap_type_strf_type
        module procedure unwrap_type_strflist_type
        module procedure unwrap_type_coordination_type
        module procedure unwrap_type_point_list_type
        module procedure unwrap_type_twofold_axes_type
        module procedure unwrap_type_zone_axis_type
        module procedure unwrap_type_strain_tensor_type
        module procedure unwrap_type_esmeralda_laue_image_type
        module procedure unwrap_type_coordination_asu_type
        module procedure unwrap_type_coordination_crystal_type
        module procedure unwrap_type_coordination_orbit_type
        module procedure unwrap_type_crystal_bond_type
        module procedure unwrap_type_crystal_type
        module procedure unwrap_type_envelope_type
        module procedure unwrap_type_envelope_list_type
        module procedure unwrap_type_graphical_crystal_type
        module procedure unwrap_type_shub_spgr_info_type
        module procedure unwrap_type_spgr_info_type
        module procedure unwrap_type_table_equiv_type
        module procedure unwrap_type_wyck_info_type
        module procedure unwrap_type_bck_type
        module procedure unwrap_type_interval_type
        module procedure unwrap_type_excl_reg_type
        module procedure unwrap_type_pattern_type
        module procedure unwrap_type_job_info_type
        module procedure unwrap_type_blockinfo_type
        module procedure unwrap_type_genvec_type
        module procedure unwrap_type_sxtal_attributes_type
        module procedure unwrap_type_powder_attributes_type
        module procedure unwrap_type_phase_type
        module procedure unwrap_type_molecule_type
        module procedure unwrap_type_molcrystal_type
        module procedure unwrap_type_group_k_type
        module procedure unwrap_type_basic_numc_type
        module procedure unwrap_type_basic_numi_type
        module procedure unwrap_type_basic_numr_type
        module procedure unwrap_type_calibration_detector_type
        module procedure unwrap_type_diffractometer_type
        module procedure unwrap_type_generic_numor_type
        module procedure unwrap_type_ill_data_record_type
        module procedure unwrap_type_powder_numor_type
        module procedure unwrap_type_sxtal_numor_type
        module procedure unwrap_type_sxtal_orient_type
        module procedure unwrap_type_atomic_properties_type
        module procedure unwrap_type_bvel_par_type
        module procedure unwrap_type_bvs_par_type
        module procedure unwrap_type_sbvs_par_type
        module procedure unwrap_type_atoms_conf_list_type
        module procedure unwrap_type_psd_val_type
        module procedure unwrap_type_sxd_val_type
        module procedure unwrap_type_twin_type
        module procedure unwrap_type_multistate_vector_type
        module procedure unwrap_type_simann_conditions_type
        module procedure unwrap_type_state_vector_type
        module procedure unwrap_type_deriv_tof_type
        module procedure unwrap_type_excluded_regions_type
        module procedure unwrap_type_header_type
        module procedure unwrap_type_image_conditions
        module procedure unwrap_type_laue_instrument_type
        module procedure unwrap_type_laue_ref_type
        module procedure unwrap_type_laue_ref_list_type
        module procedure unwrap_type_peakfind_parameters_type
        module procedure unwrap_type_peak_type
        module procedure unwrap_type_peak_list_type
        module procedure unwrap_type_rational
        module procedure unwrap_type_symm_oper_type
        module procedure unwrap_type_rot_mat_type
        module procedure unwrap_type_spin_operator_type
        module procedure unwrap_type_spin_group_type
        module procedure unwrap_type_kvect_info_type
        module procedure unwrap_type_orbit_list
        module procedure unwrap_type_sym_oper_type
        module procedure unwrap_type_msym_oper_type
        module procedure unwrap_type_magnetic_domain_type
        module procedure unwrap_type_magsymm_k_type
    end interface

    interface wrap_type
        module procedure wrap_atm_type
        module procedure wrap_atm_cell_type
        module procedure wrap_atom_equiv_type
        module procedure wrap_atom_equiv_list_type
        module procedure wrap_atlist_type
        module procedure wrap_matom_type
        module procedure wrap_matom_list_type
        module procedure wrap_file_list_type
        module procedure wrap_string_array_type
        module procedure wrap_file_type
        module procedure wrap_pkb_type
        module procedure wrap_peak_search_cond_type
        module procedure wrap_anomalous_sc_type
        module procedure wrap_chem_info_type
        module procedure wrap_magnetic_form_type
        module procedure wrap_xray_form_type
        module procedure wrap_xray_wavelength_type
        module procedure wrap_scattering_species_type
        module procedure wrap_strf_type
        module procedure wrap_strflist_type
        module procedure wrap_coordination_type
        module procedure wrap_point_list_type
        module procedure wrap_cell_type
        module procedure wrap_twofold_axes_type
        module procedure wrap_zone_axis_type
        module procedure wrap_strain_tensor_type
        module procedure wrap_esmeralda_laue_image_type
        module procedure wrap_coordination_asu_type
        module procedure wrap_coordination_crystal_type
        module procedure wrap_coordination_orbit_type
        module procedure wrap_crystal_bond_type
        module procedure wrap_crystal_type
        module procedure wrap_envelope_type
        module procedure wrap_envelope_list_type
        module procedure wrap_graphical_crystal_type
        module procedure wrap_shub_spgr_info_type
        module procedure wrap_spgr_info_type
        module procedure wrap_table_equiv_type
        module procedure wrap_wyck_info_type
        module procedure wrap_diffpat_type
        module procedure wrap_diffpatt_conditions_type
        module procedure wrap_bck_type
        module procedure wrap_interval_type
        module procedure wrap_excl_reg_type
        module procedure wrap_pattern_type
        module procedure wrap_job_info_type
        module procedure wrap_blockinfo_type
        module procedure wrap_genvec_type
        module procedure wrap_sxtal_attributes_type
        module procedure wrap_powder_attributes_type
        module procedure wrap_phase_type
        module procedure wrap_molecule_type
        module procedure wrap_molcrystal_type
        module procedure wrap_group_k_type
        module procedure wrap_basic_numc_type
        module procedure wrap_basic_numi_type
        module procedure wrap_basic_numr_type
        module procedure wrap_calibration_detector_type
        module procedure wrap_diffractometer_type
        module procedure wrap_generic_numor_type
        module procedure wrap_ill_data_record_type
        module procedure wrap_powder_numor_type
        module procedure wrap_sxtal_numor_type
        module procedure wrap_sxtal_orient_type
        module procedure wrap_refl_type
        module procedure wrap_reflist_type
        module procedure wrap_atomic_properties_type
        module procedure wrap_bvel_par_type
        module procedure wrap_bvs_par_type
        module procedure wrap_sbvs_par_type
        module procedure wrap_irf_type
        module procedure wrap_atoms_conf_list_type
        module procedure wrap_psd_val_type
        module procedure wrap_sxd_val_type
        module procedure wrap_twin_type
        module procedure wrap_multistate_vector_type
        module procedure wrap_simann_conditions_type
        module procedure wrap_state_vector_type
        module procedure wrap_deriv_tof_type
        module procedure wrap_excluded_regions_type
        module procedure wrap_header_type
        module procedure wrap_image_conditions
        module procedure wrap_laue_instrument_type
        module procedure wrap_laue_ref_type
        module procedure wrap_laue_ref_list_type
        module procedure wrap_peakfind_parameters_type
        module procedure wrap_peak_type
        module procedure wrap_peak_list_type
        module procedure wrap_rational
        module procedure wrap_symm_oper_type
        module procedure wrap_group_type
        module procedure wrap_rot_mat_type
        module procedure wrap_spin_operator_type
        module procedure wrap_spin_group_type
        module procedure wrap_kvect_info_type
        module procedure wrap_point_orbit
        module procedure wrap_orbit_list
        module procedure wrap_sym_oper_type
        module procedure wrap_msym_oper_type
        module procedure wrap_magnetic_domain_type
        module procedure wrap_magsymm_k_type
        module procedure wrap_point_orbit_kv
    end interface

    interface

        module subroutine list_to_class_array1d_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_atm_type

        module subroutine list_to_class_array2d_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_atm_type

        module subroutine list_to_class_array1d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_atm_std_type

        module subroutine list_to_class_array2d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_atm_std_type

        module subroutine list_to_class_array1d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(modatm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_modatm_std_type

        module subroutine list_to_class_array2d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(modatm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_modatm_std_type

        module subroutine list_to_class_array1d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_atm_ref_type

        module subroutine list_to_class_array2d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_atm_ref_type

        module subroutine list_to_class_array1d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(modatm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_modatm_ref_type

        module subroutine list_to_class_array2d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(modatm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_modatm_ref_type

        module subroutine list_to_class_array1d_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_cell_type

        module subroutine list_to_class_array2d_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_cell_type

        module subroutine list_to_class_array1d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_cell_g_type

        module subroutine list_to_class_array2d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_cell_g_type

        module subroutine list_to_class_array1d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_ls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_cell_ls_type

        module subroutine list_to_class_array2d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_ls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_cell_ls_type

        module subroutine list_to_class_array1d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_gls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_cell_gls_type

        module subroutine list_to_class_array2d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_gls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_cell_gls_type

        module subroutine list_to_class_array1d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_diffpat_type

        module subroutine list_to_class_array2d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_diffpat_type

        module subroutine list_to_class_array1d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_e_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_diffpat_e_type

        module subroutine list_to_class_array2d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_e_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_diffpat_e_type

        module subroutine list_to_class_array1d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_diffpat_g_type

        module subroutine list_to_class_array2d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_diffpat_g_type

        module subroutine list_to_class_array1d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpatt_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_diffpatt_conditions_type

        module subroutine list_to_class_array2d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpatt_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_diffpatt_conditions_type

        module subroutine list_to_class_array1d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(powpatt_cw_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_powpatt_cw_conditions_type

        module subroutine list_to_class_array2d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(powpatt_cw_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_powpatt_cw_conditions_type

        module subroutine list_to_class_array1d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(powpatt_tof_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_powpatt_tof_conditions_type

        module subroutine list_to_class_array2d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(powpatt_tof_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_powpatt_tof_conditions_type

        module subroutine list_to_class_array1d_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(refl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_refl_type

        module subroutine list_to_class_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(refl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_refl_type

        module subroutine list_to_class_array1d_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(srefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_srefl_type

        module subroutine list_to_class_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_srefl_type

        module subroutine list_to_class_array1d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(mrefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_mrefl_type

        module subroutine list_to_class_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_mrefl_type

        module subroutine list_to_class_array1d_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(reflist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_reflist_type

        module subroutine list_to_class_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_reflist_type

        module subroutine list_to_class_array1d_refp_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(refp_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_refp_type

        module subroutine list_to_class_array2d_refp_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(refp_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_refp_type

        module subroutine list_to_class_array1d_irf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_irf_type

        module subroutine list_to_class_array2d_irf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_irf_type

        module subroutine list_to_class_array1d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_cw_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_irf_cw_type

        module subroutine list_to_class_array2d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_cw_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_irf_cw_type

        module subroutine list_to_class_array1d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_tof_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_irf_tof_type

        module subroutine list_to_class_array2d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(irf_tof_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_irf_tof_type

        module subroutine list_to_class_array1d_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(group_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_group_type

        module subroutine list_to_class_array2d_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(group_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_group_type

        module subroutine list_to_class_array1d_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(spg_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_spg_type

        module subroutine list_to_class_array2d_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(spg_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_spg_type

        module subroutine list_to_class_array1d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(superspacegroup_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_superspacegroup_type

        module subroutine list_to_class_array2d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(superspacegroup_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_superspacegroup_type

        module subroutine list_to_class_array1d_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(point_orbit), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_point_orbit

        module subroutine list_to_class_array2d_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(point_orbit), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_point_orbit

        module subroutine list_to_class_array1d_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(orbit_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_orbit_type

        module subroutine list_to_class_array2d_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(orbit_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_orbit_type

        module subroutine list_to_class_array1d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(point_orbit_kv), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_point_orbit_kv

        module subroutine list_to_class_array2d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(point_orbit_kv), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_point_orbit_kv

        module subroutine list_to_class_array1d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(mod_orbit), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array1d_mod_orbit

        module subroutine list_to_class_array2d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(mod_orbit), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_class_array2d_mod_orbit

        module subroutine list_to_type_array1d_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_type

        module subroutine list_to_type_array2d_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_type

        module subroutine list_to_type_array1d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_std_type

        module subroutine list_to_type_array2d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_std_type

        module subroutine list_to_type_array1d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_modatm_std_type

        module subroutine list_to_type_array2d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_modatm_std_type

        module subroutine list_to_type_array1d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_ref_type

        module subroutine list_to_type_array2d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_ref_type

        module subroutine list_to_type_array1d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_modatm_ref_type

        module subroutine list_to_type_array2d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_modatm_ref_type

        module subroutine list_to_type_array1d_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_cell_type

        module subroutine list_to_type_array2d_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_cell_type

        module subroutine list_to_type_array1d_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atom_equiv_type

        module subroutine list_to_type_array2d_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atom_equiv_type

        module subroutine list_to_type_array1d_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atom_equiv_list_type

        module subroutine list_to_type_array2d_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atom_equiv_list_type

        module subroutine list_to_type_array1d_atlist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atlist_type

        module subroutine list_to_type_array2d_atlist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atlist_type

        module subroutine list_to_type_array1d_matom_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_matom_type

        module subroutine list_to_type_array2d_matom_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_matom_type

        module subroutine list_to_type_array1d_matom_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_matom_list_type

        module subroutine list_to_type_array2d_matom_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_matom_list_type

        module subroutine list_to_type_array1d_file_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_file_list_type

        module subroutine list_to_type_array2d_file_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_file_list_type

        module subroutine list_to_type_array1d_string_array_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(string_array_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_string_array_type

        module subroutine list_to_type_array2d_string_array_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(string_array_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_string_array_type

        module subroutine list_to_type_array1d_file_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_file_type

        module subroutine list_to_type_array2d_file_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_file_type

        module subroutine list_to_type_array1d_pkb_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_pkb_type

        module subroutine list_to_type_array2d_pkb_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_pkb_type

        module subroutine list_to_type_array1d_peak_search_cond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_search_cond_type

        module subroutine list_to_type_array2d_peak_search_cond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_search_cond_type

        module subroutine list_to_type_array1d_anomalous_sc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_anomalous_sc_type

        module subroutine list_to_type_array2d_anomalous_sc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_anomalous_sc_type

        module subroutine list_to_type_array1d_chem_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_chem_info_type

        module subroutine list_to_type_array2d_chem_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_chem_info_type

        module subroutine list_to_type_array1d_magnetic_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magnetic_form_type

        module subroutine list_to_type_array2d_magnetic_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magnetic_form_type

        module subroutine list_to_type_array1d_xray_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_xray_form_type

        module subroutine list_to_type_array2d_xray_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_xray_form_type

        module subroutine list_to_type_array1d_xray_wavelength_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_xray_wavelength_type

        module subroutine list_to_type_array2d_xray_wavelength_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_xray_wavelength_type

        module subroutine list_to_type_array1d_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_scattering_species_type

        module subroutine list_to_type_array2d_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_scattering_species_type

        module subroutine list_to_type_array1d_strf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strf_type

        module subroutine list_to_type_array2d_strf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strf_type

        module subroutine list_to_type_array1d_strflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strflist_type

        module subroutine list_to_type_array2d_strflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strflist_type

        module subroutine list_to_type_array1d_coordination_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_type

        module subroutine list_to_type_array2d_coordination_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_type

        module subroutine list_to_type_array1d_point_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_list_type

        module subroutine list_to_type_array2d_point_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_list_type

        module subroutine list_to_type_array1d_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_type

        module subroutine list_to_type_array2d_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_type

        module subroutine list_to_type_array1d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_g_type

        module subroutine list_to_type_array2d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_g_type

        module subroutine list_to_type_array1d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_ls_type

        module subroutine list_to_type_array2d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_ls_type

        module subroutine list_to_type_array1d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_gls_type

        module subroutine list_to_type_array2d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_gls_type

        module subroutine list_to_type_array1d_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_twofold_axes_type

        module subroutine list_to_type_array2d_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_twofold_axes_type

        module subroutine list_to_type_array1d_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_zone_axis_type

        module subroutine list_to_type_array2d_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_zone_axis_type

        module subroutine list_to_type_array1d_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strain_tensor_type

        module subroutine list_to_type_array2d_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strain_tensor_type

        module subroutine list_to_type_array1d_esmeralda_laue_image_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(esmeralda_laue_image_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_esmeralda_laue_image_type

        module subroutine list_to_type_array2d_esmeralda_laue_image_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(esmeralda_laue_image_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_esmeralda_laue_image_type

        module subroutine list_to_type_array1d_coordination_asu_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_asu_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_asu_type

        module subroutine list_to_type_array2d_coordination_asu_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_asu_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_asu_type

        module subroutine list_to_type_array1d_coordination_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_crystal_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_crystal_type

        module subroutine list_to_type_array2d_coordination_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_crystal_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_crystal_type

        module subroutine list_to_type_array1d_coordination_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_orbit_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_orbit_type

        module subroutine list_to_type_array2d_coordination_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_orbit_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_orbit_type

        module subroutine list_to_type_array1d_crystal_bond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_bond_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_crystal_bond_type

        module subroutine list_to_type_array2d_crystal_bond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_bond_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_crystal_bond_type

        module subroutine list_to_type_array1d_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_crystal_type

        module subroutine list_to_type_array2d_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_crystal_type

        module subroutine list_to_type_array1d_envelope_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_envelope_type

        module subroutine list_to_type_array2d_envelope_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_envelope_type

        module subroutine list_to_type_array1d_envelope_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_envelope_list_type

        module subroutine list_to_type_array2d_envelope_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_envelope_list_type

        module subroutine list_to_type_array1d_graphical_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(graphical_crystal_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_graphical_crystal_type

        module subroutine list_to_type_array2d_graphical_crystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(graphical_crystal_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_graphical_crystal_type

        module subroutine list_to_type_array1d_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_shub_spgr_info_type

        module subroutine list_to_type_array2d_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_shub_spgr_info_type

        module subroutine list_to_type_array1d_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spgr_info_type

        module subroutine list_to_type_array2d_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spgr_info_type

        module subroutine list_to_type_array1d_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_table_equiv_type

        module subroutine list_to_type_array2d_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_table_equiv_type

        module subroutine list_to_type_array1d_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_wyck_info_type

        module subroutine list_to_type_array2d_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_wyck_info_type

        module subroutine list_to_type_array1d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_type

        module subroutine list_to_type_array2d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_type

        module subroutine list_to_type_array1d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_e_type

        module subroutine list_to_type_array2d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_e_type

        module subroutine list_to_type_array1d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_g_type

        module subroutine list_to_type_array2d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_g_type

        module subroutine list_to_type_array1d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpatt_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpatt_conditions_type

        module subroutine list_to_type_array2d_diffpatt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpatt_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpatt_conditions_type

        module subroutine list_to_type_array1d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_cw_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powpatt_cw_conditions_type

        module subroutine list_to_type_array2d_powpatt_cw_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_cw_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powpatt_cw_conditions_type

        module subroutine list_to_type_array1d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_tof_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powpatt_tof_conditions_type

        module subroutine list_to_type_array2d_powpatt_tof_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_tof_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powpatt_tof_conditions_type

        module subroutine list_to_type_array1d_bck_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bck_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bck_type

        module subroutine list_to_type_array2d_bck_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bck_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bck_type

        module subroutine list_to_type_array1d_interval_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_interval_type

        module subroutine list_to_type_array2d_interval_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_interval_type

        module subroutine list_to_type_array1d_excl_reg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excl_reg_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_excl_reg_type

        module subroutine list_to_type_array2d_excl_reg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excl_reg_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_excl_reg_type

        module subroutine list_to_type_array1d_pattern_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pattern_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_pattern_type

        module subroutine list_to_type_array2d_pattern_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pattern_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_pattern_type

        module subroutine list_to_type_array1d_job_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_job_info_type

        module subroutine list_to_type_array2d_job_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_job_info_type

        module subroutine list_to_type_array1d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_blockinfo_type

        module subroutine list_to_type_array2d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_blockinfo_type

        module subroutine list_to_type_array1d_genvec_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_genvec_type

        module subroutine list_to_type_array2d_genvec_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_genvec_type

        module subroutine list_to_type_array1d_sxtal_attributes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_attributes_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_attributes_type

        module subroutine list_to_type_array2d_sxtal_attributes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_attributes_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_attributes_type

        module subroutine list_to_type_array1d_powder_attributes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_attributes_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powder_attributes_type

        module subroutine list_to_type_array2d_powder_attributes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_attributes_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powder_attributes_type

        module subroutine list_to_type_array1d_phase_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(phase_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_phase_type

        module subroutine list_to_type_array2d_phase_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(phase_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_phase_type

        module subroutine list_to_type_array1d_molecule_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_molecule_type

        module subroutine list_to_type_array2d_molecule_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_molecule_type

        module subroutine list_to_type_array1d_molcrystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_molcrystal_type

        module subroutine list_to_type_array2d_molcrystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_molcrystal_type

        module subroutine list_to_type_array1d_group_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_group_k_type

        module subroutine list_to_type_array2d_group_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_group_k_type

        module subroutine list_to_type_array1d_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numc_type

        module subroutine list_to_type_array2d_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numc_type

        module subroutine list_to_type_array1d_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numi_type

        module subroutine list_to_type_array2d_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numi_type

        module subroutine list_to_type_array1d_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numr_type

        module subroutine list_to_type_array2d_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numr_type

        module subroutine list_to_type_array1d_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_calibration_detector_type

        module subroutine list_to_type_array2d_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_calibration_detector_type

        module subroutine list_to_type_array1d_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffractometer_type

        module subroutine list_to_type_array2d_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffractometer_type

        module subroutine list_to_type_array1d_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_generic_numor_type

        module subroutine list_to_type_array2d_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_generic_numor_type

        module subroutine list_to_type_array1d_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_ill_data_record_type

        module subroutine list_to_type_array2d_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_ill_data_record_type

        module subroutine list_to_type_array1d_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powder_numor_type

        module subroutine list_to_type_array2d_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powder_numor_type

        module subroutine list_to_type_array1d_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_numor_type

        module subroutine list_to_type_array2d_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_numor_type

        module subroutine list_to_type_array1d_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_orient_type

        module subroutine list_to_type_array2d_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_orient_type

        module subroutine list_to_type_array1d_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_refl_type

        module subroutine list_to_type_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_refl_type

        module subroutine list_to_type_array1d_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_srefl_type

        module subroutine list_to_type_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_srefl_type

        module subroutine list_to_type_array1d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_mrefl_type

        module subroutine list_to_type_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_mrefl_type

        module subroutine list_to_type_array1d_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_reflist_type

        module subroutine list_to_type_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_reflist_type

        module subroutine list_to_type_array1d_refp_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refp_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_refp_type

        module subroutine list_to_type_array2d_refp_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refp_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_refp_type

        module subroutine list_to_type_array1d_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atomic_properties_type

        module subroutine list_to_type_array2d_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atomic_properties_type

        module subroutine list_to_type_array1d_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bvel_par_type

        module subroutine list_to_type_array2d_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bvel_par_type

        module subroutine list_to_type_array1d_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bvs_par_type

        module subroutine list_to_type_array2d_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bvs_par_type

        module subroutine list_to_type_array1d_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sbvs_par_type

        module subroutine list_to_type_array2d_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sbvs_par_type

        module subroutine list_to_type_array1d_irf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_type

        module subroutine list_to_type_array2d_irf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_type

        module subroutine list_to_type_array1d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_cw_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_cw_type

        module subroutine list_to_type_array2d_irf_cw_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_cw_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_cw_type

        module subroutine list_to_type_array1d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_tof_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_tof_type

        module subroutine list_to_type_array2d_irf_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_tof_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_tof_type

        module subroutine list_to_type_array1d_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atoms_conf_list_type

        module subroutine list_to_type_array2d_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atoms_conf_list_type

        module subroutine list_to_type_array1d_psd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_psd_val_type

        module subroutine list_to_type_array2d_psd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_psd_val_type

        module subroutine list_to_type_array1d_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxd_val_type

        module subroutine list_to_type_array2d_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxd_val_type

        module subroutine list_to_type_array1d_twin_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_twin_type

        module subroutine list_to_type_array2d_twin_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_twin_type

        module subroutine list_to_type_array1d_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_multistate_vector_type

        module subroutine list_to_type_array2d_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_multistate_vector_type

        module subroutine list_to_type_array1d_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_simann_conditions_type

        module subroutine list_to_type_array2d_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_simann_conditions_type

        module subroutine list_to_type_array1d_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_state_vector_type

        module subroutine list_to_type_array2d_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_state_vector_type

        module subroutine list_to_type_array1d_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_deriv_tof_type

        module subroutine list_to_type_array2d_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_deriv_tof_type

        module subroutine list_to_type_array1d_excluded_regions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excluded_regions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_excluded_regions_type

        module subroutine list_to_type_array2d_excluded_regions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excluded_regions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_excluded_regions_type

        module subroutine list_to_type_array1d_header_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(header_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_header_type

        module subroutine list_to_type_array2d_header_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(header_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_header_type

        module subroutine list_to_type_array1d_image_conditions(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(image_conditions), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_image_conditions

        module subroutine list_to_type_array2d_image_conditions(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(image_conditions), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_image_conditions

        module subroutine list_to_type_array1d_laue_instrument_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_instrument_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_instrument_type

        module subroutine list_to_type_array2d_laue_instrument_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_instrument_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_instrument_type

        module subroutine list_to_type_array1d_laue_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_ref_type

        module subroutine list_to_type_array2d_laue_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_ref_type

        module subroutine list_to_type_array1d_laue_ref_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_ref_list_type

        module subroutine list_to_type_array2d_laue_ref_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_ref_list_type

        module subroutine list_to_type_array1d_peakfind_parameters_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peakfind_parameters_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peakfind_parameters_type

        module subroutine list_to_type_array2d_peakfind_parameters_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peakfind_parameters_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peakfind_parameters_type

        module subroutine list_to_type_array1d_peak_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_type

        module subroutine list_to_type_array2d_peak_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_type

        module subroutine list_to_type_array1d_peak_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_list_type

        module subroutine list_to_type_array2d_peak_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_list_type

        module subroutine list_to_type_array1d_rational(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_rational

        module subroutine list_to_type_array2d_rational(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_rational

        module subroutine list_to_type_array1d_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_symm_oper_type

        module subroutine list_to_type_array2d_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_symm_oper_type

        module subroutine list_to_type_array1d_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_group_type

        module subroutine list_to_type_array2d_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_group_type

        module subroutine list_to_type_array1d_rot_mat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rot_mat_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_rot_mat_type

        module subroutine list_to_type_array2d_rot_mat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rot_mat_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_rot_mat_type

        module subroutine list_to_type_array1d_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spg_type

        module subroutine list_to_type_array2d_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spg_type

        module subroutine list_to_type_array1d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_superspacegroup_type

        module subroutine list_to_type_array2d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_superspacegroup_type

        module subroutine list_to_type_array1d_spin_operator_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_operator_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spin_operator_type

        module subroutine list_to_type_array2d_spin_operator_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_operator_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spin_operator_type

        module subroutine list_to_type_array1d_spin_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_group_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spin_group_type

        module subroutine list_to_type_array2d_spin_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_group_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spin_group_type

        module subroutine list_to_type_array1d_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_kvect_info_type

        module subroutine list_to_type_array2d_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_kvect_info_type

        module subroutine list_to_type_array1d_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_orbit

        module subroutine list_to_type_array2d_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_orbit

        module subroutine list_to_type_array1d_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_orbit_type

        module subroutine list_to_type_array2d_orbit_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_orbit_type

        module subroutine list_to_type_array1d_orbit_list(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_list), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_orbit_list

        module subroutine list_to_type_array2d_orbit_list(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_list), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_orbit_list

        module subroutine list_to_type_array1d_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sym_oper_type

        module subroutine list_to_type_array2d_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sym_oper_type

        module subroutine list_to_type_array1d_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_msym_oper_type

        module subroutine list_to_type_array2d_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_msym_oper_type

        module subroutine list_to_type_array1d_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magnetic_domain_type

        module subroutine list_to_type_array2d_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magnetic_domain_type

        module subroutine list_to_type_array1d_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magsymm_k_type

        module subroutine list_to_type_array2d_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magsymm_k_type

        module subroutine list_to_type_array1d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit_kv), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_orbit_kv

        module subroutine list_to_type_array2d_point_orbit_kv(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit_kv), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_orbit_kv

        module subroutine list_to_type_array1d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mod_orbit), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_mod_orbit

        module subroutine list_to_type_array2d_mod_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mod_orbit), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_mod_orbit

        module subroutine list_to_type_array1d_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_type_no_alloc

        module subroutine list_to_type_array2d_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_type_no_alloc

        module subroutine list_to_type_array1d_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_std_type_no_alloc

        module subroutine list_to_type_array2d_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_std_type_no_alloc

        module subroutine list_to_type_array1d_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_modatm_std_type_no_alloc

        module subroutine list_to_type_array2d_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_modatm_std_type_no_alloc

        module subroutine list_to_type_array1d_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_ref_type_no_alloc

        module subroutine list_to_type_array2d_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_ref_type_no_alloc

        module subroutine list_to_type_array1d_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_modatm_ref_type_no_alloc

        module subroutine list_to_type_array2d_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_modatm_ref_type_no_alloc

        module subroutine list_to_type_array1d_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atm_cell_type_no_alloc

        module subroutine list_to_type_array2d_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atm_cell_type_no_alloc

        module subroutine list_to_type_array1d_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atom_equiv_type_no_alloc

        module subroutine list_to_type_array2d_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atom_equiv_type_no_alloc

        module subroutine list_to_type_array1d_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atom_equiv_list_type_no_alloc

        module subroutine list_to_type_array2d_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atom_equiv_list_type_no_alloc

        module subroutine list_to_type_array1d_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atlist_type_no_alloc

        module subroutine list_to_type_array2d_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atlist_type_no_alloc

        module subroutine list_to_type_array1d_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_matom_type_no_alloc

        module subroutine list_to_type_array2d_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_matom_type_no_alloc

        module subroutine list_to_type_array1d_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_matom_list_type_no_alloc

        module subroutine list_to_type_array2d_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_matom_list_type_no_alloc

        module subroutine list_to_type_array1d_file_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_file_list_type_no_alloc

        module subroutine list_to_type_array2d_file_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_file_list_type_no_alloc

        module subroutine list_to_type_array1d_string_array_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(string_array_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_string_array_type_no_alloc

        module subroutine list_to_type_array2d_string_array_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(string_array_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_string_array_type_no_alloc

        module subroutine list_to_type_array1d_file_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_file_type_no_alloc

        module subroutine list_to_type_array2d_file_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(file_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_file_type_no_alloc

        module subroutine list_to_type_array1d_pkb_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_pkb_type_no_alloc

        module subroutine list_to_type_array2d_pkb_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_pkb_type_no_alloc

        module subroutine list_to_type_array1d_peak_search_cond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_search_cond_type_no_alloc

        module subroutine list_to_type_array2d_peak_search_cond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_search_cond_type_no_alloc

        module subroutine list_to_type_array1d_anomalous_sc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_anomalous_sc_type_no_alloc

        module subroutine list_to_type_array2d_anomalous_sc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_anomalous_sc_type_no_alloc

        module subroutine list_to_type_array1d_chem_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_chem_info_type_no_alloc

        module subroutine list_to_type_array2d_chem_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_chem_info_type_no_alloc

        module subroutine list_to_type_array1d_magnetic_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magnetic_form_type_no_alloc

        module subroutine list_to_type_array2d_magnetic_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magnetic_form_type_no_alloc

        module subroutine list_to_type_array1d_xray_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_xray_form_type_no_alloc

        module subroutine list_to_type_array2d_xray_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_xray_form_type_no_alloc

        module subroutine list_to_type_array1d_xray_wavelength_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_xray_wavelength_type_no_alloc

        module subroutine list_to_type_array2d_xray_wavelength_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_xray_wavelength_type_no_alloc

        module subroutine list_to_type_array1d_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_scattering_species_type_no_alloc

        module subroutine list_to_type_array2d_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_scattering_species_type_no_alloc

        module subroutine list_to_type_array1d_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strf_type_no_alloc

        module subroutine list_to_type_array2d_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strf_type_no_alloc

        module subroutine list_to_type_array1d_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strflist_type_no_alloc

        module subroutine list_to_type_array2d_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strflist_type_no_alloc

        module subroutine list_to_type_array1d_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_type_no_alloc

        module subroutine list_to_type_array2d_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_type_no_alloc

        module subroutine list_to_type_array1d_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_list_type_no_alloc

        module subroutine list_to_type_array2d_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_list_type_no_alloc

        module subroutine list_to_type_array1d_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_type_no_alloc

        module subroutine list_to_type_array2d_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_type_no_alloc

        module subroutine list_to_type_array1d_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_g_type_no_alloc

        module subroutine list_to_type_array2d_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_g_type_no_alloc

        module subroutine list_to_type_array1d_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_ls_type_no_alloc

        module subroutine list_to_type_array2d_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_ls_type_no_alloc

        module subroutine list_to_type_array1d_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_cell_gls_type_no_alloc

        module subroutine list_to_type_array2d_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_cell_gls_type_no_alloc

        module subroutine list_to_type_array1d_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_twofold_axes_type_no_alloc

        module subroutine list_to_type_array2d_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_twofold_axes_type_no_alloc

        module subroutine list_to_type_array1d_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_zone_axis_type_no_alloc

        module subroutine list_to_type_array2d_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_zone_axis_type_no_alloc

        module subroutine list_to_type_array1d_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_strain_tensor_type_no_alloc

        module subroutine list_to_type_array2d_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_strain_tensor_type_no_alloc

        module subroutine list_to_type_array1d_esmeralda_laue_image_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(esmeralda_laue_image_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_esmeralda_laue_image_type_no_alloc

        module subroutine list_to_type_array2d_esmeralda_laue_image_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(esmeralda_laue_image_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_esmeralda_laue_image_type_no_alloc

        module subroutine list_to_type_array1d_coordination_asu_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_asu_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_asu_type_no_alloc

        module subroutine list_to_type_array2d_coordination_asu_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_asu_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_asu_type_no_alloc

        module subroutine list_to_type_array1d_coordination_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_crystal_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_crystal_type_no_alloc

        module subroutine list_to_type_array2d_coordination_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_crystal_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_crystal_type_no_alloc

        module subroutine list_to_type_array1d_coordination_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_orbit_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_coordination_orbit_type_no_alloc

        module subroutine list_to_type_array2d_coordination_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_orbit_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_coordination_orbit_type_no_alloc

        module subroutine list_to_type_array1d_crystal_bond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_bond_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_crystal_bond_type_no_alloc

        module subroutine list_to_type_array2d_crystal_bond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_bond_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_crystal_bond_type_no_alloc

        module subroutine list_to_type_array1d_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_crystal_type_no_alloc

        module subroutine list_to_type_array2d_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(crystal_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_crystal_type_no_alloc

        module subroutine list_to_type_array1d_envelope_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_envelope_type_no_alloc

        module subroutine list_to_type_array2d_envelope_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_envelope_type_no_alloc

        module subroutine list_to_type_array1d_envelope_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_envelope_list_type_no_alloc

        module subroutine list_to_type_array2d_envelope_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(envelope_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_envelope_list_type_no_alloc

        module subroutine list_to_type_array1d_graphical_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(graphical_crystal_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_graphical_crystal_type_no_alloc

        module subroutine list_to_type_array2d_graphical_crystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(graphical_crystal_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_graphical_crystal_type_no_alloc

        module subroutine list_to_type_array1d_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_shub_spgr_info_type_no_alloc

        module subroutine list_to_type_array2d_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_shub_spgr_info_type_no_alloc

        module subroutine list_to_type_array1d_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spgr_info_type_no_alloc

        module subroutine list_to_type_array2d_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spgr_info_type_no_alloc

        module subroutine list_to_type_array1d_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_table_equiv_type_no_alloc

        module subroutine list_to_type_array2d_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_table_equiv_type_no_alloc

        module subroutine list_to_type_array1d_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_wyck_info_type_no_alloc

        module subroutine list_to_type_array2d_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_wyck_info_type_no_alloc

        module subroutine list_to_type_array1d_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_type_no_alloc

        module subroutine list_to_type_array2d_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_type_no_alloc

        module subroutine list_to_type_array1d_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_e_type_no_alloc

        module subroutine list_to_type_array2d_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_e_type_no_alloc

        module subroutine list_to_type_array1d_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpat_g_type_no_alloc

        module subroutine list_to_type_array2d_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpat_g_type_no_alloc

        module subroutine list_to_type_array1d_diffpatt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpatt_conditions_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffpatt_conditions_type_no_alloc

        module subroutine list_to_type_array2d_diffpatt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpatt_conditions_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffpatt_conditions_type_no_alloc

        module subroutine list_to_type_array1d_powpatt_cw_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_cw_conditions_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powpatt_cw_conditions_type_no_alloc

        module subroutine list_to_type_array2d_powpatt_cw_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_cw_conditions_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powpatt_cw_conditions_type_no_alloc

        module subroutine list_to_type_array1d_powpatt_tof_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_tof_conditions_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powpatt_tof_conditions_type_no_alloc

        module subroutine list_to_type_array2d_powpatt_tof_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powpatt_tof_conditions_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powpatt_tof_conditions_type_no_alloc

        module subroutine list_to_type_array1d_bck_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bck_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bck_type_no_alloc

        module subroutine list_to_type_array2d_bck_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bck_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bck_type_no_alloc

        module subroutine list_to_type_array1d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_interval_type_no_alloc

        module subroutine list_to_type_array2d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_interval_type_no_alloc

        module subroutine list_to_type_array1d_excl_reg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excl_reg_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_excl_reg_type_no_alloc

        module subroutine list_to_type_array2d_excl_reg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excl_reg_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_excl_reg_type_no_alloc

        module subroutine list_to_type_array1d_pattern_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pattern_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_pattern_type_no_alloc

        module subroutine list_to_type_array2d_pattern_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pattern_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_pattern_type_no_alloc

        module subroutine list_to_type_array1d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_job_info_type_no_alloc

        module subroutine list_to_type_array2d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_job_info_type_no_alloc

        module subroutine list_to_type_array1d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_blockinfo_type_no_alloc

        module subroutine list_to_type_array2d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_blockinfo_type_no_alloc

        module subroutine list_to_type_array1d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_genvec_type_no_alloc

        module subroutine list_to_type_array2d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_genvec_type_no_alloc

        module subroutine list_to_type_array1d_sxtal_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_attributes_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_attributes_type_no_alloc

        module subroutine list_to_type_array2d_sxtal_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_attributes_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_attributes_type_no_alloc

        module subroutine list_to_type_array1d_powder_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_attributes_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powder_attributes_type_no_alloc

        module subroutine list_to_type_array2d_powder_attributes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_attributes_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powder_attributes_type_no_alloc

        module subroutine list_to_type_array1d_phase_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(phase_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_phase_type_no_alloc

        module subroutine list_to_type_array2d_phase_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(phase_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_phase_type_no_alloc

        module subroutine list_to_type_array1d_molecule_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_molecule_type_no_alloc

        module subroutine list_to_type_array2d_molecule_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_molecule_type_no_alloc

        module subroutine list_to_type_array1d_molcrystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_molcrystal_type_no_alloc

        module subroutine list_to_type_array2d_molcrystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_molcrystal_type_no_alloc

        module subroutine list_to_type_array1d_group_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_group_k_type_no_alloc

        module subroutine list_to_type_array2d_group_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_group_k_type_no_alloc

        module subroutine list_to_type_array1d_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numc_type_no_alloc

        module subroutine list_to_type_array2d_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numc_type_no_alloc

        module subroutine list_to_type_array1d_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numi_type_no_alloc

        module subroutine list_to_type_array2d_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numi_type_no_alloc

        module subroutine list_to_type_array1d_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_basic_numr_type_no_alloc

        module subroutine list_to_type_array2d_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_basic_numr_type_no_alloc

        module subroutine list_to_type_array1d_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_calibration_detector_type_no_alloc

        module subroutine list_to_type_array2d_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_calibration_detector_type_no_alloc

        module subroutine list_to_type_array1d_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_diffractometer_type_no_alloc

        module subroutine list_to_type_array2d_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_diffractometer_type_no_alloc

        module subroutine list_to_type_array1d_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_generic_numor_type_no_alloc

        module subroutine list_to_type_array2d_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_generic_numor_type_no_alloc

        module subroutine list_to_type_array1d_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_ill_data_record_type_no_alloc

        module subroutine list_to_type_array2d_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_ill_data_record_type_no_alloc

        module subroutine list_to_type_array1d_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_powder_numor_type_no_alloc

        module subroutine list_to_type_array2d_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_powder_numor_type_no_alloc

        module subroutine list_to_type_array1d_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_numor_type_no_alloc

        module subroutine list_to_type_array2d_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_numor_type_no_alloc

        module subroutine list_to_type_array1d_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxtal_orient_type_no_alloc

        module subroutine list_to_type_array2d_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxtal_orient_type_no_alloc

        module subroutine list_to_type_array1d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_refl_type_no_alloc

        module subroutine list_to_type_array2d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_refl_type_no_alloc

        module subroutine list_to_type_array1d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_srefl_type_no_alloc

        module subroutine list_to_type_array2d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_srefl_type_no_alloc

        module subroutine list_to_type_array1d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_mrefl_type_no_alloc

        module subroutine list_to_type_array2d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_mrefl_type_no_alloc

        module subroutine list_to_type_array1d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_reflist_type_no_alloc

        module subroutine list_to_type_array2d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_reflist_type_no_alloc

        module subroutine list_to_type_array1d_refp_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refp_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_refp_type_no_alloc

        module subroutine list_to_type_array2d_refp_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refp_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_refp_type_no_alloc

        module subroutine list_to_type_array1d_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atomic_properties_type_no_alloc

        module subroutine list_to_type_array2d_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atomic_properties_type_no_alloc

        module subroutine list_to_type_array1d_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bvel_par_type_no_alloc

        module subroutine list_to_type_array2d_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bvel_par_type_no_alloc

        module subroutine list_to_type_array1d_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_bvs_par_type_no_alloc

        module subroutine list_to_type_array2d_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_bvs_par_type_no_alloc

        module subroutine list_to_type_array1d_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sbvs_par_type_no_alloc

        module subroutine list_to_type_array2d_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sbvs_par_type_no_alloc

        module subroutine list_to_type_array1d_irf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_type_no_alloc

        module subroutine list_to_type_array2d_irf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_type_no_alloc

        module subroutine list_to_type_array1d_irf_cw_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_cw_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_cw_type_no_alloc

        module subroutine list_to_type_array2d_irf_cw_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_cw_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_cw_type_no_alloc

        module subroutine list_to_type_array1d_irf_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_tof_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_irf_tof_type_no_alloc

        module subroutine list_to_type_array2d_irf_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(irf_tof_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_irf_tof_type_no_alloc

        module subroutine list_to_type_array1d_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_atoms_conf_list_type_no_alloc

        module subroutine list_to_type_array2d_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_atoms_conf_list_type_no_alloc

        module subroutine list_to_type_array1d_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_psd_val_type_no_alloc

        module subroutine list_to_type_array2d_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_psd_val_type_no_alloc

        module subroutine list_to_type_array1d_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sxd_val_type_no_alloc

        module subroutine list_to_type_array2d_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sxd_val_type_no_alloc

        module subroutine list_to_type_array1d_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_twin_type_no_alloc

        module subroutine list_to_type_array2d_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_twin_type_no_alloc

        module subroutine list_to_type_array1d_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_multistate_vector_type_no_alloc

        module subroutine list_to_type_array2d_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_multistate_vector_type_no_alloc

        module subroutine list_to_type_array1d_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_simann_conditions_type_no_alloc

        module subroutine list_to_type_array2d_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_simann_conditions_type_no_alloc

        module subroutine list_to_type_array1d_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_state_vector_type_no_alloc

        module subroutine list_to_type_array2d_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_state_vector_type_no_alloc

        module subroutine list_to_type_array1d_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_deriv_tof_type_no_alloc

        module subroutine list_to_type_array2d_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_deriv_tof_type_no_alloc

        module subroutine list_to_type_array1d_excluded_regions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excluded_regions_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_excluded_regions_type_no_alloc

        module subroutine list_to_type_array2d_excluded_regions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(excluded_regions_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_excluded_regions_type_no_alloc

        module subroutine list_to_type_array1d_header_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(header_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_header_type_no_alloc

        module subroutine list_to_type_array2d_header_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(header_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_header_type_no_alloc

        module subroutine list_to_type_array1d_image_conditions_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(image_conditions), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_image_conditions_no_alloc

        module subroutine list_to_type_array2d_image_conditions_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(image_conditions), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_image_conditions_no_alloc

        module subroutine list_to_type_array1d_laue_instrument_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_instrument_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_instrument_type_no_alloc

        module subroutine list_to_type_array2d_laue_instrument_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_instrument_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_instrument_type_no_alloc

        module subroutine list_to_type_array1d_laue_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_ref_type_no_alloc

        module subroutine list_to_type_array2d_laue_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_ref_type_no_alloc

        module subroutine list_to_type_array1d_laue_ref_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_laue_ref_list_type_no_alloc

        module subroutine list_to_type_array2d_laue_ref_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(laue_ref_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_laue_ref_list_type_no_alloc

        module subroutine list_to_type_array1d_peakfind_parameters_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peakfind_parameters_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peakfind_parameters_type_no_alloc

        module subroutine list_to_type_array2d_peakfind_parameters_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peakfind_parameters_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peakfind_parameters_type_no_alloc

        module subroutine list_to_type_array1d_peak_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_type_no_alloc

        module subroutine list_to_type_array2d_peak_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_type_no_alloc

        module subroutine list_to_type_array1d_peak_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_list_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_peak_list_type_no_alloc

        module subroutine list_to_type_array2d_peak_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_list_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_peak_list_type_no_alloc

        module subroutine list_to_type_array1d_rational_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_rational_no_alloc

        module subroutine list_to_type_array2d_rational_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_rational_no_alloc

        module subroutine list_to_type_array1d_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_symm_oper_type_no_alloc

        module subroutine list_to_type_array2d_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_symm_oper_type_no_alloc

        module subroutine list_to_type_array1d_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_group_type_no_alloc

        module subroutine list_to_type_array2d_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_group_type_no_alloc

        module subroutine list_to_type_array1d_rot_mat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rot_mat_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_rot_mat_type_no_alloc

        module subroutine list_to_type_array2d_rot_mat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rot_mat_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_rot_mat_type_no_alloc

        module subroutine list_to_type_array1d_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spg_type_no_alloc

        module subroutine list_to_type_array2d_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spg_type_no_alloc

        module subroutine list_to_type_array1d_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_superspacegroup_type_no_alloc

        module subroutine list_to_type_array2d_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_superspacegroup_type_no_alloc

        module subroutine list_to_type_array1d_spin_operator_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_operator_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spin_operator_type_no_alloc

        module subroutine list_to_type_array2d_spin_operator_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_operator_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spin_operator_type_no_alloc

        module subroutine list_to_type_array1d_spin_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_group_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_spin_group_type_no_alloc

        module subroutine list_to_type_array2d_spin_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spin_group_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_spin_group_type_no_alloc

        module subroutine list_to_type_array1d_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_kvect_info_type_no_alloc

        module subroutine list_to_type_array2d_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_kvect_info_type_no_alloc

        module subroutine list_to_type_array1d_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_orbit_no_alloc

        module subroutine list_to_type_array2d_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_orbit_no_alloc

        module subroutine list_to_type_array1d_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_orbit_type_no_alloc

        module subroutine list_to_type_array2d_orbit_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_orbit_type_no_alloc

        module subroutine list_to_type_array1d_orbit_list_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_list), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_orbit_list_no_alloc

        module subroutine list_to_type_array2d_orbit_list_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(orbit_list), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_orbit_list_no_alloc

        module subroutine list_to_type_array1d_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_sym_oper_type_no_alloc

        module subroutine list_to_type_array2d_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_sym_oper_type_no_alloc

        module subroutine list_to_type_array1d_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_msym_oper_type_no_alloc

        module subroutine list_to_type_array2d_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_msym_oper_type_no_alloc

        module subroutine list_to_type_array1d_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magnetic_domain_type_no_alloc

        module subroutine list_to_type_array2d_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magnetic_domain_type_no_alloc

        module subroutine list_to_type_array1d_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_magsymm_k_type_no_alloc

        module subroutine list_to_type_array2d_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_magsymm_k_type_no_alloc

        module subroutine list_to_type_array1d_point_orbit_kv_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit_kv), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_point_orbit_kv_no_alloc

        module subroutine list_to_type_array2d_point_orbit_kv_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit_kv), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_point_orbit_kv_no_alloc

        module subroutine list_to_type_array1d_mod_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mod_orbit), dimension(*), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array1d_mod_orbit_no_alloc

        module subroutine list_to_type_array2d_mod_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mod_orbit), dimension(:,:), intent(inout) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_type_array2d_mod_orbit_no_alloc

        module subroutine unwrap_class_atm_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_type

        module subroutine unwrap_class_atm_std_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_std_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_std_type

        module subroutine unwrap_class_modatm_std_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(modatm_std_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_modatm_std_type

        module subroutine unwrap_class_atm_ref_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_ref_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_ref_type

        module subroutine unwrap_class_modatm_ref_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(modatm_ref_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_modatm_ref_type

        module subroutine unwrap_type_atm_cell_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atm_cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atm_cell_type

        module subroutine unwrap_type_atom_equiv_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atom_equiv_type

        module subroutine unwrap_type_atom_equiv_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atom_equiv_list_type

        module subroutine unwrap_type_atlist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atlist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atlist_type

        module subroutine unwrap_type_matom_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_matom_type

        module subroutine unwrap_type_matom_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_matom_list_type

        module subroutine unwrap_type_file_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(file_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_file_list_type

        module subroutine unwrap_type_string_array_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(string_array_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_string_array_type

        module subroutine unwrap_type_file_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(file_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_file_type

        module subroutine unwrap_type_pkb_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pkb_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_pkb_type

        module subroutine unwrap_type_peak_search_cond_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peak_search_cond_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_peak_search_cond_type

        module subroutine unwrap_type_anomalous_sc_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(anomalous_sc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_anomalous_sc_type

        module subroutine unwrap_type_chem_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(chem_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_chem_info_type

        module subroutine unwrap_type_magnetic_form_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_magnetic_form_type

        module subroutine unwrap_type_xray_form_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_xray_form_type

        module subroutine unwrap_type_xray_wavelength_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_wavelength_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_xray_wavelength_type

        module subroutine unwrap_type_scattering_species_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(scattering_species_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_scattering_species_type

        module subroutine unwrap_type_strf_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strf_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_strf_type

        module subroutine unwrap_type_strflist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_strflist_type

        module subroutine unwrap_type_coordination_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_coordination_type

        module subroutine unwrap_type_point_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(point_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_point_list_type

        module subroutine unwrap_class_cell_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_type

        module subroutine unwrap_class_cell_g_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_g_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_g_type

        module subroutine unwrap_class_cell_ls_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_ls_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_ls_type

        module subroutine unwrap_class_cell_gls_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_gls_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_gls_type

        module subroutine unwrap_type_twofold_axes_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twofold_axes_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_twofold_axes_type

        module subroutine unwrap_type_zone_axis_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(zone_axis_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_zone_axis_type

        module subroutine unwrap_type_strain_tensor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strain_tensor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_strain_tensor_type

        module subroutine unwrap_type_esmeralda_laue_image_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(esmeralda_laue_image_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_esmeralda_laue_image_type

        module subroutine unwrap_type_coordination_asu_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_asu_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_coordination_asu_type

        module subroutine unwrap_type_coordination_crystal_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_crystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_coordination_crystal_type

        module subroutine unwrap_type_coordination_orbit_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_orbit_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_coordination_orbit_type

        module subroutine unwrap_type_crystal_bond_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(crystal_bond_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_crystal_bond_type

        module subroutine unwrap_type_crystal_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(crystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_crystal_type

        module subroutine unwrap_type_envelope_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(envelope_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_envelope_type

        module subroutine unwrap_type_envelope_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(envelope_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_envelope_list_type

        module subroutine unwrap_type_graphical_crystal_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(graphical_crystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_graphical_crystal_type

        module subroutine unwrap_type_shub_spgr_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(shub_spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_shub_spgr_info_type

        module subroutine unwrap_type_spgr_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_spgr_info_type

        module subroutine unwrap_type_table_equiv_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(table_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_table_equiv_type

        module subroutine unwrap_type_wyck_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(wyck_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_wyck_info_type

        module subroutine unwrap_class_diffpat_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_type

        module subroutine unwrap_class_diffpat_e_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_e_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_e_type

        module subroutine unwrap_class_diffpat_g_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_g_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_g_type

        module subroutine unwrap_class_diffpatt_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpatt_conditions_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpatt_conditions_type

        module subroutine unwrap_class_powpatt_cw_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(powpatt_cw_conditions_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_powpatt_cw_conditions_type

        module subroutine unwrap_class_powpatt_tof_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(powpatt_tof_conditions_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_powpatt_tof_conditions_type

        module subroutine unwrap_type_bck_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bck_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_bck_type

        module subroutine unwrap_type_interval_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(interval_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_interval_type

        module subroutine unwrap_type_excl_reg_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(excl_reg_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_excl_reg_type

        module subroutine unwrap_type_pattern_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pattern_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_pattern_type

        module subroutine unwrap_type_job_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(job_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_job_info_type

        module subroutine unwrap_type_blockinfo_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(blockinfo_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_blockinfo_type

        module subroutine unwrap_type_genvec_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(genvec_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_genvec_type

        module subroutine unwrap_type_sxtal_attributes_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_attributes_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sxtal_attributes_type

        module subroutine unwrap_type_powder_attributes_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(powder_attributes_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_powder_attributes_type

        module subroutine unwrap_type_phase_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(phase_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_phase_type

        module subroutine unwrap_type_molecule_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molecule_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_molecule_type

        module subroutine unwrap_type_molcrystal_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molcrystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_molcrystal_type

        module subroutine unwrap_type_group_k_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(group_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_group_k_type

        module subroutine unwrap_type_basic_numc_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_basic_numc_type

        module subroutine unwrap_type_basic_numi_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numi_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_basic_numi_type

        module subroutine unwrap_type_basic_numr_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numr_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_basic_numr_type

        module subroutine unwrap_type_calibration_detector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(calibration_detector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_calibration_detector_type

        module subroutine unwrap_type_diffractometer_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(diffractometer_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_diffractometer_type

        module subroutine unwrap_type_generic_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(generic_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_generic_numor_type

        module subroutine unwrap_type_ill_data_record_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(ill_data_record_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_ill_data_record_type

        module subroutine unwrap_type_powder_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(powder_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_powder_numor_type

        module subroutine unwrap_type_sxtal_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sxtal_numor_type

        module subroutine unwrap_type_sxtal_orient_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_orient_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sxtal_orient_type

        module subroutine unwrap_class_refl_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refl_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_refl_type

        module subroutine unwrap_class_srefl_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(srefl_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_srefl_type

        module subroutine unwrap_class_mrefl_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(mrefl_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_mrefl_type

        module subroutine unwrap_class_reflist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(reflist_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_reflist_type

        module subroutine unwrap_class_refp_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refp_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_refp_type

        module subroutine unwrap_type_atomic_properties_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atomic_properties_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atomic_properties_type

        module subroutine unwrap_type_bvel_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvel_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_bvel_par_type

        module subroutine unwrap_type_bvs_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_bvs_par_type

        module subroutine unwrap_type_sbvs_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sbvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sbvs_par_type

        module subroutine unwrap_class_irf_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_type

        module subroutine unwrap_class_irf_cw_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_cw_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_cw_type

        module subroutine unwrap_class_irf_tof_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_tof_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_tof_type

        module subroutine unwrap_type_atoms_conf_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atoms_conf_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_atoms_conf_list_type

        module subroutine unwrap_type_psd_val_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(psd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_psd_val_type

        module subroutine unwrap_type_sxd_val_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sxd_val_type

        module subroutine unwrap_type_twin_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twin_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_twin_type

        module subroutine unwrap_type_multistate_vector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(multistate_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_multistate_vector_type

        module subroutine unwrap_type_simann_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(simann_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_simann_conditions_type

        module subroutine unwrap_type_state_vector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(state_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_state_vector_type

        module subroutine unwrap_type_deriv_tof_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(deriv_tof_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_deriv_tof_type

        module subroutine unwrap_type_excluded_regions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(excluded_regions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_excluded_regions_type

        module subroutine unwrap_type_header_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(header_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_header_type

        module subroutine unwrap_type_image_conditions(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(image_conditions), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_image_conditions

        module subroutine unwrap_type_laue_instrument_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(laue_instrument_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_laue_instrument_type

        module subroutine unwrap_type_laue_ref_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(laue_ref_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_laue_ref_type

        module subroutine unwrap_type_laue_ref_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(laue_ref_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_laue_ref_list_type

        module subroutine unwrap_type_peakfind_parameters_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peakfind_parameters_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_peakfind_parameters_type

        module subroutine unwrap_type_peak_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peak_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_peak_type

        module subroutine unwrap_type_peak_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peak_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_peak_list_type

        module subroutine unwrap_type_rational(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(rational), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_rational

        module subroutine unwrap_type_symm_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(symm_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_symm_oper_type

        module subroutine unwrap_class_group_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(group_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_group_type

        module subroutine unwrap_type_rot_mat_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(rot_mat_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_rot_mat_type

        module subroutine unwrap_class_spg_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(spg_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_spg_type

        module subroutine unwrap_class_superspacegroup_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(superspacegroup_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_superspacegroup_type

        module subroutine unwrap_type_spin_operator_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(spin_operator_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_spin_operator_type

        module subroutine unwrap_type_spin_group_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(spin_group_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_spin_group_type

        module subroutine unwrap_type_kvect_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(kvect_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_kvect_info_type

        module subroutine unwrap_class_point_orbit(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(point_orbit), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_point_orbit

        module subroutine unwrap_class_orbit_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(orbit_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_orbit_type

        module subroutine unwrap_type_orbit_list(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(orbit_list), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_orbit_list

        module subroutine unwrap_type_sym_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_sym_oper_type

        module subroutine unwrap_type_msym_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(msym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_msym_oper_type

        module subroutine unwrap_type_magnetic_domain_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_domain_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_magnetic_domain_type

        module subroutine unwrap_type_magsymm_k_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magsymm_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_type_magsymm_k_type

        module subroutine unwrap_class_point_orbit_kv(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(point_orbit_kv), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_point_orbit_kv

        module subroutine unwrap_class_mod_orbit(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(mod_orbit), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_mod_orbit

        module subroutine unwrap_class_atm_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_type_no_alloc

        module subroutine unwrap_class_atm_std_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_std_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_std_type_no_alloc

        module subroutine unwrap_class_modatm_std_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(modatm_std_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_modatm_std_type_no_alloc

        module subroutine unwrap_class_atm_ref_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_ref_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_atm_ref_type_no_alloc

        module subroutine unwrap_class_modatm_ref_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(modatm_ref_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_modatm_ref_type_no_alloc

        module subroutine unwrap_class_cell_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_type_no_alloc

        module subroutine unwrap_class_cell_g_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_g_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_g_type_no_alloc

        module subroutine unwrap_class_cell_ls_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_ls_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_ls_type_no_alloc

        module subroutine unwrap_class_cell_gls_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_gls_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_cell_gls_type_no_alloc

        module subroutine unwrap_class_diffpat_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_type_no_alloc

        module subroutine unwrap_class_diffpat_e_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_e_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_e_type_no_alloc

        module subroutine unwrap_class_diffpat_g_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_g_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpat_g_type_no_alloc

        module subroutine unwrap_class_diffpatt_conditions_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpatt_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_diffpatt_conditions_type_no_alloc

        module subroutine unwrap_class_powpatt_cw_conditions_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(powpatt_cw_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_powpatt_cw_conditions_type_no_alloc

        module subroutine unwrap_class_powpatt_tof_conditions_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(powpatt_tof_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_powpatt_tof_conditions_type_no_alloc

        module subroutine unwrap_class_refl_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refl_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_refl_type_no_alloc

        module subroutine unwrap_class_srefl_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(srefl_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_srefl_type_no_alloc

        module subroutine unwrap_class_mrefl_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(mrefl_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_mrefl_type_no_alloc

        module subroutine unwrap_class_reflist_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(reflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_reflist_type_no_alloc

        module subroutine unwrap_class_refp_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refp_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_refp_type_no_alloc

        module subroutine unwrap_class_irf_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_type_no_alloc

        module subroutine unwrap_class_irf_cw_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_cw_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_cw_type_no_alloc

        module subroutine unwrap_class_irf_tof_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(irf_tof_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_irf_tof_type_no_alloc

        module subroutine unwrap_class_group_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(group_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_group_type_no_alloc

        module subroutine unwrap_class_spg_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(spg_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_spg_type_no_alloc

        module subroutine unwrap_class_superspacegroup_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(superspacegroup_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_superspacegroup_type_no_alloc

        module subroutine unwrap_class_point_orbit_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(point_orbit), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_point_orbit_no_alloc

        module subroutine unwrap_class_orbit_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(orbit_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_orbit_type_no_alloc

        module subroutine unwrap_class_point_orbit_kv_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(point_orbit_kv), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_point_orbit_kv_no_alloc

        module subroutine unwrap_class_mod_orbit_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(mod_orbit), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_class_mod_orbit_no_alloc

        module subroutine wrap_atm_type(for_var,py_var,ierror)
            class(atm_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_type

        module subroutine wrap_atm_cell_type(for_var,py_var,ierror)
            type(atm_cell_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_cell_type

        module subroutine wrap_atom_equiv_type(for_var,py_var,ierror)
            type(atom_equiv_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_type

        module subroutine wrap_atom_equiv_list_type(for_var,py_var,ierror)
            type(atom_equiv_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_list_type

        module subroutine wrap_atlist_type(for_var,py_var,ierror)
            type(atlist_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atlist_type

        module subroutine wrap_matom_type(for_var,py_var,ierror)
            type(matom_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_type

        module subroutine wrap_matom_list_type(for_var,py_var,ierror)
            type(matom_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_list_type

        module subroutine wrap_file_list_type(for_var,py_var,ierror)
            type(file_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_file_list_type

        module subroutine wrap_string_array_type(for_var,py_var,ierror)
            type(string_array_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_string_array_type

        module subroutine wrap_file_type(for_var,py_var,ierror)
            type(file_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_file_type

        module subroutine wrap_pkb_type(for_var,py_var,ierror)
            type(pkb_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_pkb_type

        module subroutine wrap_peak_search_cond_type(for_var,py_var,ierror)
            type(peak_search_cond_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_peak_search_cond_type

        module subroutine wrap_anomalous_sc_type(for_var,py_var,ierror)
            type(anomalous_sc_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_anomalous_sc_type

        module subroutine wrap_chem_info_type(for_var,py_var,ierror)
            type(chem_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_chem_info_type

        module subroutine wrap_magnetic_form_type(for_var,py_var,ierror)
            type(magnetic_form_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_form_type

        module subroutine wrap_xray_form_type(for_var,py_var,ierror)
            type(xray_form_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_form_type

        module subroutine wrap_xray_wavelength_type(for_var,py_var,ierror)
            type(xray_wavelength_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_wavelength_type

        module subroutine wrap_scattering_species_type(for_var,py_var,ierror)
            type(scattering_species_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_scattering_species_type

        module subroutine wrap_strf_type(for_var,py_var,ierror)
            type(strf_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strf_type

        module subroutine wrap_strflist_type(for_var,py_var,ierror)
            type(strflist_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strflist_type

        module subroutine wrap_coordination_type(for_var,py_var,ierror)
            type(coordination_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_type

        module subroutine wrap_point_list_type(for_var,py_var,ierror)
            type(point_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_list_type

        module subroutine wrap_cell_type(for_var,py_var,ierror)
            class(cell_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_cell_type

        module subroutine wrap_twofold_axes_type(for_var,py_var,ierror)
            type(twofold_axes_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_twofold_axes_type

        module subroutine wrap_zone_axis_type(for_var,py_var,ierror)
            type(zone_axis_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_zone_axis_type

        module subroutine wrap_strain_tensor_type(for_var,py_var,ierror)
            type(strain_tensor_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strain_tensor_type

        module subroutine wrap_esmeralda_laue_image_type(for_var,py_var,ierror)
            type(esmeralda_laue_image_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_esmeralda_laue_image_type

        module subroutine wrap_coordination_asu_type(for_var,py_var,ierror)
            type(coordination_asu_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_asu_type

        module subroutine wrap_coordination_crystal_type(for_var,py_var,ierror)
            type(coordination_crystal_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_crystal_type

        module subroutine wrap_coordination_orbit_type(for_var,py_var,ierror)
            type(coordination_orbit_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_orbit_type

        module subroutine wrap_crystal_bond_type(for_var,py_var,ierror)
            type(crystal_bond_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_crystal_bond_type

        module subroutine wrap_crystal_type(for_var,py_var,ierror)
            type(crystal_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_crystal_type

        module subroutine wrap_envelope_type(for_var,py_var,ierror)
            type(envelope_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_envelope_type

        module subroutine wrap_envelope_list_type(for_var,py_var,ierror)
            type(envelope_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_envelope_list_type

        module subroutine wrap_graphical_crystal_type(for_var,py_var,ierror)
            type(graphical_crystal_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_graphical_crystal_type

        module subroutine wrap_shub_spgr_info_type(for_var,py_var,ierror)
            type(shub_spgr_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_shub_spgr_info_type

        module subroutine wrap_spgr_info_type(for_var,py_var,ierror)
            type(spgr_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_spgr_info_type

        module subroutine wrap_table_equiv_type(for_var,py_var,ierror)
            type(table_equiv_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_table_equiv_type

        module subroutine wrap_wyck_info_type(for_var,py_var,ierror)
            type(wyck_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_wyck_info_type

        module subroutine wrap_diffpat_type(for_var,py_var,ierror)
            class(diffpat_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffpat_type

        module subroutine wrap_diffpatt_conditions_type(for_var,py_var,ierror)
            class(diffpatt_conditions_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffpatt_conditions_type

        module subroutine wrap_bck_type(for_var,py_var,ierror)
            type(bck_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_bck_type

        module subroutine wrap_interval_type(for_var,py_var,ierror)
            type(interval_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_interval_type

        module subroutine wrap_excl_reg_type(for_var,py_var,ierror)
            type(excl_reg_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_excl_reg_type

        module subroutine wrap_pattern_type(for_var,py_var,ierror)
            type(pattern_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_pattern_type

        module subroutine wrap_job_info_type(for_var,py_var,ierror)
            type(job_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_job_info_type

        module subroutine wrap_blockinfo_type(for_var,py_var,ierror)
            type(blockinfo_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_blockinfo_type

        module subroutine wrap_genvec_type(for_var,py_var,ierror)
            type(genvec_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_genvec_type

        module subroutine wrap_sxtal_attributes_type(for_var,py_var,ierror)
            type(sxtal_attributes_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_attributes_type

        module subroutine wrap_powder_attributes_type(for_var,py_var,ierror)
            type(powder_attributes_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_powder_attributes_type

        module subroutine wrap_phase_type(for_var,py_var,ierror)
            type(phase_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_phase_type

        module subroutine wrap_molecule_type(for_var,py_var,ierror)
            type(molecule_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_molecule_type

        module subroutine wrap_molcrystal_type(for_var,py_var,ierror)
            type(molcrystal_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_molcrystal_type

        module subroutine wrap_group_k_type(for_var,py_var,ierror)
            type(group_k_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_k_type

        module subroutine wrap_basic_numc_type(for_var,py_var,ierror)
            type(basic_numc_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numc_type

        module subroutine wrap_basic_numi_type(for_var,py_var,ierror)
            type(basic_numi_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numi_type

        module subroutine wrap_basic_numr_type(for_var,py_var,ierror)
            type(basic_numr_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numr_type

        module subroutine wrap_calibration_detector_type(for_var,py_var,ierror)
            type(calibration_detector_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_calibration_detector_type

        module subroutine wrap_diffractometer_type(for_var,py_var,ierror)
            type(diffractometer_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffractometer_type

        module subroutine wrap_generic_numor_type(for_var,py_var,ierror)
            type(generic_numor_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_generic_numor_type

        module subroutine wrap_ill_data_record_type(for_var,py_var,ierror)
            type(ill_data_record_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_ill_data_record_type

        module subroutine wrap_powder_numor_type(for_var,py_var,ierror)
            type(powder_numor_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_powder_numor_type

        module subroutine wrap_sxtal_numor_type(for_var,py_var,ierror)
            type(sxtal_numor_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_numor_type

        module subroutine wrap_sxtal_orient_type(for_var,py_var,ierror)
            type(sxtal_orient_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_orient_type

        module subroutine wrap_refl_type(for_var,py_var,ierror)
            class(refl_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_refl_type

        module subroutine wrap_reflist_type(for_var,py_var,ierror)
            class(reflist_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_reflist_type

        module subroutine wrap_atomic_properties_type(for_var,py_var,ierror)
            type(atomic_properties_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atomic_properties_type

        module subroutine wrap_bvel_par_type(for_var,py_var,ierror)
            type(bvel_par_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvel_par_type

        module subroutine wrap_bvs_par_type(for_var,py_var,ierror)
            type(bvs_par_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvs_par_type

        module subroutine wrap_sbvs_par_type(for_var,py_var,ierror)
            type(sbvs_par_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sbvs_par_type

        module subroutine wrap_irf_type(for_var,py_var,ierror)
            class(irf_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_irf_type

        module subroutine wrap_atoms_conf_list_type(for_var,py_var,ierror)
            type(atoms_conf_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atoms_conf_list_type

        module subroutine wrap_psd_val_type(for_var,py_var,ierror)
            type(psd_val_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_psd_val_type

        module subroutine wrap_sxd_val_type(for_var,py_var,ierror)
            type(sxd_val_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxd_val_type

        module subroutine wrap_twin_type(for_var,py_var,ierror)
            type(twin_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_twin_type

        module subroutine wrap_multistate_vector_type(for_var,py_var,ierror)
            type(multistate_vector_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_multistate_vector_type

        module subroutine wrap_simann_conditions_type(for_var,py_var,ierror)
            type(simann_conditions_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_simann_conditions_type

        module subroutine wrap_state_vector_type(for_var,py_var,ierror)
            type(state_vector_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_state_vector_type

        module subroutine wrap_deriv_tof_type(for_var,py_var,ierror)
            type(deriv_tof_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_deriv_tof_type

        module subroutine wrap_excluded_regions_type(for_var,py_var,ierror)
            type(excluded_regions_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_excluded_regions_type

        module subroutine wrap_header_type(for_var,py_var,ierror)
            type(header_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_header_type

        module subroutine wrap_image_conditions(for_var,py_var,ierror)
            type(image_conditions), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_image_conditions

        module subroutine wrap_laue_instrument_type(for_var,py_var,ierror)
            type(laue_instrument_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_laue_instrument_type

        module subroutine wrap_laue_ref_type(for_var,py_var,ierror)
            type(laue_ref_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_laue_ref_type

        module subroutine wrap_laue_ref_list_type(for_var,py_var,ierror)
            type(laue_ref_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_laue_ref_list_type

        module subroutine wrap_peakfind_parameters_type(for_var,py_var,ierror)
            type(peakfind_parameters_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_peakfind_parameters_type

        module subroutine wrap_peak_type(for_var,py_var,ierror)
            type(peak_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_peak_type

        module subroutine wrap_peak_list_type(for_var,py_var,ierror)
            type(peak_list_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_peak_list_type

        module subroutine wrap_rational(for_var,py_var,ierror)
            type(rational), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_rational

        module subroutine wrap_symm_oper_type(for_var,py_var,ierror)
            type(symm_oper_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_symm_oper_type

        module subroutine wrap_group_type(for_var,py_var,ierror)
            class(group_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_type

        module subroutine wrap_rot_mat_type(for_var,py_var,ierror)
            type(rot_mat_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_rot_mat_type

        module subroutine wrap_spin_operator_type(for_var,py_var,ierror)
            type(spin_operator_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_spin_operator_type

        module subroutine wrap_spin_group_type(for_var,py_var,ierror)
            type(spin_group_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_spin_group_type

        module subroutine wrap_kvect_info_type(for_var,py_var,ierror)
            type(kvect_info_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_kvect_info_type

        module subroutine wrap_point_orbit(for_var,py_var,ierror)
            class(point_orbit), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_orbit

        module subroutine wrap_orbit_list(for_var,py_var,ierror)
            type(orbit_list), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_orbit_list

        module subroutine wrap_sym_oper_type(for_var,py_var,ierror)
            type(sym_oper_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sym_oper_type

        module subroutine wrap_msym_oper_type(for_var,py_var,ierror)
            type(msym_oper_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_msym_oper_type

        module subroutine wrap_magnetic_domain_type(for_var,py_var,ierror)
            type(magnetic_domain_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_domain_type

        module subroutine wrap_magsymm_k_type(for_var,py_var,ierror)
            type(magsymm_k_type), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magsymm_k_type

        module subroutine wrap_point_orbit_kv(for_var,py_var,ierror)
            class(point_orbit_kv), intent(inout) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_orbit_kv

    end interface

End Module CFML_Wraps
