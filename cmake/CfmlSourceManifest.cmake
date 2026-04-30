# Canonical explicit manifest for the repo-owned CFML core build.
#
# This file is intentionally derived from the currently validated ordering in
# pybuild.toml so the repo-owned CMake build can replace the script-generated
# assembly flow without changing source-selection semantics.

set(CRYSFML_CORE_MANIFEST_IS_SCAFFOLD OFF)

# The vendored Intel macOS file really uses the historical "MacOs" spelling.
set(CRYSFML_GLOBAL_DEPS_VARIANTS
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Linux_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Linux_INTEL.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_MacOS_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_MacOs_INTEL.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Windows_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Windows_INTEL.f90"
)

macro(crysfml_manifest_append_main module_key main_file)
    list(APPEND CRYSFML_CORE_SOURCES "${CRYSFML_VENDOR_SRC_ROOT}/${main_file}.f90")
endmacro()

macro(crysfml_manifest_append_main_and_components module_key main_file components_dir)
    crysfml_manifest_append_main("${module_key}" "${main_file}")
    foreach(_crysfml_component ${ARGN})
        list(APPEND CRYSFML_CORE_SOURCES
             "${CRYSFML_VENDOR_SRC_ROOT}/${components_dir}/${_crysfml_component}.f90")
    endforeach()
endmacro()

macro(crysfml_manifest_append_components_only module_key components_dir)
    foreach(_crysfml_component ${ARGN})
        list(APPEND CRYSFML_CORE_SOURCES
             "${CRYSFML_VENDOR_SRC_ROOT}/${components_dir}/${_crysfml_component}.f90")
    endforeach()
endmacro()

macro(crysfml_manifest_append_test_program test_key test_dir test_name)
    list(APPEND CRYSFML_VENDOR_TEST_PROGRAM_KEYS "${test_key}")
    list(APPEND CRYSFML_VENDOR_TEST_PROGRAM_SOURCES
         "${CRYSFML_VENDOR_ROOT}/Testing/${test_dir}/${test_name}.f90")
endmacro()

set(CRYSFML_CORE_MODULE_KEYS
    global_deps
    cfml_fft
    cfml_maths
    cfml_extincorr
    cfml_random
    cfml_messages
    cfml_strings
    cfml_rational
    cfml_superspace_database
    cfml_magnetic_database
    cfml_magsuperspace_database
    cfml_bvs_tables
    cfml_scattering_tables
    cfml_bonds_tables
    cfml_symmetry_tables
    cfml_tables
    cfml_profiles
    cfml_optimization_lsq
    cfml_optimization
    cfml_simulated_annealing
    cfml_diffpatt
    cfml_metrics
    cfml_optimization_sann
    cfml_eos
    cfml_gspacegroups
    cfml_bckpeaks
    cfml_ill_instrm_data
    cfml_atoms
    cfml_propagation_vectors
    cfml_reflections
    cfml_structure_factors
    cfml_geom
    cfml_kvec_symmetry
    cfml_kvec_structure_factors
    cfml_maps
    cfml_molecules
    cfml_keywords_code_parser
    cfml_ioform
    cfml_keycodes
    cfml_vtk
    cfml_sxtal_geom
    cfml_export_vtk
    cfml_enbvs
    cfml_kvec_polarimetry
    cfml_powder
    cfml_laue
    forpy
    cfml_utilities
    cfml_wraps_utils
    cfml_py_utilities
)

set(CRYSFML_CORE_SOURCES "")

# Module 02: CFML_FFT
crysfml_manifest_append_main_and_components(cfml_fft CFML_FFT CFML_FFT
    FFT_Gen
    FFT_Convol
)

# Module 03: CFML_Maths
crysfml_manifest_append_main_and_components(cfml_maths CFML_Maths CFML_Maths
    Math_Diagonalize_GEN
    Math_Equal_Vector
    Math_Determinant
    Math_Co_Prime
    Math_Cross_Product
    Math_Debye
    Math_Co_Linear
    Math_Erfc_Der
    Math_Diagonalize_SH
    Math_Factorial
    Math_Equal_Matrix
    Math_Inverse_Matrix
    Math_Is_Diagonal_Matrix
    Math_PolynomialFit
    Math_Lower_Triangular
    Math_Is_Null_Vector
    Math_Norm
    Math_Locate
    Math_Tensor_Product
    Math_Linear_Dependent
    Math_Polyhedron_Volume
    Math_Resolv_System
    Math_SistCoord_Changes
    Math_Rotation_Axes
    Math_Modulo_Lat
    Math_Negligible
    Math_Trace
    Math_Scalar
    Math_Outerprod
    Math_Smoothing_Interpol
    Math_Zbelong
    Math_Spher_Harm
    Math_Pgcd
    Math_Mat_Cross
    Math_Poly_Legendre
    Math_Upper_Triangular
    Math_Points_In_Line2D
    Math_RowEchelon
    Math_Swap
    Math_Rank
    Math_Sort
    Math_In_Limits
)

# Module 04: CFML_ExtinCorr
crysfml_manifest_append_main_and_components(cfml_extincorr CFML_ExtinCorr CFML_ExtinCorr
    Ext_BeckerCoppens
    Ext_FlippingRatios
    Ext_ShelxCorr
)

# Module 05: CFML_Random
crysfml_manifest_append_main_and_components(cfml_random CFML_Random CFML_Random
    Random_Beta_Sm
    Random_Binomial_Sm
    Random_VonMises_Sm
    Random_InvGauss_Sm
    Random_Poisson_Sm
    Random_Gamma_Sm
    Random_Normal_Sm
    Random_Cauchy_Sm
    Random_T_Sm
)

# Module 06: CFML_Messages
crysfml_manifest_append_main_and_components(cfml_messages CFML_Messages CFML_Messages
    Con_Print_Message
    Con_Info_Message
    Con_Err_Message
    Con_Write_ScrollMsg
    Con_Wait_Message
)

# Module 07: CFML_Strings
crysfml_manifest_append_main_and_components(cfml_strings CFML_Strings CFML_Strings
    StringFullp
    StringNum
    StringReadKey
    StringTools
)

# Module 08: CFML_Rational
crysfml_manifest_append_main_and_components(cfml_rational CFML_Rational CFML_Rational
    RAT_constructor
    RAT_generic
    RAT_is_integer
    RAT_assignment
    RAT_operator_add
    RAT_operator_eq
    RAT_operator_ge
    RAT_Equal_rational
    RAT_operator_divisor
    RAT_operator_gt
    RAT_operator_le
    RAT_operator_minus
    RAT_rowechelon
    RAT_operator_lt
    RAT_overloads
    RAT_operator_multiply
    RAT_operator_neq
)

# Module 09: CFML_SuperSpace_Database
crysfml_manifest_append_main(cfml_superspace_database CFML_SuperSpace_Database)

# Module 10: CFML_Magnetic_Database
crysfml_manifest_append_main(cfml_magnetic_database CFML_Magnetic_Database)

# Module 11: CFML_magSuperSpace_Database
crysfml_manifest_append_main(cfml_magsuperspace_database CFML_magSuperSpace_Database)

# Module 12: CFML_BVS_Tables
crysfml_manifest_append_main(cfml_bvs_tables CFML_BVS_Tables)

# Module 13: CFML_Scattering_Tables
crysfml_manifest_append_main(cfml_scattering_tables CFML_Scattering_Tables)

# Module 14: CFML_Bonds_Tables
crysfml_manifest_append_main(cfml_bonds_tables CFML_Bonds_Tables)

# Module 15: CFML_Symmetry_Tables
crysfml_manifest_append_main(cfml_symmetry_tables CFML_Symmetry_Tables)

# Module 16: CFML_Tables
crysfml_manifest_append_components_only(cfml_tables CFML_Tables
    Tab_Del_BVST
    Tab_Set_BVST
    Tab_Set_ScatterT
    Tab_Get_ScatterT
    Tab_Del_ScatterT
    Tab_Get_SpgT
    Tab_Del_SpgT
    Tab_Set_SpgT
    Tab_Get_SpgSymbols
    Tab_Get_BondsT
    Tab_Del_BondsT
    Tab_Set_BondsT
    Tab_Allocating_SuperSpaceDBase
    Tab_Read_SSG_DBase
    Tab_Allocating_MagneticDBase
    Tab_Read_MagneticDBase
)

# Module 17: CFML_Profiles
crysfml_manifest_append_main_and_components(cfml_profiles CFML_Profiles CFML_Profiles
    Profile_BacktoBack
    Profile_Exponential
    Profile_Finger
    Profile_Gaussian
    Profile_Init_ProfVal
    Profile_IkedaCarpenter
    Profile_TCHpVoigt
    Profile_Hat
    Profile_PseudoVoigt
    Profile_Lorentzian
    Profile_TOF_Jorgensen
    Profile_TOF_Jorg_Vondreele
    Profile_TOF_Carpenter
)

# Module 18: CFML_Optimization_LSQ
crysfml_manifest_append_main_and_components(cfml_optimization_lsq CFML_Optimization_LSQ CFML_Optimization_LSQ
    OPT_LSQ_LevebergMarquardt_AnalyDer
    OPT_LSQ_Marquardt_Fit
    OPT_LSQ_Output
    OPT_LSQ_LevebergMarquardt_NumDer
)

# Module 19: CFML_Optimization
crysfml_manifest_append_main_and_components(cfml_optimization CFML_Optimization CFML_Optimization
    OPT_Global_Csendes
    OPT_Cg_Quasi_Newton
    OPT_Local_Optim
    OPT_Simplex
)

# Module 20: CFML_Simulated_Annealing
crysfml_manifest_append_main(cfml_simulated_annealing CFML_Simulated_Annealing)

# Module 21: CFML_Diffpatt
crysfml_manifest_append_main_and_components(cfml_diffpatt CFML_Diffpatt CFML_DiffPatt
    DiffP_ReadPatt_ILL
    DiffP_ReadPatt_CIF
    DiffP_FWHM_Peak
    DiffP_BackgPatt
    DiffP_ReadPatt_LLB
    DiffP_NoisyPoints
    DiffP_ReadPatt_GSAS
    DiffP_ReadPatt_ISIS
    DiffP_ReadPatt_NLS
    DiffP_ReadPatt_PAN
    DiffP_ReadPatt_FREE
    DiffP_ReadPatt_PSI
    DiffP_ReadPatt_Socabim
    DiffP_ReadPatt_TimeVar
    DiffP_ReadPatt_XYSIG
    DiffP_WritePatterns
    DiffP_ReadPatterns
    DiffP_Add_Patterns
)

# Module 22: CFML_Metrics
crysfml_manifest_append_main_and_components(cfml_metrics CFML_Metrics CFML_Metrics
    Metrics_Tensor
    Metrics_Gen
    Metrics_ThConver
    Metrics_IO
    Metrics_NiggliCell
)

# Module 23: CFML_Optimization_SAnn
crysfml_manifest_append_components_only(cfml_optimization_sann CFML_Optimization_SAnn
    SAnn_General
    SAnn_LocalOpt
    SAnn_MultiConf
    SAnn_SetnCheck
    SAnn_inout
)

# Module 24: CFML_EoS
crysfml_manifest_append_main_and_components(cfml_eos CFML_EoS CFML_EoS
    EoS_Calc
    EoS_CopyEDat
    EoS_Get_APL
    EoS_Get_HeatCap
    EoS_CellPar
    EoS_PrincipalEoS
    Eos_Allocate
    EoS_Transform_ESD
    EoS_ModDir
    Eos_DerivPartial
    EoS_Get_Tensor
    Eos_Get_Bulk
    Eos_Checks
    EoS_Get_Angle
    Eos_AlphaCalc
    Eos_Conlev
    Eos_FfCalc
    EoS_LinEoS_Allowed
    Eos_Get_Pressure
    Eos_Get_Temperature
    Eos_Get_Tait
    Eos_Gruneisen
    Eos_NormPressure
    Eos_Set
    Eos_Get_Properties
    Eos_Read
    Eos_Get_Volume
    Eos_PVT_Table
    Eos_Get_Transition
    Eos_Init
    Eos_K_Cal
    Eos_Pthermal
    Eos_Strain
    Eos_Write
    Eos_dKdTCalc
)

# Module 25: CFML_gSpaceGroups
crysfml_manifest_append_main_and_components(cfml_gspacegroups CFML_gSpaceGroups CFML_gSpaceGroups
    gS_Allocate_Opers
    gS_Allocate_SpaceG
    gS_ApplySO
    gS_Get_Cosets
    gS_CheckGener
    gS_Get_CrystalSys
    gS_Get_GenerStr
    gS_Get_Dimension
    gS_Get_Generators
    gS_Get_Hall_Gener
    gS_Get_HM_Standard
    gS_Get_LattType
    gS_Get_OriginShift
    gS_Is_LattCentring
    gS_Get_X_Matrix
    gS_Is_InversionCentre
    gS_Get_Oper_Symb
    gS_Reorder_Oper
    gS_Smallest_IntegralVec
    gS_Identify_Groups
    gS_Get_Mult_OPTable
    gS_Get_Orb_Stabilizer_Constr
    gS_Get_PseudoStdBase
    gS_Match_Spg3D
    gS_Get_Mat_Symb
    gS_Get_Symb_Mat
    gS_Set_SpaceG
    gS_Inverse_OP
    gS_Get_LauePG
    gS_Get_SubGrp
    gS_Init_Procedures
    gS_Match_Shubnikov_Grp
    gS_Get_Rotations
    gS_Get_Ops_Gener
    gS_Spg_Const_VGen
    gS_Rational_RedTraslation
    gS_Get_Symb_Oper
    gS_operator_equal
    gS_Sort_Operator
    gS_operator_mult
    gS_Write_SpaceG
    gS_Is_Antilattice
    gS_Spg_Const_Str
    gS_Symm_Symbols
    gS_OnePrimeOp
)

# Module 26: CFML_BckPeaks
crysfml_manifest_append_main(cfml_bckpeaks CFML_BckPeaks)

# Module 27: CFML_ILL_Instrm_Data
crysfml_manifest_append_main(cfml_ill_instrm_data CFML_ILL_Instrm_Data)

# Module 28: CFML_Atoms
crysfml_manifest_append_main_and_components(cfml_atoms CFML_Atoms CFML_Atoms
    Atm_ExtendList
    Atm_ChangeList
    Atm_Allocating_Atoms
    Atm_PointList
    Atm_RW_Bin_AtmList
    Atm_Write_AtmList
    Atm_SymmConstr_MomDisp
)

# Module 29: CFML_Propagation_Vectors
crysfml_manifest_append_main(cfml_propagation_vectors CFML_Propagation_Vectors)

# Module 30: CFML_Reflections
crysfml_manifest_append_main_and_components(cfml_reflections CFML_Reflections CFML_Reflections
    Refl_H_Convent
    Refl_AsymUnit
    Refl_Generate
    Refl_H_EquivList
    Refl_H_Equal
    Refl_Conditions
    Refl_H_Equiv
    Refl_H_S
    Refl_H_Absent
    Refl_MaxNum
    Refl_Write_List
    Refl_UnitaryVec
    Refl_Init_RefList
    Refl_H_Mult
)

# Module 31: CFML_Structure_Factors
crysfml_manifest_append_main_and_components(cfml_structure_factors CFML_Structure_Factors CFML_Structure_Factors
    SF_AtomicFactors
    SF_Calculations
    SF_Scattering_Species
    SF_Initialize
    SF_Write_SF
    SF_Create_Tables
)

# Module 32: CFML_Geom
crysfml_manifest_append_main_and_components(cfml_geom CFML_Geom CFML_Geom
    Geom_Angles
    Geom_Allocations
    Geom_Matrices
    Geom_Coordination
    Geom_Orbits
    Geom_Distances
)

# Module 33: CFML_kvec_Symmetry
crysfml_manifest_append_main_and_components(cfml_kvec_symmetry CFML_kvec_Symmetry CFML_kvec_Symmetry
    ksym_auxsub
    ksym_init
    ksym_functions
    ksym_suscept
    ksym_read
    ksym_write
)

# Module 34: CFML_kvec_Structure_Factors
crysfml_manifest_append_main_and_components(cfml_kvec_structure_factors CFML_kvec_Structure_Factors CFML_kvec_Structure_Factors
    kStrf_Init_FxTables
    kStrf_MiV
    kStrf_MStrT
    kStrf_satellites
    kStrf_write
)

# Module 35: CFML_Maps
crysfml_manifest_append_main_and_components(cfml_maps CFML_Maps CFML_Maps
    Maps_Mapping
    Maps_MarchingCubes
    Maps_Percolation
)

# Module 36: CFML_Molecules
crysfml_manifest_append_main_and_components(cfml_molecules CFML_Molecules CFML_Molecules
    Mol_Formula
    Mol_Cartesian_to
    Mol_Fractional_to
    Mol_IndexList
    Mol_Orientation
    Mol_Initialize
    Mol_WriteInfo
    Mol_Spherical_to
    Mol_ReadInfo
    Mol_ZMatrix_to
    Mol_to_AtList
)

# Module 37: CFML_Keywords_Code_Parser
crysfml_manifest_append_main_and_components(cfml_keywords_code_parser CFML_Keywords_Code_Parser CFML_Keywords_Code_Parser
    KWC_FillCodes_Gen
    KWC_Allocation
    KWC_Deletion
    KWC_WriteRefCodes
    KWC_FillCodes_MolX
    KWC_FillCodes_FAtm
    KWC_SplitOperations
    KWC_GetConCodes
    KWC_VStateToAtomPar
    KWC_ReadCodes
    KWC_GetRestrCodes
    KWC_RefCodes
)

# Module 38: CFML_IOForm
crysfml_manifest_append_main_and_components(cfml_ioform CFML_IOForm CFML_IOForm
    Format_GEN
    Format_MCIF
    Format_Blocks
    Format_CIF
    Format_CFL
    Format_FST
    Format_SHX
)

# Module 39: CFML_KeyCodes
crysfml_manifest_append_main_and_components(cfml_keycodes CFML_KeyCodes CFML_KeyCodes
    KeyCod_VecRef
    KeyCod_RGB
    KeyCod_Atm
    KeyCod_Patt
    KeyCod_GenPar
    keyCod_Phas
    KeyCod_WriteInfo
    KeyCod_Restraints
    KeyCod_Molec
)

# Module 40: CFML_VTK
crysfml_manifest_append_main_and_components(cfml_vtk CFML_VTK CFML_VTK
    VTK_Scan_Utils
)

# Module 41: CFML_SXTAL_Geom
crysfml_manifest_append_main_and_components(cfml_sxtal_geom CFML_SXTAL_Geom CFML_SXTAL_Geom
    SXTAL_Angles
    SXTAL_IO
    SXTAL_Matx_Zvect
    SXTAL_PSD
    SXTAL_FlatCone
    SXTAL_UB
)

# Module 42: CFML_Export_VTK
crysfml_manifest_append_main(cfml_export_vtk CFML_Export_VTK)

# Module 43: CFML_EnBVS
crysfml_manifest_append_main_and_components(cfml_enbvs CFML_EnBVS CFML_EnBVS
    EnBVS_CostF
    EnBVS_Energy
    EnBVS_Maps
    EnBVS_SetTab
)

# Module 44: CFML_kvec_Polarimetry
crysfml_manifest_append_main_and_components(cfml_kvec_polarimetry CFML_kvec_Polarimetry CFML_kvec_Polarimetry
    Polar_Calculations_Dom
    Polar_Calculations
    Polar_Functions
    Polar_Init
    Polar_Write
)

# Module 45: CFML_Powder
crysfml_manifest_append_main_and_components(cfml_powder CFML_Powder CFML_Powder
    Pow_IRF
    Pow_Lorentz_Absorption
    Pow_MicroStructure
    Pow_Preferred_Orientation
)

# Module 46: CFML_Laue
crysfml_manifest_append_main_and_components(cfml_laue CFML_Laue CFML_Laue
    Laue_Allocate
    Laue_Geom
    Laue_HBin
    Laue_Image
    Laue_Init
    Laue_Instrument
    Laue_Output
    Laue_PeakFind
    Laue_Ref_Calc
    Laue_Ref_Gen
    Laue_Stereo
)

# Module 47: Forpy
crysfml_manifest_append_main(forpy Forpy)

# Module 48: CFML_Utilities
crysfml_manifest_append_main_and_components(cfml_utilities CFML_Utilities CFML_Utilities
    Utilities_Patterns
)

# Module 49: CFML_Wraps_Utils
crysfml_manifest_append_main_and_components(cfml_wraps_utils CFML_Wraps_Utils CFML_Wraps_Utils
    Wraps_Utils
)

# Module 50: CFML_Py_Utilities
crysfml_manifest_append_main_and_components(cfml_py_utilities CFML_Py_Utilities CFML_Py_Utilities
    Py_Utilities_Patterns
    Py_Utilities_IOForm
    Py_Utilities_Structure_Factors
    Py_Utilities_Esmeralda
)

set(CRYSFML_VENDOR_TEST_PROGRAM_KEYS "")
set(CRYSFML_VENDOR_TEST_PROGRAM_SOURCES "")
crysfml_manifest_append_test_program("Bond_Str/Bond_StrN" "Bond_Str" "Bond_StrN")
crysfml_manifest_append_test_program("CIFs/io_files" "CIFs" "io_files")
crysfml_manifest_append_test_program("Grp_230/groups_230" "Grp_230" "groups_230")
crysfml_manifest_append_test_program("hkl_gen/hkl_gen" "hkl_gen" "hkl_gen")
crysfml_manifest_append_test_program("hkl_gen/simple_hkl_gen" "hkl_gen" "simple_hkl_gen")
crysfml_manifest_append_test_program("Molecules/mol_tpcr" "Molecules" "mol_tpcr")
crysfml_manifest_append_test_program("PowderPattern/Simple_calc_powder" "PowderPattern" "Simple_calc_powder")
crysfml_manifest_append_test_program("PowderPattern/Simple_calc_Mag_powder" "PowderPattern" "Simple_calc_Mag_powder")
crysfml_manifest_append_test_program("StructureFactors/Calc_Sfac" "StructureFactors" "Calc_Sfac")

foreach(_path IN LISTS CRYSFML_GLOBAL_DEPS_VARIANTS CRYSFML_CORE_SOURCES CRYSFML_VENDOR_TEST_PROGRAM_SOURCES)
    if(NOT EXISTS "${_path}")
        message(FATAL_ERROR "Expected vendored CFML manifest path is missing: ${_path}")
    endif()
endforeach()

unset(_path)
unset(_crysfml_component)