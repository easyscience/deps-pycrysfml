# =======
# SOURCES
# =======

# CFML_GlobalDeps
# This module is OS and compiler dependent.
if(WIN32)
   # Windows
    if(${COMPILER_NAME} STREQUAL ifort OR ${COMPILER_NAME} STREQUAL ifx)
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Windows_INTEL.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Windows_GFOR.f90)
    endif()
elseif(APPLE)
    # MacOS
    if(${COMPILER_NAME} STREQUAL ifort OR ${COMPILER_NAME} STREQUAL ifx)
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_MacOS_INTEL.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_MacOS_GFOR.f90)
    endif()
elseif(UNIX)
    # Unix
    if(${COMPILER_NAME} STREQUAL ifort OR ${COMPILER_NAME} STREQUAL ifx)
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Linux_INTEL.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Linux_GFOR.f90)
    endif()
endif()

if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${GLOBAL_DEPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${GLOBAL_DEPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Messages
file(GLOB SUBMOD_MESSAGES_SRC CFML_Messages/*.f90)
set(MESSAGES_SRC CFML_Messages.f90
                 CFML_Messages/Con_Err_Message.f90
                 CFML_Messages/Con_Info_Message.f90
                 CFML_Messages/Con_Print_Message.f90
                 CFML_Messages/Con_Wait_Message.f90
                 CFML_Messages/Con_Write_ScrollMsg.f90)
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${MESSAGES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${MESSAGES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# Mathematics: CFML_Maths,CFML_FFT,CFML_Random
file(GLOB SUBMOD_MATHS_SRC CFML_Maths/*.f90)
file(GLOB SUBMOD_FFT_SRC CFML_FFT/*.f90)
file(GLOB SUBMOD_RANDOM_SRC CFML_Random/*.f90)
set(MATHS_SRC CFML_Maths.f90
              CFML_FFT.f90
              CFML_Random.f90
              #CFML_Trigonometry.f90
              ${SUBMOD_MATHS_SRC}
              ${SUBMOD_FFT_SRC}
              ${SUBMOD_RANDOM_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${MATHS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${MATHS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Forpy
set(Forpy_SRC Forpy.f90)

if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${Forpy_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${Forpy_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Py_Utilities
file(GLOB SUBMOD_PYTHON_SRC CFML_Py_Utilities/*.f90)
set(PYTHON_SRC CFML_Py_Utilities.f90
                ${SUBMOD_PYTHON_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${PYTHON_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${PYTHON_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_VTK
file(GLOB SUBMOD_VTK_SRC CFML_VTK/*.f90)
set(VTK_SRC CFML_VTK.f90
                ${SUBMOD_VTK_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${VTK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${VTK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Strings
file(GLOB SUBMOD_STRINGS_SRC CFML_Strings/*.f90)
set(STRINGS_SRC CFML_Strings.f90
                ${SUBMOD_STRINGS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${STRINGS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${STRINGS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Rational
file(GLOB SUBMOD_RATIONAL_SRC CFML_Rational/*.f90)
set(RATIONAL_SRC CFML_Rational.f90
                ${SUBMOD_RATIONAL_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${RATIONAL_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${RATIONAL_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Metrics
file(GLOB SUBMOD_METRICS_SRC CFML_Metrics/*.f90)
set(METRICS_SRC CFML_Metrics.f90
                ${SUBMOD_METRICS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${METRICS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${METRICS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Tables
set(SUBMOD_TABLES_1_SRC CFML_Tables/Tab_Del_ScatterT.f90
                        CFML_Tables/Tab_Get_ScatterT.f90
                        CFML_Tables/Tab_Del_BondsT.f90
                        CFML_Tables/Tab_Get_BondsT.f90
                        CFML_Tables/Tab_Del_SpgT.f90
                        CFML_Tables/Tab_Get_SpgT.f90
                        CFML_Tables/Tab_Del_BVST.f90
                        CFML_Tables/Tab_Allocating_MagneticDBase.f90
                        CFML_Tables/Tab_Read_MagneticDBase.f90
                        CFML_Tables/Tab_Allocating_SuperSpaceDBase.f90
                        CFML_Tables/Tab_Read_SSG_DBase.f90)
set(SUBMOD_TABLES_2_SRC CFML_Tables/Tab_Set_ScatterT.f90
                        CFML_Tables/Tab_Set_BondsT.f90
                        CFML_Tables/Tab_Get_SpgSymbols.f90
                        CFML_Tables/Tab_Set_SpgT.f90
                        CFML_Tables/Tab_Set_BVST.f90)
set(TABLES_1_SRC CFML_Scattering_Tables.f90
                 CFML_Bonds_Tables.f90
                 ${SUBMOD_TABLES_1_SRC})
set(TABLES_2_SRC CFML_Symmetry_Tables.f90
                 CFML_BVS_Tables.f90
                 CFML_Magnetic_Database.f90
                 CFML_SuperSpace_Database.f90)
set(TABLES_3_SRC ${SUBMOD_TABLES_2_SRC})

if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${TABLES_1_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS0}")
else()
    set_source_files_properties(${TABLES_1_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${TABLES_2_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS0}")

    set_source_files_properties(${TABLES_3_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS0}")
else()
    set_source_files_properties(${TABLES_2_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS0}")

    set_source_files_properties(${TABLES_3_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS0}")
endif()

# CFML_gSpaceGroups
file(GLOB SUBMOD_GROUPS_SRC CFML_gSpaceGroups/*.f90)
set(GROUPS_SRC CFML_gSpaceGroups.f90
               ${SUBMOD_GROUPS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${GROUPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${GROUPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Profiles
file(GLOB SUBMOD_PROFILES_SRC CFML_Profiles/Prof*.f90)
set(PROFILES_1_SRC CFML_Profiles.f90
                   ${SUBMOD_PROFILES_SRC})
set(PROFILES_2_SRC CFML_Profiles/Profile_Init_ProfVal.f90)
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${PROFILES_1_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${PROFILES_1_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${PROFILES_2_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${PROFILES_2_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS0}")
endif()

# CFML_DiffPatt
file(GLOB SUBMOD_DIFFPATT_SRC CFML_DiffPatt/*.f90)
set(DIFFPATT_SRC CFML_Diffpatt.f90
                 ${SUBMOD_DIFFPATT_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${DIFFPATT_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${DIFFPATT_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_BckPeaks
set(BCKPEAKS_SRC CFML_BckPeaks.f90)
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${BCKPEAKS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${BCKPEAKS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_ExtintCorr
file(GLOB SUBMOD_EXTINCORR_SRC CFML_ExtinCorr/*.f90)
set(EXTINCORR_SRC CFML_ExtinCorr.f90
                  ${SUBMOD_EXTINCORR_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${EXTINCORR_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${EXTINCORR_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_EOS
file(GLOB SUBMOD_EOS_SRC CFML_EoS/*.f90)
set(EOS_SRC CFML_EoS.f90
            ${SUBMOD_EOS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${EOS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${EOS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Atoms
file(GLOB SUBMOD_ATOMS_SRC CFML_Atoms/*.f90)
set(ATOMS_SRC CFML_Atoms.f90
              ${SUBMOD_ATOMS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${ATOMS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${ATOMS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Reflections
file(GLOB SUBMOD_REFLECTIONS_SRC CFML_Reflections/*.f90)
set(REFLECTIONS_SRC CFML_Reflections.f90
                 ${SUBMOD_REFLECTIONS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${REFLECTIONS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${REFLECTIONS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Propagation_vectors
set(PROPAGK_SRC CFML_Propagation_Vectors.f90)
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${PROPAGK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${PROPAGK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_IOForm
file(GLOB SUBMOD_IOFORM_SRC CFML_IOForm/*.f90)
set(IOFORM_SRC CFML_IOForm.f90 ${SUBMOD_IOFORM_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${IOFORM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${IOFORM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Geom
file(GLOB SUBMOD_GEOM_SRC CFML_Geom/*.f90)
set(GEOM_SRC CFML_Geom.f90
                 ${SUBMOD_GEOM_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${GEOM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${GEOM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Maps
file(GLOB SUBMOD_MAPS_SRC CFML_Maps/*.f90)
set(MAPS_SRC CFML_Maps.f90
                 ${SUBMOD_MAPS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${MAPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${MAPS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Optimization
file(GLOB SUBMOD_OPT_SRC CFML_Optimization/*.f90)
set(OPT_SRC CFML_Optimization.f90
                 ${SUBMOD_OPT_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${OPT_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${OPT_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Optimization_LSQ
file(GLOB SUBMOD_OPT_LSQ_SRC CFML_Optimization_LSQ/*.f90)
set(OPT_LSQ_SRC CFML_Optimization_LSQ.f90
                 ${SUBMOD_OPT_LSQ_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${OPT_LSQ_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${OPT_LSQ_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Optimization_SAnn
file(GLOB SUBMOD_OPT_SAN_SRC CFML_Optimization_SAnn/*.f90)
set(OPT_SAN_SRC CFML_Simulated_Annealing.f90
                 ${SUBMOD_OPT_SAN_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${OPT_SAN_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${OPT_SAN_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Export_VTK
set(Export_VTK_SRC CFML_Export_VTK.f90)
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${Export_VTK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${Export_VTK_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_EnBVS
file(GLOB SUBMOD_EnBVS_SRC CFML_EnBVS/*.f90)
set(EnBVS_SRC CFML_EnBVS.f90
                 ${SUBMOD_EnBVS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${EnBVS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${EnBVS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# Instrm_ILL
set(InstrmILL_SRC CFML_ILL_Instrm_Data.f90)

if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${InstrmILL_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS2}")
else()
    set_source_files_properties(${InstrmILL_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_SXTAL_Geom
file(GLOB SUBMOD_SXTALgeom_SRC CFML_SXTAL_Geom/*.f90)
set(SXTALgeom_SRC CFML_SXTAL_Geom.f90
                 ${SUBMOD_SXTALgeom_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${SXTALgeom_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${SXTALgeom_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Structure_Factors
file(GLOB SUBMOD_SF_SRC CFML_Structure_Factors/*.f90)
set(SF_SRC CFML_Structure_Factors.f90
                 ${SUBMOD_SF_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${SF_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${SF_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Molecules
file(GLOB SUBMOD_MOLECULES_SRC CFML_Molecules/*.f90)
set(MOLECULES_SRC CFML_Molecules.f90
                 ${SUBMOD_MOLECULES_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${MOLECULES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${MOLECULES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_KeyCodes
file(GLOB SUBMOD_KEYCODES_SRC CFML_KeyCodes/*.f90)
set(KEYCODES_SRC CFML_KeyCodes.f90
                 ${SUBMOD_KEYCODES_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${KEYCODES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${KEYCODES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
    endif()

# CFML_kvec_symmetry
file(GLOB SUBMOD_KVEC_SYMMETRY_SRC CFML_kvec_Symmetry/*.f90)
set(KVECSYMM_SRC CFML_kvec_Symmetry.f90
                 ${SUBMOD_KVEC_SYMMETRY_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${KVECSYMM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${KVECSYMM_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_kvec_Structure_Factors
file(GLOB SUBMOD_KVEC_STRFACT_SRC CFML_kvec_Structure_Factors/*.f90)
set(KVECSTRF_SRC CFML_kvec_Structure_Factors.f90
                 ${SUBMOD_KVEC_STRFACT_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${KVECSTRF_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${KVECSTRF_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_kvec_Polarimetry
file(GLOB SUBMOD_KVEC_POLAR_SRC CFML_kvec_Polarimetry/*.f90)
set(KVECPOLAR_SRC CFML_kvec_Polarimetry.f90
                 ${SUBMOD_KVEC_POLAR_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${KVECPOLAR_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${KVECPOLAR_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_KeyWords_Code_Parser
file(GLOB SUBMOD_KWC_SRC CFML_Keywords_Code_Parser/*.f90)
set(KWCPARSER_SRC CFML_Keywords_Code_Parser.f90
                 ${SUBMOD_KWC_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${KWCPARSER_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${KWCPARSER_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Powder
file(GLOB SUBMOD_POWDER_SRC CFML_Powder/*.f90)
set(POWDER_SRC CFML_Powder.f90
                 ${SUBMOD_POWDER_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${POWDER_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${POWDER_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Utilities
file(GLOB SUBMOD_UTILITIES_SRC CFML_Utilities/*.f90)
set(UTILITIES_SRC CFML_Utilities.f90
                 ${SUBMOD_UTILITIES_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${UTILITIES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${UTILITIES_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Wraps_Utils
file(GLOB SUBMOD_WRAPS_UTILS_SRC CFML_Wraps_Utils/*.f90)
set(WRAPS_UTILS_SRC CFML_Wraps_Utils.f90
                 ${SUBMOD_WRAPS_UTILS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${WRAPS_UTILS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${WRAPS_UTILS_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

# CFML_Laue
file(GLOB SUBMOD_LAUE_SRC CFML_Laue/*.f90)
set(LAUE_SRC CFML_Laue.f90
                 ${SUBMOD_LAUE_SRC})
if(${COMPILER_NAME} STREQUAL ifort)
    set_source_files_properties(${LAUE_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS3}")
else()
    set_source_files_properties(${LAUE_SRC}
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGSC} ${OPT_FLAGS1}")
endif()

#  List of all the source files
set(CRYSFML08_SRC
    ${Forpy_SRC}
    ${GLOBAL_DEPS_SRC}
    ${MESSAGES_SRC}
    ${MATHS_SRC}
    ${STRINGS_SRC}
    ${RATIONAL_SRC}
    ${METRICS_SRC}
    ${TABLES_1_SRC}
    ${TABLES_2_SRC}
    ${TABLES_3_SRC}
    ${GROUPS_SRC}
    ${PROFILES_1_SRC}
    ${PROFILES_2_SRC}
    ${DIFFPATT_SRC}
    ${BCKPEAKS_SRC}
    ${EXTINCORR_SRC}
    ${EOS_SRC}
    ${ATOMS_SRC}
    ${REFLECTIONS_SRC}
    ${PROPAGK_SRC}
    ${IOFORM_SRC}
    ${GEOM_SRC}
    ${MAPS_SRC}
    ${OPT_SRC}
    ${OPT_LSQ_SRC}
    ${OPT_SAN_SRC}
    ${Export_VTK_SRC}
    ${EnBVS_SRC}
    ${InstrmILL_SRC}
    ${SXTALgeom_SRC}
    ${SF_SRC}
    ${MOLECULES_SRC}
    ${KEYCODES_SRC}
    ${KVECSYMM_SRC}
    ${KVECSTRF_SRC}
    ${KVECPOLAR_SRC}
    ${KWCPARSER_SRC}
    ${VTK_SRC}
    ${PYTHON_SRC}
    ${POWDER_SRC}
    ${UTILITIES_SRC}
	${WRAPS_UTILS_SRC}
    ${LAUE_SRC})

# =====
# BUILD
# =====

add_library(CrysFML08 STATIC ${CRYSFML08_SRC})

# The directory where the CrysFML modules files will be stored.
set(CRYSFML08_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/crysfml08 CACHE INTERNAL "")

# Sets the path where to place the mod files for the crysfml_common library.
set_target_properties(CrysFML08 PROPERTIES Fortran_MODULE_DIRECTORY ${CRYSFML08_MODULE_DIRECTORY})
