set(SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/crysfml08lib.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Atoms.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_BckPeaks.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Bonds_Tables.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_BVS_Tables.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_DiffPatt.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_EnBVS.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_ExtinCorr.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Geom.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_gSpaceGroups.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_ILL_Instrm_Data.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_IOForm.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_kvec_Symmetry.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Laue.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Metrics.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Molecules.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Profiles.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Propagation_Vectors.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Py_Utilities.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Rational.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Reflections.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Scattering_Tables.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Simulated_Annealing.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Strings.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Structure_Factors.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_SXTAL_Geom.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Symmetry_Tables.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Fortran/CFML_Wraps/Wraps_Utilities.f90
)

add_library(crysfml08lib SHARED ${SOURCES})

# Suffix for Python extension modules
if (WIN32)
    set_target_properties(crysfml08lib PROPERTIES
        PREFIX ""                            # Remove prefix "lib" from library name
        SUFFIX ".pyd"                        # Extension ".pyd" instead of ".dll"
    )
else()
    set_target_properties(crysfml08lib PROPERTIES
        PREFIX ""                            # Remove prefix "lib" from library name
        SUFFIX ".so"                         # Extension ".so" instead of ".dylib"
    )
endif()

# Include directories
target_include_directories(crysfml08lib PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../crysfml08)

# External libraries
if (WIN32 OR APPLE)
    target_link_libraries(crysfml08lib PRIVATE CrysFML08 ${PYTHON_LIBRARY_PATH})
elseif (UNIX)
    target_link_libraries(crysfml08lib PRIVATE CrysFML08)
endif()