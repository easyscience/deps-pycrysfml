# Canonical pyCFML manifest scaffold for the repo-owned build.
#
# The wrapper ordering mirrors the currently validated pybuild.toml manifest
# rather than the vendored PythonAPI/CMakeLists.txt, which has already drifted.

set(PYCFML_FORTRAN_SOURCE_ROOT "${CRYSFML_VENDOR_PYTHON_API_ROOT}/Fortran")
set(PYCFML_PYTHON_SOURCE_ROOT "${CRYSFML_VENDOR_PYTHON_API_ROOT}/Python")

set(PYCFML_EXTENSION_ENTRY_SOURCE
    "${PYCFML_FORTRAN_SOURCE_ROOT}/${CRYSFML_EXTENSION_MODULE_NAME}.f90"
)
set(PYCFML_WRAPPER_ENTRY_SOURCE
    "${PYCFML_FORTRAN_SOURCE_ROOT}/CFML_Wraps.f90"
)

set(PYCFML_WRAP_SOURCE_NAMES
    Wraps_Atoms
    Wraps_Bonds_Tables
    Wraps_Strings
    Wraps_BckPeaks
    Wraps_Scattering_Tables
    Wraps_Structure_Factors
    Wraps_Geom
    Wraps_Metrics
    Wraps_ExtinCorr
    Wraps_Symmetry_Tables
    Wraps_DiffPatt
    Wraps_IOForm
    Wraps_Molecules
    Wraps_Propagation_Vectors
    Wraps_ILL_Instrm_Data
    Wraps_Reflections
    Wraps_BVS_Tables
    Wraps_EnBVS
    Wraps_SXTAL_Geom
    Wraps_Simulated_Annealing
    Wraps_Profiles
    Wraps_Rational
    Wraps_gSpaceGroups
    Wraps_kvec_Symmetry
    Wraps_Powder
    Wraps_Utilities
    Wraps_Py_Utilities
)

set(PYCFML_PYTHON_MODULE_NAMES
    cfml_bckpeaks
    cfml_diffpatt
    cfml_gspacegroups
    cfml_ioform
    cfml_metrics
    cfml_py_utilities
    cfml_reflections
    cfml_scattering_tables
    cfml_symmetry_tables
)

set(PYCFML_FORTRAN_SOURCES
    "${PYCFML_EXTENSION_ENTRY_SOURCE}"
    "${PYCFML_WRAPPER_ENTRY_SOURCE}"
)
foreach(_wrap_name IN LISTS PYCFML_WRAP_SOURCE_NAMES)
    list(APPEND PYCFML_FORTRAN_SOURCES
         "${PYCFML_FORTRAN_SOURCE_ROOT}/CFML_Wraps/${_wrap_name}.f90")
endforeach()

set(PYCFML_PYTHON_SOURCES "")
foreach(_module_name IN LISTS PYCFML_PYTHON_MODULE_NAMES)
    list(APPEND PYCFML_PYTHON_SOURCES
         "${PYCFML_PYTHON_SOURCE_ROOT}/${_module_name}.py")
endforeach()

foreach(_path IN LISTS PYCFML_FORTRAN_SOURCES PYCFML_PYTHON_SOURCES)
    if(NOT EXISTS "${_path}")
        message(FATAL_ERROR "Expected pyCFML source is missing: ${_path}")
    endif()
endforeach()

unset(_module_name)
unset(_path)
unset(_wrap_name)