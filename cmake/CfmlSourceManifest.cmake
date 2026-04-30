# Canonical grouped manifest scaffold for the repo-owned CFML core build.
#
# The currently validated source ordering lives in pybuild.toml. This scaffold
# keeps the grouping and platform-specific global-deps filenames explicit so the
# next migration slice can replace the current script-generated build with
# reviewable target-based CMake lists.

set(CRYSFML_CORE_MANIFEST_IS_SCAFFOLD ON)

# The vendored Intel macOS file really uses the historical "MacOs" spelling.
set(CRYSFML_GLOBAL_DEPS_VARIANTS
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Linux_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Linux_INTEL.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_MacOS_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_MacOs_INTEL.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Windows_GFOR.f90"
    "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Windows_INTEL.f90"
)

set(CRYSFML_CORE_SOURCE_GROUP_NAMES
    messages
    maths_fft_random
    strings
    rational
    metrics
    tables
    gspacegroups
    profiles
    diffpatt
    bckpeaks
    extincorr
    eos
    atoms
    reflections
    propagation_vectors
    ioform
    keycodes
    vtk
    export_vtk
    enbvs
    sxtal_geom
    structure_factors
    molecules
    keyword_code_parser
    utilities
    forpy
    wraps_utils
    py_utilities
    powder
    laue
)

set(CRYSFML_VENDOR_TEST_PROGRAM_KEYS
    "Bond_Str/Bond_StrN"
    "CIFs/io_files"
    "Grp_230/groups_230"
    "hkl_gen/hkl_gen"
    "hkl_gen/simple_hkl_gen"
    "Molecules/mol_tpcr"
    "PowderPattern/powder_pattern"
    "SpaceGroups/space_groups"
    "StructureFactors/Calc_Sfac"
)

foreach(_path IN LISTS CRYSFML_GLOBAL_DEPS_VARIANTS)
    if(NOT EXISTS "${_path}")
        message(FATAL_ERROR "Expected vendored CFML global-deps variant is missing: ${_path}")
    endif()
endforeach()

unset(_path)