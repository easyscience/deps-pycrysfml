cmake_minimum_required(VERSION 3.25)

project(crysfml_packaging LANGUAGES NONE)

option(CRYSFML_ENABLE_PYTHON_PACKAGE "Expose the Python package scaffold in the repo-owned build." ON)
option(CRYSFML_ENABLE_VENDOR_TESTS "Expose vendored CFML test-program manifests in the repo-owned build." OFF)
option(CRYSFML_ENABLE_VENDOR_ODR "Reserve space for the vendored ODR build in the repo-owned build." OFF)

set(CRYSFML_VENDOR_ROOT
    "${CMAKE_CURRENT_SOURCE_DIR}/repo/CFML"
    CACHE PATH
    "Path to the vendored CrysFML source tree."
)
set(CRYSFML_VENDOR_SRC_ROOT
    "${CRYSFML_VENDOR_ROOT}/Src"
    CACHE PATH
    "Path to the vendored CrysFML Fortran sources."
)
set(CRYSFML_VENDOR_PYTHON_API_ROOT
    "${CRYSFML_VENDOR_ROOT}/PythonAPI"
    CACHE PATH
    "Path to the vendored pyCFML source tree."
)
set(CRYSFML_CURATED_PACKAGE_INIT
    "${CMAKE_CURRENT_SOURCE_DIR}/src/__init__.py"
    CACHE FILEPATH
    "Current curated package init used by the release build."
)
set(CRYSFML_PACKAGE_NAME
    "crysfml"
    CACHE STRING
    "Installed Python package name."
)
set(CRYSFML_EXTENSION_MODULE_NAME
    "crysfml08lib"
    CACHE STRING
    "Binary extension module name."
)
set(CRYSFML_DATABASE_FILENAME
    "magnetic_data.txt"
    CACHE STRING
    "Bundled CFML database filename."
)
set(CRYSFML_CURATED_DATABASE_FILE
    "${CRYSFML_VENDOR_SRC_ROOT}/Databases/${CRYSFML_DATABASE_FILENAME}"
    CACHE FILEPATH
    "Bundled CFML database file."
)

function(crysfml_require_existing_path path_var description)
    if(NOT EXISTS "${${path_var}}")
        message(FATAL_ERROR "${description} was not found at '${${path_var}}'")
    endif()
endfunction()

crysfml_require_existing_path(CRYSFML_VENDOR_ROOT "Vendored CFML root")
crysfml_require_existing_path(CRYSFML_VENDOR_SRC_ROOT "Vendored CFML source root")
crysfml_require_existing_path(CRYSFML_VENDOR_PYTHON_API_ROOT "Vendored pyCFML source root")
crysfml_require_existing_path(CRYSFML_CURATED_PACKAGE_INIT "Curated package init")
crysfml_require_existing_path(CRYSFML_CURATED_DATABASE_FILE "Bundled CFML database")

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CfmlSourceManifest.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/PyCfmlSourceManifest.cmake")

list(LENGTH CRYSFML_CORE_MODULE_KEYS _crysfml_core_module_count)
list(LENGTH CRYSFML_CORE_SOURCES _crysfml_core_source_count)
list(LENGTH CRYSFML_GLOBAL_DEPS_VARIANTS _crysfml_global_deps_count)
list(LENGTH CRYSFML_VENDOR_TEST_PROGRAM_SOURCES _crysfml_vendor_test_program_count)
list(LENGTH PYCFML_FORTRAN_SOURCES _pycfml_fortran_source_count)
list(LENGTH PYCFML_PYTHON_SOURCES _pycfml_python_source_count)

message(STATUS "Configured repo-owned crysfml transition scaffold")
message(STATUS "  Vendor root             : ${CRYSFML_VENDOR_ROOT}")
message(STATUS "  Core source root        : ${CRYSFML_VENDOR_SRC_ROOT}")
message(STATUS "  Python API root         : ${CRYSFML_VENDOR_PYTHON_API_ROOT}")
message(STATUS "  Package name            : ${CRYSFML_PACKAGE_NAME}")
message(STATUS "  Extension module        : ${CRYSFML_EXTENSION_MODULE_NAME}")
message(STATUS "  Global deps variants    : ${_crysfml_global_deps_count}")
message(STATUS "  Core module entries     : ${_crysfml_core_module_count}")
message(STATUS "  Core source files       : ${_crysfml_core_source_count}")
message(STATUS "  Vendored test sources   : ${_crysfml_vendor_test_program_count}")
message(STATUS "  pyCFML Fortran sources  : ${_pycfml_fortran_source_count}")
message(STATUS "  pyCFML Python sources   : ${_pycfml_python_source_count}")
message(STATUS "  Build scaffold only     : native targets land in the next migration slice")

set(CRYSFML_TRANSITION_SCAFFOLD_SOURCES
    "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt"
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/CfmlSourceManifest.cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/PyCfmlSourceManifest.cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/docs/proper-build-transition.md"
    "${CRYSFML_CURATED_PACKAGE_INIT}"
    ${CRYSFML_GLOBAL_DEPS_VARIANTS}
    ${CRYSFML_CORE_SOURCES}
    ${CRYSFML_VENDOR_TEST_PROGRAM_SOURCES}
    ${PYCFML_FORTRAN_SOURCES}
    ${PYCFML_PYTHON_SOURCES}
)

add_custom_target(crysfml_transition_scaffold SOURCES ${CRYSFML_TRANSITION_SCAFFOLD_SOURCES})