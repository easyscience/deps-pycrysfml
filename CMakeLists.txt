cmake_minimum_required(VERSION 3.25)

project(crysfml_packaging LANGUAGES NONE)

option(CRYSFML_ENABLE_CORE_TARGET "Build the repo-owned cfml_core target." ON)
option(CRYSFML_ENABLE_PYCFML_EXTENSION "Build the repo-owned pycfml_extension target." ON)
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

if(CRYSFML_ENABLE_CORE_TARGET OR CRYSFML_ENABLE_PYCFML_EXTENSION)
    if(NOT DEFINED CMAKE_Fortran_COMPILER)
        find_program(CRYSFML_DEFAULT_GFORTRAN_EXECUTABLE NAMES gfortran)
        if(CRYSFML_DEFAULT_GFORTRAN_EXECUTABLE)
            set(CMAKE_Fortran_COMPILER
                "${CRYSFML_DEFAULT_GFORTRAN_EXECUTABLE}"
                CACHE FILEPATH
                "Default Fortran compiler for the repo-owned cfml_core target."
            )
        endif()
    endif()
    enable_language(Fortran)
endif()

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CfmlSourceManifest.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CompilerProfiles.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/PyCfmlSourceManifest.cmake")

if(CRYSFML_ENABLE_CORE_TARGET OR CRYSFML_ENABLE_PYCFML_EXTENSION)
    crysfml_set_default_build_type(Release)
endif()

if(CRYSFML_ENABLE_CORE_TARGET)
    crysfml_select_global_deps_source(CRYSFML_SELECTED_GLOBAL_DEPS_SOURCE)

    set(CRYSFML_CORE_BUILD_SOURCES
        "${CRYSFML_SELECTED_GLOBAL_DEPS_SOURCE}"
        ${CRYSFML_CORE_SOURCES}
    )

    add_library(cfml_core STATIC ${CRYSFML_CORE_BUILD_SOURCES})
    add_library(crysfml::cfml_core ALIAS cfml_core)

    set_target_properties(cfml_core PROPERTIES
        OUTPUT_NAME CrysFML08
        POSITION_INDEPENDENT_CODE ON
        Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/cfml_core"
    )
    target_include_directories(cfml_core
        PUBLIC
            "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules/cfml_core>"
    )
    crysfml_apply_gnu_fortran_target_profile(cfml_core)
endif()

if(CRYSFML_ENABLE_PYCFML_EXTENSION)
    if(NOT CRYSFML_ENABLE_CORE_TARGET)
        message(FATAL_ERROR "pycfml_extension requires CRYSFML_ENABLE_CORE_TARGET=ON")
    endif()

    find_package(Python COMPONENTS Interpreter Development.Module REQUIRED)

    add_library(pycfml_extension MODULE ${PYCFML_FORTRAN_SOURCES})
    add_library(crysfml::pycfml_extension ALIAS pycfml_extension)

    set_target_properties(pycfml_extension PROPERTIES
        OUTPUT_NAME "${CRYSFML_EXTENSION_MODULE_NAME}"
        POSITION_INDEPENDENT_CODE ON
        PREFIX ""
        Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/pycfml_extension"
    )
    if(WIN32)
        set_target_properties(pycfml_extension PROPERTIES SUFFIX ".pyd")
    else()
        set_target_properties(pycfml_extension PROPERTIES SUFFIX ".so")
    endif()

    target_link_libraries(pycfml_extension
        PRIVATE
            crysfml::cfml_core
            Python::Module
    )
    crysfml_apply_gnu_fortran_target_profile(pycfml_extension)
endif()

list(LENGTH CRYSFML_CORE_MODULE_KEYS _crysfml_core_module_count)
list(LENGTH CRYSFML_CORE_SOURCES _crysfml_core_source_count)
list(LENGTH CRYSFML_GLOBAL_DEPS_VARIANTS _crysfml_global_deps_count)
list(LENGTH CRYSFML_VENDOR_TEST_PROGRAM_SOURCES _crysfml_vendor_test_program_count)
list(LENGTH PYCFML_FORTRAN_SOURCES _pycfml_fortran_source_count)
list(LENGTH PYCFML_PYTHON_SOURCES _pycfml_python_source_count)
if(CRYSFML_ENABLE_CORE_TARGET)
    list(LENGTH CRYSFML_CORE_BUILD_SOURCES _crysfml_core_build_source_count)
endif()
if(CRYSFML_ENABLE_PYCFML_EXTENSION)
    list(LENGTH PYCFML_FORTRAN_SOURCES _pycfml_extension_build_source_count)
endif()

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
if(CRYSFML_ENABLE_CORE_TARGET)
    message(STATUS "  Selected global deps    : ${CRYSFML_SELECTED_GLOBAL_DEPS_SOURCE}")
    message(STATUS "  cfml_core build inputs  : ${_crysfml_core_build_source_count}")
    message(STATUS "  cfml_core compiler      : ${CMAKE_Fortran_COMPILER_ID}")
endif()
if(CRYSFML_ENABLE_PYCFML_EXTENSION)
    message(STATUS "  pycfml_extension inputs : ${_pycfml_extension_build_source_count}")
    message(STATUS "  Python interpreter      : ${Python_EXECUTABLE}")
else()
    message(STATUS "  Build scaffold only     : native targets are disabled")
endif()

set(CRYSFML_TRANSITION_SCAFFOLD_SOURCES
    "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt"
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/CfmlSourceManifest.cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/CompilerProfiles.cmake"
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