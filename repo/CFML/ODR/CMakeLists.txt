set(ODR_SP_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/lpkbls.f
    ${CMAKE_CURRENT_SOURCE_DIR}/odr.f
    ${CMAKE_CURRENT_SOURCE_DIR}/ODR_wrapper.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/sp_precision.f
)

set(ODR_DP_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/lpkbls.f
    ${CMAKE_CURRENT_SOURCE_DIR}/odr.f
    ${CMAKE_CURRENT_SOURCE_DIR}/dp_precision.f
)

add_library(odr_sp STATIC ${ODR_SP_SOURCES})
add_library(odr_dp STATIC ${ODR_SP_SOURCES})

# Compile and link options
if (CMAKE_BUILD_TYPE STREQUAL Debug)
    target_compile_options(odr_sp PRIVATE ${Fortran_COMPILER_FLAGS_DEBUG})
    target_link_options(odr_sp PRIVATE ${Fortran_LINKER_FLAGS_DEBUG})
    target_compile_options(odr_dp PRIVATE ${Fortran_COMPILER_FLAGS_DEBUG})
    target_link_options(odr_dp PRIVATE ${Fortran_LINKER_FLAGS_DEBUG})
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    target_compile_options(odr_sp PRIVATE ${Fortran_COMPILER_FLAGS_RELEASE} ${Fortran_COMPILER_FLAGS_OPT})
    target_link_options(odr_sp PRIVATE ${Fortran_LINKER_FLAGS_RELEASE})
    target_compile_options(odr_dp PRIVATE ${Fortran_COMPILER_FLAGS_RELEASE} ${Fortran_COMPILER_FLAGS_OPT})
    target_link_options(odr_dp PRIVATE ${Fortran_LINKER_FLAGS_RELEASE})
endif()

# The directory where the ODR modules files will be stored.
set(ODR_SP_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/odr_sp CACHE INTERNAL "")
set(ODR_DP_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/odr_dp CACHE INTERNAL "")
set_target_properties(odr_sp PROPERTIES Fortran_MODULE_DIRECTORY ${ODR_SP_MODULE_DIRECTORY})
set_target_properties(odr_dp PROPERTIES Fortran_MODULE_DIRECTORY ${ODR_DP_MODULE_DIRECTORY})

# Include directories
target_include_directories(odr_sp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../crysfml08)
target_include_directories(odr_dp PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../crysfml08)

# External libraries
target_link_libraries(odr_sp PRIVATE CrysFML08)
target_link_libraries(odr_dp PRIVATE CrysFML08)