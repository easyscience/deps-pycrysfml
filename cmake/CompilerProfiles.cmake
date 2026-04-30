function(crysfml_set_default_build_type default_build_type)
    if(CMAKE_CONFIGURATION_TYPES OR CMAKE_BUILD_TYPE)
        return()
    endif()

    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo MinSizeRel)
endfunction()

function(crysfml_select_global_deps_source out_var)
    if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        message(FATAL_ERROR
                "cfml_core currently supports GNU Fortran only; got '${CMAKE_Fortran_COMPILER_ID}'")
    endif()

    if(WIN32)
        set(_selected "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Windows_GFOR.f90")
    elseif(APPLE)
        set(_selected "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_MacOS_GFOR.f90")
    elseif(UNIX)
        set(_selected "${CRYSFML_VENDOR_SRC_ROOT}/CFML_GlobalDeps_Linux_GFOR.f90")
    else()
        message(FATAL_ERROR "Unsupported platform '${CMAKE_SYSTEM_NAME}' for cfml_core")
    endif()

    if(NOT EXISTS "${_selected}")
        message(FATAL_ERROR "Selected CFML global-deps source is missing: ${_selected}")
    endif()

    set(${out_var} "${_selected}" PARENT_SCOPE)
endfunction()

function(crysfml_apply_cfml_core_profile target_name)
    if(NOT TARGET "${target_name}")
        message(FATAL_ERROR "Target '${target_name}' does not exist")
    endif()

    if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        message(FATAL_ERROR
                "cfml_core currently supports GNU Fortran only; got '${CMAKE_Fortran_COMPILER_ID}'")
    endif()

    target_compile_options(${target_name}
        PRIVATE
            -cpp
            -fdec-math
            -ffree-line-length-none
            -fno-stack-arrays
            -fPIC
            $<$<CONFIG:Debug>:-O0>
            $<$<NOT:$<CONFIG:Debug>>:-O2>
    )
endfunction()