cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

option(PYTHON_API "Build Python API" OFF)
option(QPARALLEL "qparallel option for ifort on Windows" ON)
option(MD "Use CRT dynamic library in Windows. Needed to link against HDF5 libraries" OFF)
option(ODR "Build ODR library" OFF)
option(TESTING "Compile Testing programs" OFF)

if(DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} CACHE STRING "The Fortran compiler. One of 'ifort', 'ifx' or 'gfortran'.")
else()
    set(CMAKE_Fortran_COMPILER ifx CACHE STRING "The Fortran compiler. One of 'ifort' 'ifx' or 'gfortran'.")
endif()
message(STATUS "Setting compiler to ${CMAKE_Fortran_COMPILER}")

if(DEFINED CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "The build type. One of 'Release' or 'Debug'.")
else()
    set(CMAKE_BUILD_TYPE Release CACHE STRING "The build type. One of 'Release' or 'Debug'.")
endif()
message(STATUS "Setting build type to ${CMAKE_BUILD_TYPE}")

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME_WE)

#set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if (DEFINED CMAKE_INSTALL_PREFIX)
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE PATH "The installation path.")
else()
    set(CMAKE_INSTALL_PREFIX ${CMAKE_HOME_DIRECTORY}/${COMPILER_NAME} CACHE PATH "The installation path.")
endif()
message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")

# Variable HDF5: directory where HDF5 libraries are stored
if(DEFINED HDF5)
    set(HDF5 ${HDF5} CACHE PATH "Path to HDF5 installation directory" FORCE)
    # Include and link directories
    if (WIN32)
        set(HDF5_INC ${HDF5}/include/static)
        set(HDF5_LIB libhdf5_fortran libhdf5_f90cstub libhdf5)
    elseif (APPLE)
        set(HDF5_INC ${HDF5}/include)
        set(HDF5_LIB hdf5_fortran hdf5)
    elseif (UNIX)
        set(HDF5_INC ${HDF5}/mod/static)
        set(HDF5_LIB hdf5_fortran hdf5_f90cstub hdf5)
    endif()
    link_directories(${HDF5}/lib)
    message(STATUS "HDF5 set to ${HDF5}")
    message(STATUS "HDF5_INC set to ${HDF5_INC}")
    message(STATUS "Link directory for CrysFML08 set to ${CRYSFML08}/lib")
    message(STATUS "Link directory for HDF5 set to ${HDF5}/lib")
endif()


# =======
# PROJECT
# =======

# The project is declared. It is a Fortran project.
project(libcrysfml Fortran)

# Disable verbose mode.
set(CMAKE_VERBOSE_MAKEFILE OFF)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake)
include(set_compiler_flags)
set_compiler_flags()

# =====
# BUILD
# =====

# CrysFML08 build
add_subdirectory(Src)

# ODR build
if (ODR)
    add_subdirectory(ODR)
endif()

# Testing programs
if (TESTING)
    add_subdirectory(Testing)
endif()

# Python API build
if (PYTHON_API)
    message(STATUS "Python API will be built")
    if (WIN32 OR APPLE)
        if (NOT PYTHON_INTERPRETER_PATH OR NOT PYTHON_LIBRARY_PATH)
            set(Python3_USE_STATIC_LIBS, TRUE)
            find_package(Python3 COMPONENTS Interpreter Development)
            set(PYTHON_INTERPRETER_PATH ${Python3_EXECUTABLE})
            set(PYTHON_LIBRARY_PATH ${Python3_LIBRARIES})
            #message(STATUS "Python interpreter found: " ${PYTHON_INTERPRETER_PATH})
            message(STATUS "Python library     found: " ${PYTHON_LIBRARY_PATH})
        else()
            #message(STATUS "Python interpreter provided: " ${PYTHON_INTERPRETER_PATH})
            message(STATUS "Python library     provided: " ${PYTHON_LIBRARY_PATH})
        endif()
    endif()
    add_subdirectory(PythonAPI)
    #execute_process(COMMAND "${CMAKE_COMMAND}" "-E" "env"  CRYSFML08=${CMAKE_HOME_DIRECTORY} ${Python3_EXECUTABLE} apigen.py
    #--build=${CMAKE_BINARY_DIR}/api --cmake=True --verbose=False WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/Scripts/PythonAPI)
endif()

# =======
# INSTALL
# =======

# CrysFML08 Library
install(TARGETS CrysFML08 DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install(DIRECTORY ${CRYSFML08_MODULE_DIRECTORY}/ # last / matters
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include
        FILES_MATCHING
        PATTERN "*.*mod"
        PATTERN CMakeFiles EXCLUDE)
# ODR Library
if (ODR)
    install(TARGETS odr_sp DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
    install(TARGETS odr_dp DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
    install(DIRECTORY ${ODR_SP_MODULE_DIRECTORY}/ # last / matters
            DESTINATION ${CMAKE_INSTALL_PREFIX}/include/odr_sp
            FILES_MATCHING
            PATTERN "*.*mod"
            PATTERN CMakeFiles EXCLUDE)
    install(DIRECTORY ${ODR_DP_MODULE_DIRECTORY}/ # last / matters
            DESTINATION ${CMAKE_INSTALL_PREFIX}/include/odr_dp
            FILES_MATCHING
            PATTERN "*.*mod"
            PATTERN CMakeFiles EXCLUDE)
endif()

# Testing programs
if (TESTING)
    install(TARGETS groups08 DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
    install(TARGETS nFP DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
    if (DEFINED HDF5)
        install(TARGETS get_data DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
    endif()
endif()

# Python API
if (PYTHON_API)
    install(TARGETS crysfml08lib DESTINATION ${CMAKE_INSTALL_PREFIX}/pycrysfml)
    file(GLOB PY_FILES ${CMAKE_CURRENT_SOURCE_DIR}/PythonAPI/Python/*.py)
    install(FILES ${PY_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/pycrysfml)
endif()


