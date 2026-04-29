set(SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/D2B_data_mod.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/D2B_int_mod.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/D2B_read_mod.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/GetData_Globals.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Get_data.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/../../HDF5/Nexus_Mod.f90
)

add_executable(get_data ${SOURCES})

# Include directories
target_include_directories(get_data PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../../crysfml08 ${HDF5_INC}) 

# External libraries
target_link_libraries(get_data PRIVATE CrysFML08 ${HDF5_LIB})
if (UNIX OR APPLE)
    find_package(ZLIB)
    if(ZLIB_FOUND)
        target_link_libraries(get_data PRIVATE ZLIB::ZLIB)
    else()
        message(FATAL_ERROR "ZLIB library not found in your system. Please install it.")
    endif()
elseif(WIN32)
    # These libraries come with the HDF5 binaries distribution in Windows
    target_link_libraries(get_data PRIVATE libszip libzlib)
endif()



