set(SOURCES_NFP
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_globals.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_read_files.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_Reflections.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_Simulation.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_Optimization.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/Src/nFP_main.f90
)

add_executable(nFP ${SOURCES_NFP})

# Include directories
target_include_directories(nFP PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../../crysfml08)

# External libraries
target_link_libraries(nFP PRIVATE CrysFML08)