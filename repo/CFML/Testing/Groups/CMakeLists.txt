set(SOURCES_GROUPS08
    ${CMAKE_CURRENT_SOURCE_DIR}/groups08.f90
)

add_executable(groups08 ${SOURCES_GROUPS08})

# Include directories
target_include_directories(groups08 PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../../crysfml08)

# External libraries
target_link_libraries(groups08 PRIVATE CrysFML08)