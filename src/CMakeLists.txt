cmake_minimum_required(VERSION 3.12..3.19)

# Old policy - required for empty targets below
cmake_policy(SET CMP0115 OLD)

# Empty targets that will be populated in subdirectories
add_executable(lagrit_exe)
add_library(lagrit_lib)
add_dependencies(lagrit_exe lagrit_lib)

add_subdirectory(lg_interface)
add_subdirectory(lg_util)
add_subdirectory(lg_core)

# Enable Fortran processor directives
# This should be in lg_core/CMakeLists.txt, but
# must be here for scope reasons
set_property(
    SOURCE
        ${CMAKE_CURRENT_LIST_DIR}/lg_core/dumpexodusII.f
    PROPERTY
        Fortran_PREPROCESS ON)

# Properties for LaGriT library
set_target_properties(lagrit_lib
  PROPERTIES
    OUTPUT_NAME "lagrit"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}")

# Properties for LaGriT executable
set_target_properties(lagrit_exe
  PROPERTIES
    OUTPUT_NAME "lagrit"
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/")

if(WIN32)
    lagrit_config_windows(lagrit_exe)
    lagrit_config_windows(lagrit_lib)
endif()

target_link_libraries(lagrit_exe lagrit_lib)

#if(NOT ${BUILD_SHARED_LIBS})
#    set_target_properties(lagrit_exe PROPERTIES LINK_SEARCH_START_STATIC 1)
#    set_target_properties(lagrit_exe PROPERTIES LINK_SEARCH_END_STATIC 1)
#endif()

install(
    TARGETS
        lagrit_lib
        lagrit_exe
    LIBRARY
        DESTINATION lib
    ARCHIVE
        DESTINATION lib
    RUNTIME
        DESTINATION bin
    INCLUDES
        DESTINATION include)
