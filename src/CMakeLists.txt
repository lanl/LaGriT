cmake_minimum_required(VERSION 3.12..3.19)

# Force old policy - required for empty targets below
#cmake_policy(SET CMP0115 OLD)

set(CMAKE_CXX_STANDARD 11)

# Empty targets that will be populated in subdirectories
if(LG_BUILD_AS_LIBRARY)
    add_library(lagrit)

    # Properties for LaGriT library
    set_target_properties(lagrit
      PROPERTIES
        OUTPUT_NAME     lagrit
        VERSION         ${PROJECT_VERSION}
        SOVERSION       ${PROJECT_VERSION_MAJOR})

    #set(CMAKE_POSITION_INDEPENDENT_CODE ON)
else()
    add_executable(lagrit)

    # Properties for LaGriT executable
    set_target_properties(lagrit
      PROPERTIES
        OUTPUT_NAME                 lagrit
        Fortran_MODULE_DIRECTORY    "${CMAKE_CURRENT_BINARY_DIR}/modules/")
endif()

if(NOT ${BUILD_SHARED_LIBS})
    set_target_properties(lagrit
        PROPERTIES
            LINK_SEARCH_START_STATIC    1
            LINK_SEARCH_END_STATIC      1)
endif()

if(WIN32)
    lagrit_config_windows(lagrit)
endif()

add_subdirectory(lg_interface)
add_subdirectory(lg_util)
add_subdirectory(lg_core)

if(LG_BUILD_EXAMPLES AND LG_BUILD_AS_LIBRARY)
    add_subdirectory(examples)
endif()

install(
    TARGETS
        lagrit
    LIBRARY
        DESTINATION lib
    PUBLIC_HEADER
        DESTINATION include
)
