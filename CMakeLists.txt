cmake_minimum_required(VERSION 3.13)

project(
    LaGriT
    VERSION         3.3.3
    DESCRIPTION     "LaGriT Meshing Utilities"
    LANGUAGES       Fortran CXX C)

option(LG_ENABLE_EXODUS
    "Set to ON to enable ExodusII mesh output"
    OFF)

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules/")
include(cmake/LaGriT.cmake)

# Only do these if this is the main project, and not if it is included through add_subdirectory
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    # Testing only available if this is the main app
    # Note this needs to be done in the main CMakeLists
    # since it calls enable_testing, which must be in the
    # main CMakeLists.
    include(CTest)

    if((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME OR MODERN_CMAKE_BUILD_TESTING) AND BUILD_TESTING)
        add_subdirectory(test)
    endif()
endif()

include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/DetectBitSize.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-Fortran.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-C.cmake")

add_subdirectory(src)
#add_subdirectory(examples)
