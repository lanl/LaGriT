cmake_minimum_required(VERSION 3.13)

project(LaGriT
    VERSION
        3.3.3
    DESCRIPTION
        "LaGriT Meshing Utilities"
    LANGUAGES
        Fortran CXX C)

option(LG_ENABLE_EXODUS
    "Set to ON to enable ExodusII mesh output"
    OFF)

set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/DetectBitSize.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-Fortran.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-C.cmake")

add_subdirectory(src)
