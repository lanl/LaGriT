cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(LaGriT VERSION 3.3.3
               DESCRIPTION "LaGriT Meshing Utilities"
               LANGUAGES Fortran CXX C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

# ===== Detect platform, bit-size, and compilers ==================== #
message(STATUS "Detected System:")
include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/DetectBitSize.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-Fortran.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-C.cmake")

# ////////// BEGIN COMPILER SETTINGS //////////

## ////////// BEGIN LINKER CONFIGURATION //////////
# Extra linker and compile options for Windows
if(MSVC)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Dwin64")
    set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>
    )
endif()

string(TIMESTAMP CMAKE_DATE_COMPILE "%Y/%m/%d")
configure_file(
    ${SRC_CORE}/lagrit.h.in
    ${SRC_CORE}/lagrit.h
    @ONLY
)

# ////////// BEGIN TARGET CONFIGURATION //////////
# Grab `lg_util/` source files
file(
    GLOB_RECURSE
    SRC_UTIL_FILES
    "${SRC_UTIL}/*.c"
    "${SRC_UTIL}/*.cpp"
    "${SRC_UTIL}/*.f"
    "${SRC_UTIL}/*.F"
    "${SRC_UTIL}/*.f90"
)

# Grab `lg_core/` source files
file(
    GLOB_RECURSE
    SRC_CORE_FILES
    "${SRC_CORE}/*.c"
    "${SRC_CORE}/*.cpp"
    "${SRC_CORE}/*.f"
    "${SRC_CORE}/*.F"
    "${SRC_CORE}/*.f90"
)

# Files we don't want compiled
list(FILTER SRC_UTIL_FILES EXCLUDE REGEX ".*mm2000_header.F$")
list(FILTER SRC_CORE_FILES EXCLUDE REGEX ".*lagrit_main.f$")

# This file has the `#ifdef EXODUS` preprocessor directive
# The -cpp flag ensures the compiler can process those directives
set_source_files_properties(
    "${SRC_CORE}/dumpexodusII.f"
    PROPERTIES COMPILE_OPTIONS "-cpp"
)

# Compile the main LaGriT binary
add_executable(lagrit
  ${SRC_CORE}/lagrit_main.f
)

# Compile LaGriT routines as a library
add_library(liblagrit
    ${LIBLAGRIT_BUILD_TYPE}
    ${SRC_UTIL_FILES}
    ${SRC_CORE_FILES}
)

# Set the shared library version
set_target_properties(liblagrit
  PROPERTIES
    OUTPUT_NAME "lagrit"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}"
)

set_target_properties(lagrit
  PROPERTIES
    OUTPUT_NAME "lagrit"
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
)

target_include_directories(lagrit
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${TPL_INCLUDE_DIRS}
)

target_link_libraries(lagrit
  PRIVATE
    liblagrit
  PUBLIC
    ${TPL_LIBRARIES}
)
