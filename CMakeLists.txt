# 1. Get tests running
# 2. Get Exodus working
# 3. Get CI set up to build/test on all platforms
# 4. Message Shaoping about being able to compile

cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(LaGriT VERSION 3.3.0
               DESCRIPTION "LaGriT Meshing Utilities"
               LANGUAGES Fortran CXX C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

option(LaGriT_BUILD_STATIC "Compile as a static (fat) executable." ON)

message(STATUS "==========================================")
message(STATUS "============Configuring LaGriT============")
message(STATUS "===================v${PROJECT_VERSION}=================")

message(STATUS "Compile LaGriT as a static binary = ${LaGriT_BUILD_STATIC}")

# ==== Is SEACAS/ExodusII available? =========== #
find_package(Exodus)
find_package(NetCDF)

if (Exodus_FOUND)
  message(STATUS "ExodusII support enabled: ${Exodus_LIBRARIES}")
  add_definitions(-DLAGRIT_INCLUDE_EXODUSII)
else()
  message("WARNING: SEACAS/ExodusII could not be found. Compiling without ExodusII support.")
endif()

include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")

# === FORTAN COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/FortranSettings.cmake")
# === C COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/CSettings.cmake")
# === C++ COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/CXXSettings.cmake")

#include(FortranCInterface)
#FortranCInterface_VERIFY(CXX)
#FortranCInterface_HEADER(
#    LaGriT.h
#    MACRO_NAMESPACE "LG_"
#    SYMBOL_NAMESPACE "LG_"
#    SYMBOLS mysub mymod:my_sub
#)

if (LaGriT_BUILD_STATIC)
  set(LIBLAGRIT_BUILD_TYPE "STATIC")
else()
  set(LIBLAGRIT_BUILD_TYPE "SHARED")
  set(BUILD_SHARED_LIBS TRUE)
endif()

set(INSTALL_DIR "${CMAKE_SOURCE_DIR}/install")
set(SRC_UTIL "${CMAKE_SOURCE_DIR}/lg_util/src")
set(SRC_CORE "${CMAKE_SOURCE_DIR}/src")

# === DETECT BIT SIZE ======================== #
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  message(STATUS "Architecture: 64-bit")
  configure_file("${SRC_UTIL}/mm2000_m64.h" "${SRC_UTIL}/mm2000.h" COPYONLY)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(STATUS "Architecture: 32-bit")
  configure_file("${SRC_UTIL}/mm2000_m32.h" "${SRC_UTIL}/mm2000.h" COPYONLY)
endif()
# ////////// BEGIN COMPILER SETTINGS //////////

## ////////// BEGIN LINKER CONFIGURATION //////////
## Extra linker and compile options for Windows
#if(MSVC)
#    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Dwin64")
#    set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
#    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
#    add_compile_options(
#        $<$<CONFIG:>:/MT>
#        $<$<CONFIG:Debug>:/MTd>
#        $<$<CONFIG:Release>:/MT>
#    )
#endif()

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

# Compile LaGriT routines as a library
add_library(
    liblagrit
    ${LIBLAGRIT_BUILD_TYPE} # STATIC or SHARED
    #${SRC_ROOT}/lg_interface/interface2.F
    ${SRC_UTIL_FILES}
    ${SRC_CORE_FILES}
)

# Set the shared library version
set_target_properties(
  liblagrit
  PROPERTIES
  OUTPUT_NAME "lagrit"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
)

# Compile the main LaGriT binary
add_executable(
  lagrit.exe
  ${SRC_CORE}/lagrit_main.f
)

set_target_properties(
  lagrit.exe
  PROPERTIES
  OUTPUT_NAME "lagrit"
  LINKER_LANGUAGE Fortran
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
)

# Change from 'lagrit' (OUTPUT_NAME) to 'lagrit.exe' on Windows
if (WIN32)
  set_target_properties(lagrit.exe PROPERTIES SUFFIX ".exe")
endif()

if (LaGriT_BUILD_STATIC)
  set_target_properties(lagrit.exe PROPERTIES LINK_SEARCH_START_STATIC 1)
  set_target_properties(lagrit.exe PROPERTIES LINK_SEARCH_END_STATIC 1)

  if(WIN32)
    set(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
  else()
    set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
  endif()
endif()

target_include_directories(
  lagrit.exe
  PUBLIC ${Exodus_INCLUDE_DIRS} # includes 'exodusII.h'
)

target_link_libraries(lagrit.exe
  PRIVATE liblagrit           # liblagrit.[dylib|so|a|dll]
  PUBLIC ${Exodus_LIBRARIES}  # Exodus/Exodus_Fortran libs
)

# Install lagrit to ${CMAKE_INSTALL_PATH}/bin/
install(
    TARGETS lagrit.exe
    DESTINATION "bin"
)

# Install liblagrit to ${CMAKE_INSTALL_PATH}/lib/
install(
    TARGETS liblagrit
    DESTINATION "lib"
)

#install(
#  TARGETS ???
#  DESTINTION "include"
#)
