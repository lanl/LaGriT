cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(LaGriT VERSION 3.3.3
               DESCRIPTION "LaGriT Meshing Utilities"
               LANGUAGES Fortran CXX C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

option(LaGriT_BUILD_STATIC "Compile as a static (fat) executable." ON)

message(STATUS "==========================================")
message(STATUS "============Configuring LaGriT============")
message(STATUS "===================v${PROJECT_VERSION}=================")

message(STATUS "Compile LaGriT as a static binary = ${LaGriT_BUILD_STATIC}")

set(INSTALL_DIR "${CMAKE_SOURCE_DIR}/install")
set(SRC_UTIL "${CMAKE_SOURCE_DIR}/lg_util/src")
set(SRC_CORE "${CMAKE_SOURCE_DIR}/src")

# ==== Is SEACAS/ExodusII available? =========== #
set(SEACASProj_ENABLE_SEACASExodus          YES)
set(SEACASProj_ENABLE_SEACASExodus_for      YES)
set(SEACASProj_ENABLE_SEACASExoIIv2for32    YES)
set(SEACASProj_ENABLE_TESTS                 YES)
set(SEACASExodus_ENABLE_STATIC              ${STATIC})
set(SEACASProj_ENABLE_Fortran               YES)
set(SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST   YES)
set(SEACASProj_HIDE_DEPRECATED_CODE         NO)
set(SEACASExodus_ENABLE_THREADSAFE          NO)
set(HDF5_NO_SYSTEM_PATHS                    YES)
add_subdirectory("${CMAKE_SOURCE_DIR}/seacas/" seacas.out)

execute_process(
  COMMAND
    "FORTRAN=YES CGNS=OFF MATIO=OFF NEEDS_ZLIB=YES GNU_PARALLEL=OFF BUILD=YES ./install-tpl.sh"
  WORKING_DIRECTORY
    "${CMAKE_SOURCE_DIR}/seacas/"
)

#ExternalProject_Get_Property(project_a install_dir)
#include_directories(${install_dir}/include)

#if (Exodus_FOUND) # AND NetCDF_FOUND AND HDF5_FOUND AND ZLIB_FOUND
#  message(STATUS "ExodusII support enabled: ${Exodus_LIBRARIES}")
#  add_definitions(-DLAGRIT_INCLUDE_EXODUSII)
#  set(TPL_LIBRARIES "${Exodus_LIBRARIES}")# ${HDF5_LIBRARIES} ${NetCDF_LIBRARIES} ${ZLIB_LIBRARIES}")
#  set(TPL_INCLUDE_DIRS "${Exodus_INCLUDE_DIR}")# ${HDF5_INCLUDE_DIR} ${NetCDF_INCLUDE_DIR} ${ZLIB_INCLUDE_DIR}")
#else()
#  message("WARNING: ExodusII and/or other dependencies could not be found. Compiling without ExodusII support.")
#endif()

# ===== Detect platform, bit-size, and compilers ==================== #
message(STATUS "Detected System:")
include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/DetectBitSize.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-Fortran.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-C.cmake")

if (LaGriT_BUILD_STATIC)
  set(LIBLAGRIT_BUILD_TYPE "STATIC")
else()
  set(LIBLAGRIT_BUILD_TYPE "SHARED")
  set(BUILD_SHARED_LIBS TRUE)
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

message(STATUS "Compilers:")
message(STATUS "  FORTRAN [compiler = \"${CMAKE_Fortran_COMPILER}\"; flags = \"${CMAKE_Fortran_FLAGS}\"]")
message(STATUS "  C [compiler = \"${CMAKE_C_COMPILER}\"; flags = \"${CMAKE_C_FLAGS}\"]")
message(STATUS "  C++ [compiler = \"${CMAKE_CXX_COMPILER}\"; flags = \"${CMAKE_CXX_FLAGS}\"]")

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

target_include_directories(lagrit.exe
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}
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
  PUBLIC ${TPL_INCLUDE_DIRS} # includes 'exodusII.h'
)

target_link_libraries(lagrit.exe
  PRIVATE liblagrit           # liblagrit.[dylib|so|a|dll]
  PUBLIC ${TPL_LIBRARIES}     # Exodus / NetCDF / HDF5 / ZLIB
)
