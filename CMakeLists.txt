# Default build with Exodus OFF
# for testing cmake options:
# -DCMAKE_BUILD_TYPE=Debug
# -DLAGRIT_BUILD_EXODUS=ON (detect Exodus?, default OFF)
#
# Some common cmake variables:
# https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html
# -D CMAKE_FIND_DEBUG_MODE=ON
#
# Not supported yet but available:
# -D CMAKE_INSTALL_PREFIX (default /usr/local if make install)
# -D LaGriT_BUILD_STATIC=ON (default .a, use OFF for .so etc)
#
# Long version with explicit paths to build lagrit with Exodus 
# This file is useful for debugging cmake with Exodus
# Note Windows is not working and is commented out
# Sep 28 2022 tamiller@lanl.gov lagrit.lanl.gov
# SEACAS Exodus Gregory Sjaardema gsjaardema@gmail.com
##############################################################################

cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

# set cmake variables for version numbers and compilers
project(LaGriT VERSION 3.3.4
               DESCRIPTION "LaGriT Meshing Utilities"
               LANGUAGES Fortran CXX C)

option(LAGRIT_BUILD_STATIC "Compile as a static (fat) executable." ON)
option(LAGRIT_BUILD_EXODUS "Build with Exodus." OFF)

# Set Directories for lagrit compile and build 
set(INSTALL_DIR "${CMAKE_SOURCE_DIR}/install")
set(SRC_UTIL "${CMAKE_SOURCE_DIR}/lg_util/src")
set(SRC_CORE "${CMAKE_SOURCE_DIR}/src")

set(TPL_LIBRARIES "")
set(TPL_INCLUDE_DIRS "")

message(STATUS "==========================================")
message(STATUS "============Configuring LaGriT============")
message(STATUS "===================v${PROJECT_VERSION}=================")

message(STATUS "Compile LaGriT as a static binary = ${LAGRIT_BUILD_STATIC}")
message(STATUS "Compile LaGriT with ExodusII = ${LAGRIT_BUILD_EXODUS}")

# Add Third Party Libraries here and add all to TPL_LIBRARIES
# These include Exodus and PETSc (planned for Voronoi codes)

if (LAGRIT_BUILD_EXODUS)

message(STATUS "-----Configuring Exodus-----")

# EXODUS_ROOT: explicit path to a SEACAS install prefix (contains lib/ and include/).
# Leave empty to let CMake auto-detect from known locations or fetch from source.
set(EXODUS_ROOT "" CACHE PATH
  "Path to SEACAS/Exodus install prefix (contains lib/ and include/). \
Leave empty to auto-detect or set LAGRIT_FETCH_EXODUS=ON to build automatically.")

# Auto-detect from common locations (checked in priority order)
if(NOT EXODUS_ROOT)
  foreach(_candidate
      "$ENV{SEACAS_ROOT}"                          # user environment variable
      "${CMAKE_SOURCE_DIR}/TPLs/seacas"            # legacy: install-exodus.sh output
      "${CMAKE_BINARY_DIR}/_deps/seacas"           # cmake fetch output
  )
    if(_candidate AND EXISTS "${_candidate}/include/exodusII.h")
      set(EXODUS_ROOT "${_candidate}")
      message(STATUS "  Auto-detected Exodus: ${EXODUS_ROOT}")
      break()
    endif()
  endforeach()
endif()

# If still not found, either fetch it or hard-fail with a useful message.
option(LAGRIT_FETCH_EXODUS
  "Automatically fetch and build SEACAS/Exodus via CMake if not found locally." OFF)

if(NOT EXODUS_ROOT)
  if(LAGRIT_FETCH_EXODUS)
    set(EXODUS_ROOT "${CMAKE_BINARY_DIR}/_deps/seacas")
    message(STATUS "  Exodus not found locally — will fetch and build from GitHub at build time")
    message(STATUS "  (requires network access, bash, and make)")
    include("${CMAKE_SOURCE_DIR}/cmake/FetchExodus.cmake")
    set(_LAGRIT_EXODUS_FETCHED TRUE)
  else()
    message(FATAL_ERROR
      "ExodusII not found.  Options:\n"
      "  1. cmake -DLAGRIT_FETCH_EXODUS=ON        (auto-fetch + build, needs network)\n"
      "  2. Run install-exodus.sh first, then cmake -DLAGRIT_BUILD_EXODUS=ON\n"
      "  3. cmake -DEXODUS_ROOT=/path/to/seacas   (point to an existing install)\n"
      "  4. Set env var SEACAS_ROOT=/path/to/seacas before running cmake")
  endif()
endif()

set(Exodus_INCLUDE_DIR "${EXODUS_ROOT}/include")
set(Exodus_LIB_DIR     "${EXODUS_ROOT}/lib")

# Validate paths — skip when fetching because the files don't exist yet.
if(NOT _LAGRIT_EXODUS_FETCHED)
  if(NOT EXISTS "${Exodus_INCLUDE_DIR}/exodusII.h")
    message(FATAL_ERROR
      "exodusII.h not found in ${Exodus_INCLUDE_DIR}.\n"
      "Check EXODUS_ROOT=${EXODUS_ROOT} or re-run install-exodus.sh.")
  endif()
  if(NOT EXISTS "${Exodus_INCLUDE_DIR}/exodusII.inc")
    message(FATAL_ERROR
      "exodusII.inc not found in ${Exodus_INCLUDE_DIR}.\n"
      "Check EXODUS_ROOT=${EXODUS_ROOT} or re-run install-exodus.sh.")
  endif()
  message(STATUS "  Exodus include : ${Exodus_INCLUDE_DIR}")
  message(STATUS "  Exodus lib dir : ${Exodus_LIB_DIR}")
endif()

# Build the linker flag string, platform-aware.
# Linux needs -lcurl for static netcdf and -static-libgfortran/-static-libgcc.
# macOS does not need -lcurl, and -static-libgfortran may or may not be needed.
set(TPL_LINK_LIBS
  "-L${Exodus_LIB_DIR} -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -ldl")
if(UNIX AND NOT APPLE)
  string(APPEND TPL_LINK_LIBS " -lcurl -static-libgfortran -static-libgcc")
elseif(APPLE)
  string(APPEND TPL_LINK_LIBS " -static-libgfortran")
endif()

set(TPL_INCLUDE_DIRS "${Exodus_INCLUDE_DIR}")
set(TPL_LIBRARIES    "${Exodus_LIB_DIR}")

message(STATUS "-----Done Exodus-----")

endif()
# END TPLs

if (LAGRIT_BUILD_EXODUS)
  add_definitions(-DLAGRIT_INCLUDE_EXODUSII)
  message(STATUS "ExodusII support enabled: ${LAGRIT_BUILD_EXODUS}")
  message(STATUS "ExodusII lib dir        : ${Exodus_LIB_DIR}")
  message(STATUS "ExodusII include        : ${Exodus_INCLUDE_DIR}")
else()
  message("LaGriT Compiling without ExodusII support.")
  message("To include Exodus, use -DLAGRIT_BUILD_EXODUS=ON")
endif()

# message( FATAL_ERROR "Debug exit at STOP." )

# ===== Detect local platform, bit-size, and compilers ==================== #
message(STATUS "Detecting LaGriT build using local cmake files:")
include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/DetectBitSize.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-Fortran.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/CompilerFlags-C.cmake")

if (LAGRIT_BUILD_STATIC)
  set(LIBLAGRIT_BUILD_TYPE "STATIC")
else()
  set(LIBLAGRIT_BUILD_TYPE "SHARED")
  set(BUILD_SHARED_LIBS TRUE)
endif()

# cmake variable to set DEBUG or RELEASE on command line 
if (CMAKE_BUILD_TYPE)
  message("LaGriT compile type set to: ${CMAKE_BUILD_TYPE}")
endif()

# ////////// BEGIN COMPILER SETTINGS //////////

if (TPL_LIBRARIES)
message(STATUS "Using Third Party Libraries.")
message(STATUS "TPL libs: ${TPL_LIBRARIES}")
message(STATUS "TPL include: ${TPL_INCLUDE_DIRS}")
include_directories(${TPL_INCLUDE_DIRS})
endif()

## ////////// BEGIN LINKER CONFIGURATION //////////
## Windows Extra linker and compile options
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

# create lagrit.h using PROJECT_VERSION_* in template lagrit.h.in
string(TIMESTAMP CMAKE_DATE_COMPILE "%Y/%m/%d")
configure_file(
    ${SRC_CORE}/lagrit.h.in
    ${SRC_CORE}/lagrit.h
    @ONLY
)

# Validate compiler & symbol interop
include(FortranCInterface)
FortranCInterface_VERIFY()

# Create fc_mangle.h for c-fortran routines to handle symbol mangling
# Names here should also be declared in src/lg_f_interface.h
# More information: https://www.netlib.org/lapack/lawnspdf/lawn270.pdf
FortranCInterface_HEADER(
    ${SRC_CORE}/fc_mangle.h
    SYMBOLS
        INITLAGRIT # syntax: <subroutine>
        DOTASK
        CMO_GET_NAME
        CMO_GET_INFO
        CMO_GET_INTINFO
        FC_CMO_GET_INT
        FC_CMO_GET_VINT
        FC_CMO_GET_DOUBLE
        FC_CMO_GET_VDOUBLE
        FPASS_TYPES
        INSIDE_TET
        LINESEG_TRI)

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

# Grab `src/` source files
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

message(STATUS "Using Compiler Settings:")
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
  lagrit
  ${SRC_CORE}/lagrit_main.f
)

set_target_properties(
  lagrit
  PROPERTIES
  OUTPUT_NAME "lagrit"
  LINKER_LANGUAGE Fortran
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
)

# Change from 'lagrit' (OUTPUT_NAME) to 'lagrit.exe' on Windows
if (WIN32)
  set_target_properties(lagrit PROPERTIES SUFFIX ".exe")
endif()

# When Exodus is auto-fetched, both targets must wait for seacas_exodus to
# finish (configure + build + install) before compiling or linking.
if(_LAGRIT_EXODUS_FETCHED)
  add_dependencies(liblagrit seacas_exodus)  # needs headers (exodusII.h)
  add_dependencies(lagrit    seacas_exodus)  # needs libs   (libexodus.a etc.)
endif()

if (LAGRIT_BUILD_STATIC)
  set_target_properties(lagrit PROPERTIES LINK_SEARCH_START_STATIC 1)
  set_target_properties(lagrit PROPERTIES LINK_SEARCH_END_STATIC 1)

  if(WIN32)
    set(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
  else()
    set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
  endif()

endif()
# end static build options

target_include_directories(
  lagrit
  PUBLIC ${TPL_INCLUDE_DIRS} # includes 'exodusII.h' 'exodusII.inc'
)

target_link_libraries(lagrit
   PRIVATE liblagrit           # liblagrit.[dylib|so|a|dll]
   PUBLIC ${TPL_LINK_LIBS}     # Exodus / NetCDF / HDF5 / ZLIB
)

# this produces errors 
#make[2]: *** No rule to make target '../TPLs/seacas/lib/libexoIIv2for.a', needed by 'lagrit'.  Stop.
# target_link_libraries(lagrit
#     PRIVATE
#         liblagrit
#         "${Exodus_LIBRARIES}/libexodus.a"
#         "${Exodus_LIBRARIES}/libexoIIv2for.a"
#         "${Exodus_LIBRARIES}/libnetcdf.a"
#         "${Exodus_LIBRARIES}/libhdf5.a"
#         "${Exodus_LIBRARIES}/libhdf5_hl.a"
# )

# Install lagrit to ${CMAKE_INSTALL_PATH}/bin/
install(
    TARGETS lagrit
    DESTINATION "bin"
)

# Install liblagrit to ${CMAKE_INSTALL_PATH}/lib/
install(
    TARGETS liblagrit
    DESTINATION "lib"
)

