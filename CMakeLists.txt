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

# this is the long version to build LAGRIT with Exodus
# it is useful if Exodus cmake files or libs are not found
# new versions of exodus should set definitions with find_package()
# Look for SEACAS/ExodusII libraries
# then set the "TPL_LIBRARIES" variable to full paths of found libs
# NAMES exodus libexodus exoIIv2for netcdf hdf5_hl hdf5 libz

set(EXODUS_ROOT "${CMAKE_SOURCE_DIR}/TPLs/seacas")
set(Exodus_INCLUDE_DIR "${EXODUS_ROOT}/include")
set(Exodus_LIBRARIES "${EXODUS_ROOT}/lib")

if(EXISTS "${Exodus_INCLUDE_DIR}")
  message(STATUS "  Found exodus include dir: ${Exodus_INCLUDE_DIR}")
else()
  message("FATAL_ERROR: exodus include dir not found in ${EXODUS_ROOT}")
endif()

if(EXISTS "${Exodus_INCLUDE_DIR}/exodusII.inc")
  set(EXO_INC "${Exodus_INCLUDE_DIR}/exodusII.inc")
  message(STATUS "  Found exodusII.inc : ${EXO_INC}")
else()
  message("FATAL_ERROR: exodusII.inc not found in ${EXO_INC}")
endif()

# set libraries with individual calls to each lib name
if(EXISTS "${Exodus_LIBRARIES}")
find_library(EXO_FOUND libexodus.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
find_library(EXO_FORT_FOUND libexodus_for.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
find_library(EXO_FORT32_FOUND libexoIIv2for32.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
find_library(HDF5_FOUND libhdf5.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
find_library(NETCDF_FOUND libnetcdf.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
find_library(ZLIB_FOUND libz.a 
    PATHS ${EXODUS_ROOT} ${Exodus_LIBRARIES} NO_DEFAULT_PATH)
  message(STATUS "  Found Exodus      lib: ${EXO_FOUND}")
  message(STATUS "  Found Exodus_fort lib: ${EXO_FORT_FOUND}")
  message(STATUS "  Found HDF5        lib: ${HDF5_FOUND}")
  message(STATUS "  Found NetCDF      lib: ${NETCDF_FOUND}")
  message(STATUS "  Found ZLIB        lib: ${ZLIB_FOUND}")
else()
  message("FATAL_ERROR: Exodus libs not found in ${Exodus_LIBRARIES}")
endif()

# IF NOT FOUND hard-wire root and paths here
# set(EXODUS_ROOT "/project/eesdev/tam/LaGriT-build/TPLs/seacas")
# set(Exodus_LIBRARIES "${EXODUS_ROOT}/lib")
# set(Exodus_INCLUDE_DIR "${EXODUS_ROOT}/include")
# message(STATUS "Set Explicit Exodus ROOT: ${SEACASExodus_ROOT}")

# trick cmake to add exodus libs correctly for linking
# nc-config --static should pick up all link FLAGS from netcdf 
# -lcurl needed for linking netcdf libs on linux 
set(TPL_LINK_LIBS "-L${EXODUS_ROOT}/lib -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lcurl -lhdf5 -lz -ldl -static-libgfortran -static-libgcc")

message(STATUS "-----Done Exodus-----")

# SET TPLS with exodus and petsc includes and libs
if (Exodus_INCLUDE_DIR AND Exodus_LIBRARIES) # AND NetCDF_FOUND AND HDF5_FOUND AND ZLIB_FOUND
  set(TPL_INCLUDE_DIRS "${Exodus_INCLUDE_DIR}")
#  set(TPL_LIBRARIES ${HDF5_FOUND} ${NETCDF_FOUND} ${ZLIB_FOUND} ${EXO_FOUND} ${EXO_FORT32_FOUND} ${EXO_FORT_FOUND} )
  set(TPL_LIBRARIES "${Exodus_LIBRARIES}" )

else()
  message("ERROR: ExodusII and/or other dependencies could not be found.")
  message( FATAL_ERROR "Build ExodusII or use -DLaGriT_BUILD_EXODUS=OFF" )
endif()

endif()
# END TPLs

if (LAGRIT_BUILD_EXODUS)
  add_definitions(-DLAGRIT_INCLUDE_EXODUSII)
  message(STATUS "ExodusII support enabled: ${LAGRIT_BUILD_EXODUS}")
  message(STATUS "ExodusII libraries      : ${Exodus_LIBRARIES}")
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

