# https://github.com/fortran-lang/stdlib-cmake-example/blob/main/CMakeLists.txt

message(STATUS "==========================================")
message(STATUS "============Configuring LaGriT============")
message(STATUS "==========================================")

cmake_minimum_required(VERSION 3.12)

project(
    LaGriT
    VERSION 3.3.0
    DESCRIPTION "LaGriT Meshing Utilities"
    LANGUAGES Fortran CXX C
)

# ==== Is SEACAS/ExodusII available? =========== #
#find_package(ExodusII)
#if (ExodusII_FOUND)
#    message(STATUS "ExodusII support enabled: ${ExodusII_LIBRARIES}")
#    add_definitions(-DLAGRIT_INCLUDE_EXODUSII)
#    include_directories(LaGriT
#        PRIVATE ${ExodusII_INCLUDE_DIRS}
#    )
#    target_link_libraries(LaGriT
#        PRIVATE ${ExodusII_LIBRARIES}
#    )
#else()
#    message("WARNING: SEACAS/ExodusII could not be found. Compiling without ExodusII support.")
#endif(ExodusII_FOUND)

include("${CMAKE_SOURCE_DIR}/cmake/PlatformSettings.cmake")

# === FORTAN COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/FortranSettings.cmake")
# === C COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/CSettings.cmake")
# === C++ COMPILER FLAGS ======================== #
include("${CMAKE_SOURCE_DIR}/cmake/compiler/CXXSettings.cmake")




include(FortranCInterface)
FortranCInterface_VERIFY(CXX)
FortranCInterface_HEADER(
    FCMangle.h
    MACRO_NAMESPACE "FC_"
    SYMBOL_NAMESPACE "FC_"
    SYMBOLS mysub mymod:my_sub
)

set(BUILD_SHARED_LIBS TRUE)

# Set source and target paths
set(EXE lagrit)
set(LIB liblagrit)

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

# COMPILERS:
#   - GNU GCC
#   - Clang GCC
#   - GNU G++
#   - Clang G++
#   - GNU GFORTRAN
#   - Intel Fortran

#CMAKE_Fortran_FLAGS
#CMAKE_C_FLAGS
#CMAKE_CXX_FLAGS







## ////////// BEGIN LINKER CONFIGURATION //////////
## Extra linker and compile options for Windows
#if(MSVC)
#    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
#    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Dwin64")
#    set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
#    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
#    add_compile_options(
#        $<$<CONFIG:>:/MT>
#        $<$<CONFIG:Debug>:/MTd>
#        $<$<CONFIG:Release>:/MT>
#    )
#endif()
#
## ////////// BEGIN HEADER FILE CONFIGURATION //////////
#string(TIMESTAMP CMAKE_DATE_COMPILE "%Y/%m/%d")
#configure_file(
#    ${SRC_ROOT}/lagrit.h.in
#    ${SRC_CORE}/lagrit.h
#    @ONLY
#)

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

list(FILTER SRC_UTIL_FILES EXCLUDE REGEX ".*mm2000_header.F$")
list(FILTER SRC_CORE_FILES EXCLUDE REGEX ".*lagrit_main.f$")

# This file has the `#ifdef EXODUS` preprocessor directive
# Ensure the compiler can process those directives
set_source_files_properties(
    "${SRC_CORE}/dumpexodusII.f"
    PROPERTIES Fortran_PREPROCESS ON
)

#list(
#    REMOVE_ITEM
#    SRC_CORE_FILES
#    ${SRC_CORE}/lagrit_main.f
#)
#
#list(
#    REMOVE_ITEM
#    SRC_UTIL_FILES
#    ${SRC_CORE}/mm2000_header.F
#)

# Compile LaGriT routines as a library
add_library(
    ${LIB}
    SHARED
    #${SRC_ROOT}/lg_interface/interface2.F
    ${SRC_UTIL_FILES}
    ${SRC_CORE_FILES}
)

# Compile the main LaGriT binary
add_executable(
    ${EXE}
    ${SRC_CORE}/lagrit_main.f
)

set_target_properties(
    ${EXE}
    PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
)

target_link_libraries(
    ${EXE}
    ${LIB}
)

# ////////// END TARGET CONFIGURATION //////////
install(
    DIRECTORY
    DESTINATION ${directory}
)

install(
    TARGETS ${EXE}
    DESTINATION "${INSTALL_DIR}/bin"
)

install(
    TARGETS ${LIB}
    DESTINATION "${INSTALL_DIR}/lib"
)

install(
    DIRECTORY inc/
    DESTINATION "${INSTALL_DIR}/include"
    FILES_MATCHING PATTERN "*.h"
)
