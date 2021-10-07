# set(CMAKE_Fortran_COMPILER "C:/Program Files (x86)/IntelSWTools/parallel_studio_xe_2020.4.912/compilers_and_libraries_2020/windows/bin/intel64/ifort.exe")
cmake_minimum_required(VERSION 3.12)

project(
    LaGriT
    VERSION 3.3.0
    DESCRIPTION "LaGriT Meshing Utilities"
    LANGUAGES Fortran CXX C
)

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
set(SRC_ROOT "${CMAKE_SOURCE_DIR}/src")
set(SRC_UTIL "${SRC_ROOT}/lg_util")
set(SRC_CORE "${SRC_ROOT}/lg_core")

# ////////// BEGIN COMPILER SETTINGS //////////
# Detect whether our C compiler is MSCV or GNU-like (LLVM/GCC)
if(CMAKE_C_COMPILER_ID STREQUAL "GNU"   OR
   CMAKE_C_COMPILER_ID STREQUAL "Clang" OR
   CMAKE_C_COMPILER_ID STREQUAL "AppleClang")
  set(GCC_LIKE TRUE)
elseif(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
  set(MSVC_LIKE TRUE)
endif()

# Detect whether our C++ compiler is MSCV or GNU-like (LLVM/GCC)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU"   OR
   CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR
   CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
  set(GCXX_LIKE TRUE)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  set(MSVCXX_LIKE TRUE)
endif()

# Detect whether our Fortran compiler is GNU (gfortran) or Intel (ifort)
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Qsafe-cray-ptr -integer-size=64 -QRimplicit-import-")
endif()

# Set C compiler flags
if(DEFINED GCC_LIKE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -m64")
elseif(DEFINED MSVC_LIKE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w /m64 /QRimplicit-import-")
endif()

# Set C++ compiler flags
if(DEFINED GCXX_LIKE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w -m64")
elseif(DEFINED MSVCXX_LIKE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w /m64 /QRimplicit-import-")
endif()

# ////////// BEGIN LINKER CONFIGURATION //////////
# Extra linker and compile options for Windows
if(MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>
    )
endif()

# ////////// BEGIN HEADER FILE CONFIGURATION //////////
string(TIMESTAMP CMAKE_DATE_COMPILE "%Y/%m/%d")
configure_file(
    ${SRC_ROOT}/lagrit.h.in
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

# Compile LaGriT routines as a library
add_library(
    ${LIB}
    SHARED
    ${SRC_ROOT}/lg_interface/interface.f90
    ${SRC_UTIL_FILES}
    ${SRC_CORE_FILES}
)

# Compile the main LaGriT binary
add_executable(
    ${EXE}
    ${SRC_ROOT}/lagrit_main.f
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