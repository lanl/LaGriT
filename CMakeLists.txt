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

#set(CMAKE_CXX_STANDARD 17)
#set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
set(BUILD_SHARED_LIBS TRUE)

# Set source and target paths
set(EXE lagrit)
set(SRC_ROOT "${CMAKE_SOURCE_DIR}/src")
set(SRC_UTIL "${SRC_ROOT}/lg_util")
set(SRC_CORE "${SRC_ROOT}/lg_core")

#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

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
    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -m64 -fcray-pointer -fdefault-integer-8 -std=legacy")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -Qsafe-cray-ptr -integer-size=64 -QRimplicit-import-")
endif()

# Set C compiler flags
if(DEFINED GCC_LIKE)
    set(CMAKE_C_FLAGS         "${CMAKE_C_FLAGS} -w -m64")
elseif(DEFINED MSVC_LIKE)
    set(CMAKE_C_FLAGS         "${CMAKE_C_FLAGS} /w /m64 /QRimplicit-import-")
endif()

# Set C++ compiler flags
if(DEFINED GCXX_LIKE)
    set(CMAKE_CXX_FLAGS         "${CMAKE_CXX_FLAGS} -w -m64")
elseif(DEFINED MSVCXX_LIKE)
    set(CMAKE_CXX_FLAGS         "${CMAKE_CXX_FLAGS} /w /m64 /QRimplicit-import-")
endif()

# ////////// END COMPILER SETTINGS //////////

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
    "${SRC_UTIL}/*.h"
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
    "${SRC_CORE}/*.h"
)

# Compile LaGriT routines as a library
add_library(
    liblagrit
    SHARED
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
    Fortran_FORMAT "FREE"
    LINKER_LANGUAGE Fortran
)

target_link_libraries(
    ${EXE}
    liblagrit
)

# ////////// END TARGET CONFIGURATION //////////