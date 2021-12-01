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

# https://gitlab.kitware.com/cmake/community/-/wikis/doc/tutorials/How-To-Write-Platform-Checks#platform-variables
if(UNIX)
    if(APPLE)
        message(STATUS "Operating System: macOS")
        add_definitions(-Dmacx64)
    else()
        message(STATUS "Operating System: Linux")
        add_definitions(-Dlinx64)
    endif()
elseif(WIN32)
    message(STATUS "Operating System: Windows")
    add_definitions(-Dwin64)
elseif(MINGW)
    message(STATUS "Operating System: Windows (MinGW)")
    add_definitions(-Dwin64)
elseif(CYGWIN)
    message(STATUS "Operating System: Windows (Cygwin)")
    add_definitions(-Dwin64)
else()
    message("OPERATING SYSTEM COULD NOT BE DETECTED")
endif()



# === FORTAN COMPILER FLAGS ======================== #
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  message(STATUS "Fortran compiler: GNU GFORTRAN")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  message(STATUS "Fortran compiler: Intel Fortran")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -O -Qm64 -Qsafe-cray-ptr -integer-size=64 -assume:nominus0 -QRimplicit-import-")
endif()

# === C COMPILER FLAGS ======================== #
if("${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
  MESSAGE(STATUS "C compiler: Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -m64")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  MESSAGE(STATUS "C compiler: GNU GCC")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -m64")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")
  MESSAGE(STATUS "C compiler: Intel C")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
  MESSAGE(STATUS "C compiler: Microsoft Visual C")
endif()

# === C++ COMPILER FLAGS ======================== #
if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
  message(STATUS "C++ compiler: Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w -m64")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  message(STATUS "C++ compiler: GNU G++")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w -m64")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  message(STATUS "C++ compiler: Intel C++")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  message(STATUS "C++ compiler: Microsoft Visual C++")
endif()




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

list(
    REMOVE_ITEM
    SRC_CORE_FILES
    ${SRC_CORE}/lagrit_main.f
)

list(
    REMOVE_ITEM
    SRC_UTIL_FILES
    ${SRC_CORE}/mm2000_header.F
)

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