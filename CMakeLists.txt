cmake_minimum_required(VERSION 3.1)
project(LaGriT
    VERSION 1.0
    DESCRIPTION "LaGriT Meshing Utilities"
    LANGUAGES Fortran C CXX)

# LG UTIL FLAGS
#    set (CMAKE_C_FLAGS "-arch x86_64 -Dmacx64 -m64 -O")
#    set (CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
#    # -std=legacy
#    set (CMAKE_Fortran_FLAGS "-arch x86_64 -Dmacx64 -fcray-pointer -fdefault-integer-8 -m64 -cpp -DNOEXODUS")
# LG UTIL FLAGS

if (APPLE)
    set(CMAKE_C_FLAGS "-arch x86_64 -m64 -Dmacx64 -O -Wimplicit-function-declaration -Wno-implicit-function-declaration")
    set(CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
    if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
        set(CMAKE_Fortran_FLAGS "-w -O -Qsafe-cray-ptr -integer-size=64 -Dmacx64 -QRimplicit-import-")
    else()
        set(CMAKE_Fortran_FLAGS "-arch x86_64 -m64 -Dmacx64 -fcray-pointer -fdefault-integer-8 -std=legacy -cpp -fallow-argument-mismatch")
    endif()
endif()
# LG UTIL FLAGS
# LG UTIL FLAGS

FILE (GLOB_RECURSE LG_UTIL_SRC
    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")

#set_source_files_properties(${LG_UTIL_SRC} PROPERTIES COMPILE_FLAGS -Wno-effc++)

FILE (GLOB_RECURSE LG_MAIN_SRC
    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")

#set_source_files_properties(${LG_MAIN_SRC} PROPERTIES COMPILE_FLAGS -Wno-effc++)

add_library(lg_util STATIC ${LG_UTIL_SRC})

add_executable(lagrit
    ${LG_MAIN_SRC}
)

target_link_libraries(
    lagrit
    lg_util
)

#ADD_LIBRARY (lg_util STATIC ${LG_UTIL_SRC})
#
## Build LaGriT executable
#FILE (GLOB_RECURSE TARGET_SRC
#    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")
#
#ADD_EXECUTABLE(${TARGET} ${TARGET_SRC})
#TARGET_LINK_LIBRARIES(${TARGET} ${LIBS} lg_util)
#include_directories()

# option(USE_NEW_AUDIO_ENGINE "Enable new experimental audio engine" OFF)

#if (APPLE)
#    set (CMAKE_C_FLAGS "-arch x86_64 -Dmacx64 -m64 -O")
#    set (CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
#    # -std=legacy
#    set (CMAKE_Fortran_FLAGS "-arch x86_64 -Dmacx64 -fcray-pointer -fdefault-integer-8 -m64 -cpp -DNOEXODUS")
#elseif (UNIX)
## ...
#elseif (WIN32)
#    set (CMAKE_C_FLAGS "/w /m64 /Dwin64 /QRimplicit-import-")
#    set (CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
#    set (CMAKE_Fortran_FLAGS "/w /O /Qsafe-cray-ptr /integer-size=64 /Dwin64 /QRimplicit-import-")
#    set (CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
#endif()
#
#
#SET (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
#
##FIND_PACKAGE (EXODUSII REQUIRED)
##INCLUDE_DIRECTORIES (${EXODUSII_INCLUDE_DIR})
##SET (LIBS ${LIBS} ${EXODUSII_LIBRARIES})
##SET (LIBS ${LIBS} "/MD")
#
## Build lg_util library
#FILE (GLOB_RECURSE LG_UTIL_SRC
#    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")
#
## Required to handle MSVC linking issues
#if(MSVC)
#    add_compile_options(
#        $<$<CONFIG:>:/MT>
#        $<$<CONFIG:Debug>:/MTd>
#        $<$<CONFIG:Release>:/MT>
#    )
#endif()
#
#