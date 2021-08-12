# set(CMAKE_Fortran_COMPILER "C:/Program Files (x86)/IntelSWTools/parallel_studio_xe_2020.4.912/compilers_and_libraries_2020/windows/bin/intel64/ifort.exe")

message(STATUS "==========================================")
message(STATUS "============Configuring LaGriT============")
message(STATUS "==========================================")

cmake_minimum_required(VERSION 3.1)
project(LaGriT
    VERSION 1.0
    DESCRIPTION "LaGriT Meshing Utilities"
    LANGUAGES Fortran C CXX)

set(CMAKE_C_FLAGS "/w /m64 /Dwin64 /QRimplicit-import- /g")
set(CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
set(CMAKE_Fortran_FLAGS "/w /Qsafe-cray-ptr /integer-size=64 /Dwin64 /QRimplicit-import- /debug")
set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

file(GLOB_RECURSE LG_UTIL_SRC
    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")

file(GLOB_RECURSE LG_MAIN_SRC
    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")


if(MSVC)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>
    )
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
    set(BUILD_SHARED_LIBS TRUE)
endif()

list(FILTER LG_MAIN_SRC EXCLUDE REGEX ".*lagrit_main.f$")
add_library(liblagrit SHARED ${LG_UTIL_SRC} ${LG_MAIN_SRC})
add_executable(lagrit
    src/lagrit_main.f
)
target_link_libraries(
    lagrit
    ${LIBS}
    liblagrit
)








#add_library(lg_util STATIC ${LG_UTIL_SRC})
#add_library(lg_util SHARED ${LG_UTIL_SRC})
#target_link_libraries(lg_util /QRimplicit-import-)

#      add_executable(lagrit
#          ${LG_MAIN_SRC}
#          ${LG_UTIL_SRC}
#      )

#target_link_libraries(
#    lagrit
#    /QRimplicit-import-
#)
#target_link_libraries(
#    lagrit
#    ${LIBS}
#    lg_util
#    /QRimplicit-import-
#)