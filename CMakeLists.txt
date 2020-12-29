MESSAGE (STATUS "==========================================")
MESSAGE (STATUS "============Configuring LaGriT============")
MESSAGE (STATUS "==========================================")

CMAKE_MINIMUM_REQUIRED (VERSION 2.8.8)

SET (TARGET "lagrit")
PROJECT (${TARGET} Fortran C CXX)

set (CMAKE_C_FLAGS "/w /m64 /Dwin64 /QRimplicit-import-")
set (CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
set (CMAKE_Fortran_FLAGS "/w /O /Qsafe-cray-ptr /integer-size=64 /Dwin64 /QRimplicit-import-")
set (CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")

SET (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

#FIND_PACKAGE (EXODUSII REQUIRED)
#INCLUDE_DIRECTORIES (${EXODUSII_INCLUDE_DIR})
#SET (LIBS ${LIBS} ${EXODUSII_LIBRARIES})
#SET (LIBS ${LIBS} "/MD")

# Build lg_util library
FILE (GLOB_RECURSE LG_UTIL_SRC
    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")

# Required to handle MSVC linking issues
if(MSVC)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>
    )
endif()

ADD_LIBRARY (lg_util STATIC ${LG_UTIL_SRC})

# Build LaGriT executable
FILE (GLOB_RECURSE TARGET_SRC
    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")

ADD_EXECUTABLE(${TARGET} ${TARGET_SRC})
TARGET_LINK_LIBRARIES(${TARGET} ${LIBS} lg_util)
