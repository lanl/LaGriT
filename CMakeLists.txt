MESSAGE (STATUS "==========================================")
MESSAGE (STATUS "============Configuring LaGriT============")
MESSAGE (STATUS "==========================================")

CMAKE_MINIMUM_REQUIRED (VERSION 2.8.8)

SET (TARGET "lagrit")
PROJECT (${TARGET})

ENABLE_LANGUAGE (Fortran)

set (CMAKE_C_FLAGS "-w -m64 -Dlinx64")
set (CMAKE_Fortran_FLAGS "-w -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64")

SET (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

FIND_PACKAGE (EXODUSII REQUIRED)
INCLUDE_DIRECTORIES (${EXODUSII_INCLUDE_DIR})
SET (LIBS ${LIBS} ${EXODUSII_LIBRARIES})

# Build lg_util library
FILE (GLOB_RECURSE LG_UTIL_SRC
    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")
ADD_LIBRARY (lg_util STATIC ${LG_UTIL_SRC})

# Build LaGriT executable
FILE (GLOB_RECURSE TARGET_SRC
    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")

ADD_EXECUTABLE(${TARGET} ${TARGET_SRC})
TARGET_LINK_LIBRARIES(${TARGET} ${LIBS} lg_util)
