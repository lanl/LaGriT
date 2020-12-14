MESSAGE (STATUS "==========================================")
MESSAGE (STATUS "============Configuring LaGriT============")
MESSAGE (STATUS "==========================================")

CMAKE_MINIMUM_REQUIRED (VERSION 2.8.8)

SET (TARGET "lagrit")
PROJECT (${TARGET} Fortran C CXX)

#ENABLE_LANGUAGE (Fortran)

set (CMAKE_C_FLAGS "-w -m64 -Dwin64 -MD /QRimplicit-import-")
set (CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
set (CMAKE_Fortran_FLAGS "-w -O -Qsafe-cray-ptr -integer-size=64 -Dwin64 /QRimplicit-import-")
set (CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")

SET (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

#FIND_PACKAGE (EXODUSII REQUIRED)
#INCLUDE_DIRECTORIES (${EXODUSII_INCLUDE_DIR})
#SET (LIBS ${LIBS} ${EXODUSII_LIBRARIES})
#SET (LIBS ${LIBS} "/NODEFAULTLIB:library")

# Build lg_util library
FILE (GLOB_RECURSE LG_UTIL_SRC
    "lg_util/*.c" "lg_util/*.f" "lg_util/*.h" "lg_util/mm2000.F")

ADD_LIBRARY (lg_util STATIC ${LG_UTIL_SRC})

# Build LaGriT executable
FILE (GLOB_RECURSE TARGET_SRC
    "src/*.c" "src/*.cpp" "src/*.f" "src/*.f90" "src/*.F" "src/*.h")

ADD_EXECUTABLE(${TARGET} ${TARGET_SRC})

if(MSVC AND NOT (MSVC_VERSION LESS 1900))
    # prevent error LNK2019: unresolved external symbol __imp_fread referenced in function cread
    # see http://stackoverflow.com/a/32418900/469659
    TARGET_LINK_LIBRARIES(${TARGET} "legacy_stdio_definitions.lib")
endif()
TARGET_LINK_LIBRARIES(${TARGET} ${LIBS} lg_util)
