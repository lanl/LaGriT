cmake_minimum_required(VERSION 3.16)

# Validate compiler & symbol interop
include(FortranCInterface)
FortranCInterface_VERIFY()

# Generate a header file for handling compiler symbol mangling
# More information: https://www.netlib.org/lapack/lawnspdf/lawn270.pdf
FortranCInterface_HEADER(
    fc_mangle.h
    SYMBOLS
        INITLAGRIT
        DOTASK
        CMO_GET_NAME
        CMO_GET_INFO
        CMO_GET_INTINFO
        INSIDE_TET
        LINESEG_TRI)

target_include_directories(lagrit
    PUBLIC
        include
    PRIVATE
        # Include `src/lg_interface.h`
        ${CMAKE_CURRENT_LIST_DIR}/src/
        # Include generated `fc_mangle.h`
        ${CMAKE_CURRENT_BINARY_DIR}
)

# Install this header file for end-users to use
set_target_properties(lagrit
    PROPERTIES
        PUBLIC_HEADER "${CMAKE_CURRENT_LIST_DIR}/include/lg.h")

# Set all library source files
target_sources(lagrit
    PUBLIC
        include/lg.h
    PRIVATE
        src/public_interface.cpp
        src/bind_c_to_fortran.f90
        src/lg_interface.h)
