cmake_minimum_required(VERSION 3.16)

# Set all library source files
target_sources(lagrit
    PUBLIC
        src/lg_interface.h
        src/interface.f90)

# Validate compiler & symbol interop
include(FortranCInterface)
FortranCInterface_VERIFY()
#FortranCInterface_VERIFY(CXX)

# Generate a header file for handling compiler symbol mangling
# More information: https://www.netlib.org/lapack/lawnspdf/lawn270.pdf
FortranCInterface_HEADER(
    fc_mangle.h
    SYMBOLS
        INSIDE_TET
        MYTEST
        lineseg_tri)

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
        PUBLIC_HEADER "${CMAKE_CURRENT_LIST_DIR}/include/lagrit.h")
