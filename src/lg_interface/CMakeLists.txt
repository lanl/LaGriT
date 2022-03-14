cmake_minimum_required(VERSION 3.18)

# Set all library source files
target_sources(lagrit_lib
    PRIVATE
        src/interface.f90)

# Validate compiler & symbol interop
include(FortranCInterface)
FortranCInterface_VERIFY()
FortranCInterface_VERIFY(CXX)

# Generate a header file for handling compiler symbol mangling
# More information: https://www.netlib.org/lapack/lawnspdf/lawn270.pdf
FortranCInterface_HEADER(
    ${CMAKE_BINARY_DIR}/include/lagrit_cmake_interface.h
    MACRO_NAMESPACE         ""
    SYMBOL_NAMESPACE        ""
    SYMBOLS
        inside_tet
        lineseg_tri
    )

target_include_directories(lagrit_lib
    PRIVATE
        include/
        ${CMAKE_BINARY_DIR}/include/)

include(GenerateExportHeader)
generate_export_header(lagrit_lib)
