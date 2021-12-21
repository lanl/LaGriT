#
# Find the Exodus finite element data model library from Sandia
#
#  EXODUS_FOUND - System has Exodus
#  EXODUS_INCLUDE_DIR - The LibXml2 include directory
#  EXODUS_LIBRARIES - The libraries needed to use LibXml2
#
# Original author: https://gitlab.kitware.com/xdmf/xdmf/blob/master/CMake/FindExodus.cmake

#FIND_PACKAGE(NetCDF REQUIRED)
# Original lagrit build line: -L$(EXO_LIB_DIR) -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -ldl

FIND_PATH(Exodus_INCLUDE_DIR NAMES exodusII.h)

#FIND_LIBRARY(EXODUS_LIBRARIES NAMES exodusii exodusIIv2c exodus_for exodus)
FIND_LIBRARY(Exodus_LIBRARIES
    NAMES exodus_for exodus
)

INCLUDE(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set EXODUS_FOUND to TRUE if
# all listed variables are TRUE
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Exodus DEFAULT_MSG Exodus_LIBRARIES Exodus_INCLUDE_DIR)

MARK_AS_ADVANCED(Exodus_INCLUDE_DIR Exodus_LIBRARIES)

