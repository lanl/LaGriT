# FetchExodus.cmake
# Fetches, patches, builds, and installs SEACAS/Exodus via CMake ExternalProject.
# Included by CMakeLists.txt when LAGRIT_BUILD_EXODUS=ON and LAGRIT_FETCH_EXODUS=ON.
#
# After this file is included, the target 'seacas_exodus' exists.
# lagrit / liblagrit should declare add_dependencies(... seacas_exodus) so that
# seacas is fully built (headers + libs in place) before LaGriT compiles/links.
#
# Output goes to:  ${CMAKE_BINARY_DIR}/_deps/seacas/lib/   (libraries)
#                  ${CMAKE_BINARY_DIR}/_deps/seacas/include/ (headers)
# EXODUS_ROOT is set to ${CMAKE_BINARY_DIR}/_deps/seacas by the caller.

include(ExternalProject)

# Default to the SEACAS master branch so no tag maintenance is needed.
# Override with -DSEACAS_GIT_TAG=v2024-01-10 (or any release tag) if you need
# a reproducible/offline build.  See github.com/sandialabs/seacas/releases.
set(SEACAS_GIT_TAG "master" CACHE STRING
    "SEACAS git branch or tag to fetch (default: master for latest)")
mark_as_advanced(SEACAS_GIT_TAG)

# Seacas traditionally uses a single directory as both source tree and install
# prefix ($ACCESS in seacas scripts).  We follow that convention here.
set(_seacas_dir "${CMAKE_BINARY_DIR}/_deps/seacas")

# ---- Detect platform-specific settings ----
if(APPLE)
  # Apple Clang has no Fortran; find gfortran from Homebrew or MacPorts.
  find_program(_seacas_fc
    NAMES gfortran-14 gfortran-13 gfortran-12 gfortran
    PATHS
      /opt/homebrew/bin        # Homebrew on Apple Silicon
      /usr/local/bin           # Homebrew on Intel Mac
      /opt/local/bin           # MacPorts
    NO_DEFAULT_PATH
  )
  if(NOT _seacas_fc)
    find_program(_seacas_fc NAMES gfortran)   # fallback: anything on PATH
  endif()
  if(NOT _seacas_fc)
    message(FATAL_ERROR
      "gfortran not found — required to build Exodus with Fortran support.\n"
      "Install via Homebrew:  brew install gcc\n"
      "Install via MacPorts:  sudo port install gcc12\n"
      "Or point CMAKE_Fortran_COMPILER to a working gfortran.")
  endif()
  # macOS ships its own zlib; avoid rebuilding it (often causes conflicts).
  set(_seacas_needs_zlib "NO")
  # macOS netcdf static build does not need -lcurl.
  set(_seacas_add_curl "NO")
else()
  # Linux: use whatever Fortran compiler CMake found.
  set(_seacas_fc "${CMAKE_Fortran_COMPILER}")
  set(_seacas_needs_zlib "YES")
  set(_seacas_add_curl "YES")
endif()

message(STATUS "  SEACAS tag         : ${SEACAS_GIT_TAG}")
message(STATUS "  SEACAS dir         : ${_seacas_dir}")
message(STATUS "  SEACAS FC          : ${_seacas_fc}")
message(STATUS "  SEACAS NEEDS_ZLIB  : ${_seacas_needs_zlib}")
message(STATUS "  SEACAS add curl    : ${_seacas_add_curl}")

ExternalProject_Add(seacas_exodus
  GIT_REPOSITORY  "https://github.com/sandialabs/seacas.git"
  GIT_TAG         "${SEACAS_GIT_TAG}"
  GIT_SHALLOW     TRUE
  GIT_PROGRESS    TRUE

  PREFIX          "${CMAKE_BINARY_DIR}/_deps"
  SOURCE_DIR      "${_seacas_dir}"
  # INSTALL_DIR is informational here; actual install goes to SOURCE_DIR via
  # seacas's own ACCESS convention (handled in seacas_configure.cmake).
  INSTALL_DIR     "${_seacas_dir}"

  # Step 1: patch seacas files, build HDF5/NetCDF/zlib TPLs, configure Exodus.
  CONFIGURE_COMMAND
    ${CMAKE_COMMAND}
      -DSOURCE_DIR=${_seacas_dir}
      -DFC=${_seacas_fc}
      -DCC=${CMAKE_C_COMPILER}
      -DCXX=${CMAKE_CXX_COMPILER}
      -DNEEDS_ZLIB=${_seacas_needs_zlib}
      -DADD_CURL=${_seacas_add_curl}
      -P "${CMAKE_SOURCE_DIR}/cmake/seacas_configure.cmake"

  # Step 2: compile Exodus.
  BUILD_COMMAND
    ${CMAKE_MAKE_PROGRAM} -C "${_seacas_dir}/build"

  # Step 3: install Exodus headers and libs into _seacas_dir.
  INSTALL_COMMAND
    ${CMAKE_MAKE_PROGRAM} -C "${_seacas_dir}/build" install

  # Declare byproducts for Ninja generator (otherwise Ninja won't know these
  # files will be created by this target).
  BUILD_BYPRODUCTS
    "${_seacas_dir}/lib/libexodus.a"
    "${_seacas_dir}/lib/libexodus_for.a"
    "${_seacas_dir}/lib/libexoIIv2for32.a"
    "${_seacas_dir}/lib/libnetcdf.a"
    "${_seacas_dir}/lib/libhdf5.a"
    "${_seacas_dir}/lib/libhdf5_hl.a"
    "${_seacas_dir}/lib/libz.a"
    "${_seacas_dir}/include/exodusII.h"
)
