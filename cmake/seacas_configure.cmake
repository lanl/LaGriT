# seacas_configure.cmake
# Invoked by ExternalProject_Add CONFIGURE_COMMAND as:
#   cmake -DSOURCE_DIR=... -DFC=... -DCC=... -DCXX=...
#         -DNEEDS_ZLIB=... -DADD_CURL=... -P seacas_configure.cmake
#
# This script is responsible for:
#   1. Patching TPL/netcdf/runcmake.sh  (disable netcdf plugins/tests)
#   2. Patching cmake-exodus            (add -lcurl on Linux)
#   3. Running seacas/install-tpl.sh    (builds HDF5, NetCDF, zlib)
#   4. Running seacas/cmake-exodus      (configures the Exodus cmake build)
#
# The seacas convention is that ACCESS=SOURCE_DIR acts as both source root and
# install prefix.  All TPLs and Exodus install into ${SOURCE_DIR}/lib and
# ${SOURCE_DIR}/include.

cmake_minimum_required(VERSION 3.12)

message(STATUS "[seacas] ---- Configuring SEACAS/Exodus ----")
message(STATUS "[seacas]   Source/install : ${SOURCE_DIR}")
message(STATUS "[seacas]   FC             : ${FC}")
message(STATUS "[seacas]   CC             : ${CC}")
message(STATUS "[seacas]   CXX            : ${CXX}")
message(STATUS "[seacas]   NEEDS_ZLIB     : ${NEEDS_ZLIB}")
message(STATUS "[seacas]   ADD_CURL       : ${ADD_CURL}")

# ---------------------------------------------------------------------------
# 1. Patch TPL/netcdf/runcmake.sh
#    Disable netcdf plugins/multifilters/nczarr-filters and tests.
#    These are the features most likely to cause build failures on HPC/macOS.
#
#    We use bracket strings [=[ ... ]=] so cmake does NOT expand ${MPI} etc.
# ---------------------------------------------------------------------------
set(_runcmake "${SOURCE_DIR}/TPL/netcdf/runcmake.sh")
if(EXISTS "${_runcmake}")
  file(READ "${_runcmake}" _nc)
  if(NOT _nc MATCHES "ENABLE_PLUGINS")
    # The line we anchor on (verbatim content in the file, no cmake expansion)
    set(_nc_search  [=[-DENABLE_PNETCDF:BOOL=${MPI} \]=])
    set(_nc_replace [=[-DENABLE_PNETCDF:BOOL=${MPI} \
         -DENABLE_PLUGINS:BOOL=OFF \
         -DENABLE_MULTIFILTERS:BOOL=NO \
         -DENABLE_NCZARR_FILTERS:BOOL=OFF \
         -DENABLE_TESTS:BOOL=OFF \]=])
    string(REPLACE "${_nc_search}" "${_nc_replace}" _nc_patched "${_nc}")
    if(_nc_patched STREQUAL _nc)
      # Anchor string not found — warn but don't fail; the build might still work.
      message(WARNING
        "[seacas] Could not patch TPL/netcdf/runcmake.sh "
        "(search string not found — seacas may have changed its layout). "
        "If netcdf fails to build, check the file manually.")
    else()
      file(WRITE "${_runcmake}" "${_nc_patched}")
      message(STATUS "[seacas] Patched TPL/netcdf/runcmake.sh")
    endif()
  else()
    message(STATUS "[seacas] TPL/netcdf/runcmake.sh already patched")
  endif()
else()
  message(WARNING "[seacas] TPL/netcdf/runcmake.sh not found — skipping netcdf patch")
endif()

# ---------------------------------------------------------------------------
# 2. Patch TPL/hdf5/runcmake.sh
#    Disable SZIP support — the SZIP library is not available on arm64 macOS
#    and is not needed by LaGriT.
# ---------------------------------------------------------------------------
set(_hdf5_runcmake "${SOURCE_DIR}/TPL/hdf5/runcmake.sh")
if(EXISTS "${_hdf5_runcmake}")
  file(READ "${_hdf5_runcmake}" _hdf5)
  if(NOT _hdf5 MATCHES "HDF5_ENABLE_SZIP_SUPPORT")
    # Anchor on the CMAKE_INSTALL_PREFIX line which is present in all versions
    set(_hdf5_search  [=[-DCMAKE_INSTALL_PREFIX:PATH=${ACCESS} \]=])
    set(_hdf5_replace [=[-DCMAKE_INSTALL_PREFIX:PATH=${ACCESS} \
  -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF \
  -DHDF5_ENABLE_SZIP_ENCODING:BOOL=OFF \]=])
    string(REPLACE "${_hdf5_search}" "${_hdf5_replace}" _hdf5_patched "${_hdf5}")
    if(_hdf5_patched STREQUAL _hdf5)
      message(WARNING
        "[seacas] Could not patch TPL/hdf5/runcmake.sh "
        "(search string not found). Build may fail on arm64 macOS.")
    else()
      file(WRITE "${_hdf5_runcmake}" "${_hdf5_patched}")
      message(STATUS "[seacas] Patched TPL/hdf5/runcmake.sh (disabled SZIP)")
    endif()
  else()
    message(STATUS "[seacas] TPL/hdf5/runcmake.sh already patched")
  endif()
else()
  message(WARNING "[seacas] TPL/hdf5/runcmake.sh not found — skipping SZIP patch")
endif()

# ---------------------------------------------------------------------------
# 4. Patch cmake-exodus to add -lcurl (Linux only)
#    netcdf static libraries on Linux need curl at link time.
# ---------------------------------------------------------------------------
if(ADD_CURL)
  set(_cmake_exodus "${SOURCE_DIR}/cmake-exodus")
  if(EXISTS "${_cmake_exodus}")
    file(READ "${_cmake_exodus}" _ce)
    if(NOT _ce MATCHES "curl;z;dl")
      string(REPLACE
        "DSeacas_EXTRA_LINK_FLAGS=z;dl"
        "DSeacas_EXTRA_LINK_FLAGS=curl;z;dl"
        _ce_patched "${_ce}")
      if(_ce_patched STREQUAL _ce)
        message(WARNING
          "[seacas] Could not patch cmake-exodus for -lcurl "
          "(search string not found). Linking may fail on Linux.")
      else()
        file(WRITE "${_cmake_exodus}" "${_ce_patched}")
        message(STATUS "[seacas] Patched cmake-exodus for -lcurl")
      endif()
    else()
      message(STATUS "[seacas] cmake-exodus already has curl")
    endif()
  else()
    message(WARNING "[seacas] cmake-exodus not found — skipping curl patch")
  endif()
endif()

# ---------------------------------------------------------------------------
# 5. Build TPLs: HDF5, NetCDF, (optionally) zlib
#    install-tpl.sh installs everything under $ACCESS = SOURCE_DIR.
#    Skip if netcdf is already present (idempotent re-run).
# ---------------------------------------------------------------------------
if(EXISTS "${SOURCE_DIR}/lib/libnetcdf.a")
  message(STATUS "[seacas] TPLs already built — skipping install-tpl.sh")
else()
  message(STATUS "[seacas] Building TPLs (HDF5, NetCDF, zlib) — this takes several minutes ...")

  # Set environment variables consumed by install-tpl.sh
  set(ENV{FC}           "${FC}")
  set(ENV{CC}           "${CC}")
  set(ENV{CXX}          "${CXX}")
  set(ENV{ACCESS}       "${SOURCE_DIR}")   # seacas install prefix
  set(ENV{CGNS}         "NO")
  set(ENV{MATIO}        "NO")
  set(ENV{GNU_PARALLEL} "NO")
  set(ENV{FMT}          "NO")
  set(ENV{CATCH2}       "NO")
  set(ENV{SZIP}         "NO")   # arm64 macOS has no SZIP lib; HDF5 doesn't need it
  set(ENV{SHARED}       "NO")
  if(NEEDS_ZLIB)
    set(ENV{NEEDS_ZLIB} "YES")
  else()
    set(ENV{NEEDS_ZLIB} "NO")
  endif()

  # On macOS, Homebrew/MacPorts bins may not be on the default cmake PATH.
  if(APPLE)
    set(ENV{PATH}
      "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:$ENV{PATH}")
  endif()

  execute_process(
    COMMAND bash install-tpl.sh
    WORKING_DIRECTORY "${SOURCE_DIR}"
    RESULT_VARIABLE _tpl_rc
  )
  if(NOT _tpl_rc EQUAL 0)
    message(FATAL_ERROR
      "[seacas] install-tpl.sh failed (exit ${_tpl_rc}).\n"
      "Check output above.  Common causes:\n"
      "  Linux  — missing: wget/curl, make, cmake\n"
      "  macOS  — gfortran not on PATH; try: brew install gcc")
  endif()
  message(STATUS "[seacas] TPLs built successfully")
endif()

# ---------------------------------------------------------------------------
# 6. Configure Exodus with cmake-exodus
#    Creates ${SOURCE_DIR}/build/CMakeCache.txt.
#    Skip if already configured (idempotent).
# ---------------------------------------------------------------------------
if(EXISTS "${SOURCE_DIR}/build/CMakeCache.txt")
  message(STATUS "[seacas] Exodus already configured — skipping cmake-exodus")
else()
  message(STATUS "[seacas] Configuring Exodus ...")
  file(MAKE_DIRECTORY "${SOURCE_DIR}/build")

  set(ENV{FC}      "${FC}")
  set(ENV{CC}      "${CC}")
  set(ENV{CXX}     "${CXX}")
  set(ENV{ACCESS}  "${SOURCE_DIR}")
  set(ENV{FORTRAN} "YES")
  set(ENV{SHARED}  "NO")

  if(APPLE)
    set(ENV{PATH}
      "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:$ENV{PATH}")
  endif()

  execute_process(
    COMMAND bash ../cmake-exodus
    WORKING_DIRECTORY "${SOURCE_DIR}/build"
    RESULT_VARIABLE _ce_rc
  )
  if(NOT _ce_rc EQUAL 0)
    message(FATAL_ERROR
      "[seacas] cmake-exodus failed (exit ${_ce_rc}).\n"
      "Check output above.")
  endif()
  message(STATUS "[seacas] Exodus configured successfully")
endif()
