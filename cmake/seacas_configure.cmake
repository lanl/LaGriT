# seacas_configure.cmake
# Invoked by ExternalProject_Add CONFIGURE_COMMAND as:
#   cmake -DSOURCE_DIR=... -DFC=... -DCC=... -DCXX=...
#         -DNEEDS_ZLIB=... -DADD_CURL=... -P seacas_configure.cmake
#
# This script is responsible for:
#   1. Patching TPL/netcdf/runcmake.sh  (disable netcdf plugins/tests)
#   2. Patching TPL/hdf5/runcmake.sh    (disable SZIP)
#   3. Patching install-tpl.sh          (git clone HDF5 instead of tarball)
#   4. Patching cmake-exodus            (add -lcurl on Linux)
#   5. Running seacas/install-tpl.sh    (builds HDF5, NetCDF, zlib)
#   6. Running seacas/cmake-exodus      (configures the Exodus cmake build)
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
#    Explicitly disable SZIP — when NEEDS_SZIP=NO seacas omits the flag
#    entirely and HDF5 cmake auto-detects and enables SZIP if any szip lib
#    is present (common on macOS/arm64).  We force it OFF explicitly.
# ---------------------------------------------------------------------------
set(_hdf5_runcmake "${SOURCE_DIR}/TPL/hdf5/runcmake.sh")
if(EXISTS "${_hdf5_runcmake}")
  file(READ "${_hdf5_runcmake}" _hdf5)
  if(NOT _hdf5 MATCHES "HDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF")
    # Anchor on the Z_LIB line which is stable across seacas versions
    set(_hdf5_search  [=[-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON \]=])
    set(_hdf5_replace [=[-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON \
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
# 3. Patch install-tpl.sh to fetch HDF5 with git clone instead of a tarball
#    The GitHub archive download (codeload.github.com) is reset partway on
#    some VPNs/proxies and cannot be resumed, leaving a truncated tarball and
#    a half-extracted source tree (HDF5 cmake then fails: "src" not found).
#    A shallow git clone into the same directory works, and matches how
#    seacas already fetches netcdf.
# ---------------------------------------------------------------------------
set(_install_tpl "${SOURCE_DIR}/install-tpl.sh")
if(EXISTS "${_install_tpl}")
  file(READ "${_install_tpl}" _it)
  set(_it_marker [=[HDFGroup/hdf5.git hdf5-${hdf_version}]=])
  string(FIND "${_it}" "${_it_marker}" _it_done)
  if(_it_done EQUAL -1)
    set(_it_curl_search  [=[curl -O -L --insecure https://github.com/HDFGroup/hdf5/archive/refs/tags/${hdf_version}.tar.gz]=])
    set(_it_curl_replace [=[git clone --depth 1 --branch ${hdf_version} https://github.com/HDFGroup/hdf5.git hdf5-${hdf_version}]=])
    set(_it_tar_search   [=[tar -zxf ${hdf_version}.tar.gz]=])
    set(_it_tar_replace  [=[:  # cloned with git, nothing to extract]=])
    string(REPLACE "${_it_curl_search}" "${_it_curl_replace}" _it_patched "${_it}")
    string(REPLACE "${_it_tar_search}"  "${_it_tar_replace}"  _it_patched "${_it_patched}")
    if(_it_patched STREQUAL _it)
      message(WARNING
        "[seacas] Could not patch install-tpl.sh to git clone HDF5 "
        "(search string not found). HDF5 will be downloaded as a tarball.")
    else()
      file(WRITE "${_install_tpl}" "${_it_patched}")
      message(STATUS "[seacas] Patched install-tpl.sh (git clone HDF5)")
    endif()
  else()
    message(STATUS "[seacas] install-tpl.sh already patched")
  endif()
else()
  message(WARNING "[seacas] install-tpl.sh not found — skipping HDF5 git clone patch")
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
      "  network — download interrupted; remove ${SOURCE_DIR}/TPL/<pkg>/<pkg>-* and re-run make\n"
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
