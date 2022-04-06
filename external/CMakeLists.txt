cmake_minimum_required(VERSION 3.16)
project(fetchContent_example CXX)

include(FetchContent)

set(LG_BUILD_HDF5 ON)
set(LG_BUILD_NETCDF ON)
set(LG_BUILD_EXODUS ON)

if (SUBPROJECT_INSTALL_PREFIX)
  set(_SUBPROJECT_INSTALL_PREFIX ${SUBPROJECT_INSTALL_PREFIX}/ )
else()
  set(_SUBPROJECT_INSTALL_PREFIX ${CMAKE_CURRENT_LIST_DIR})
endif()

# ===== HDF5 ===================================== #

if(LG_BUILD_HDF5)
    message(STATUS "Fetching HDF5...")

    # Set HDF5 CMake flags
    set(BUILD_TESTING                           OFF CACHE INTERNAL "")
    set(CMAKE_INSTALL_PREFIX                    "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
    set(DEFAULT_API_VERSION                     V18 CACHE INTERNAL "")
    set(HDF5_ENABLE_PARALLEL                    OFF CACHE INTERNAL "")
    set(HDF5_ENABLE_Z_LIB_SUPPORT               OFF CACHE INTERNAL "") # WARNING: default on
    set(HDF5_BUILD_CPP_LIB                      OFF CACHE INTERNAL "")
    #set(HDF5_BUILD_FORTRAN                     ??? CACHE INTERNAL "")
    set(HDF5_BUILD_HL_LIB                       ON CACHE INTERNAL "")
    set(HDF5_DISABLE_COMPILER_WARNINGS          ON CACHE INTERNAL "")

    FetchContent_Declare(
        HDF5
        GIT_REPOSITORY      "https://github.com/HDFGroup/hdf5.git"
        GIT_TAG             "db30c2da68ece4a155e9e50c28ec16d6057509b2" # tag v1.10.7
    )

    FetchContent_MakeAvailable(HDF5)
endif()

# ===== NetCDF ===================================== #

if(LG_BUILD_NETCDF)
    # Set NetCDF CMake flags
    set(BUILD_TESTING                           OFF CACHE INTERNAL "")
    set(CMAKE_INSTALL_PREFIX                    "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
    set(CMAKE_INSTALL_LIBDIR                    lib CACHE INTERNAL "")
    set(ENABLE_NETCDF_4                         ON CACHE INTERNAL "")
    set(ENABLE_PNETCDF                          OFF CACHE INTERNAL "")
    set(ENABLE_CDF5                             ON CACHE INTERNAL "")
    set(ENABLE_MMAP                             ON CACHE INTERNAL "")
    set(ENABLE_DAP                              OFF CACHE INTERNAL "")
    set(ENABLE_V2_API                           OFF CACHE INTERNAL "")
    set(ENABLE_CONVERSION_WARNINGS              OFF CACHE INTERNAL "")

    if(LG_BUILD_HDF5)
        set(HDF5_ROOT                               "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
        set(HDF5_DIR                                "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
    endif()

    message(STATUS "Fetching NetCDF...")
    FetchContent_Declare(
        NetCDF
        GIT_REPOSITORY      "https://github.com/Unidata/netcdf-c.git"
        GIT_TAG             "2c68546b230fb50b61426acf2a3e4c389ad4dfb0" # tag v4.6.2
    )

    FetchContent_MakeAvailable(NetCDF)
endif()

# ===== ExodusII ===================================== #

if(LG_BUILD_EXODUS)
    # Set ExodusII CMake flags

    #set(CMAKE_CXX_FLAGS="${CXXFLAGS} ${CXX_WARNING_FLAGS} ${SANITIZE}" CACHE INTERNAL "")
    #set(CMAKE_C_FLAGS="${CFLAGS} ${C_WARNING_FLAGS} ${SANITIZE}" CACHE INTERNAL "")
    #set(CMAKE_Fortran_FLAGS="${FFLAGS} ${F77_WARNING_FLAGS} ${SANITIZE}" CACHE INTERNAL "")
    set(CMAKE_INSTALL_RPATH                     "${_SUBPROJECT_INSTALL_PREFIX}/lib" CACHE INTERNAL "")
    set(SEACASProj_ENABLE_SEACASExodus          YES CACHE INTERNAL "")
    set(SEACASProj_ENABLE_SEACASExodus_for      YES CACHE INTERNAL "")
    set(SEACASProj_ENABLE_SEACASExoIIv2for32    YES CACHE INTERNAL "")
    set(SEACASProj_ENABLE_TESTS                 YES CACHE INTERNAL "")
    set(CMAKE_INSTALL_PREFIX                    "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
    set(SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST YES CACHE INTERNAL "")
    set(SEACASProj_HIDE_DEPRECATED_CODE         NO CACHE INTERNAL "")
    set(SEACASProj_ENABLE_Fortran               YES CACHE INTERNAL "")
    set(TPL_ENABLE_Netcdf                       YES CACHE INTERNAL "")
    set(TPL_ENABLE_MPI                          NO CACHE INTERNAL "")
    set(TPL_ENABLE_Pthread                      NO CACHE INTERNAL "")
    set(SEACASExodus_ENABLE_THREADSAFE          NO CACHE INTERNAL "")
    #set(NetCDF_ROOT:PATH=${NETCDF_PATH} CACHE INTERNAL "")
    #set(HDF5_ROOT:PATH=${HDF5_PATH} CACHE INTERNAL "")
    set(HDF5_NO_SYSTEM_PATHS                    YES CACHE INTERNAL "")

    if(BUILD_SHARED_LIBS)
        set(SEACASExodus_ENABLE_STATIC          NO CACHE INTERNAL "")
    else()
        set(SEACASExodus_ENABLE_STATIC          YES CACHE INTERNAL "")
    endif()

    if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
        set(CMAKE_MACOSX_RPATH                  ON CACHE INTERNAL "")
    endif()

    message(STATUS "Fetching ExodusII...")
    FetchContent_Declare(
        Exodus
        GIT_REPOSITORY   "https://github.com/gsjaardema/seacas.git"
        GIT_TAG          "3eba1c8f0ec86f8359064c7f8ed5f5ac07a1ce68" # tag v2021-10-11
    )

    FetchContent_MakeAvailable(Exodus)
endif()



#${ACCESS}

#install(TARGETS ${PROJECT_NAME}
#        RUNTIME DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/bin
#        ARCHIVE DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/lib/static
#        LIBRARY DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/lib)
