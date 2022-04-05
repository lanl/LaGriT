cmake_minimum_required(VERSION 3.14)
project(fetchContent_example CXX)

include(FetchContent)

if (SUBPROJECT_INSTALL_PREFIX)
  set(_SUBPROJECT_INSTALL_PREFIX ${SUBPROJECT_INSTALL_PREFIX}/ )
else()
  set(_SUBPROJECT_INSTALL_PREFIX . )
endif()

message(STATUS "Downloading / configuring HDF5")

# Force-set CMake configure flags
set(BUILD_TESTING OFF CACHE INTERNAL "")
set(CMAKE_INSTALL_PREFIX "${_SUBPROJECT_INSTALL_PREFIX}" CACHE INTERNAL "")
set(DEFAULT_API_VERSION V18 CACHE INTERNAL "")
set(HDF5_ENABLE_PARALLEL OFF CACHE INTERNAL "")
set(HDF5_ENABLE_Z_LIB_SUPPORT OFF CACHE INTERNAL "") # WARNING: default on
set(HDF5_BUILD_CPP_LIB OFF CACHE INTERNAL "")
#set(HDF5_BUILD_FORTRAN ??? CACHE INTERNAL "")
set(HDF5_BUILD_HL_LIB ON CACHE INTERNAL "")
set(HDF5_DISABLE_COMPILER_WARNINGS ON CACHE INTERNAL "")

FetchContent_Declare(
    HDF5
    GIT_REPOSITORY      "https://github.com/HDFGroup/hdf5.git"
    GIT_TAG             "db30c2da68ece4a155e9e50c28ec16d6057509b2" # tag v1.10.7
)

FetchContent_MakeAvailable(HDF5)

#message(STATUS "Configuring NetCDF")
#FetchContent_Declare(
#    NetCDF
#    GIT_REPOSITORY      "https://github.com/Unidata/netcdf-c.git"
#    GIT_TAG             "2c68546b230fb50b61426acf2a3e4c389ad4dfb0" # tag v4.6.2
#)
#
#FetchContent_MakeAvailable(
#    NetCDF
#    CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/"
#)

#install(TARGETS ${PROJECT_NAME}
#        RUNTIME DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/bin
#        ARCHIVE DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/lib/static
#        LIBRARY DESTINATION ${_SUBPROJECT_INSTALL_PREFIX}/lib)
