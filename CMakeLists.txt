cmake_minimum_required(VERSION 3.16)
project(del_vor LANGUAGES Fortran)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Fortran lines are kept to 72 columns by convention; the compiler
# is told free form with the standard 132-column limit.
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} -ffree-form -std=f2018 -Wall -Wextra -Wno-compare-reals")
  set(CMAKE_Fortran_FLAGS_DEBUG
      "-g -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -stand f18")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# The core library: everything except the programs.
file(GLOB DEL_VOR_CORE ${CMAKE_SOURCE_DIR}/src/del_vor_*.F90)
add_library(del_vor_core STATIC ${DEL_VOR_CORE})

add_executable(del_vor src/del_vor.F90)
target_link_libraries(del_vor del_vor_core)

# Self tests of the kernels (test/README.md); not installed.
add_executable(del_vor_selftest test/del_vor_selftest.F90)
target_link_libraries(del_vor_selftest del_vor_core)

install(TARGETS del_vor RUNTIME DESTINATION bin)
