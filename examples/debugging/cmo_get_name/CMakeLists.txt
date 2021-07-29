cmake_minimum_required(VERSION 3.10)

project(LaGriT VERSION 0.1.0)
enable_language(Fortran CXX)

include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

#add_subdirectory(interfaces)
add_executable(F_lagrit_test lagrit_main.F)
add_executable(C_lagrit_test main.cpp)

add_library(LGTest SHARED subroutine.F interface_lagrit.F)

target_link_libraries(F_lagrit_test PUBLIC LGTest)
target_link_libraries(C_lagrit_test PUBLIC LGTest)
