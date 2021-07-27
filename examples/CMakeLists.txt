cmake_minimum_required(VERSION 3.1)
project(TestInitLG)

set(CMAKE_C_FLAGS "/w /m64 /Dwin64 /QRimplicit-import-")
set(CMAKE_CXX_FLAGS ${CMAKE_C_FLAGS})
set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(MSVC)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>
    )
endif()


add_library(LIBLaGriT SHARED IMPORTED)
set_property(TARGET LIBLaGriT PROPERTY
             IMPORTED_LOCATION "C:\\Users\\livingston\\Desktop (WIN)\\LaGriT-Windows\\examples\\liblagrit.dll")
set_property(TARGET LIBLaGriT PROPERTY
             IMPORTED_IMPLIB "C:\\Users\\livingston\\Desktop (WIN)\\LaGriT-Windows\\examples\\liblagrit.lib")
add_executable(TestInitLG main.cpp)
target_link_libraries(TestInitLG PRIVATE LIBLaGriT)