cmake_minimum_required(VERSION 3.1)
project(LGDemo)

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


add_library(liblagrit SHARED IMPORTED)
set_property(TARGET liblagrit PROPERTY
             IMPORTED_LOCATION "C:\\Users\\livingston\\Desktop (WIN)\\LaGriT-Windows\\examples\\complete_interface\\libs\\liblagrit.dll")
set_property(TARGET liblagrit PROPERTY
             IMPORTED_IMPLIB "C:\\Users\\livingston\\Desktop (WIN)\\LaGriT-Windows\\examples\\complete_interface\\libs\\liblagrit.lib")
add_executable(LGDemo src/main.cpp src/Lagrit.cpp)
target_link_libraries(LGDemo PRIVATE liblagrit)