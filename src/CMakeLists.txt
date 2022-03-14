# Validate compiler & symbol interop
include(FortranCInterface)
FortranCInterface_VERIFY()
FortranCInterface_VERIFY(CXX)

if(MSVC)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Dwin64")
    set(CMAKE_EXE_LINKER_FLAGS "/NODEFAULTLIB:msvcrtd.lib")
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
    add_compile_options(
        $<$<CONFIG:>:/MT>
        $<$<CONFIG:Debug>:/MTd>
        $<$<CONFIG:Release>:/MT>)
endif()

# Empty targets that will be populated in subdirectories
add_executable(lagrit_exe "")
add_library(lagrit_lib "")

add_subdirectory(lg_util)
add_subdirectory(lg_core)
add_subdirectory(lg_public)

# Enable Fortran processor directives
# This should be in lg_core/CMakeLists.txt, but
# must be here for scope reasons
set_property(
    SOURCE
        ${CMAKE_CURRENT_LIST_DIR}/lg_core/dumpexodusII.f
    PROPERTY
        Fortran_PREPROCESS ON)

# Properties for LaGriT library
set_target_properties(lagrit_lib
  PROPERTIES
    OUTPUT_NAME "lagrit"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}")

# Properties for LaGriT executable
set_target_properties(lagrit_exe
  PROPERTIES
    OUTPUT_NAME "lagrit"
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/")

target_link_libraries(lagrit_exe lagrit_lib)

if(NOT ${BUILD_SHARED_LIBS})
    set_target_properties(lagrit_exe PROPERTIES LINK_SEARCH_START_STATIC 1)
    set_target_properties(lagrit_exe PROPERTIES LINK_SEARCH_END_STATIC 1)
endif()