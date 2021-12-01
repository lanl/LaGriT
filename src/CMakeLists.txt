set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

# Grab `lg_core/` source files
file(
    GLOB_RECURSE
    SRC_CORE_FILES
    "${dir}/*.c"
    "${dir}/*.cpp"
    "${dir}/*.f"
    "${dir}/*.F"
    "${dir}/*.f90"
)

# Compile the main LaGriT binary
add_executable(
    "${PROJECT_NAME}-exe"
    "${dir}/lagrit_main.f"
)

set_target_properties(
    ${EXE}
    PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
)

target_link_libraries(
    ${EXE}
    ${LIB}
)
