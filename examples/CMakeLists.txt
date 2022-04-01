add_executable(lagrit_cxx_example example.c)
target_link_libraries(lagrit_cxx_example
    PRIVATE lagrit_lib)
target_include_directories(lagrit_cxx_example
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include)

configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/example.py.in"
    "${CMAKE_CURRENT_BINARY_DIR}/example.py"
    @ONLY)

install(
    TARGETS
        lagrit_cxx_example
    DESTINATION examples)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/example.py" DESTINATION examples)
