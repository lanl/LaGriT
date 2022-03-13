cmake_minimum_required(VERSION 3.12)
project(LagritTests)

find_package(Python3 COMPONENTS Interpreter)

set(LG_TEST_ARGS
    "-exe $<TARGET_FILE:lagrit_exe>"
    "--levels 1")

add_test(
    NAME lagrit_tests
    COMMAND
        ${Python3_EXECUTABLE} runtests.py ${LG_TEST_ARGS}
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_SOURCE_DIR}
)