# removing legacy flags like -m64
# flag -m64 may be needed for C code lg_util/opsys.h
 
# ======= C flags =========================================
if("${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
  MESSAGE(STATUS "  C compiler: Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -m64")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  MESSAGE(STATUS "  C compiler: GNU GCC")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -m64")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")
  MESSAGE(STATUS "  C compiler: Intel C")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
  MESSAGE(STATUS "  C compiler: Microsoft Visual C")

else()
  message(STATUS "  C compiler not recognized: ${CMAKE_C_COMPILER_ID}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w -m64")

endif()

# ======= C++ flags =======================================
if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
  message(STATUS "  C++ compiler: Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w -stdlib=libc++ -std=c++0x")

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  message(STATUS "  C++ compiler: GNU G++")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  message(STATUS "  C++ compiler: Intel C++")

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  message(STATUS "  C++ compiler: Microsoft Visual C++")

else()
  message(STATUS "  C++ compiler not recognized: ${CMAKE_CXX_COMPILER_ID}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")

endif()
