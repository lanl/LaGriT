
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  message(STATUS "  Fortran compiler: GNU GFORTRAN")
  set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} -cpp -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero -fno-range-check")
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  message(STATUS "  Fortran compiler: Intel Fortran")
  set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} -fpp -w -O -Qm64 -Qsafe-cray-ptr -integer-size=64 -assume:nominus0 -QRimplicit-import-")
endif()
