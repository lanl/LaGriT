# CMake Build System

The CMake build system is a way of enabling LaGriT to
be cross-platform, cross-architecture, and cross-compiler.

See Build and Install Instructions: https://github.com/lanl/LaGriT#readme

There's a really good tutorial-overview on CMake here, it covers how to handle multiple build configurations: https://cliutils.gitlab.io/modern-cmake/


## Code Development

You can build as Debug (-g) or as Release with the CMake flag "-D CMAKE_BUILD_TYPE=[Debug|Release]".
There's two stages in CMake; the 'configure' stage (where you run 'cmake ..') and the build stage (where you run 'make' using the Makefile created by cmake)
      
The build type can only be set in the configure stage. One way to handle building both Debug and release is to make two build directories. Here's a short guide: https://riptutorial.com/cmake/example/7357/switching-between-build-types--e-g--debug-and-release

You can set cmake and make to show verbose screen reporting. These are run from your buld directory with the following options:
```
cmake -D CMAKE_FIND_DEBUG_MODE=ON ..
make VERBOSE=1
``

For LANL developers, you may need to generate a token to use as password. See instructions at https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token


## Create your build directory:

Setup a work directory for dev and debug work (example using name "debug").
Use cmake to create Makefiles and build files. You will do this only once.
Compile the lagrit executable using 'make' or 'make VERBOSE=1'
```
mkdir debug && cd debug
cmake .. -D CMAKE_BUILD_TYPE=Debug
make
```

Run ./lagrit and type command **test** which creates hex mesh and reports expected values.
      
## Modify code

Modify code by working with lagrit source files in LaGriT/src
Do not add any non-code develpment files in the /src directory, they may be detected and attempted to use during compile time.

When you want to compile, do not run cmake again, it already created your makefiles, .o, and other dependencies.

Go to LaGriT/*your_build_name* directory and type
```
make
```

**lagrit** will be built in LaGriT/*your_build_name* directory.

*If you add new files to /src you will need to run cmake again. This will detect all files in src/ and create a new set of Makefiles.*


      
### Example Directory structure for development:
      
```
LaGriT/
|
└── build/
    └── lagrit executable, Makefiles built by cmake, cmake files    
└── src/
    └── file.cpp, file.f, file.c, lagrit.h, etc
|
├── CMakeLists.txt
└── cmake/
    └── CompilerFlags-C.cmake, CompilerFlags-Fortran.cmake, DetectBitSize.cmake, PlatformSettings.cmake
```

### Example Screen Output for cmake .. -DLAGRIT_BUILD_EXODUS=ON

```
-- The Fortran compiler identification is GNU 7.5.0
-- The CXX compiler identification is GNU 7.5.0
-- The C compiler identification is GNU 7.5.0
-- ==========================================
-- ============Configuring LaGriT============
-- ===================v3.3.3=================
-- Compile LaGriT as a static binary = ON
-- Compile LaGriT with ExodusII = ON
-- ExodusII libraries      : /project/eesdev/tam/LaGriT/TPLs/seacas/lib
-- ExodusII include        : /project/eesdev/tam/LaGriT/TPLs/seacas/include
-- Detected System:
--   Operating System: Linux
--   Architecture: 64-bit
--   Fortran compiler: GNU GFORTRAN
--   C compiler: GNU GCC
--   C++ compiler: GNU G++
-- Using Third Party Libraries.
-- TPL libs: /project/eesdev/tam/LaGriT/TPLs/seacas/lib
-- TPL include: /project/eesdev/tam/LaGriT/TPLs/seacas/include
-- Detecting Fortran/C Interface - Found GLOBAL and MODULE mangling
-- Verifying Fortran/C Compiler Compatibility - Success
-- Compilers:
--   FORTRAN [compiler = "/usr/bin/f95"; flags = " -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero -fno-range-check"]
--   C [compiler = "/usr/bin/cc"; flags = " -w -m64"]
--   C++ [compiler = "/usr/bin/c++"; flags = " -w -m64"]
-- Configuring done
-- Generating done
-- Build files have been written to: /project/eesdev/tam/LaGriT/build
```

### Example Screen Output for make VERBOSE=1

```
[  2%] Building C object CMakeFiles/liblagrit.dir/lg_util/src/gmvwrite.c.o
/usr/bin/cc -DLAGRIT_INCLUDE_EXODUSII -Dlinx64 -I/project/eesdev/tam/LaGriT/TPLs/seacas/include -w -m64 -MD -MT CMakeFiles/liblagrit.dir/lg_util/src/gmvwrite.c.o -MF CMakeFiles/liblagrit.dir/lg_util/src/gmvwrite.c.o.d -o CMakeFiles/liblagrit.dir/lg_util/src/gmvwrite.c.o -c /project/eesdev/tam/LaGriT/lg_util/src/gmvwrite.c

[ 36%] Building Fortran object CMakeFiles/liblagrit.dir/src/dumpexodusII.f.o
/usr/bin/f95 -DLAGRIT_INCLUDE_EXODUSII -Dlinx64 -I/project/eesdev/tam/LaGriT/TPLs/seacas/include -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero -fno-range-check -cpp -c /project/eesdev/tam/LaGriT/src/dumpexodusII.f -o CMakeFiles/liblagrit.dir/src/dumpexodusII.f.o

[ 70%] Building CXX object CMakeFiles/liblagrit.dir/src/poi_helperFunctions.cpp.o
/usr/bin/c++ -DLAGRIT_INCLUDE_EXODUSII -Dlinx64 -I/project/eesdev/tam/LaGriT/TPLs/seacas/include -w -m64 -MD -MT CMakeFiles/liblagrit.dir/src/poi_helperFunctions.cpp.o -MF CMakeFiles/liblagrit.dir/src/poi_helperFunctions.cpp.o.d -o CMakeFiles/liblagrit.dir/src/poi_helperFunctions.cpp.o -c /project/eesdev/tam/LaGriT/src/poi_helperFunctions.cpp

[100%] Linking CXX static library liblagrit.a
/n/swdev/packages/Ubuntu-18.04-x86_64/cmake/cmake-3.22.1/bin/cmake -P CMakeFiles/liblagrit.dir/cmake_clean_target.cmake

/n/swdev/packages/Ubuntu-18.04-x86_64/cmake/cmake-3.22.1/bin/cmake -E cmake_link_script CMakeFiles/liblagrit.dir/link.txt

/usr/bin/ranlib liblagrit.a
make[2]: Leaving directory '/project/eesdev/tam/LaGriT/build'
[100%] Built target liblagrit

[100%] Linking Fortran executable lagrit
/n/swdev/packages/Ubuntu-18.04-x86_64/cmake/cmake-3.22.1/bin/cmake -E cmake_link_script CMakeFiles/lagrit.exe.dir/link.txt

/usr/bin/f95 -static-libgcc -static-libstdc++  -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero -fno-range-check CMakeFiles/lagrit.exe.dir/src/lagrit_main.f.o -o lagrit  liblagrit.a -L/project/eesdev/tam/LaGriT/TPLs/seacas/lib -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -ldl -static-libgfortran -static-libgcc -lstdc++ 

[100%] Built target lagrit
```


## Header File configuration

CMake automatically configures two header files:

- `lg_util/src/mm2000.h`
- `src/lagrit.h`

The original files have the same name, but ending with `.in`.

CMake configures the files as following. Consider a line from `lagrit.h.in`:

```fortran
      parameter      (v_major=@PROJECT_VERSION_MAJOR@)
```

Here, `@PROJECT_VERSION_MAJOR@` is a CMake variable which will be replaced by CMake.

Here is how CMake configures the file:

```cmake
configure_file(
    ${SRC_CORE}/lagrit.h.in
    ${SRC_CORE}/lagrit.h
    @ONLY
)
```

`${SRC_CORE}/lagrit.h.in` is the 'template' file, and `${SRC_CORE}/lagrit.h` is the output file. The `@ONLY` line means to **only** replace variables between the `@` symbol.

The variable above - `@PROJECT_VERSION_MAJOR@` is an instrinsic CMake variable. You can define your own. For example, CMake code within `SetBitSize.cmake` configures the `lg_util/src/mm2000.h.in` file.
