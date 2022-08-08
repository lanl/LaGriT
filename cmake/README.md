# CMake Build System

The CMake build system is a way of enabling LaGriT to
be cross-platform, cross-architecture, and cross-compiler.

See Build and Install Instructions: https://github.com/lanl/LaGriT#readme

There's a really good tutorial-overview on CMake here, it covers how to handle multiple build configurations: https://cliutils.gitlab.io/modern-cmake/


## Code Development

You can build as Debug (-g) or as Release with the CMake flag "-D CMAKE_BUILD_TYPE=[Debug|Release]".
There's two stages in CMake; the 'configure' stage (where you run 'cmake ..') and the build stage (where you run 'make' using the Makefile created by cmake)
      
The build type can only be set in the configure stage. One way to handle building both Debug and release is to make two build directories. Here's a short guide: https://riptutorial.com/cmake/example/7357/switching-between-build-types--e-g--debug-and-release

For LANL developers, you may need to generate a token to use as password. See instructions at https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token


## Create your build directory:

Setup a work directory for dev and debug work (example using name "debug")
Compile new executable using ```make``` in the debug/ directory.
```
mkdir debug/ && cd debug/ && cmake .. -D CMAKE_BUILD_TYPE=Debug
make
```

Before changing code you might first add a directory and build a release (no debug) version:
```
mkdir release/ && cd release/ && cmake .. -D CMAKE_BUILD_TYPE=Release
make
```

A quick check for successful compile, run ./lagrit and type command **test** which creates hex mesh and reports values.
      
## Modify code

Modify code by working with files files in LaGriT/src
Do not add any non-code develpment files in the /src directory, they may be detected and attempted to use during compile time.
When you want to compile, do not run cmake again, it already created your makefiles, .o, and other dependencies.

Go to LaGriT/*your_build_name* directory and type
```
make
```

**lagrit** will be built in LaGriT/*your_build_name* directory.

*If you add new files you will need to run cmake again. This will detect all files in src/ and create a new set of Makefils.*


      
### Example Directory structure:
      
```
LaGriT/
├── CMakeLists.txt
└── debug/
    └── lagrit executable, Makefile built by cmake, cmake files    
└── src/
    └── file.cpp, file.f, file.c, etc
```

### Example Screen Output for cmake and make

```
% mkdir build/ && cd build/
es22:build:bsh% cmake ..
-- The Fortran compiler identification is GNU 7.5.0
-- The CXX compiler identification is GNU 7.5.0
-- The C compiler identification is GNU 7.5.0
-- Detecting Fortran compiler ABI info
-- Detecting Fortran compiler ABI info - done
-- Check for working Fortran compiler: /usr/bin/f95 - skipped
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Check for working C compiler: /usr/bin/cc - skipped
-- Detecting C compile features
-- Detecting C compile features - done
-- ==========================================
-- ============Configuring LaGriT============
-- ===================v3.3.3=================
-- Compile LaGriT as a static binary = ON
-- Could NOT find Exodus (missing: Exodus_LIBRARIES)
-- Found NetCDF: /usr/include
-- HDF5 C compiler wrapper is unable to compile a minimal HDF5 program.
-- Found HDF5: /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.so (found version "1.10.0.1")
-- Found ZLIB: /usr/lib/x86_64-linux-gnu/libz.so (found version "1.2.11")
WARNING: ExodusII and/or other dependencies could not be found. Compiling without ExodusII support.
-- Exodus Dependency Status:
--   Found Exodus: FALSE
--   Found NetCDF: TRUE
--   Found HDF5:   TRUE
--   Found ZLIB:   TRUE
-- Detected System:
--   Operating System: Linux
--   Architecture: 64-bit
--   Fortran compiler: GNU GFORTRAN
--   C compiler: GNU GCC
--   C++ compiler: GNU G++
-- Detecting Fortran/C Interface
-- Detecting Fortran/C Interface - Found GLOBAL and MODULE mangling
-- Verifying Fortran/C Compiler Compatibility
-- Verifying Fortran/C Compiler Compatibility - Success
-- Compilers:
--   FORTRAN [compiler = "/usr/bin/f95"; flags = " -m64 -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero -fno-range-check"]
--   C [compiler = "/usr/bin/cc"; flags = " -w -m64"]
--   C++ [compiler = "/usr/bin/c++"; flags = " -w -m64"]
-- Configuring done
-- Generating done
-- Build files have been written to: /project/eesdev/tam/clone/LaGriT-master/build

% make
Scanning dependencies of target liblagrit
[  0%] Building Fortran object CMakeFiles/liblagrit.dir/src/ColoredGraphModule.f90.o
[  0%] Building Fortran object CMakeFiles/liblagrit.dir/src/GraphModule.f90.o
[ 99%] Building Fortran object CMakeFiles/liblagrit.dir/src/zq.f.o
...
[100%] Linking CXX static library liblagrit.a
[100%] Built target liblagrit
Scanning dependencies of target lagrit.exe
[100%] Building Fortran object CMakeFiles/lagrit.exe.dir/src/lagrit_main.f.o
[100%] Linking Fortran executable lagrit
[100%] Built target lagrit

es22:build:bsh% ./lagrit
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
