# LaGriT and ExodusII with cmake

The CMake build system is a way of enabling LaGriT to be cross-platform, cross-architecture, and cross-compiler. This page is a verbose description of building LaGriT and includes instructions for building the optional library for ExodusII.

The CMake build is controlled by CMakeLists.txt and files in directory /cmake

The ExodusII libraries can be built using install-exodus.sh
For additional help on ExodusII, see instructions at https://github.com/sandialabs/seacas
**Note for Exodus with LaGriT, FORTRAN must be set to YES**


There's a really good tutorial-overview on CMake here, it covers how to handle multiple build configurations: https://cliutils.gitlab.io/modern-cmake/


## Download LaGriT

Download the repo as shown under the Code button on git.
For developers, be sure to use SSH version to clone. 

```
$ git clone git@github.com:lanl/LaGriT.git 
$ cd LaGriT/
```

If SSH Key needed, generate a key for your machine.
[See GitHub Docs for SSH Key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
```
Generate a SSH key (NOT RSA) in your .ssh directory.
ssh-keygen -t ed25519 -C "email.lanl.gov"
Copy contents of id_ed25519.pub into your SSH Keys on github (under settings).
```

## Build ExodusII (optional)

See instructions in install-exodus.sh

LaGriT will use libs and files located in TPLs/seacas

Expected libs and include files:

```
lib/libexodus.a          lib/libexoIIv2for32.a  lib/libhdf5_hl.a      lib/libnetcdf.a
lib/libexodus_for.a      lib/libhdf5.a          lib/libhdf5_tools.a   lib/libz.a
include/exodus_config.h  include/exodusII.h     include/exodusII.inc  include/exodusII_par.h
include/netcdf.h         include/hdf5.h
```

Once the ExodusII is successful, build LaGriT as usual but with the EXODUS flag:

```
mkdir build/ && cd build/
cmake .. -DCMAKE_BUILD_TYPE=Debug -DLAGRIT_BUILD_EXODUS=ON
make
```

## Create your build directory:

You can build as Debug (-g) or as Release with the CMake flag "-D CMAKE_BUILD_TYPE=[Debug|Release]".
There's two stages in CMake; the 'configure' stage (where you run 'cmake ..') and the build stage (where you run 'make' using the Makefile created by cmake)

The build type can only be set in the configure stage (with cmake) and will apply to all files in your build directory. If you make a change to cmake, you will need to run in a new directory, or remove all the files and directories created with your previous cmake configuration.

Setup a work directory for dev and debug work (example using name "debug").
Use cmake to create Makefiles and build files. You will do this only once.
Compile the lagrit executable using 'make' or 'make VERBOSE=1'
```
mkdir debug && cd debug
cmake .. -D CMAKE_BUILD_TYPE=Debug
make
```

Run ./lagrit and type command **test** which creates hex mesh and reports expected values.

For debugging the build process, set cmake and make to show verbose screen reporting. These are run from your build directory with the following options:
```
cmake -D CMAKE_FIND_DEBUG_MODE=ON ..
make VERBOSE=1
```
      
## Modify code and update **lagrit** executable

Modify code by working with lagrit source files in LaGriT/src
Do not add any non-code develpment files in the /src directory, they may be detected and attempted to use during compile time.

When you want to compile, do not run cmake again, it already created your makefiles, .o, and other dependencies.

Go to LaGriT/*your_build_name* directory and type
```
make
```

**lagrit** will be built in LaGriT/*your_build_name* directory.

*If you add new files to /src you will need to run cmake again. This will detect all files in src/ and create a new set of Makefiles.*


      
### Example Directory structure for developers:
      
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

## Automatic Configurations

This is work performed by cmake and is usually not modified. Cmake is used to do some automatic file edits that depend on the machine or platform you are using. 

### Header File configuration

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

### C-Fortran Compatibility

LaGriT codes include both Fortran and C/C++ code files. The driver routines are Fortran, the C/C++ files use wrapper functions and definitions set by cmake before code is compiled. These are set during the configuration and should not need to be modified unless new routines are added.

cmake writes fc_mangle.h for c-fortran routines. This file handles the symbol mangling for routines declared in src/lg_f_interface.h include file. For more information on this method visit https://www.netlib.org/lapack/lawnspdf/lawn270.pdf

The C++ wrapper routines need to be listed in CMakeLists.txt:

```
FortranCInterface_HEADER(
    ${SRC_CORE}/fc_mangle.h
    SYMBOLS
        INITLAGRIT # syntax: <subroutine>
        DOTASK
        CMO_GET_NAME
        CMO_GET_INFO
        CMO_GET_INTINFO
        FC_CMO_GET_INT
        FC_CMO_GET_VINT
        FC_CMO_GET_DOUBLE
        FC_CMO_GET_VDOUBLE
        FPASS_TYPES
        INSIDE_TET
        LINESEG_TRI)
```




