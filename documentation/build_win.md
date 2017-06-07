# Building LaGriT on Windows
## 1. Installing Cygwin

  Get the latest version of Cygwin for your operating system
  Download link: https://cygwin.com/install.html

  This guide is for the x86_64 setup file since its for 64 bit OS.

  The installation procedure with the setup file is straightforward. 
  My installation and download path: C:/Users/304285/dev/cygwin
  Find the best mirror here: https://cygwin.com/mirrors.html

  Necessary libraries to install:

    a) zlib-devel
    b) libhdf5-devel
    c) libnetcdf-devel
    d) libcurl-devel

  Helpful libraries to install:

    a) wget - helpful for downloading resources from url
    b) git - another plugin for obtaining external resources
    c) make - used by zlib, HDF5, NetCDF for building (make sure its GNU Make)
    d) cmake - used by ExodusII
    e) dos2unix - sometimes cygwin confuses Windows ecoding, use this binary on the file
      Every now and then this wouldn't fix the issue unless forced 'dos2unix -f filename'
    f) gzip - for unzipping .tar files with 'tar -zxvf'
    g) curl - Web requests service mainly used by PHP, but ExodusII wants it too for some reason

  Compilers needed to install:

    a) gcc-core
    b) gcc-fortran
    c) gcc-g++


  **NOTE:** Keep the setup file in your Cygwin directory as you can use it later to download additional packages and keep your cygwin directory updated.

  Extra Resources:

    GitHub

      1) zlib-1.2.8 - https://github.com/madler/zlib
      2) HDF5-1.9.227 - https://github.com/live-clones/hdf5
      3) NetCDF - https://github.com/Unidata/netcdf-c

## 2. Getting the source

  The following versions of libraries were used to compile LaGrit, if the devel versions of first three libraries were installed then these downloads might not be necessary:

    a) zlib-1.2.8 - from http://www.zlib.net/
    b) HDF5-1.8.15-patch1 - from https://www.hdfgroup.org/downloads/index.html
    c) NetCDF-4.3.3.1.tar.gz - from http://www.unidata.ucar.edu/downloads/netcdf/index.jsp
    d) ExodusII-6.09 - from http://sourceforge.net/projects/exodusii/files/
    f) LaGrit-3.2
  
  **NOTE:** Cygwin emulates a UNIX system, so all of the downloads can be normal .tar collections. This makes it easy to install the dependencies since we don't have to deal with Windows. Just make sure to get 64 bit downloads.

  **NOTE:** This configuration also depends on what you use as your fortran, c++, and gcc compilers. You can add these variables to the ./configure commands without adding the word 'export', or you can set these as global variables in your .bashrc file(recommended).

      a) export FC="/bin/gfortran"
      b) export CC="/bin/gcc"
      c) export CXX="/bin/c++"
      d) export FC90="/bin/gfortran"
      e) export RANLIB="/bin/gcc-ranlib"
      f) export AR="/bin/gcc-ar"

*************************
### 2.1 Compiling zlib
**Do not get if you downloaded zlib-devel from Cygwin package source, it comes with a static version of zlib**
*************************

  1. Unpack zlib and navigate to the directory

  2. Compile for Cygwin:

    ./configure --static --prefix=$HOME --64

  3. Type in 'make' and then make sure there is no errors with 'make check'

  4. Use 'make install' to finish installing zlib

  5. Use 'make clean' to clean up the library directory of build files.

*************************
### 2.2 Compiling HDF5
**Use this to compile a static version of hdf5, otherwise libhdf5-devel is enough**
*************************
  1. Unpack the tar source file, in it you should find a folder named release_docs, which contains installation instructions in file called INSTALL_Cygwin.txt I will follow these, while giving the actual commands I used.

    **NOTE** When you run the configure script, it might complain about new line characters. If this happens stop the script with Ctrl+C, and run 'dos2unix -f targetfile' on the file where it throws an error. Use 'make clean' afterwards to clean up your build.

  2. In Hdf5 directory use the following command to configure the build script

        CFLAGS="-m64 -g" FCFLAGS="-m64 -g" CXXFLAGS="-m64 -g" ./configure --disable-shared --with-zlib=/cygdrive/c/Users/304285/dev/cygwin/home/304285/include,/cygdrive/c/Users/304285/dev/cygwin/home/304285/lib --prefix=$HOME --enable-fortran --enable-cxx --enable-static-exec

        CFLAGS="-m64 -g" FCFLAGS="-m64 -g" CXXFLAGS="-m64 -g" ./configure --disable-shared --with-zlib=/lib/libz.a --prefix=$HOME --enable-fortran --enable-cxx --enable-static-exec 

        --host=x86_64-w64-mingw32
        --build=x86_64-w64-mingw32

        --with-gnu-ld

  3. Once configuration is complete, type in `make` to build the library, and `make check` to test it.

  4. Run `make install` and `make check-install` to test it.

  5. Run `make clean` to clean up your library of build files.

*************************
### 2.3. Compiling NetCDF
**Use this to compile a static version of NetCDF, otherwise libnetcdf-devel is enough**
*************************
  **NOTE:** After version 4.1.3, NetCDF-fortran became a different distribution so the use of environment variables set for fortran is unnecessary.

  1. Unpack NetCDF and navigate to the directory, the installation instructions can be traced here: http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-install/

  2. Configure the build script

         CPPFLAGS="-I$HOME/include -m64 -g" LDFLAGS="-L$HOME/lib -m64 -g" LIBS="-L$HOME/lib -lhdf5_hl -lhdf5 -lz" ./configure --disable-shared --disable-dap --enable-netcdf-4 --prefix=$HOME CFLAGS="-m64 -g"

    **NOTE:** Possibly helpful options: `--build`, `--host`

  3. Build the library with `make`, and test it with `make check`

  4. Finish the install with `make install` and `make clean`

## 3. Compiling Exodus

  1. Now before we start installing ExodusII, we need to make some slight modifications to NetCDF in `$NETCDF_HOME/include/netcdf.h` by changing the following variables as stated in exodus README file:

        #define NC_MAX_DIMS  65536 
        #define NC_MAX_VARS 524288 
        #define NC_MAX_VAR_DIMS 8

  2. Unpack Exodus and navigate to the exodus directory

  3. Here we have two options for compiling ExodusII as described in the README, 1st is with Makefile.standalone, and 2nd is with cmake

    1) Using Makefile.standalone (INCOMPLETE: Might need to add additional arguments):

      a) Use the following make command:

        make -f Makefile.standalone NETCDF=$HOME ARFLAGS=-rcv CFLAGS="-m64" FCFLAGS="-m64" CXXFLAGS="-m64"

      b) Copy the Exodus libararies to our local libraries
        
        cp libexodus.a libexoIIv2for.a $HOME/lib

    2) Using cmake (Harder difficulty since requires manipulation of additional libs but completed):

      **NOTE:** To be able to use cmake on my Cygwin installation I had to copy over my cmake executable from `/bin/cmake` to `/home/304285/bin/cmake`, and my cmake root files from `/usr/share/cmake-3.1.2` to `/home/304285/share/cmake-3.1.2`. My PATH variable was set up to read my /home/304285/bin directory last and it overwrote the cmake I had in /bin/cmake. (Otherwise it throws `could not find CMAKE_ROOT/Module directory not found/Error executing cmake::LoadCache()`)

      a) Run 'sh cmake-script' once you make appropriate changes to you cmake-script file, this was mine:

        EXTRA_ARGS=$@

        # Fortran compiler
        FC="/bin/gfortran"

        # Root to where netcdf and hdf5 libraries and includes 
        # are installed. Libraries will be in LIB_ROOT/lib and 
        # includes will be in LIB_ROOT/include
        LIB_ROOT=/usr/local/lib

        # Root of where to install the exodus libraries and
        # include files.  Library will be in INSTALL_ROOT/lib
        # and include file in INSTALL_ROOT/include
        INSTALL_ROOT=$HOME

        # Complete path to where the exodus.py file should be installed
        PYTHON_INSTALL=${INSTALL_ROOT}/python

        # Add the following for a static build, mine kept throwing me memory truncation error
        # -DCMAKE_EXE_LINKER_FLAGS:STRING="-static" \

        rm -f CMakeCache.txt

        cmake  \
        -DBUILD_SHARED:BOOL=OFF \
        -DBUILD_SHARED_LIBS:BOOL=OFF \
        -DCMAKE_INSTALL_PREFIX:PATH=${INSTALL_ROOT} \
        -DCMAKE_Fortran_COMPILER:FILEPATH=${FC} \
        -DNETCDF_NCDUMP:PATH='which ncdump' \
        -DNETCDF_INCLUDE_DIR:PATH=/usr/include \
        -DNETCDF_LIBRARY:PATH=/lib/libnetcdf.dll.a \
        -DHDF5HL_LIBRARY:PATH=/lib/libhdf5_hl.dll.a \
        -DHDF5_LIBRARY:PATH=/lib/libhdf5.dll.a \
        -DZLIB_LIBRARY:PATH=/lib/libz.dll.a \
        -DCURL_LIBRARY:PATH=/lib/libcurl.dll.a \
        -DPYTHON_INSTALL:PATH=${PYTHON_INSTALL} \
        -DCMAKE_C_FLAGS:STRING="-g -m64 -Dwin64 -mwindows" \
        -DCMAKE_CXX_FLAGS:STRING="-g -m64" \
        -DCMAKE_Fortran_FLAGS:STRING="-g -m64 -fcray-pointer -fdefault-integer-8 -Dwin64 -w -mwindows -fno-exceptions" \
        -DCMAKE_RANLIB:FILEPATH=/bin/gcc-ranlib \
        -DCMAKE_AR:FILEPATH=/bin/gcc-ar \
        -DCYGWIN:BOOL=TRUE \
        -DCMAKE_LEGACY_CYGWIN_WIN32=1 \
        -DCMAKE_VERBOSE_MAKEFILE:BOOL=TRUE \
        $EXTRA_ARGS

      b) There is a bug when it makes tests with static libraries, the following 2 files need to be changed for 'make' to work:

        1) ../exodus-6.09/exodus/forbind/CMakeLists change line 56 and have ${HDF5HL_LIBRARY} come before ${HDF5_LIBRARY}
        2) ../exodus-6.09/exodus/cbind/CMakeLists change line 284 with the same library ordering

      c) Now run 'make'

      **NOTE:** When compiling with static linker flags, make throws a truncating error, to fix this I have attempted many options in the cmake-script that are listed here, however in the end I just ended up not using "-static" as a linker flag:

          # -DCMAKE_GENERATOR:STRING="Unix Makefiles" \
          # -DCMAKE_LEGACY_CYGWIN_WIN32=1 \
          # -DCMAKE_Fortran_FLAGS:STRING="-g -fcray-pointer -fno-exceptions -fdefault-integer-8 -m64" \
          # -DCMAKE_STATIC_LINKER_FLAGS:STRING="-ncmodel=medium" \
          # --large-address-aware --relax --high-entropy-va
          # -DCMAKE_EXE_LINKER_FLAGS:STRING="-static" \
          # -DCYGWIN:BOOL=TRUE \
          # -DCMAKE_LEGACY_CYGWIN_WIN32=1 \

      d) To test your build use `make check`

     **NOTE:** After testing the build it throws a message about truncating to 32 bit. What causes this is probably the fact that floats on Windows 64 bit are 4 bit, while on Cygwin they are 8 bit since Cygwin emulated a Linux system.

      e) `make install`

## 4. Compiling lg_util

  1. Unpack the `lg_util` source and navigate to the `src/` directory

  2. Remove the previous header file and recreate it for your machine:

        rm -f mm2000.h
        $CC -E -m64 -Dwin64 mm2000_header.f -o mm2000.h
        mkdir objects_cygwin64_g_cygwin
        cp -p mm2000.h objects_cygwin64_g_cygwin/

  3. Build and install the library

        make install MOPT=64 COPT=-g LIBDIR=$HOME/lib COMPILER=cygwin

    **NOTE:** In the Makefile, there is an `ifeq(COMPILER=cygwin)`, in it CFLAGS are linked to my include folder in cygwin, readjust this as necessary.

  4. Run ranlib on the lg_util libraries to fix archives (it appears to grab the wrong version from the path)

        $RANLIB $HOME/lib/util_*

    **NOTE:** Use the following clean command to when rebuilding:

        make clean COMPILER=cygwin COPT=-g MOPT=64 LIBDIR=$HOME/lib

## 5. Compiling LaGriT

1. Unpack the lagrit source and navigate to the source dir

2. Clean the directory up first by running these two commands, `rm *.o` and `rm *.mod` 

3. Build the lagrit library

        make lib MOPT=64 COPT=-g COMPILER=cygwin CFLAGS="-I$HOME/include" LIBDIR=$HOME/lib

3. Build the lagrit executable with these commands:

        cp -f lagrit_cygwin.h lagrit.h
        cp -f  machine_m64.h machine.h
        $FC -g -fcray-pointer -fdefault-integer-8 -m64 -Dwin64 -c -o lagrit_main.o lagrit_main.f 
        $FC -g -fcray-pointer -fdefault-integer-8 -m64 -Dwin64 -c -o lagrit_fdate.o lagrit_fdate.f 
        $FC -g -Dwin64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit objects_cygwin_g_cygwin/lagrit_main.o objects_cygwin_g_cygwin/lagrit_fdate.o lagrit_cygwin_g_cygwin.a /cygdrive/c/Users/304285/dev/cygwin/home/304285/lib/util_cygwin64_g_cygwin.a -L${HOME}/lib -lexoIIv2for -lexodus -L/lib -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

  **NOTE:** In the future add `-static option` before the `-o` in order to make a statically compiled executable

4. Copy the .exe file and DLLs to a Windows system and run LaGrit!

  **NOTE:** Use the following clean command to when rebuilding:

        make clean COMPILER=cygwin COPT=-g MOPT=64 LIBDIR=$HOME/lib
