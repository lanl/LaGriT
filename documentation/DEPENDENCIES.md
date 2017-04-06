# Installing LaGriT Dependencies

LaGriT requires a number of external dependencies to fully compile. 

* [HDF5](https://github.com/live-clones/hdf5) -- required
* [NetCDF](https://github.com/Unidata/netcdf-c) -- required with modifications
* [zlib](https://github.com/madler/zlib) -- required
* [ExodusII](https://github.com/gsjaardema/seacas) -- optional but recommended

ExodusII is used for file exportation. If you have no need of this format, you may skip the ExodusII installation after building netCDF and HDF5 and following the *Managing Exodus* instructions in [INSTALL.md](INSTALL.md). 

LaGriT also requires the following compilers:

* gcc-core
* gcc-fortran
* gcc-g++

If you are on Windows, it will be necessary to install [Cygwin](https://cygwin.com/install.html).

**NOTE:** This configuration also depends on what you use as your fortran, c++, and gcc compilers. You can add these variables to the `./configure` commands without adding the word 'export', or you can set these as global variables in your `.bashrc` file (recommended).

      export FC="/bin/gfortran"
      export CC="/bin/gcc"
      export CXX="/bin/c++"
      export FC90="/bin/gfortran"
      export RANLIB="/bin/gcc-ranlib"
      export AR="/bin/gcc-ar"


## 1. Compiling zlib

1.1. Unpack zlib and navigate to the directory

1.2. Compile for Cygwin:

```sh
./configure --static --prefix=$HOME --64
```

1.3. Type in `make` and then make sure there is no errors with `make check`

1.4. Use `make install` to finish installing zlib

1.5. Use `make clean` to clean up the library directory of build files.


## 2. Compiling HDF5

2.1. Unpack the tar source file, in it you should find a folder named release_docs, which contains installation instructions in file called `INSTALL_Cygwin.txt`. I will follow these, while giving the actual commands I used.

>***CYGWIN NOTE:** On Windows, when you run the configure script, it might complain about new line characters. If this happens stop the script with Ctrl+C, and run `dos2unix -f targetfile` on the file where it throws an error. Use `make clean` afterwards to clean up your build.*

2.2. In Hdf5 directory use the following command to configure the build script:

```sh
CFLAGS="-m64 -g" FCFLAGS="-m64 -g" CXXFLAGS="-m64 -g" ./configure --disable-shared --with-zlib=/cygdrive/c/Users/304285/dev/cygwin/home/304285/include,/cygdrive/c/Users/304285/dev/cygwin/home/304285/lib --prefix=$HOME --enable-fortran --enable-cxx --enable-static-exec

CFLAGS="-m64 -g" FCFLAGS="-m64 -g" CXXFLAGS="-m64 -g" ./configure --disable-shared --with-zlib=/lib/libz.a --prefix=$HOME --enable-fortran --enable-cxx --enable-static-exec 

--host=x86_64-w64-mingw32
--build=x86_64-w64-mingw32
--with-gnu-ld
```

2.3. Once configuration is complete, type in `make` to build the library, and `make check` to test it.

2.4. Run `make install` and `make check-install` to test it.

2.5. Run `make clean` to clean up your library of build files.


## 3. Compiling netCDF

**NOTE:** After version 4.1.3, NetCDF-fortran became a different distribution so the use of environment variables set for fortran is unnecessary.

3.1. Unpack NetCDF and navigate to the directory, the installation instructions can be traced here: http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-install/

3.2. Configure the build script

```sh
CPPFLAGS="-I$HOME/include -m64 -g" LDFLAGS="-L$HOME/lib -m64 -g" LIBS="-L$HOME/lib -lhdf5_hl -lhdf5 -lz" ./configure --disable-shared --disable-dap --enable-netcdf-4 --prefix=$HOME CFLAGS="-m64 -g"
```

**NOTE:** Possibly helpful options: `--build`, `--host`

3.3. Build the library with `make`, and test it with `make check`

3.4. Finish the install with `make install` and `make clean`


## 4. Compiling Exodus

*For Exodus II bug reports, documentation errors, and enhancement suggestions, contact:*

>**Gregory D. Sjaardema**
>
> (505) 844-2701
> 
> gdsjaar@sandia.gov


4.1. Now before we start installing ExodusII, we need to make some slight modifications to NetCDF in `$NETCDF_HOME/include/netcdf.h` by changing the following variables as stated in exodus `README` file:

    #define NC_MAX_DIMS  65536 
    #define NC_MAX_VARS 524288 
    #define NC_MAX_VAR_DIMS 8

4.2. Unpack Exodus and navigate to the exodus directory

4.3. Here we have two options for compiling ExodusII as described in the `README`, 1st is with `Makefile.standalone`, and 2nd is with `cmake`

**4.3.1. Using Makefile.standalone (INCOMPLETE: Might need to add additional arguments):**

a) Use the following make command:

	make -f Makefile.standalone NETCDF=$HOME ARFLAGS=-rcv CFLAGS="-m64" FCFLAGS="-m64" CXXFLAGS="-m64"

b) Copy the Exodus libararies to our local libraries
        
	cp libexodus.a libexoIIv2for.a $HOME/lib

**4.3.2. Using cmake (Harder difficulty since requires manipulation of additional libs but completed):**

>***CYGWIN NOTE:** To be able to use cmake on my Cygwin installation I had to copy over my cmake executable from `/bin/cmake` to `/home/304285/bin/cmake`, and my cmake root files from `/usr/share/cmake-3.1.2` to `/home/304285/share/cmake-3.1.2`. My PATH variable was set up to read my `/home/304285/bin` directory last and it overwrote the cmake I had in `/bin/cmake`.
(Otherwise it throws `could not find CMAKE_ROOT/Module directory not found/Error executing cmake::LoadCache()`)*

a) Run `sh cmake-script` once you make appropriate changes to your `cmake-script` file, this was mine:

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

>i) `../exodus-6.09/exodus/forbind/CMakeLists`: change line 56 and have `${HDF5HL_LIBRARY}` come before `${HDF5_LIBRARY}`
>
>ii) `../exodus-6.09/exodus/cbind/CMakeLists`: change line 284 with the same library ordering


c) Now run `make`

**NOTE:** When compiling with static linker flags, make throws a truncating error, to fix this I have attempted many options in the cmake-script that are listed here, however in the end I just ended up not using `-static` as a linker flag:

          # -DCMAKE_GENERATOR:STRING="Unix Makefiles" \
          # -DCMAKE_LEGACY_CYGWIN_WIN32=1 \
          # -DCMAKE_Fortran_FLAGS:STRING="-g -fcray-pointer -fno-exceptions -fdefault-integer-8 -m64" \
          # -DCMAKE_STATIC_LINKER_FLAGS:STRING="-ncmodel=medium" \
          # --large-address-aware --relax --high-entropy-va
          # -DCMAKE_EXE_LINKER_FLAGS:STRING="-static" \
          # -DCYGWIN:BOOL=TRUE \
          # -DCMAKE_LEGACY_CYGWIN_WIN32=1 \

d) To test your build use `make check`

>***CYGWIN NOTE:** After testing the build it throws a message about truncating to 32 bit. What causes this is probably the fact that floats on Windows 64 bit are 4 bit, while on Cygwin they are 8 bit since Cygwin emulated a Linux system.*

e) Run `make install`

---


**If you run into errors building LaGriT or have suggestions on how to improve this documentation, please email Terry Miller (tamiller@lanl.gov), Dylan Harp (dharp@lanl.gov), or Daniel Livingston (livingston@lanl.gov).**

