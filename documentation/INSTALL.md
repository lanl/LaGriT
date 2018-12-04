# Building LaGriT
----------------------------------------------

If you are trying to build LaGriT on a Windows PC, follow the directions [here](build_win.md). If you are on Linux or macOS, you can build LaGriT manually by following the directions in this document or using the automatic installer script.

## Automatic Build (Linux, macOS)
### USAGE:

    make [options] [target]

### TARGETS:

* `make release`
    * Optimized build (with shared libraries)

* `make static`
    * Optimized build (with static libraries)

* `make DEBUG=1 release`
    * Debug build (with shared libraries)

* `make DEBUG=1 static`
    * Debug build (with static libraries)

* `make exodus`
    * Download and build the SANDIA ExodusII Fortran library.
    By default, it will download to the current working directory.
    This can be changed by running

      * `make EXO_BUILD_DIR=/exodus/path/out/ exodus`

* `make test`
    * Run the LaGriT test suite on the created binary.
    If you have changed or moved LaGriT from src/lagrit, use
    the option `EXE_NAME=/path/to/lagrit`

### OPTIONS:

* `CC` (default: gcc) : C source compiler
* `FC` (default: gfortran) : Fortran source compiler
* `FC90` (default: gfortran) : Fortran90 source compiler
* `WITHEXODUS` (default: 1) : Build with or without Exodus
* `DEBUG` (default: 0) : Built in debug (1) or optimized (0) mode
* `EXO_LIB_DIR` (default: LAGRIT_ROOT_DIR/seacas/lib) : ExodusII library location
* `EXE_NAME` (default: src/lagrit) : binary filename for LaGriT

## Manual Build (Linux, macOS)
### 1. Installing Dependencies
For Exodus implementation, LaGriT requires the following packages installed:

| Library       | Source                                                  |
| ------------- | ------------------------------------------------------- |
| ExodusII 7.0+ | https://github.com/gsjaardema/seacas                    |
| netCDF        | https://www.unidata.ucar.edu/downloads/netcdf/index.jsp |
| zlib          | https://github.com/madler/zlib                          |
| HDF5          | https://support.hdfgroup.org/HDF5/                      |

If you don't already have [Exodus](http://gsjaardema.github.io/seacas/exodusII-new.pdf) built on your system, run

    make exodus

in the LaGriT root directory.
Or, on Ubuntu, you can build Exodus directly from a [PPA](https://launchpad.net/~nschloe/+archive/ubuntu/seacas-nightly/):

    sudo add-apt-repository ppa:nschloe/seacas-nightly
    sudo apt-get update
    sudo apt-get install seacas-bin

To manually build Exodus, read the [SEACAS Documentation](https://github.com/gsjaardema/seacas/blob/master/README.md).

For most architectures, should be as simple as:

    $ export CGNS=NO
    $ export MATIO=NO
    $ export SHARED=NO
    $ export NEEDS_ZLIB=YES
    $ git clone https://github.com/gsjaardema/seacas.git $(EXO_BUILD_DIR)/seacas
    $ cd $(EXO_BUILD_DIR)/seacas
    $ ./install-tpl.sh
    $ cd TPL
    $ ../cmake-config -DFORTRAN=YES
    $ make && make install

---


### 2. Cloning and Building LaGriT
Clone the repository and begin the build process.

    $ git clone https://github.com/lanl/LaGriT.git
    $ cd LaGriT/src/

After cloning LaGriT to the directory you want it installed, navigate to the `src/` folder within that directory.

#### 2.1 Building the LaGriT `lg_util` library

If this is your first time installing LaGriT, you will need to navigate to `LaGriT/lg_util/src` and build `lg_util`. To do this, you may either follow the directions in `README` for advanced operations, or simply run


    $ make clean
    $ make lib

You may pass options to this Makefile, such as:

* `CC` (default: `gcc`)
* `FC` (default: `gfortran`)
* `BIT_SIZE` (default: output of `$(shell getconf LONG_BIT)`)
* `DEBUG` (default: `0`)

Since this library is stable and generally not updated with most releases, it is usually not necessary to recompile with each pull. 
The `lib` target does nothing more than compile every source file into an object file and combine them into a library using `ar rcu [targets]`.

#### 2.2 Building the LaGriT `src/` library

Navigate to `src/` and run

    $ make clean
    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_main.o lagrit_main.f
    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_fdate.o lagrit_fdate.f
    $ make lib

The `lib` target does nothing more than compile every source file into an object file and combine them into a library using `ar rcu [targets]`.
Note that `dumpexodusII.f` must be compiled with the `-cpp` flag, due to `#ifdef` statements. Further, `tempgable.f` must be compiled with the flag `-std=legacy`. These flags may be different on compilers other than GNU gfortran.

-----------------

To build without Exodus, in lei of `make lib`, run

    $ make WITHEXODUS=0 lib

Critically, this flag set to `0` does two things:

1. Compiles `dumpexodusII.f` with flags: `-cpp -DNOEXODUS`
2. Does **not** compile any source files matching the glob `exo_*.c`

#### 2.3 Building LaGriT

Within the `src/` directory, link the generated libraries using the command:

    $ gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit lagrit_main.o lagrit_fdate.o lg_main_lib.a ../lg_util/src/lg_util_lib.a -L$ACCESS -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

Note that `ACCESS` is a shell variable pointing to `$(EXODUS_DIRECTORY)/lib`.

------------

To build without Exodus, use

    $ gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit lagrit_main.o lagrit_fdate.o lg_main_lib.a ../lg_util/src/lg_util_lib.a -lz -lm -lstdc++

#### Running LaGriT

From inside `LaGriT/src/`, run

    $ ./lagrit

to start the program.

---

## What's next? ##


Visit the [offical LANL hompage of LaGriT](http://lagrit.lanl.gov), view the [LaGriT commands](http://lagrit.lanl.gov/commands.shtml), take a look at [what LaGriT](http://lagrit.lanl.gov/graphics.shtml) can do, or view [publications supported in part by LaGriT](http://lagrit.lanl.gov/publications.shtml).

**If you run into errors building LaGriT or have suggestions on how to improve this documentation, please email Terry Miller (tamiller@lanl.gov), Dylan Harp (dharp@lanl.gov), or Daniel Livingston (livingston@lanl.gov).**


