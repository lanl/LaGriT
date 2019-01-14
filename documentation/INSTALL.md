# Building LaGriT
----------------------------------------------

LaGriT installation is intended to be as straightforward as possible for Linux and macOS systems. If you encounter any problems with the build process, submit a ticket on the (Issues page)[https://github.com/lanl/LaGriT/issues).

If you are on Windows, see 

## 1. Automatic Build (Linux, macOS)

### 1.1. Pre-requisites

* GCC / G++ / GFORTRAN 8.0.0+
* CMake 5.9+ / Make
* Git
* Bash

Linux users will also require the following:

* [m4](https://www.gnu.org/software/m4/)
* [bison](https://www.gnu.org/software/bison/)
* libz-dev

On Ubuntu, run

    sudo apt-get -y install gfortran libz-dev m4 bison

to install all non-stock pre-requisites.

On macOS, run

    brew update && brew install gcc
    
if `gfortran` is not present on your system.

### 1.2. Cloning LaGriT

Download the repo by running:

    git clone https://github.com/lanl/LaGriT.git
    cd LaGriT

### 1.3. Building Exodus

If you don't already have [Exodus](http://gsjaardema.github.io/seacas/exodusII-new.pdf) built on your system, run

    make exodus

or, on Ubuntu, you can build Exodus directly from a [PPA](https://launchpad.net/~nschloe/+archive/ubuntu/seacas-nightly/):

    sudo add-apt-repository ppa:nschloe/seacas-nightly
    sudo apt-get update
    sudo apt-get install seacas-bin

Note that Exodus is optional, though recommended for full functionality.

### 1.4. Compiling LaGriT

Finally, to build and test a shared, optimized LaGriT binary, run

    make release
    make test

To build LaGriT without Exodus, 

    make WITH_EXODUS=0 release

More options are available by running `make help`.

### 1.5. Testing LaGriT

Run the command

    make test
    
to validate the LaGriT compilation. Note that if you compiled without Exodus, one test should fail (`write_exo`).

## 2. Makefile Usage

### 2.1. Usage

    make [options] [target]

### 2.2. Targets

* `make release`
    * Optimized build (with shared libraries)

* `make static`
    * Optimized build (with static libraries)

* `make DEBUG=1 release`
    * Debug build (with shared libraries)

* `make DEBUG=1 static`
    * Debug build (with static libraries)

* `make exodus`
    * Download and build the Sandia ExodusII Fortran library.
    By default, it will download to the current working directory.
    This can be changed by running

      * `make EXO_BUILD_DIR=/exodus/path/out/ exodus`

* `make test`
    * Run the LaGriT test suite on the created binary.
    If you have changed or moved LaGriT from src/lagrit, use
    the option `EXE_NAME=/path/to/lagrit`

### 2.3. Options

* `CC` (default: gcc) : C source compiler
* `FC` (default: gfortran) : Fortran source compiler
* `FC90` (default: gfortran) : Fortran90 source compiler
* `WITH_EXODUS` (default: 1) : Build with or without Exodus
* `DEBUG` (default: 0) : Built in debug (1) or optimized (0) mode
* `EXO_LIB_DIR` (default: LAGRIT_ROOT_DIR/seacas/lib) : ExodusII library location
* `EXE_NAME` (default: src/lagrit) : binary filename for LaGriT

## 3. Manual Build

If you are experiencing issues with the standard compilation procedure, you may need to do a manual install.

### 3.1. Exodus Compilation

See Section 1.3 in this document to build Exodus automatically.

To manually build Exodus, read the [SEACAS Documentation](https://github.com/gsjaardema/seacas/blob/master/README.md) for the most up-to-date instructions.

For most architectures, this should be as simple as:

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

If the build was unsuccessful, a non-zero exit code will be thrown along with this error:

    -- Configuring incomplete, errors occurred!

### 3.2. Building the LaGriT `lg_util` library

If this is your first time installing LaGriT, you will need to navigate to `LaGriT/lg_util/src` and build `lg_util`. To do this, you may either follow the directions in `README` for advanced operations, or simply run

    $ make clean
    $ make lib

You may pass options to this Makefile, such as:

* `CC` (default: `gcc`)
* `FC` (default: `gfortran`)
* `BIT_SIZE` (default: output of `$(shell getconf LONG_BIT)`)
* `DEBUG` (default: `0`)

Since this library is stable and generally not updated with most releases, it is usually not necessary to recompile with each pull.
The `lib` target compiles every source file into an object file and combines them into a library using `ar rcu [targets]`.

### 3.3. Building the LaGriT `src/` library

Navigate to `src/` and run

    $ make clean
    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_main.o lagrit_main.f
    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_fdate.o lagrit_fdate.f
    $ make lib

The `lib` target compiles every source file into an object file and combines them into a library using `ar rcu [targets]`.
Note that `dumpexodusII.f` must be compiled with the `-cpp` flag, due to `#ifdef` statements. Further, `tempgable.f` must be compiled with the flag `-std=legacy`. These flags may be different on compilers other than GNU gfortran.

-----------------

To build without Exodus, in lieu of `make lib`, run

    $ make WITH_EXODUS=0 lib

Critically, this flag set to `0` does two things:

1. Compiles `dumpexodusII.f` with flags: `-cpp -DNOEXODUS`
2. Does **not** compile any source files matching the glob `exo_*.c`

### 3.4. Building LaGriT

Within the `src/` directory, link the generated libraries using the command:

    $ gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit lagrit_main.o lagrit_fdate.o lg_main_lib.a ../lg_util/src/lg_util_lib.a -L$ACCESS -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++ -ldl

Note that `ACCESS` is a shell variable pointing to `$(EXODUS_DIRECTORY)/lib`.

If Exodus has not been built, several flags become unneccessary:

    $ gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit lagrit_main.o lagrit_fdate.o lg_main_lib.a ../lg_util/src/lg_util_lib.a -lm -lstdc++

Note that some flags are architecture-dependant. For example, `-Dlinx64` is a Linux-unique command that translates to `-Dmacx64` on macOS systems.

### 3.5. Running LaGriT

From inside `LaGriT/src/`, run

    $ ./lagrit

to start the program.

A comprehensive test suite can be found in `LaGriT/test`. Run the command

    python suite.py -f -l 1 -exe=path/to/lagrit
    
to validate the build.

## 4. Windows Build

Currently, Windows support is limited and will require a manual install. See [here](build_win.md) for build instructions.
Windows support is under active development. Read this [issue](#null) for current development status.

## What's next? ##

For documentation, tutorials, and commands, visit the [LaGriT homepage](http://lagrit.lanl.gov).

**If you run into errors building LaGriT or have suggestions on how to improve this documentation, please email Terry Miller (tamiller@lanl.gov), Dylan Harp (dharp@lanl.gov), or Daniel Livingston (livingston@lanl.gov).**


