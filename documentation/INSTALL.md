# Building LaGriT
----------------------------------------------

If you are trying to build LaGriT on a Windows PC, follow the directions [here](build_win.md). If you are on Linux or macOS, you can build LaGriT manually by following the directions in this document or using the automatic installer script.

## Automatic Build (Linux, macOS)
To use the install script, download the repo by running:

    $ git clone https://github.com/lanl/LaGriT.git

Then `cd LaGriT/` and:

    $ ./install.sh --release

| Flag | Option |
| ------ | ------ |
| `-h`, `--help` | Displays usage and build options |
| `-r`, `--release` | Builds a standard release executable with shared libraries |
| `-s`, `--static` | Builds a static executable |
| `-d`, `--debug` | Builds a debug executable with shared libraries |
| `-se`, `--skipexodus` | Removes dependencies on Exodus and bypasses Exodus install |
| `-e=PATH`, `--exodus=PATH` | Pass `/path/to/exodus/lib` for existing Exodus build* |

**Script automatically parses `LD_LIBRARY_PATH` for Exodus libraries - pass PATH if `echo $LD_LIBRARY_PATH` doesn't find Exodus automatically**

## Manual Build (Linux, macOS)
### 1. Installing Dependencies
LaGriT requires the following packages installed:

| Library | Source |
| ------ | ------ |
| ExodusII 6.39 | https://github.com/gsjaardema/seacas |
| netCDF | https://www.unidata.ucar.edu/downloads/netcdf/index.jsp |
| zlib | https://github.com/madler/zlib |
| HDF5 | https://support.hdfgroup.org/HDF5/ |


For instructions on installing these dependencies, [visit here](DEPENDENCIES.md). LaGriT supports the ExodusII file format as output (which sits atop netCDF). If you have no need for this format, you may continue building LaGriT by skipping the ExodusII installation and following the directions in [Manging Exodus](#21-managing-exodus). 

---


### 2. Cloning and Building LaGriT
Clone the repository and begin the build process.

    $ git clone https://github.com/lanl/LaGriT.git
    $ cd LaGriT/src/

After cloning LaGriT to the directory you want it installed, navigate to the `src/` folder within that directory.

#### 2.1 Managing Exodus

LaGriT has the capability to export to the ExodusII file format. If you do not need this format, or would rather not install Exodus, run the following commands from within the `src/` directory:

    $ cp dumpexodusII.f.withnoexo dumpexodusII.f
    $ echo "" > exo_init_ext.c
    $ echo "" > exo_put_sets.c

These three files make calls to `exodusII.h`. The first line replaces `dumpexodusII.f` with a file that does not make this call, while the second two lines empty out ExodusII-centric C files.

**If instead you are using Exodus,** export the path to the Exodus library to a variable that will be read by the linker:

    $ export $ACCESS=/path/to/exodus/lib/
   

#### 2.2 Building the LaGriT library

If this is your first time installing LaGriT, you will need to navigate to `LaGriT/lg_util/src` and build `lg_util`. To do this, you may either follow the directions in `README` for advanced operations, or simply run


    $ make clean
    $ make MOPT=64 lib
    $ export $LAGRIT_UTIL_SRC_DIR=`pwd`


Since this library is stable and generally not updated with most releases, it  is usually not necessary to recompile with each pull. 

#### 2.3 Building LaGriT

Navigate back to the `LaGriT/src` directory by running `$ cd ../src`.

Clean the directory by running

    $ rm *.o
    $ rm *.mod

Next, link the Fortran libraries and make:

    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_main.o lagrit_main.f
    $ gfortran  -O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_fdate.o lagrit_fdate.f
    $ make MOPT=64 lib

One more command and we are done:

    $ gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit lagrit_main.o lagrit_fdate.o lagrit_ulin64_o_gcc.a $LAGRIT_UTIL_SRC_DIR/util_ulin64_o_gcc.a -L$ACCESS -lexoIIv2for -lexodus -lnetcdf -lm -lstdc++

#### Congratulations - LaGriT is built! ####
From inside `LaGriT/src/`, run

    $ ./lagrit

and welcome to the wonderful world of mesh generation!


---

## What's next? ##


Visit the [offical LANL hompage of LaGriT](http://lagrit.lanl.gov), view the [LaGriT commands](http://lagrit.lanl.gov/commands.shtml), take a look at [what LaGriT](http://lagrit.lanl.gov/graphics.shtml) can do, or view [publications supported in part by LaGriT](http://lagrit.lanl.gov/publications.shtml).

**If you run into errors building LaGriT or have suggestions on how to improve this documentation, please email Terry Miller (tamiller@lanl.gov), Dylan Harp (dharp@lanl.gov), or Daniel Livingston (livingston@lanl.gov).**


