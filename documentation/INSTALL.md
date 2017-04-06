# Building LaGriT
### 1. Installing Dependencies
LaGriT requires the following packages installed:

| Library | Source |
| ------ | ------ |
| ExodusII 6.09 | (https://github.com/gsjaardema/seacas) |
| netCDF | (http://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html) |
| zlib | (https://github.com/madler/zlib) |
| HDF5 | (https://support.hdfgroup.org/HDF5/) |


For instructions on installing these dependencies, [visit here](DEPENDENCIES.md). LaGriT supports the ExodusII file format as output (which sits atop netCDF). If you have no need for this format, you may continue building LaGriT by skipping the ExodusII installation and following the directions in [Manging Exodus](#21-managing-exodus). 

---


### 2. Cloning and Building LaGriT
Clone the repository and begin the build process.
```sh
$ git clone https://github.com/lanl/LaGriT.git
$ cd LaGriT/src/
```

After cloning LaGriT to the directory you want it installed, navigate to the `src/` folder within that directory.

#### 2.1 Managing Exodus

LaGriT has the capability to export to the ExodusII file format. If you do not need this format, or would rather not install Exodus, run the following commands from within the `src/` directory:

```sh 
$ cp dumpexodusII.f.withnoexo dumpexodusII.f
$ echo "" > exo_init_ext.c
$ echo "" > exo_put_sets.c
```

These three files make calls to `exodusII.h`. The first line replaces `dumpexodusII.f` with a file that does not make this call, while the second two lines empty out ExodusII-centric C files.

**If instead you are using Exodus,** make sure it is built in such a way that `exodusII.h` can be read at buildtime. On a modularized Ubuntu configuration, this would require running

```sh
$ module load exodusii/6.09/gcc-4.8.2-serial
$ module list
```

where `exodusii/6.09/gcc-4.8.2-serial` may need to be changed to match your particular configuration.

#### 2.2 Building the LaGriT library

If this is your first time installing LaGriT, you will need to navigate to `LaGriT/lg_util/src` and build `lg_util`. To do this, you may either follow the directions in `README` for advanced operations, or simply run

```sh
$ make clean
$ make MOPT=64 lib
```

Since this library is stable and generally not updated with most releases, it  is usually not necessary to recompile with each pull. 

#### 2.3 Building LaGriT

Navigate back to the `LaGriT/src` directory by running `$ cd ../src`.

Similar to the above step, run the commands

```sh
$ rm *.o
$ rm *.mod
$ make MOPT=64 lib
```

Lorem ipsum solom dolor

```sh
$ cp lagrit_ulin64.h lagrit.h
$ cp  machine_m64.h machine.h
```

Next, link the Fortran libraries using
```sh
$ gfortran  -g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_main.o lagrit_main.f
$ gfortran  -g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_fdate.o lagrit_fdate.f
```


One more command and we are done:

```sh
$ gfortran -g -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit lagrit_main.o lagrit_fdate.o lagrit_ulin64_o_gcc.a ../lg_util/src/util_ulin64_o_gcc.a -L/n/swdev/packages/Ubuntu-14.04-x86_64/exodusii/6.09/gcc-4.8.2-serial/lib -lexodus -lexoIIv2for -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++
```

Before running this command, note a few things about it:
1. You will need to change `/n/swdev/packages/Ubuntu-14.04-x86_64/exodusii/6.09/gcc-4.8.2-serial/lib` to match your Exodus install.
2. Based on compilation, `../lg_util/src/util_ulin64_o_gcc.a` may have a different name...if an error is returned, navigate to this directory and change the command to match the `util_ulin64_*.a` file within.

#### Congradulations - LaGriT is built! ####
From inside `LaGriT/src/`, run

```sh
$ ./mylagrit
```

and welcome to the wonderful world of mesh generation!

---

## What's next? ##


Visit the [offical LANL hompage of LaGriT](http://lagrit.lanl.gov), view the [LaGriT commands](http://lagrit.lanl.gov/commands.shtml), take a look at [what LaGriT](http://lagrit.lanl.gov/graphics.shtml) can do, or view [publications supported in part by LaGriT](http://lagrit.lanl.gov/publications.shtml).

**If you run into errors building LaGriT or have suggestions on how to improve this documentation, please email Terry Miller (tamiller@lanl.gov), Dylan Harp (dharp@lanl.gov), or Daniel Livingston (livingston@lanl.gov).**


