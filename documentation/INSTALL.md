# Building LaGriT
### 1. Installing Dependencies
LaGriT requires the following packages installed:

| Library | Source |
| ------ | ------ |
| ExodusII 6.09 | [https://github.com/gsjaardema/seacas] |
| netCDF | [http://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html] |
| zlib | [https://github.com/madler/zlib] |
| HDF5 | [https://support.hdfgroup.org/HDF5/] |

For bug reports, documentation errors, and enhancement suggestions for ExodusII, contact:
>Gregory D. Sjaardema
>PHONE: (505) 844-2701
>EMAIL: gdsjaar@sandia.gov


### 2. Cloning and Building LaGriT
Clone the repository and begin the build process.
```sh
$ git clone https://github.com/lanl/LaGriT.git
$ cd LaGriT/src/
$ module load exodusii/6.09/gcc-4.8.2-serial
$ module list
```

If Exodus is not built on your system, you will either need to build Exodus or remove the dependencies on it from the LaGriT source code (See Managing Exodus below).

Next, we will remove files from previous builds (?) before running make. This is to clean in the event of any previous building, and may return `No such file or directory` - this is acceptable.

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

#### 2.1 Fresh install?
If this is your first time installing LaGriT, you will need to navigate to `LaGriT/lg_util/src` and build `lg_util`. To do this, you may either follow the directions in `README` for advanced operations, or simply run

```sh
$ make clean
$ make MOPT=64 lib
```

After this, *or* if `lg_utils` has already been built before (i.e., if LaGriT has been previously installed and you're simply doing a pull), simply run the following command from `LaGriT/src`:

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


Visit the [offical LANL hompage of LaGriT](http://lagrit.lanl.gov), take a look at [what LaGriT](http://lagrit.lanl.gov/graphics.shtml) can do, or view [publications supported in part by LaGriT](http://lagrit.lanl.gov/publications.shtml).

