## LaGriT: Los Alamos Grid Toolbox ##

**LANL Software: LA-CC-15-069  No. C15097**


[![Build Status](https://github.com/lanl/LaGriT/actions/workflows/test-lagrit.yml/badge.svg)](https://github.com/lanl/LaGriT/actions/workflows/test-lagrit.yml) [![Latest Version](https://img.shields.io/github/release/lanl/lagrit.svg?style=flat-square)](https://github.com/lanl/lagrit/releases) [![PyPI](https://img.shields.io/pypi/l/Django.svg)](https://github.com/lanl/LaGriT/blob/doc-test/LICENSE)

[LaGriT Home](https://lagrit.lanl.gov) • [LaGriT Documentation](http://lanl.github.io/LaGriT) • [Meshing Portfolio](https://meshing.lanl.gov/)

Los Alamos Grid Toolbox (**LaGriT**) is a library of user callable tools that provide mesh generation, mesh optimization and dynamic mesh maintenance in two and three dimensions. LaGriT is used for a variety of geology and geophysics modeling applications including porous flow and transport model construction, finite element modeling of stress/strain in crustal fault systems, seismology, discrete fracture networks, asteroids and hydrothermal systems.

The general capabilities of LaGriT can also be used outside of earth science applications and applied to nearly any system that requires a grid/mesh and initial and boundary conditions, setting of material properties and other model setup functions. It can also be use as a tool to pre- and post-process and analyze vertex and mesh based data.

**PyLaGriT** is a Python interface for LaGriT that allows LaGriT functionality to be accessed interactively and in batch mode from Python.
This allows the meshing capabilities of LaGriT to be combined with the numeric and scientific functionality of Python.
PyLaGriT allows interactive and automated querying of mesh properties, enhanced looping functionality, and user defined error checking based on LaGriT output.


### Getting Started

Download the repo by running:

```bash
$ git clone https://github.com/lanl/LaGriT.git
$ cd LaGriT/
```

### Building LaGriT ###
---

#### Building LaGriT - for developers

[Code Development with cmake](cmake/README.md)


#### Building LaGriT - Simple

The simplest way to build LaGriT uses cmake with auto dectection and no options.
Type the following (you can name the build directory anything you want).

```bash
mkdir build/ && cd build/
cmake .. && make
```

If you do not have ExodusII libs installed, the build will look similar to:
```
-- ==========================================
-- ============Configuring LaGriT============
-- ===================v3.3.3=================
-- Compile LaGriT as a static binary = ON
-- Could NOT find Exodus (missing: Exodus_LIBRARIES Exodus_INCLUDE_DIR)
WARNING: ExodusII and/or other dependencies could not be found. Compiling without ExodusII support.

[100%] Built target lagrit
```

LaGriT will auto-detect ExodusII libs and will continue to build even if ExodusII is not found. 
The only command used with ExodusII libs is `dump/exodus`. If needed, 
You will need to build ExodusII before LaGriT cmake and build.
See below for build options.

The `lagrit` executable is installed in the `build/` directory.
Type ./lagrit to make sure the executable is working.

On the LaGriT command line prompt, type `test` which will execute a set of commands.
Type `finish` to exit.

The result will look like:
```
nnodes:              27                                                         
nelements:            8                                                         
xic(1:3):  1.00 1.25 1.50                                                       
imt(1:3):     1    1    1                                                       
epsilonl:   1.9229627E-13                                                       
     Released Mesh Object: test_hex                                             
lagrit test done.                                                               
 
 Enter a command
finish                                                                          
LaGriT successfully completed             
```

### Testing LaGriT

To test LaGriT, start from top and simply run:

```bash
$ python test/runtests.py
```

Test output can be found in the `test/lagrit-tests.log` file.

Additional options are available by running:

```bash
$ python test/runtests.py --help
```


#### (Optional) Building Exodus ####

This install script included here with LaGriT may not work for all platforms.
See Exodus Installation instructions:
[Seacas ExodusII](https://github.com/gsjaardema/seacas)

```bash
$ ./install-exodus.sh
```




#### Building LaGriT - Configurable

You may also specify more advanced build directions:

```bash
$ mkdir build/ && cd build/
$ cmake .. \
    -D LaGriT_BUILD_STATIC=ON \
    -D CMAKE_BUILD_TYPE=Debug \
    -D Exodus_ROOT=${EXO_INSTALL_DIR} \
    -D CMAKE_INSTALL_PREFIX=`pwd`/../install/
$ make && make install
```

##### CMake Build Options

- `-D LaGriT_BUILD_STATIC`
  - Builds LaGriT as a static binary (default; `ON`) or as a shared library (`.so`, `.dylib`, `.dll`)
- `-D CMAKE_BUILD_TYPE`
  - Sets the build type. Choose between `Debug` and `Release`.
- `-D Exodus_ROOT`
  - Sets the root directory of Exodus. **Must be specified** if you wish to use Exodus.
- `-D CMAKE_INSTALL_PREFIX`
  - Sets where to install LaGriT when running `make install`. Defaults to `/usr/local/`.

### Supporting Documentation ###
---
* [LaGriT Documentation](https://lanl.github.io/LaGriT/)
* [PyLaGriT documentation](https://lanl.github.io/LaGriT/pylagrit/original/index.html)
* [lagrit.lanl.gov: Web Page](http://lagrit.lanl.gov)
* [Contribution Agreement for External Collaborators](CONTRIBUTING.md)
* [Copyright License](LICENSE.md)
* [Code Development with cmake](cmake/README.md)
* [Installation for old LaGriT V2](documentation/INSTALL.md)

![Refine Samples](screenshots/refine_samples_TN1000.png)

##### LaGriT Mesh Images at https://meshing.lanl.gov/proj/screenshots/GRID_GALLERY.html
