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

#### Building LaGriT - Simple

The simplest way to build LaGriT is:

```bash
mkdir build/ && cd build/
cmake .. && make
```

You will find `lagrit` executable in the `build/` directory.

Note, LaGriT will continue to build if ExodusII is not found. The Exodus and Netcdf libs are needed to write ExodusII libraries and are not otherwise needed.

```
-- ==========================================
-- ============Configuring LaGriT============
-- ===================v3.3.3=================
-- Compile LaGriT as a static binary = ON
-- Could NOT find Exodus (missing: Exodus_LIBRARIES Exodus_INCLUDE_DIR)
WARNING: ExodusII and/or other dependencies could not be found. Compiling without ExodusII support.

[100%] Built target lagrit.exe

$ ./lagrit

*               * * * * * * * * * * * * * * * * * * * * * * * *
*               *                                             *
*               *    Program:  LaGriT V3.3.3   Darwin         *
*               *    Date Compile: 2022/03/30                 *
*               *    Run Time: 2022/Mar 30  15:59:25          *
*               *    Manual:   https://lagrit.lanl.gov        *
*               *                                             *
*               * * * * * * * * * * * * * * * * * * * * * * * *

                               -----oOo-----
             LaGriT V3 LA-CC-15-069  https://github.com/lanl/LaGriT
  Copyright 2016. Triad National Security, LLC.  All rights reserved. This
  program was produced under U.S. Government contract 89233218CNA000001
  for Los Alamos National Laboratory (LANL), which is operated by Triad
  National Security, LLC for the U.S. Department of Energy/National Nuclear
  Security Administration.  All rights in the program are reserved by Triad
  National Security, LLC, and the U.S. Department of Energy/National Nuclear
  Security Administration. The Government is granted for itself and others
  acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license
  in this material to reproduce, prepare derivative works, distribute copies
  to the public, perform publicly and display publicly, and to permit others to
  do so. This software is open source and available under the BSD-3 License.
                               -----oOo-----

Output log file: lagrit.out
Command log file: lagrit.log

 Enter a command
finish
LaGriT successfully completed
```


#### (Optional) Building Exodus ####

Install to default directories:
(This is not working for some platforms, you can still build without Exodus)

```bash
$ ./install-exodus.sh
```

Exodus Installation instructions:
[Seacas ExodusII](https://github.com/gsjaardema/seacas)


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

### Testing LaGriT

To test LaGriT, simply run:

```bash
$ python test/runtests.py
```

Test output can be found in the `test/lagrit-tests.log` file.

Additional options are available by running:

```bash
$ python test/runtests.py --help
```

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
