## LaGriT: Los Alamos Grid Toolbox ##

**LANL Software: LA-CC-15-069  No. C15097**


[![Build Status](https://travis-ci.org/lanl/LaGriT.svg?branch=master)](https://travis-ci.org/lanl/LaGriT) [![Latest Version](https://img.shields.io/github/release/lanl/lagrit.svg?style=flat-square)](https://github.com/lanl/lagrit/releases) [![PyPI](https://img.shields.io/pypi/l/Django.svg)](https://github.com/lanl/LaGriT/blob/doc-test/LICENSE)

[LaGriT Home](https://lagrit.lanl.gov) • [LaGriT Documentation](http://lanl.github.io/LaGriT) • [Meshing Portfolio](https://meshing.lanl.gov/)

Los Alamos Grid Toolbox (**LaGriT**) is a library of user callable tools that provide mesh generation, mesh optimization and dynamic mesh maintenance in two and three dimensions. LaGriT is used for a variety of geology and geophysics modeling applications including porous flow and transport model construction, finite element modeling of stress/strain in crustal fault systems, seismology, discrete fracture networks, asteroids and hydrothermal systems.

The general capabilities of LaGriT can also be used outside of earth science applications and applied to nearly any system that requires a grid/mesh and initial and boundary conditions, setting of material properties and other model setup functions. It can also be use as a tool to pre- and post-process and analyze vertex and mesh based data.

**PyLaGriT** is a Python interface for LaGriT that allows LaGriT functionality to be accessed interactively and in batch mode from Python.
This allows the meshing capabilities of LaGriT to be combined with the numeric and scientific functionality of Python.
PyLaGriT allows interactive and automated querying of mesh properties, enhanced looping functionality, and user defined error checking based on LaGriT output.


### Building LaGriT (Windows) ###
---

Download the repo by running:

    git clone -b windows https://github.com/lanl/LaGriT.git
    cd LaGriT

## Requirements

* Microsoft Visual Studio
* Intel Fortran

## Steps to Build

1. Open Microsoft Visual Studio and launch the developer `cmd.exe` via `Visual Studio -> Tools -> Command Line -> Developer Command Prompt`

2. Run the Intel-provided `psxevars.bat` script to populate the command prompt with necessary developer environment variables. 

  * 2.1. First, `cd` into: `C:\Program Files (x86)\IntelSWTools\parallel_studio_xe_2020.4.912\bin\`
  * 2.2. Run: `psxevars.bat intel64`

3. Navigate back into the cloned LaGriT directory: `cd C:\Users\user\repos\LaGriT\`

4. Run CMake: `cmake -G"NMake Makefiles" -B"build" -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icx`

5. Then, `cd build\` and run `nmake`.

6. The executable binary will be compiled as `lagrit.exe`.

### Supporting Documentation ###
---
* [Installation](documentation/INSTALL.md)
* [GitHub pages](https://lanl.github.io/LaGriT/)
* [lagrit.lanl.gov: Description and Manual](http://lagrit.lanl.gov)
* [PyLaGriT documentation](https://lanl.github.io/LaGriT/pylagrit/original/index.html)
* [Contribution Agreement for External Collaborators](CONTRIBUTING.md)
* [Copyright License](LICENSE.md)

![Refine Samples](screenshots/refine_samples_TN1000.png)

##### LaGriT Mesh Images at https://meshing.lanl.gov/proj/screenshots/GRID_GALLERY.html