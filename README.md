## LaGriT: Los Alamos Grid Toolbox ##

**LANL Software: LA-CC-15-069  No. C15097**

[![Build Status](https://travis-ci.org/daniellivingston/LaGriT.svg?branch=master)](https://travis-ci.org/daniellivingston/LaGriT) [![Latest Version]

<<<<<<< HEAD
---------------


[![Build Status](https://travis-ci.org/lanl/LaGriT.svg?branch=master)](https://travis-ci.org/lanl/LaGriT) [![Latest Version]

(https://img.shields.io/github/release/lanl/lagrit.svg?style=flat-square)](https://github.com/lanl/lagrit/releases) [![PyPI](https://img.shields.io/pypi/l/Django.svg)](https://github.com/lanl/LaGriT/blob/doc-test/LICENSE)  • [LaGriT Home at lagrit.lanl.gov](https://lagrit.lanl.gov) • [LaGriT Pages](http://lanl.github.io/LaGriT) • [Meshing](https://meshing.lanl.gov/)
=======
[![Build Status](https://travis-ci.org/lanl/LaGriT.svg?branch=master)](https://travis-ci.org/lanl/LaGriT) [![Latest Version](https://img.shields.io/github/release/lanl/lagrit.svg?style=flat-square)](https://github.com/lanl/lagrit/releases) [![PyPI](https://img.shields.io/pypi/l/Django.svg)](https://github.com/lanl/LaGriT/blob/doc-test/LICENSE)

[LaGriT Home](https://lagrit.lanl.gov) • [LaGriT Documentation](http://lanl.github.io/LaGriT) • [Meshing Portfolio](https://meshing.lanl.gov/)
>>>>>>> a81a510cf87643a2f2cfda046947c0bf73dfc1cd

Los Alamos Grid Toolbox (**LaGriT**) is a library of user callable tools that provide mesh generation, mesh optimization and dynamic mesh maintenance in two and three dimensions. LaGriT is used for a variety of geology and geophysics modeling applications including porous flow and transport model construction, finite element modeling of stress/strain in crustal fault systems, seismology, discrete fracture networks, asteroids and hydrothermal systems.

The general capabilities of LaGriT can also be used outside of earth science applications and applied to nearly any system that requires a grid/mesh and initial and boundary conditions, setting of material properties and other model setup functions. It can also be use as a tool to pre- and post-process and analyze vertex and mesh based data.

**PyLaGriT** is a Python interface for LaGriT that allows LaGriT functionality to be accessed interactively and in batch mode from Python.
This allows the meshing capabilities of LaGriT to be combined with the numeric and scientific functionality of Python.
PyLaGriT allows interactive and automated querying of mesh properties, enhanced looping functionality, and user defined error checking based on LaGriT output.


### Building LaGriT ###
---

Download the repo by running:

    git clone https://github.com/lanl/LaGriT.git
    cd LaGriT

If you don't already have [Exodus](http://gsjaardema.github.io/seacas/exodusII-new.pdf) built on your system, run

    make exodus

To build and test a shared, optimized LaGriT binary, run

    make release

To build LaGriT without Exodus,

    make WITH_EXODUS=0 release

or use target `static` to build a static binary.

Finally, run

    make test

to test build integrity.

More options are available by running `make help`.

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
