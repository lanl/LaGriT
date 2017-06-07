<p align="center">
  <a href="http://lagrit.lanl.gov">
    <img src="documentation/lanl.png" width=346 height=169>
  </a>

  <h3 align="center">LaGriT: Los Alamos Grid Toolbox</h3>

  <p align="center">
    Powerful Delaunay mesh generation and optimization toolkit.
    <br>
    <a href="http://lagrit.lanl.gov"><strong>Visit LaGriT &raquo;</strong></a>
  </p>
</p>

<br>

---

[![Build Status](https://travis-ci.org/lanl/LaGriT.svg?branch=master)](https://travis-ci.org/lanl/LaGriT) [![Latest Version](https://img.shields.io/github/release/lanl/lagrit.svg?style=flat-square)](https://github.com/lanl/lagrit/releases) [![PyPI](https://img.shields.io/pypi/l/Django.svg)](https://github.com/lanl/LaGriT/blob/doc-test/LICENSE)

Los Alamos Grid Toolbox (**LaGriT**) is a library of user callable tools that provide mesh generation, mesh optimization and dynamic mesh maintenance in two and three dimensions. LaGriT is used for a variety of geology and geophysics modeling applications including porous flow and transport model construction, finite element modeling of stress/strain in crustal fault systems, seismology, discrete fracture networks, asteroids and hydrothermal systems.

The general capabilities of LaGriT can also be used outside of earth science applications and applied to nearly any system that requires a grid/mesh and initial and boundary conditions, setting of material properties and other model setup functions. It can also be use as a tool to pre- and post-process and analyze vertex and mesh based data.

**PyLaGriT** is a Python interface for LaGriT that allows LaGriT functionality to be accessed interactively and in batch mode from Python.
This allows the meshing capabilities of LaGriT to be combined with the numeric and scientific functionality of Python.
PyLaGriT allows interactive and automated querying of mesh properties, enhanced looping functionality, and user defined error checking based on LaGriT output.

*LaGriT V3 (OSS) LA-CC-15-069*

### Building LaGriT ###
---

Download the repo by running:

    git clone https://github.com/lanl/LaGriT.git

Then `cd LaGriT/` and:

    ./install.sh --release
    
Or `--help` to see other build configurations. The build script will check if Exodus is installed; if not, download and building of Exodus from the [Seacas repo](https://github.com/gsjaardema/seacas) will automatically begin. If you do not need Exodus, pass the argument `--skipexodus`.

### Supporting Documentation ###
---
* [Installation](documentation/INSTALL.md)
* [LaGriT Description and Manual](http://lagrit.lanl.gov)
* [LaGriT commands](http://lagrit.lanl.gov/commands.shtml)
* [PyLaGriT documentation](https://lanl.github.io/LaGriT/)
* [Contributions for External Collaborators](CONTRIBUTING.md)
* [License](LICENSE.md)

![Refine Samples](screenshots/refine_samples_TN1000.png)

##### LaGriT Mesh Images at https://meshing.lanl.gov/proj/screenshots/GRID_GALLERY.html
