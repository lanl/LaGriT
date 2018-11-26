---
title: "About LaGriT and PyLaGriT"
---

# LaGriT
-------------------------

**LaGriT** is a library of user callable tools that provide mesh
generation, mesh optimization and dynamic mesh maintenance in three
dimensions for a variety of applications. Geometric regions within
arbitrarily complicated geometries are defined as combinations of
bounding surfaces, where the surfaces are described analytically or
as collections of points in space. A variety of techniques for
distributing points within these geometric regions are provided.
Mesh generation uses a Delaunay tetrahedralization algorithm that
respects material interfaces and assures that there are no internal
negative coupling coefficients. The data structures created to
implement this algorithm are compact and powerful and expandable to
include hybrid meshes as well as tetrahedral meshes. Mesh refinement
and smoothing are available to modify the mesh to provide more
resolution in areas of interest. Mesh refinement adds nodes to the
mesh based on geometric criteria such as edge length or based on
field variable criteria such change in field. Mesh smoothing moves
nodes to adapt the mesh to field variable measures, and, at the same
time, maintains quality element shape. Mesh elements may become
distorted as mesh nodes move during a time dependent simulation or
are added as a result of refinement operations. Mesh reconnection
via a series of edge flips will maintain the Delaunay criterion of
the mesh. An additional requirement of time dependent simulations is
that as interface surfaces move, the corresponding region
definitions must respond dynamically. As surfaces collide, the mesh
must respond by merging points and effectively squeezing out the
material between the colliding surfaces. LaGriT provides the
necessary tools for time dependent simulations.

# PyLaGriT
------------------------------

**PyLaGriT** is a Python interface that allows LaGriT functionality to be used interactively and in batch mode. It combines the meshing capabilities of LaGriT with the numeric and scientific functionality of Python including the quering of mesh properties, enhanced looping functionality, and user defined error checking. PyLaGriT has been developed to easily generate meshes by extrusion, dimensional reduction, coarsening and refinement of synthetic and realistic data. PyLaGriT enhances the workflow, enabling rapid iterations for use in simulations incorporating uncertainty in system geometry and automatic mesh generation.
