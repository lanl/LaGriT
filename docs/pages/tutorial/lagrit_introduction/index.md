---
title: Tutorial LaGriT Introduction
---

# Tutorial - LaGriT Introduction

<div class="button" id="button-3">
   <div id="circle"></div>
   <a href="{{ "/pages/tutorial/lagrit_introduction/" | relative_url }}">Tutorial on GitHub</a>
</div>


## Running LaGriT

LaGriT consists of mesh **commands** and options that are used to create, modify, optimize, and write files. LaGriT can be run on the command line interactively, so commands are entered one at a time at the prompt, or with one or more files containing the LaGriT commands. By convention we use ".lgi" for lagrit input file names and ".mlgi" for macro files called from main command file.

Run LaGriT from the command line interactively or put commands in a file and redirect.
```
lagrit
```
or
```
lagrit < input.lgi
```
**Important Note:** the **`finish`** command must be used to exit interactive session and must be at end of command file.

## LaGriT Syntax Conventions

LaGriT reads commands and command options from each line at prompt or each line in a file. Each line starts with a **command** followed by command options. Syntax is very important and the [commands](https://lanl.github.io/LaGriT/pages/commands.html) reference pages should be followed, or errors may occur. The following conventions apply:

- Lines are a maximum of 80 characters long, identifiers and file names are a maximum of 32 characters long.
- A line can be continued by an “&” as the last character of a line to be wrapped.
- Command and token separators are comma, slash, or blank. (‘,’ ‘/’ ‘ ‘).
- White space is ignored except between slashes with empty space indicating default for that option should be used.
- LaGriT will ignore lines that start with comment indicators '*' or '#'.
See more at [Syntax Conventions](https://lanl.github.io/LaGriT/pages/docs/conventions.html)

## General Meshing Definitions

- **Mesh = Grid** = elements defining geometric shapes allowing numerical computations for simulations 
- **Node = Vertex** = point = mesh object nodes that can be used to connect into elements.
- **Element = cell** = nodes are connected into element types such as line, triangle, or tetrahedral (see page)
- **Structured mesh** = a mesh whose connectivity (topology) can be defined implicitly, and cell neighbors are predictable. Generally, a quadrilateral mesh in 2D and hexahedral mesh in 3D. 
- **Unstructured mesh** = a mesh that requires explicit connectivity information to describe cell topology. Generally used to represent complex shapes. 
- **Delaunay Triangulation** = a Delaunay mesh (triangles in 2D, tetrahedra in 3D) has cells such that the circumcircle(2D)/circumsphere(3D) contains no vertices other than the cell vertices. A Delaunay triangulation maximizes the minimum interior angle. 
- **Voronoi Tessellation** = the dual of a Delaunay triangulation, meaning that the vertices of one correspond to the cells of the other, and the edges of one correspond to the edges of the other.
    - Defined as ```V(pi) = {x | d(x, pi) ≤ d(x, pj) for all pj ∈ S, j ≠ i}```  
    - The Voronoi cell V(pi) contains all points x that are "closest" to the site pi.
    - "Closeness" is determined by the distance function d(x, p).
    - The inequality d(x, pi) ≤ d(x, pj) ensures that x is closer to pi than to any other site pj.
- **Control volume (CV) method** = in context of computational hydrology, the discretization method used by physics codes such as FEHM and PFLOTRAN for solving multiphase flow of fluid and energy (heat) in porous media. The CV method guarantees conservation of mass and heat.


## LaGriT Meshing Definitions

- **Current Mesh Object = mo** = is the mesh data structure used in LaGriT. 
- **imt** = integer material type = default name for vertex array with positive integer values indicating material ID or color.
- **itetclr** = integer tet color = default name for cell array with positive integer values indicating material ID or color (legacy name convention)
- **itp** = integer type = vertex integer tag indicating type such as interior, exterior, interior boundary, or exterior boundary. 
- **geometry** = is created with surface and region commands and is used to set mesh material values. 
- **LaGriT (.lgi and .mlgi)** = LaGriT command files suffix where .mlgi is a macro called from main command files. (Optional suffix convention) 
- **AVS (.inp)** = Advanced Visual Systems ascii file format for mesh information (vertex coordinates, cell connectivity, vertex attributes, cell attributes) recognized by most VIS applications. LaGriT can read and write AVS files. 
- **Exodus II (.exo)** = Binary mesh file format built on NetCDF data storage structures used for Amanzi/ATS and recognized by most VIS applications. LaGriT can write Exodus II files but does not read Exodus II files. 


## LaGriT Basics Tutorial 


This tutorial shows how to use LaGriT commands to create a simple structured multi-material mesh and files for simulations. This workflow starts with a hexahedral mesh which is connected into a Delaunay tetrahedral mesh. Materials are assigned to the cells and vertices using surfaces. The final step writes files used for simulations such as FEHM. 

You do not need to run LaGriT as the example output files are included in this tutorial. But new users will benefit from running the examples and making modifications to better understand results of different commands and their options.  Paraview is used to create the images in this tutorial and can read any of the AVS *.inp files. 


## [Step 1. Create a Hex Mesh](step_01.md)

## [Step 2. Convert Hex Mesh to Tet Mesh](step_02.md)

## [Step 3. Assign materials to the Mesh](step_03.md)

## [Step 4. Write Mesh and FEHM Setup Files](step_04.md) 


## Final Word


Meshing with LaGriT is not automatic and methods can be complex. But LaGriT tools provide a robust variety of meshing tools with capabilities specific to geologic applications and Voronoi control volume solvers. For some modeling applications, the LaGriT workflow can be generalized and easier to use. Applications using LaGriT for meshing include dfnWorks and Tinerator, both on GitHub and available for download. We hope this Tutorial provides insight into LaGriT and how it may solve your meshing needs.

