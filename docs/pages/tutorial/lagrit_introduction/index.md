---
title: Tutorial LaGriT Introduction
---

<link rel="stylesheet" href="{{ "/assets/css/index_page_style.css" | relative_url }}">

<div class="button" id="button-3">
   <div id="circle"></div>
   <a href="{{ "/pages/tutorial/lagrit_introduction/index" | relative_url }}">Download Tutorial</a>
</div>

# Tutorial - LaGriT Introduction

LaGriT consists of mesh **commands** and options that are used to create, modify, optimize, and write files. LaGriT can be run on the command line interactively so commands are entered one at a time at the prompt, or with one or more files containing the LaGriT commands. By convention we use ".lgi" for lagrit input file names and ".mlgi" for macro files called from main command file.

Run lagrit from the command line interactively, or put commands in a file and redirect.
```
lagrit
```
or
```
lagrit < input.lgi
```
**Important Note:** the **`finish`** command must be used to exit interactive session and must be at end of command file.



## Common terms and definitions

### General

- Mesh = Grid = elements defining geometric shapes allowing numerical computations for simulations 
- Node = Vertice = point = mesh object nodes that can be used to connect into elements.
- Element = cell = nodes are connected into element types such as line, triangle, or tetrahedral (see page)
- structured mesh = elements are arranged in a regular pattern with parallel sides and and orthogonal elements.
- unstructured mesh = elements arranged to fit complex geometries
- Delaunay = tri or tet elements arranged such that mesh is optimized for compuational accuracy
- Voronoi = the vertices of the voronoi volumes correspond to the circumcenters of the Delaunay tri and tets 
- control volume (CV) method = ensures conservation of mass and energy in fluid flow and heat transfer equations

### LaGriT

- Current Mesh Object = cmo = the mesh object that actions will act on by default.
- imt = default name for node array with positive integer values indicating color or material
- itetclr = default name for element array with positive integer values indicating color or material
- itp = boundary tags
- geometry = created with **`surface`** and **`region`** commands and is used to set mesh materials
- LaGriT (.lgi and .mlgi) = LaGriT command files where .mlgi is a macro called from main command files.
- AVS (.inp) = file format for mesh information that is easy to convert and recognized by most VIS applications
- Exodus II (.exo) = Complicated mesh file format used for Amanzi/ATS and recognized by most VIS applications


## Step 1. Create a Mesh Object

## Step 2. Check and View the Mesh

## Step 3. Assign materials

## Step 4. Set boundary nodes

## Step 5. Check and View the Mesh

## Step 6. Write Mesh and FEHM Setup Files 

## Step 7. Create a zone list for FEHM 

## Step 8. Create Surfaces for Stacked Mesh

## Step 9. Create Stacked Mesh

## Step 10. Write Exodus Mesh from Stacked Mesh (Need build with Exodus Libraries)

# Final Word

Meshing with LaGriT is not automatic and methods can be complex. But the tools provide a robust variety of meshing tools with capabilities specific to geologic applications and Voronoi control volume solvers. For some modeling applications, the LaGriT workflow can be generalized and easier to use. Applications using LaGriT for meshing include dfnWorks and Tinerator, both on github and available for download. We hope this Tutorial provides insight into LaGriT and how it may solve your meshing needs.

