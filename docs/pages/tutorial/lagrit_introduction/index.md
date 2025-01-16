
Download this Tutorial

# Tutorial LaGriT Introduction

LaGriT consists of mesh **commands** and options that are used to create, modify, optimize, and write files. LaGriT can be run on the command line interactively so commands are entered on at a time at the prompt, or with one or more files containing the LaGriT commands. By convention we use ".lgi" for lagrit input file names and ".mlgi" for macro files called from main command file.

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

Mesh = Grid = elements defining geometric shapes allowing numerical computations for simulations 
Node = Vertice = point = mesh object nodes that can be used to connect into elements.
Element = cell = nodes are connected into element types such as line, triangle, or tetrahedral (see page)
structured mesh = elements are arranged in a regular pattern with parallel sides and and orthogonal elements.
unstructured mesh = elements arranged to fit complex geometries
Delaunay = tri or tet elements arranged such that mesh is optimized for compuational accuracy
Voronoi = the vertices of the voronoi volumes correspond to the circumcenters of the Delaunay tri and tets 
control volume (CV) method = ensures conservation of mass and energy in fluid flow and heat transfer equations

### LaGriT

Current Mesh Object = cmo = the mesh object that actions will act on by default.
imt = default name for node array with positive integer values indicating color or material
itetclr = default name for element array with positive integer values indicating color or material
itp = boundary tags
geometry = created with **`surface`** and **`region`** commands and is used to set mesh materials


## Step 1. Create a Mesh Object

## Step 2. Check and View the Mesh

## Step 3. Assign materials

## Step 4. Set boundary nodes

## Step 5. Check and View the Mesh

## Step 6. Write Mesh and FEHM Setup Files 

## Step 7. Create a zone list for FEHM 

