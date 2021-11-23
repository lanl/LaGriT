---
title: GRID2GRID
tags: grid2grid, hextotet, amr
---

# GRID2GRID


-------------------


The **grid2grid** command converts a mesh with one element
type to a mesh with another. The conversion type is determined by the
second argument.

NOTE: Use of **grid2grid** to convert a 3D mesh to a tetrahedral mesh will in general result in a non-Delaunay tetrahedral mesh. If the target simulator is one that uses two-point flux approximation and Voronoi control volumes (FEHM, PFLOTRAN, TOUGH2) then using **hextotet** and then **dump/stor** to compute and output geometric coefficients, is not a good idea. If the ultimate goal is a a geometric coefficients file, one should use **connect** to connect vertices into a Delaunay mesh.

 
Note: Use of **grid2grid** to convert an octree refined hexahedral into to a tetrahedral mesh should not be done. You will get a result, however, no special algorithms are implemented to connect across interfaces where octree resolution changes and hanging nodes occur. One should instead copy the octree vertex set into a tet mesh object and use **connect** to create a tetrahedral mesh.


## SYNTAX

<pre>
<b>grid2grid</b> / ioption / [ cmo_sink ] / [ cmo_source ]
</pre>    
    

## OPTIONS


`ioption`: is a character string that determines the element type of the
source and sink meshes. There is no 'default' option - this argument
must be specified.

*  **quadtotri2**   quad to 2 triangles, no new points.  
*  **prismtotet3**   prism to 3 tets, no new points.  
*  **quattotri4**   quad to 4 triangles, with one new point.  
*  **pyrtotet4**   pyramid to 4 tets, with one new point.  
*  **hextotet5**   hex to 5 tets, no new points.  
*  **hextotet6**   hex to 6 tets, no new points.  
*  **prismtotet14**   prism to 14 tets, four new points (1 + 3 faces).  
*  **prismtotet18**   prism to 18 tets, six new points (1 + 5 faces).  
*  **hextotet24**   hex to 24 tets, seven new points (1 + 6 faces).  
*  **tree_to_fe**   quadtree or octree grid to grid with no parent-type elements to clean amr or octree structures. 


`[ cmo_snk / cmo_src ]` are the mesh_object names for the new mesh to create and the source mesh object. `cmo_src` is the original grid. `cmo_snk` is the name for the new tet or triangle grid. These may be the same grid, if so desired. If both are left blank, the current mesh object will be used. If only one mesh name
is given, it will be used at the sink mesh, and the current mesh object will be used as the source.


## EXAMPLES

```
grid2grid / hextotet24 /  cmo_tet / cmo_hex 
```
Convert each hex element in cmo_hex to 24 tets and name the new grid cmo_tet.

```
grid2grid / quadtotri4 /  new_mesh 
```
No source mesh is given, so the current mesh object (which is a quad
 mesh) will have every quad converted into 4 triangles, and saved as new_mesh.

```
grid2grid / tree_to_fe / new_mesh /  octree_mesh
```
Parent elements in octree_mesh will be removed, so only the leaf elements remain. The result will be stored in new_mesh.
This is often used to ensure a single valid mesh for other commands or for use outside of LaGriT. 
The octree attributes itetpar, itetkid, and itetlev will be updated in the new_mesh.
A description of the octree mesh and attributes are described in the command [refine](LaGriT/pages/docs/commands/REFINE.html).


