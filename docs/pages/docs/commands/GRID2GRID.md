---
title: GRID2GRID
tags: ok
---

GRID2GRID
---------

The **grid2grid** command converts a mesh with one element
type to a mesh with another. The conversion type is determined by the
second argument.

**FORMAT:**

    grid2grid / ioption / [cmo_sink] / [cmo_source]

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
*  **tree\_to\_fe**   quadtree or octree grid to grid with no parent-type elements.  

**`[ cmo_snk / cmo_src ]`** : the mesh\_object names. 

`cmo_src` is the original grid. 

`cmo_snk` is the name for the new tet or triangle grid. These may be the same grid, if so desired. If both are left blank, the current mesh object will be used. If only one mesh name
is given, it will be used at the sink mesh, and the current mesh object will be used as the source.

**EXAMPLES**:

 **`grid2grid / hextotet24`**` /  cmo_tet / cmo_hex`  
 Convert each hex element in cmo\_hex to 24 tets and name the new grid
 cmo\_tet.
 
 
 **`grid2grid / quadtotri4`**` /  new_mesh`  
 No source mesh is given, so the current mesh object (which is a quad
 mesh) will have every quad converted into 4 triangles, and saved as
 new\_mesh.
 
 **`grid2grid / tree_to_fe`**` / new_mesh /  octree_mesh`  
 Every element in octree\_mesh will be scanned to see if it is a parent
 element. If it is, it will be removed, so only the leaf elements
 remain. The result will be stored in new\_mesh.

