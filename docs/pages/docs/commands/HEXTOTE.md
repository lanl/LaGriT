---
title: HEXTOTET
tags: hex to tet connect
---

# HEXTOTET #
--------

This command, while still recognized, has a newer more readable syntax, see [grid2grid](GRID2GRID.md) instead.

The **`hextotet`** command creates a tetrahedral grid from 3D grids, and triangles from 2D.
The first parameter *ioption* determines how the conversion is performed.


NOTE: Use of **hextotet** (or **grid2grid**) to convert a 3D mesh to a tetrahedral mesh will in general result in a non-Delaunay tetrahedral mesh. If the target simulator is one that uses two-point flux approximation and Voronoi control volumes (FEHM, PFLOTRAN, TOUGH2) then using **hextotet** and then **dump/stor** to compute and output geometric coefficients, is not a good idea. If the ultimate goal is a a geometric coefficients file, one should use **connect** to connect vertices into a Delaunay mesh.

 
Note: Use of **hextotet** to convert an octree refined hexahedral into to a tetrahedral mesh should not be done. You will get a result, however, no special algorithms are implemented to connect across interfaces where octree resolution changes and hanging nodes occur. One should instead copy the octree vertex set into a tet mesh object and use **connect** to create a tetrahedral mesh.


## SYNTAX

<pre>
 <b>hextotet</b> / [ ioption ] / cmo_snk / cmo_src / [ <b>rmvolume</b>]
 </pre>
 
## OPTIONS

*`ioption`* is a numerical number indicating the number of tets or
triangles to break each element into. If this parameter is missing then
default settings are used. The defaults are underlined and will be
detirmined by reading the mesh_type of the mesh_object. If mesh_type
is quad, **2** is used. If mesh_type is prism, **3** is used. If
mesh_type is hex, **6** is used. Otherwise **24** is the default value
for ioption.

The selections include:

 **2**   quad to 2 triangles, no new points.

 **3**   prism to 3 tets, no new points.

 **4**   quad to 4 triangles, with one new point.

 **4**   pyramid to 4 tets, with one new point.

 **5**   hex to 5 tets, no new points.

 **6**   hex to 6 tets, no new points.

 **14**   prism to 14 tets, four new points (1 + 3 faces).

 **18**   prism to 18 tets, six new points (1 + 5 faces).

 **24**   hex to 24 tets, seven new points (1 + 6 faces).


*`cmo_snk / cmo_src`* : are the mesh_object names. cmo_src is the
original grid. cmo_snk is the name for the new tet or triangle grid.

**`rmvolume`** : keyword is optional and will assign
hextotet_remove_volume and hextotet_remove_duplicates to 'yes'. This
will enable hextotet to use its own algorithm for removing elements with
zero volume and duplicate points. It may be prone to epsilon errors for
grids over large areas. By default, zero volumes and duplicate points
are not removed from the new mesh object cmo_snk.




## EXAMPLES

<pre>
 hextotet / 24 / cmo_tet / cmo_hex
 </pre>

 Convert each hex element in cmo_hex to 24 tets and name the new grid cmo_tet.
 
<pre>
hextotet / / cmo_tri / cmo_quad
</pre>

 No value is given for ioption, so the default settings are used. The
 mesh_type of cmo_quad is quad, so each element is converted to two
 triangles. The new mesh_object is named cmo_tri.
 
<pre>
 hextotet / 3 / cmo_tet / cmo_pri / rmvolume
 </pre>
 
 Each prism element in cmo\_pri is converted to three tet elements.
 Zero volume elements and duplicate points are removed. The new tet
 mesh object is called cmo\_tet.
 
 <pre>
cmo/addatt/mohex/imtsav/VINT/scalar/nnodes/linear/  
cmo copyatt / mohex mohex / imtsav imt
cmo/addatt/mohex/itetsav/VINT/scalar/nelements/linear/
cmo copyatt / mohex mohex / itetsav itetclr
cmo setatt mohex imt 1
cmo setatt mohex itetclr 1
resetpts itp
hextotet / 5 / motet / mohex / 
 </pre>
 
*CAUTION If mesh is multi-material this can add signifigant time to the hextotet routine!* This example shows a good practice which will avoid this issue. The attributes *`imtsav`* and *`itetsav`* are created and values copied before setting the mesh materials in itetclr and imt to 1. The command **resetpts/itp** will reset boundary and interface accordingly. This mesh mohex has each hex element converted into 5 tets each. The number of nodes will not change.



LINKS:

 [Click here for demos](../demos/main_hextet.md)
