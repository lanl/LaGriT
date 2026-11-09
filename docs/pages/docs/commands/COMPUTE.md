---
title: COMPUTE 
tags: compute, signed_distance_field, distance_field
---

# COMPUTE

----------------------


This command contains modules that compute various attributes and functions based on one or more mesh objects. This operation will (often) result in new attributes
being added to the mesh objects. The action of the command will be controled by the keyword in the second argument position.  


## SYNTAX

<pre>
<b>compute/distance_field</b>/mo_sink/mo_source/distance_field_attribute/[vertex|facet]
 
<b>compute/signed_distance_field</b>/mo_sink/mo_source/distance_field_attribute
  
<b>compute/linear_transform</b>/mo_main/mo_surface/[direction/att_name]
</pre>


 
**`distance_field`** is the keyword for distance field calculation. Determine
  the minimum distance from mo_source (its vertices, or with `facet`
  its cells) to every vertex in mo_sink and place the result in the
  vertex based floating point attribute,
  distance_field_attribute in mo_sink. The computation is
  accelerated by using the [kdtree](kdtree.md) search algorithm.

  - **`vertex`** (the default, the same as no sixth word): the distance
  from each vertex of mo_sink to the nearest **vertex** of mo_source.
  - **`facet`**: the exact distance from each vertex of mo_sink to the
  nearest **point** of the cells of mo_source, which may be inside a
  cell, on an edge or at a vertex. mo_source holds triangles and line
  segments (in any mix), or quads only; each quad is cut into 4
  triangles at its center (as by `hextotet / 4`) for the distance.
  Other cell types (tets, hexes, ...), quads mixed with other cells and
  a mo_source with no cells are refused with an ERROR, and no
  attribute is made. The candidate cells come from a k-D tree of the
  cells' bounding boxes (a sphere of 5,120 triangles and 68,921 sink
  vertices: well under a second).

  With `vertex`, a point on a triangulated surface, in the middle of a
  triangle, is given a distance up to about the triangle's
  circumradius (about 0.58 times the edge length for an equilateral
  triangle) instead of 0. The difference shrinks with the distance from
  the surface (about r^2 / 2d far away). Use `facet` when the distance
  near the surface matters, for example for an h field graded from a
  surface.


**`signed_distance_field`** is the keyword for signed distance field
  calculation. Determine the minimum distance
  from any vertex in mo_source to every vertex in mo_sink and place the
  result in the vertex based floating
  point attribute, distance_field_attribute in mo_sink. The
  computation is accelerated by using the
  [kdtree](kdtree.md) search algorithm.

This option requires the `mo_source` MUST be either a triangle or quad surface mesh object. If the surfaces form a topologically closed volume then
positive, 'above' distance is in the     direction of the surface normal vector. Negative is 'below' the
surface. If the surface is not a closed     volume, then the assumptions described in the
  [surface](SURFACE.md) command are used to determine what is above and what is below the surface.
 

**`linear_transform`** is the keyword for an extrapolation from an attribute
  value in a surface onto every vertex of a 3D mesh.
  
Given a 3D mesh and a 2D surface, this command will extrapolate a scalar value from that surface
  onto every point of the mesh. This can be used to (for
  example):
 
  -   Propagate head values from a surface onto all vertices of a
      mesh.
  -   Expand a mesh to fit a surface, by propagating the
      appropriate spatial coordinate.
  -   Compute the depth relative to a topographic surface to each
      vertex of a mesh.

 
This is highly dependent on the spatial relation between the mesh
and the surface - values from the surface are extrapolated "downward" into the mesh in the direction
  specified in the command. The
  direction specified in the command must be one of
   `zpos, zneg, ypos, yneg, xpos, xneg`
   
  For example,
  specifying [zpos] will result in the upper (positive
  z-axis) side of the mesh having attribute values conforming exactly
  to those on the surface, while the
  lower side of the mesh will have whatever attribute values it had
  previous, with all vertices in between
  having attribute values distributed linearly between the two
  extremes. If a direction is not specified,
  it will default to zpos. If an
  attribute is not specified, it will default to the spatial attribute
  appropriate
  to the chosen direction (i.e. if the direction is
  yneg, the attribute will default to yic, the
  y-coordinate of
  each vertex. The attribute chosen must already exist in both the
  surface and main meshes.
 
  Other places to look for modules that compute some standard mesh
  attributes include, [**QUALITY**](QUALITY.md), which
  will compute aspect ratio and volume,
  [**cmo/addatt**](cmo/cmo_addatt.md), which will compute normal vectors, dihedral angles, solid
  angles, meadian points, Voronoi points and more. User functions can be computed with  [**MATH**](MATH.md).
 


## EXAMPLES

                                                                        
  
```  
 cmo / create / cmo_src                                            
 createpts/rtz/1,91,1/3.,0.,0./3.,270.,0./1,1,1/                   
 cmo / create / cmo_snk                                            
 createpts / xyz / 30 30 1 / -5. -5. -5. / 5. 5. 5. / 1 1 1        

 compute / distance_field / cmo_snk / cmo_src / dfield             

 finish           
```
  
 <img src="https://lanl.github.io/LaGriT/assets/images/distance_field_01.png" width="300">

The vertex and the facet distance to a coarse sphere (80 triangles,
radius 1, read from an AVS file), on a 121 x 121 grid of points in the
plane z = 0:

```
 read / avs / src.inp / mo_src
 read / avs / sink.inp / mo_sink
 compute / distance_field / mo_sink / mo_src / d_vertex
 compute / distance_field / mo_sink / mo_src / d_facet / facet
 dump / avs / out.inp / mo_sink
 finish
```

 <img src="https://lanl.github.io/LaGriT/assets/images/distance_field_facet.png" width="900">

The vertex distance is 0 only at the sphere's vertices (white dots) and
up to 0.35 too large between them; the facet distance is 0 all along
the surface (the triangles cut by the plane). The test suite
test/suite/test_distance_field checks `facet` against a brute-force
computation.

See also the tutorial [Tet Mesh Graded by a Surface's Edge Lengths](../../tutorial/dfn_tet_grading/index.md), which uses `distance_field` in a poisson_disk h field.     
                                                                                                           
