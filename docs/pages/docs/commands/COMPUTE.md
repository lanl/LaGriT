---
title: COMPUTE 
tags: compute, signed_distance_field, distance_field, medial_axis
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

<b>compute/medial_axis</b>/mo_axis/mo_surface/[in|out|both]/[alpha_sharp]/[rmax]
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
 
**`medial_axis`** is the keyword for the medial axis of a surface and
  its thickness. At every vertex P of `mo_surface` (a line mesh in a
  plane, 2D, or a triangle surface, 3D) it grows the largest ball that
  is tangent to the surface at P and holds no part of the surface (a
  shrinking ball). The ball's center is a point of the medial axis (the
  medial lines in 2D, the medial surface in 3D) and twice its radius is
  the thickness of the material at P.

- `mo_axis`: the new mesh object of medial points, one per vertex of
  `mo_surface` and side, with no cells (replaced if it exists). Its
  vertex attributes: `ma_r` the radius; `ma_src` the surface vertex;
  `ma_side` 1 in, 2 out (and `imt` the same); `ma_angle` the object
  angle in degrees, the widest angle at the center between P and a
  point where the ball touches the surface (a ball can touch along a
  ring, as in an ellipsoid); `ma_keep` 1 kept, 0 flagged.
- `mo_surface`: lines only or triangles only (convert quads with
  `grid2grid / quadtotri2` or `quadtotri4`). It gets `ma_thick_in`
  (2 x the radius) and `ma_keep_in`, and with `out` or `both` also
  `ma_thick_out` and `ma_keep_out`. The thickness can be used as a
  resolution field, for example in `poisson_disk`.
- `in` (the default), `out` or `both`: the side the balls grow into.
  `in` is against the normals: in 3D the cells' normals (a closed
  surface with outward normals: the inside); in 2D the normal of a
  segment is its direction crossed with the plane's axis (+z for a mesh
  in the xy plane), so a loop traversed counterclockwise has its inside
  on the `in` side. `both` grows two balls at every vertex, one into
  each side; use it for an interface between two materials.
- `alpha_sharp`: degrees, default 35. A corner wider than this
  (measured inside the material) has a branch of the medial axis that
  says nothing about thickness; its balls have an object angle under
  180 - `alpha_sharp` and are flagged (`ma_keep` 0). A wedge of angle a
  gives the angle 180 - a, so a pinch-out sharper than `alpha_sharp` is
  measured and kept. 0: nothing is flagged by its angle.
- `rmax`: the largest radius, default the diagonal of the surface's
  bounding box. A ball not stopped at `rmax` (the open side of a
  surface) keeps the radius `rmax` and is flagged.

Also flagged, with radius 0: a vertex with no normal; in 2D a vertex
that is not on exactly two segments (a junction or an end); in 3D a
vertex on an edge of three or more triangles (where three or more
materials meet).
The flagged points stay in `mo_axis`, so `pset` and `rmpoint` can
remove them (see the example). Vertex normals are the angle-weighted
average of the triangles' normals at P (3D) and the average of the two
segments' normals (2D).

The search uses the [kdtree](kdtree.md) of the surface's cells and the
exact nearest point on a triangle or segment, as `distance_field` /
`facet` does. A faceted surface always cuts a tangent ball a little
near P (the sag of its facets): points seen from the center within 90
degrees of P count only when they are closer than r (1 - cos 10°), so
turns of up to 20 degrees per vertex are read as a sampled smooth
surface and sharper ones as corners; points further round count
exactly. Flat faces give exact thicknesses; on curved surfaces the
error falls as the square of the edge length (a circle of 128
segments: 1e-4 of its radius). A surface coarser than 20 degrees per
vertex (a circle of 16 segments) is a polygon with corners, and its
balls are flagged. The balls flagged near rounded ends (an ellipse's)
can be a few percent too large; the kept ones are accurate. About 0.5 s
for a surface of 41,600 triangles.

**Resolution of the output.** `medial_axis` has no resolution
parameter: `mo_axis` gets one point per vertex of `mo_surface` and side,
so its resolution is the resolution of the input surface. The medial
points are as dense as the surface's vertices, and they follow them: a
slab gives two interleaved rows on its mid-plane, one from each face,
and a whole circle or sphere gives a single center. The thickness
attributes are likewise one value per surface vertex.

It is up to the user to give the input surface a resolution compatible
with the output resolution wanted. LaGriT has tools to make a surface
finer or coarser before `compute / medial_axis`:

- [refine2d](refine2d.md) splits every triangle into 4 by bisecting its
  edges, doubling the resolution of a triangle surface each time it is
  called. It works on triangles only: convert a quad surface to
  triangles first with [grid2grid](GRID2GRID.md) (`quadtotri2`, or
  `quadtotri4` with a new point at each quad's center).
  `compute / medial_axis` itself also needs triangles (or lines), so a
  quad surface is converted with `grid2grid` in any case.
- [refine](REFINE.md) with `maxsize` or `rivara` refines to a size the
  user gives (for example edges longer than a length), for a
  triangulation whose resolution varies.
- [derefine](DEREFINE.md) removes points to make a mesh coarser.

The new vertices of a refinement lie on the old triangles, so refining
gives more medial points and thickness values but no more geometric
detail than the surface had: a sphere of 1280 triangles refined once
with `refine2d` (642 to 2562 vertices) gives four times as many balls,
with the accuracy of the 1280 triangles.

`compute / distance_field` gives the distance from points to a surface
everywhere on a background mesh; the medial axis is where that field
has a ridge. `medial_axis` finds those points directly from the
surface and gives the thickness at every surface vertex.

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

The medial axis and the thickness of a layer (a triangle surface
around it, outward normals). The flagged points are removed from the
medial points with `pset` and `rmpoint`; `compute` leaves the mesh
object that was current before it current, so select `mo_ma` first:

```
 read / avs / layer.inp / mo_s
 compute / medial_axis / mo_ma / mo_s / in / 35.
 cmo / printatt / mo_s / ma_thick_in / minmax
 cmo / select / mo_ma
 pset / pflag / attribute / ma_keep / 1 0 0 / eq 0
 rmpoint / pset get pflag / exclusive
 rmpoint / compress
 dump / avs / medial_points.inp / mo_ma
 finish
```

In 2D, teeth 0.5 high under a flat top (a line mesh): the kept medial
points (blue) run along the middle and every 8th ball is drawn; the
branches into the corners (grey) are flagged. Above the middle tips the
thickness is 1.5:

 <img src="https://lanl.github.io/LaGriT/assets/images/compute_medial_axis_2d.png" width="520">

In 3D, a pinch-out (a wedge of 11.3 degrees, under `alpha_sharp`): a
section through the medial surface, and the thickness on the bottom,
2 x tan(11.3°/2) x the distance from the tip; the rings near the sides
and the far end are flagged (their balls touch a side or the end):

 <img src="https://lanl.github.io/LaGriT/assets/images/compute_medial_axis_3d.png" width="1040">

The test suite test/suite/test_medial_axis checks every ball by brute
force (center, empty, touching, flag), the shapes whose answers are
known (a circle, a sphere, an ellipse and an ellipsoid, slabs, an
annulus and a pipe, a neck, a wedge, two materials in 2D and 3D), a
noisy slab, a resolution sweep, coordinates of UTM size, and rebuilds
each shape as the union of its kept balls.

See also the tutorial [Tet Mesh Graded by a Surface's Edge Lengths](../../tutorial/dfn_tet_grading/index.md), which uses `distance_field` in a poisson_disk h field.     
                                                                                                           
