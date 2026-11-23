---
title: 'DUMP/CONTROL_VOLUME'
tags: dump control_volume cv_dual voronoi stor uge tough delaunay
header_image: control_volume.jpg
header_title: DUMP / CONTROL_VOLUME
header_subtitle: Voronoi control volumes of a Delaunay mesh, and the coefficients written for FEHM, PFLOTRAN and TOUGH.
---

------------------------

**`dump/control_volume`** checks a mesh object and computes its
control volumes and their geometric coefficients with cv_dual:
Voronoi control volumes (the default), median control volumes, or a
hybrid between the two. For each vertex it computes its control
volume, and for each edge the area of the face between the two
control volumes and the edge length. It writes them as FEHM `.stor`,
PFLOTRAN `.uge` or TOUGH `MESH` files and, when asked, puts the
volumes in a vertex attribute. It is the new path beside
**`dump/stor`**, which is unchanged (it uses **`matbld2d`** and
**`matbld3d`**); both can run on the same mesh to compare. The same
code is the stand-alone program `cv_dual` (built with LaGriT,
`build/cv_dual`; its [manual](../../cv_dual/cv_dual_manual.md)), and
**`dump/stor`**, **`dump/fehm`**, **`dump/pflotran`** and
**`dump/zone_outside`** run it for those commands' files, by default
since V3.4.0 ([cv_dual OR matbld](DUMP3.md#cv_dual-or-matbld)). Only
**`dump/control_volume`** takes every keyword below.

Before any geometry **`dump/control_volume`** checks the mesh (cell
types, cells of positive area/volume, conformity, orientation) and tests it for the
Delaunay property, facet by facet. A mesh that fails is not written:
Voronoi coefficients of a mesh that is not Delaunay are wrong. The
facets that fail are written to a file for ParaView.

The mesh is all triangles (2D, or a fracture network in 3D: one
itetclr per fracture) or all tets. Its arrays are used in place: the
command changes nothing in the mesh object, except the attributes of
**`attribute_cv_volume`** and the **`ccoef_*`** keywords.

## SYNTAX ##

<pre>
<b>dump</b> / <b>control_volume</b> / mo_name &
     / [<b>stor</b>, file_name] / [<b>uge</b>, file_name] / [<b>tough</b>, file_name] &
     / [<b>zero_area</b>, <b>remove</b>|<b>keep</b>] / [<b>zero_tol</b>, value] &
     / [<b>compress</b>, <b>on</b>|<b>off</b>] &
     / [<b>cv</b>, <b>voronoi</b>|<b>median</b>|<b>blend</b>|<b>hybrid</b>] / [<b>alpha</b>, value] &
     / [<b>hybrid_tol</b>, value] / [<b>alpha_min</b>, value] / [<b>hybrid_iter</b>, n] &
     / [<b>alpha_file</b>, file_name] &
     / [<b>paraview</b>, root] / [<b>paraview_faces</b>, <b>on</b>|<b>off</b>] &
     / [<b>paraview_subset</b>, <b>all</b>|<b>boundary</b>|<b>flagged</b>|<b>material</b> N|<b>box</b> x1 y1 z1 x2 y2 z2] &
     / [<b>vtu_format</b>, <b>binary</b>|<b>ascii</b>] / [<b>vtu_layout</b>, <b>new</b>|<b>legacy</b>] &
     / [<b>matrix</b>, file_name] / [<b>matrix_bins</b>, n] &
     / [<b>outside_area</b>, file_name] / [<b>exclude</b>, imt [imt ...]] &
     / [<b>delaunay_tol</b>, value] / [<b>delaunay_fatal</b>, value] &
     / [<b>violations</b>, file_name] &
     / [<b>check_only</b>, <b>on</b>|<b>off</b>] / [<b>report</b>, file_name] &
     / [<b>attribute_cv_volume</b> [, attribute_name]] &
     / [<b>ccoef_area_min</b> [, attribute_name]] / [<b>ccoef_area_max</b> [, attribute_name]] &
     / [<b>ccoef_ij_area_min</b> [, attribute_name]] / [<b>ccoef_ij_area_max</b> [, attribute_name]] &
     / [<b>ccoef_areabydist_min</b> [, attribute_name]] / [<b>ccoef_areabydist_max</b> [, attribute_name]] &
     / [<b>ccoef_ij_areabydist_min</b> [, attribute_name]] / [<b>ccoef_ij_areabydist_max</b> [, attribute_name]] &
     / [<b>ccoef_distance_min</b> [, attribute_name]] / [<b>ccoef_distance_max</b> [, attribute_name]]
</pre>

mo_name, the mesh object, is required; there is no default (**-def-**
is not taken). After it, each option is a keyword and its value,
written with a comma (or a space) between them; options are separated
by `/` and may come in any order. With no output keyword,
**`dump/control_volume`** writes `voronoi.stor`. File names are LaGriT
words: at most 32 characters, no path (LaGriT works in one folder).

The messages go to the screen and LaGriT's log (outx3dgen). An error
(a bad keyword or value, a missing mesh object, a mesh the checks
refuse) prints what is wrong and an ERROR line; nothing is written
and LaGriT goes on with the next command.

## DESCRIPTIONS ##

**`stor`**, file_name

writes the FEHM `.stor` file (ASCII), in the layout of **`dump/stor`**:
volumes, row pointers, column numbers, value pointers, diagonal
pointers and the values -A(ij)/d(ij), with 17 significant digits
(**`dump/stor`** writes 13). Its first line names the policy
(Voronoi, Median, Blend or Hybrid Coefficients) and its second line
ends with the command that made the file, in parentheses, as
`(cv_dual dump / control_volume / mo_name / stor, mesh.stor / ...)`;
FEHM ignores both lines.

**`uge`**, file_name

writes the PFLOTRAN explicit unstructured grid file (17 significant
digits): `CELLS` (one
line per vertex: id, x, y, z, volume) and `CONNECTIONS` (one line per
edge: i, j, the edge midpoint, area).

**`tough`**, file_name (**`tough2`**, the older name, also works)

writes the TOUGH `MESH` file: `ELEME` (one record per vertex) and
`CONNE` (one record per edge), as lanl/voronoi wrote them. The
material of a vertex is the rank of its **imt1** among the distinct
values.

Several outputs may be given in one command; the geometry is
computed once. No two files may share a name.

**`zero_area`**, **`remove`** | **`keep`**

On co-circular or co-spherical vertices some edges get a Voronoi face
of zero area. **`remove`** (default) leaves them out of every file;
**`keep`** writes one connection per mesh edge, zero ones included
(the matrix is one to one with the mesh edges, as **`matbld2d`**
writes it). The control volumes are the same either way.

**`zero_tol`**, value

an edge has zero area when \|A\| <= value d² (tets) or value d
(triangles), d the edge length. Default 1e-12.

**`compress`**, **`on`** | **`off`**

`.stor` only. **`on`** (default): values equal within 1e-14 of the
largest are written once and shared through the pointer block.
**`off`**: one value per matrix entry.

**`cv`**, **`voronoi`** | **`median`** | **`blend`** | **`hybrid`**

where each cell's dual point goes. A cell gets a weight alpha from 0
to 1 and its point is G + alpha (C - G), C the circumcenter, G the
centroid; in 3D each face point is the same blend of the face's
circumcenter and centroid, with the smaller alpha of the face's two
tets, so the control volumes stay closed.

- **`voronoi`** (default): alpha = 1, the Voronoi control volumes.
- **`median`**: alpha = 0, the centroids. The mesh need not be
  Delaunay: the Delaunay check is reported but the command goes on.
  In 2D no coefficient is negative.
- **`blend`**: the same alpha (keyword **`alpha`**) in every cell.
- **`hybrid`**: alpha = 1 except where a coefficient would be
  negative. A boundary cell whose circumcenter lies outside first has
  its point put on its boundary facet (closed form); then, edge by
  edge, the cell with the most negative piece of a negative
  coefficient has its alpha lowered just enough. Interior Voronoi
  coefficients of a Delaunay mesh are never negative, but moved points
  can make them so; every edge is checked. Edges still negative at
  **`alpha_min`** or after **`hybrid_iter`** passes give a WARNING;
  the files are still written.

The report gives the negative coefficients before and after, the
cells moved, and the non-orthogonality (the angle between each dual
face and its edge, 0 for Voronoi).

**`alpha`**, value

**`cv blend`** only, and required there: from 0 (median) to 1
(Voronoi).

**`hybrid_tol`**, value

**`cv hybrid`** only: a coefficient may stay negative down to -value
times the sum of its cells' pieces. Default 0. Zero-area connections
(**`zero_tol`**) are never counted as negative.

**`alpha_min`**, value

**`cv hybrid`** only: the lowest alpha it may give a cell. Default 0.

**`hybrid_iter`**, n

**`cv hybrid`** only: the most correction passes. Default 100.

**`paraview`**, root

writes the control volumes for ParaView: `root.pvd` (open this), the
mesh (`root_mesh.vtu`), one cell per control volume
(`root_cells.vtu`: a polygon for triangles, one per vertex per
fracture in a network; a polyhedron for tets), and the negative
coefficients as segments (`root_negative.vtu`, when there are any).
The cells are drawn from the dual points the coefficients came from
(moved ones with **`cv, hybrid`**).

**`paraview_faces`**, **`on`** | **`off`**

also writes `root_faces.vtu`, the dual face of each connection (tets).
Default **`off`**.

**`paraview_subset`**, **`all`** | **`boundary`** | **`flagged`** | **`material`** N | **`box`** x1 y1 z1 x2 y2 z2

which vertices get a cell: all (default), those on the boundary,
those of a negative coefficient or of a cell with alpha < 1, those of
cells of itetclr N, or those inside the box.

**`vtu_format`**, **`binary`** | **`ascii`**

binary (default) or text files.

**`vtu_layout`**, **`new`** | **`legacy`**

VTK's polyhedron layout: **`new`** (default; ParaView 6) or
**`legacy`** (ParaView 5 and older readers).

**`matrix`**, file_name

writes the coefficient matrix as a VTK file for ParaView, to see its
shape: one point per entry of the `.stor` matrix at (j, -i, 0), row 1
at the top, both halves and the diagonal. Each point holds i, j, the
area, distance and coefficient, the class (0 diagonal, 1 boundary, 2
interior, 3 interface), whether it is negative or zero-area, the
smallest alpha of the cells around the edge, the angle between the
edge and its dual face, the two vertices' **imt1**, and on the
diagonal the control volume. **`paraview_subset`** limits it to the
rows of its vertices. The report gives the matrix's bandwidth and
profile.

**`matrix_bins`**, n

with **`matrix`**: also an n x n image of the whole matrix (the file
name with `.vti`): each pixel holds how many entries fall in its
block of rows and columns, how many are negative, and the smallest
coefficient.

**`alpha_file`**, file_name

writes the mesh as a VTK file for ParaView, with each cell's alpha,
the step that set it (0 none, 1 the boundary clamp, 2 the
correction), whether the cell touches an edge still negative, and
its **itetclr**.

**`exclude`**, imt [imt ...]

the materials (**imt1** values) whose vertices are left out of the
**`stor`**, **`uge`** and **`tough`** files: the ghost generators of a
conforming Voronoi mesh, the seeds outside the model (above the
topography, beyond its sides) that make the Voronoi faces between them
and the model's own seeds fall on the model's boundary. The control
volumes and coefficients are computed on the whole mesh, as without
**`exclude`**; then the excluded vertices, and every connection to
one of them, are not written. The other vertices keep their order and
are numbered 1, 2, ... in the files (the n-th vertex kept is vertex n),
with the volumes and coefficients of the whole-mesh run. This is not
the same as removing the ghosts first (**`rmmat`**, or **`rmpoint`**
on a pset) and dumping: the volumes of the vertices on the boundary
would then be cut off by what is left of the mesh, not by the
conforming faces. A material not in the mesh is ignored; excluding
every vertex is refused. The vertex attributes (**`attribute_cv_volume`**,
the **`ccoef_*`** keywords), the ParaView, matrix and alpha files and
**`outside_area`** still cover the whole mesh. How the areas of the
faces between the model and its ghosts (the model's boundary) are
passed to FEHM is not settled yet.

```
dump / control_volume / mo_mesh / stor, model.stor / exclude, 4
dump / control_volume / mo_mesh / stor, model.stor / exclude, 4 6 9
```

**`outside_area`**, file_name

each boundary vertex's vector area (Ax, Ay, Az): the part of its
control volume on the mesh boundary, from the same points as the
coefficients (Voronoi: each boundary triangle split at its
circumcenter; median: a third each; hybrid: the points it moved), the
area vector pointing outward. One line per vertex on the boundary
(vertex number, Ax, Ay, Az, 17 significant digits), then the sums. In
2D and on a fracture's edges each vertex gets half of each boundary
edge, as a length vector. **`dump/fehm`** and **`dump/zone_outside`**
with **`cv_dual`** write the same numbers in FEHM's outside zones
(`_outside_vor.area`, `_med`, `_hyb`). With **`outside_area`** and no
other output, only this file is written.

**`delaunay_tol`**, value

For each facet (tet face, triangle edge) shared by two cells with
circumcenters c1 and c2, delta = (c2 - c1) . n, n the facet's unit
normal from cell 1 into cell 2, is the signed length of the Voronoi
edge dual to the facet; L is the facet's length (mean edge length for
a tet face). \|delta/L\| <= value is a tie (co-circular or
co-spherical) and is accepted; below -value the facet is not Delaunay
(see **`delaunay_fatal`**). Default 1e-10: true ties compute to about
1e-16.

**`delaunay_fatal`**, value

delta/L between -value and -**`delaunay_tol`**: the facet is slightly
not Delaunay (nearly co-circular or co-spherical, as LaGriT's
**`connect`** leaves a few); a warning, and the command goes on: the
coefficients there are accurate to about \|delta/L\| of a typical
coefficient. Below -value: not Delaunay, and the command stops.
Default 1e-2 (the worst seen from **`connect`** is -2.3e-3, in a
10-million-tet mesh; a flipped edge or face gives -0.1 or below). Set it equal to
**`delaunay_tol`** to make every facet beyond the tolerance fatal.

For a boundary facet, a cell whose circumcenter lies outside the mesh
(a triangle with an obtuse angle opposite a boundary edge) is counted
and written to the violations file but does not stop the command; a
negative coefficient at that edge is likely.

**`violations`**, file_name

the facets the Delaunay check flags (violations and boundary
circumcenters outside), as a VTK XML file for ParaView: one cell per
facet, with cell data **kind** (1 violation, 2 violation across a
material interface, 3 boundary circumcenter outside),
**delta_over_L**, **cell1**, **cell2**, **itetclr1**, **itetclr2**
and **ratio** (circumradius / shortest edge; large means a sliver).
Default `violations.vtu`; written only when a facet is flagged.

**`check_only`**, **`on`** | **`off`**

**`on`**: the mesh checks and the Delaunay check only; no coefficient
file is written. Default **`off`**.

**`report`**, file_name

a copy of everything **`dump/control_volume`** prints, in a file.

**`attribute_cv_volume`** [, attribute_name]

puts the control volumes (areas for triangles) in a vertex attribute
of the mesh object: **cv_volume**, or attribute_name when one is
given. It is made (VDOUBLE, scalar, nnodes; written by **`dump/avs`**)
if the mesh object does not have it; an existing attribute of that
name must be such a real vertex attribute (else an error, before
anything is written), and is overwritten. With **`check_only, on`** no
volumes are computed and the attribute is not set. Without
**`attribute_cv_volume`** the mesh object is not changed.

**`ccoef_area_min`**, **`ccoef_area_max`**, **`ccoef_ij_area_min`**,
**`ccoef_ij_area_max`**, **`ccoef_areabydist_min`**,
**`ccoef_areabydist_max`**, **`ccoef_ij_areabydist_min`**,
**`ccoef_ij_areabydist_max`**, **`ccoef_distance_min`**,
**`ccoef_distance_max`** [, attribute_name]

each puts one value per vertex in a vertex attribute of its own name
(or attribute_name), over the vertex's connections, leaving out the
zero-area ones. For the connection from vertex i to vertex j, A(ij) is
the area of the control volumes' shared face, d(ij) the length of the
edge, and A(ij)/d(ij) the geometric coefficient written in the .stor;
a negative A(ij) or A(ij)/d(ij) is a negative coefficient.

| keyword | each vertex gets |
|---|---|
| **`ccoef_area_min`**, **`ccoef_area_max`** | the smallest, the largest A(ij) |
| **`ccoef_ij_area_min`**, **`ccoef_ij_area_max`** | the vertex j at the other end of those two connections |
| **`ccoef_areabydist_min`**, **`ccoef_areabydist_max`** | the smallest, the largest A(ij)/d(ij) |
| **`ccoef_ij_areabydist_min`**, **`ccoef_ij_areabydist_max`** | the vertex j at the other end of those two connections |
| **`ccoef_distance_min`**, **`ccoef_distance_max`** | the shortest, the longest d(ij) |

The **`_ij_`** attributes are integers (VINT), the others reals
(VDOUBLE); each is scalar, one value per vertex, made if the mesh
object does not have it (an existing one must be of that kind, else
an error before anything is written) and written by **`dump/avs`**. A
vertex with no connection left gets 0. Where two connections tie, the
first found (the lowest j) is named. With **`check_only, on`** nothing
is computed and the attributes are not set.

**`ccoef_areabydist_min`** with **`ccoef_ij_areabydist_min`** gives
what matbld's attributes ccoef and ij\_ccoef gave (its most negative
coefficient and that connection's other vertex; **`dump/stor`** and
**`dump/fehm`** with **`matbld`** still make those). The vertices with
a negative coefficient:

```
pset / pneg / attribute / ccoef_areabydist_min / 1,0,0 / lt / 0.
```

### Mesh checks ###

Each of these stops the command and nothing is written:

- cells that are not all triangles or all tets;
- tets with zero or negative volume, triangles with zero area;
- a tet face shared by 3 or more tets; an edge in 3 or more triangles
  of one itetclr;
- two neighboring triangles of one itetclr oriented differently
  (each itetclr may be oriented either way, as long as it is one way);
- parent/child chains: a vertex with **itp1** = 41 (a parent, as
  **`settets/parents`** leaves an interface). Run **`resetpts/parent`**
  (the children become dudded vertices, in no cell), then
  **`rmpoint/compress`** (they are removed); split control volumes are
  not built yet.

An edge in triangles of two or more itetclr is a fracture
intersection edge: legal, counted, and its coefficients are the sum
of every fracture's. Tets of different itetclr sharing a face are a
material interface. A vertex in no cell gets a warning.

## EXAMPLES ##

```
dump / control_volume / mo_mesh / stor, mesh.stor
```
check mesh object `mo_mesh` and write `mesh.stor` with the defaults.

```
dump / control_volume / mo_mesh / stor, mesh.stor / attribute_cv_volume
dump / avs / mesh_cv.inp / mo_mesh
```
also put the control volumes in the vertex attribute **cv_volume**,
and write the mesh with it (`/ attribute_cv_volume, vol_vor /` names
it **vol_vor**).

```
dump / control_volume / mo_mesh / stor, mesh.stor / uge, mesh.uge / tough, MESH
```
three outputs from one command.

```
dump / control_volume / mo_mesh / check_only, on / report, mesh_check.txt
```
check the mesh only; keep the messages in `mesh_check.txt`; if the
mesh is not Delaunay, open `violations.vtu` in ParaView.

```
dump / control_volume / mo_mesh / stor, mesh.stor / zero_area, keep / compress, off
dump / stor / mesh_matbld / mo_mesh / ascii / none / matbld
```
one matrix entry per mesh edge, every value written, then the same
mesh through LaGriT's own matrix builder, also uncompressed (**none**),
to compare `mesh.stor` with `mesh_matbld.stor`.

```
dump / control_volume / mo_mesh / stor, mesh.stor / ccoef_areabydist_min / ccoef_ij_areabydist_min
pset / pneg / attribute / ccoef_areabydist_min / 1,0,0 / lt / 0.
dump / avs / mesh_ccoef.inp / mo_mesh
```
each vertex's most negative coefficient (or its smallest positive
one) and the vertex at the other end of that connection; the vertices
with a negative coefficient in the set pneg; the mesh with the two
attributes, to color by them in ParaView.

```
read / avs / full_mesh.inp / mo_dfn
dump / control_volume / mo_dfn / stor, out.stor / uge, out.uge / zero_area, keep
```
a fracture network from dfnWorks: each fracture is checked on its
own, intersection edges get both fractures' coefficients.

```
dump / control_volume / mo_mesh / stor, mesh.stor / cv, hybrid / alpha_file, alpha.vtu
```
no negative coefficient: the dual points of the cells that would
make one are moved toward their centroids, only as far as needed;
`alpha.vtu` shows which cells, and how far.

```
dump / control_volume / mo_mesh / stor, mesh_med.stor / cv, median
```
median control volumes, for a mesh that is not Delaunay.

```
settets / parents
resetpts / parent
rmpoint / compress
dump / control_volume / mo_mesh / stor, mesh.stor
```
a mesh with material interfaces made into parent/child chains: the
chains taken out first (the command refuses them).

```
dump / control_volume / mo_mesh / stor, mesh.stor / paraview, mesh_cv / paraview_subset, boundary
```
also the control volumes of the boundary vertices for ParaView: open
`mesh_cv.pvd`.

```
dump / control_volume / mo_mesh / cv, hybrid / outside_area, mesh_bnd.txt
```
only the boundary vector area of each boundary vertex, from the hybrid
control volumes.

```
dump / fehm / mesh / mo_mesh / cv_dual / hybrid
```
the FEHM files, with the .stor and `mesh_outside_hyb.area` from
cv_dual's hybrid (see [cv_dual OR matbld](DUMP3.md#cv_dual-or-matbld)).
