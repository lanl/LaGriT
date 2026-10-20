---
title: TRIANGULATE
tags: triangulate
---

# TRIANGULATE

---------------

**`triangulate`** fills a polygon with triangle cells. The polygon is
the ordered list of vertices in the current 2D mesh object: vertex 1,
vertex 2, ..., last vertex, and back to vertex 1. The z coordinate is
ignored; the vertices are taken to lie in the xy plane.

The method is ear clipping. A polygon of n vertices gives n - 2 cells,
and no vertices are added.

Use [**`REFINE`**](REFINE.md), [**`SMOOTH`**](SMOOTH.md) and
[**`RECON`**](RECON.md) afterwards to reach the cell size and quality
you need (see [Better meshes](#better-meshes-refine-smooth-recon) below).


## SYNTAX

<pre>
<b>triangulate</b>

<b>triangulate</b> / <b>clockwise</b> or <b>counterclockwise</b>
</pre>

**`clockwise`** or **`counterclockwise`** (default **`clockwise`**) is
the order of the vertices, seen from above the xy plane (looking down the
z axis).


## WHAT IT DOES, STEP BY STEP

1. If the last vertex repeats the first exactly, it is dropped, with a
   `WARNING: first point is the same as last point`.
2. Fewer than 3 vertices is an `ERROR`; no cells are made.
3. The polygon is cut into cells by ear clipping, in the
   orientation given.
4. If that fails, a `WARNING` says so (for example
   `Counterclockwise did not work` / `Trying clockwise`) and the other
   orientation is tried. If both fail:
   `ERROR: Unable to triangulate input`.
5. Every cell is written **clockwise**, whichever keyword was given.
6. `triangulate` then always runs, by itself:

   ```
   filter / 1,0,0
   rmpoint / compress
   resetpts / itp
   ```

   `filter` merges vertices that sit at the same place (and points every
   cell that used a duplicate at the vertex that is kept); `rmpoint /
   compress` removes the merged duplicates; `resetpts / itp` sets the
   boundary flags (`itp`). For an ordinary polygon this changes nothing.
   For a polygon with holes (next section) it is what joins the two
   sides of each slit.

Geometric tests use one tolerance, 1e-12 times the diagonal of the
polygon's bounding box, so the result does not depend on the units or
on where the polygon sits. Features closer together than that cannot be
told apart; such a polygon gives `Unable to triangulate input` rather
than a bad mesh.

No check is made that the polygon is legal: if its edges cross each
other, the result is undefined.


## POLYGONS WITH HOLES

`triangulate` takes a single vertex list, so a hole is given as a
**keyhole**: the hole is joined to the outer boundary by a zero-width
**slit** that the list travels twice, once in and once out.

<img width="700" src="https://lanl.github.io/LaGriT/assets/images/triangulate_keyhole.png">

(a) The 10-vertex list of the example below: the outside
counterclockwise, the hole clockwise, joined by a slit travelled in
(1-2) and out (6-7). (b) The mesh `triangulate` writes: 8 vertices,
8 cells; the slit is one interior edge (dashed), and the outside and
the hole are two separate boundary loops.

How to build the vertex list (for `triangulate / counterclockwise`):

1. List the outer boundary **counterclockwise**.
2. At an outer vertex A, leave the outer boundary along the slit to a
   hole vertex B.
3. Go once around the hole **clockwise** (the opposite way to the
   outside), back to B.
4. List B again, then A again, and carry on along the outer boundary.

So A and B each appear **twice**, with exactly the same coordinates
(copy them; do not recompute them). For **`clockwise`**, reverse every
direction.

Rules:

* The slit must not cross or touch any other edge, of the outside or of
  any hole. The shortest such segment from an outer vertex to a hole vertex
  is usually a good choice.
* Each hole needs its own slit. A slit may also run from one hole to
  another, as long as every hole is connected to the outer boundary.
* Do not close the list by repeating vertex 1 at the end (it is dropped,
  step 1 above).

Example: a 4 x 4 square with a 1 x 1 square hole. Vertex 1 = vertex 7 is
the outer corner A = (0, 0); vertex 2 = vertex 6 is the hole corner
B = (1.5, 1.5).

```
10 0 0 0 0
1 0.0 0.0 0.0      A      outer corner, start of the slit
2 1.5 1.5 0.0      B      along the slit to the hole
3 1.5 2.5 0.0             around the hole, clockwise
4 2.5 2.5 0.0
5 2.5 1.5 0.0
6 1.5 1.5 0.0      B      back at the slit
7 0.0 0.0 0.0      A      back along the slit
8 4.0 0.0 0.0             the outside, counterclockwise
9 4.0 4.0 0.0
10 0.0 4.0 0.0
```

```
cmo / create / mo_tri / / / triplane
read / avs / hole_1.poly / mo_tri
triangulate / counterclockwise
```

What comes out:

* Ear clipping makes 10 - 2 = **8** cells from the 10 listed vertices.
* `filter` then finds the 2 duplicates (`FILTER:Dudding duplicate
  points: 2`) and `rmpoint / compress` removes them: **8 vertices**,
  8 cells.
* The slit's two copies, which ran in opposite directions, now join the
  same two vertices, so they are **one interior edge**, shared by two
  cells. The hole is a real hole: the mesh has two boundary loops
  (the outside and the hole), and V - E + F = 0, as for any ring.
  With h holes: n - 2 cells, n - 2h vertices, 1 + h boundary loops.

The slit stays in the mesh as an ordinary edge that the triangulation
was made to contain. `refine` splits it and `recon` may flip it like
any other edge, so after the steps in the next section it leaves no
trace.


## SEVERAL SEPARATE POLYGONS

Triangulate each polygon on its own mesh object, then merge them:

```
cmo / create / mo_a / / / triplane
read / avs / part_a.poly / mo_a
triangulate / counterclockwise
addmesh / merge / mo_all / mo_all / mo_a
cmo / delete / mo_a
# ... the same for each part ...
cmo / select / mo_all
resetpts / itp
```

(`addmesh / merge` creates `mo_all` the first time.) Set `itetclr` on
each part before merging to keep them apart.


## BETTER MESHES: REFINE, SMOOTH, RECON

`triangulate` adds no vertices, so its cells are usually long and thin.
This sequence gives an even, isotropic mesh. The final `recon / 1`
makes it fit for a control volume solver that uses the Voronoi cells
of the vertices (two point flux): it adds vertices on the boundary where
needed so that **no boundary cell faces the boundary with an angle
over 90 degrees** (each boundary cell's circumcentre lies inside
the domain). See M. Murphy, D. M. Mount and C. W. Gable, "A point-placement strategy
for conforming Delaunay tetrahedralization", International Journal of
Computational Geometry & Applications 11(6), 669-682, 2001
([PDF](https://lanl.github.io/LaGriT/assets/images/2000_ijcga_murphy_mount_gable.pdf)),
and Proceedings of the Eleventh Annual ACM-SIAM Symposium on Discrete
Algorithms (SODA), 67-74, 2000.

Refine in two passes, to twice the target edge length and then to the
target, smoothing after each; one pass straight to the target leaves
the long cells of the starting mesh visible.

```
define / EDGELEN1 / 0.50
define / EDGELEN2 / 0.25

triangulate / counterclockwise

refine / rivara / / / edge / 1,0,0 / EDGELEN1 / / / inclusive
smooth / position / esug / 1 0 0
recon / 0
smooth / position / esug / 1 0 0
recon / 0
smooth / position / esug / 1 0 0
recon / 0
refine / rivara / / / edge / 1,0,0 / EDGELEN2 / / / inclusive
smooth / position / esug / 1 0 0
recon / 0
smooth / position / esug / 1 0 0
recon / 0
smooth / position / esug / 1 0 0
recon / 1
resetpts / itp
quality
```

No `filter` or `rmpoint` is needed at the end: on a correct mesh they
would change nothing.

Measured (block letters "LaGriT", 7 polygons, one with a hole): after
`triangulate`, smallest angle 4.8 degrees, 24 of 40 cells under
20 degrees; after the sequence above, smallest angle 23.9 degrees, no
cell under 20 degrees, largest angle facing the boundary 89.85
degrees. A square with a square hole: 14.0 to 24.8 degrees; an annulus:
1.3 to 30.6 degrees.


## EXAMPLES

```
triangulate
triangulate / clockwise
```
Both triangulate a polygon listed clockwise.

```
cmo / create / mo_tri / / / triplane
read / avs / polygon.inp / mo_tri
triangulate / counterclockwise
refine / rivara / / / edge / 1,0,0 / 0.05 / / / inclusive
```
Triangulate a polygon listed counterclockwise, then refine to edges of
0.05 or less. The polygon file is an AVS file of vertices only (no cells),
in order around the polygon.


## HISTORY

October 2026: dense or nearly collinear boundaries
([issue 57](https://github.com/lanl/LaGriT/issues/57)) and a diagonal
through a boundary vertex
([issue 239](https://github.com/lanl/LaGriT/issues/239)) no longer give
`Unable to triangulate input`, zero-area cells or overlapping
cells; the tolerance became relative to the polygon's size; fewer
than 3 vertices became an error.

[Click here for demos](../demos/description_tri.md)
