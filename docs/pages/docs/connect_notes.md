---
title: Notes on the connect algorithm
tags: connect, Delaunay, algorithm
---

# Notes on the connect algorithm

**connect** builds a Delaunay triangulation (2D) or tetrahedralization (3D) of the vertices of a mesh object: no vertex lies inside the circumcircle of any triangle, or the circumsphere of any tetrahedron. This page describes how it does that, how it decides whether a vertex is inside a circumsphere, and where it can fall short. The command and its options are on the [connect](commands/CONNECT1.md) page.

Changes since LaGriT V3.3.4 are marked *(after V3.3.4)*. They are listed in the [V3.3.4 release notes](../release_notes/lagrit_release_notes_V3.340.md).

## Which method is used

| Mesh object | Inside and orientation tests | Afterwards |
|---|---|---|
| 3D (tet) | tolerance tests, below | hull filled *(after V3.3.4)* |
| 3D, **connect / exact** *(after V3.3.4)* | **exact** | no points added; hull filled: exactly the Delaunay tetrahedralization of the convex hull |
| 2D (triplane), vertices in a coordinate plane (every z the same, or every y, or every x) | **exact** *(after V3.3.4)* | hull filled and edges flipped: exactly the Delaunay triangulation of the convex hull |
| 2D, vertices in a tilted plane | tolerance tests, below | **recon / 0** |

## The steps

### 1. The enclosing triangle or tetrahedron

connect first makes a triangle (2D) or tetrahedron (3D) that contains all the vertices, well outside their bounding box. Every vertex is then inserted into this one cell, which is removed at the end (step 6).

*(after V3.3.4)* In 2D the third vertex of the enclosing triangle is placed 0.005 of the box size from the centre of the vertices. Before, it was placed at 1.01 times the centre coordinate, which for vertices far from the origin (UTM coordinates, say) lay outside them, and connect made no triangles. Meshes whose box contains the origin are unchanged.

The coordinates of the enclosing cell can still be given in the command, but this is deprecated *(after V3.3.4)*: connect gives a warning, and the option will be removed.

### 2. The insertion order

The vertices are inserted one at a time. The search for the cell that contains the next vertex starts from the cell made last, so the time depends on how far apart consecutive vertices are.

*(after V3.3.4)* More than 100 vertices are inserted in a Biased Randomized Insertion Order (BRIO; Amenta, Choi and Rote 2003; src/lg_brio_order.c):

- the vertices are split at random into rounds that double in size;
- each round is sorted along a Morton (Z-order) curve: in 2D on the two coordinates with the largest spread, in 3D on all three;
- the rounds are inserted smallest first.

Consecutive vertices are then close together, and the random rounds keep the good expected behaviour of a random order. connect's time grows about linearly with the number of vertices, whatever order they are given in. For 256,000 random vertices, 2D is about 23 times faster and 3D about 5 times faster than V3.3.4. The random choices use a fixed seed, so a run is reproducible.

**connect / old_order** keeps the order of V3.3.4 and earlier, a fixed prime stride through the vertex list. The order changes the mesh only where vertices are exactly cocircular or cospherical, as on a regular grid, where either choice is Delaunay.

### 3. Inserting a vertex (Bowyer-Watson)

To insert a vertex:

1. Walk from the last cell made to a cell whose circumsphere (circumcircle in 2D) contains the vertex.
2. Grow the *cavity*: look at the face neighbours of each cell in it, and add those whose circumspheres also contain the vertex.
3. Remove the cells of the cavity, and connect the vertex to each face (edge in 2D) on the cavity's boundary.

The new cells must pass the volume tests below; if they don't, the cavity is restored and the vertex goes into a fail list.

### 4. Failed vertices and later passes

Vertices in the fail list are tried again in later passes, with a looser inside test each time (see [the tolerance tests](#the-tolerance-tests)). At the end, every vertex that is in no cell is *dudded* (its itp is set to dudded, 21), and connect lists it:

```
 Dudding      7181 points that have no associated tetrahedra.
 The mesh is complete but could not include all points.
```

A vertex ends up in no cell for one of these reasons:

- **Coincident or nearly coincident vertices.** The second of two vertices at the same place, or closer than the volume epsilon allows (in a unit box, 1e-13 apart is dudded, 1e-7 is not), would make cells of zero or tiny volume, so it fails the [volume tests](#the-volume-tests) in every pass. This is the commonest reason. Remove them first with **filter / 1,0,0** and **rmpoint / compress**.
- **Cells too small for the volume epsilon.** epsilonv is set from the size of the whole box, not from the local spacing. Where the spacing is very fine compared with the box (strong grading, a long thin domain, or one stray vertex far from the others, which enlarges the box), correct cells can be smaller than epsilonv, and their vertices fail. **setsize** before **setpts** sets the epsilons from the geometry instead.
- **Degenerate configurations in 3D and tilted 2D.** A vertex nearly on a face or an edge of an existing cell, or nearly on the circumsphere of several cells, can make a new cell almost flat, or the search for a containing cell can give up after too many steps (20, 100, 500 and 1000 in passes 1 to 4). Usually this is the coincident-vertex case at a larger distance.
- **Lost with the enclosing cell.** A vertex on a nearly flat part of the hull whose only cells reach the enclosing cell (step 6) is left with none. In 2D in a coordinate plane the hull fill puts such vertices back *(after V3.3.4)*; before, a 1000 x 1 grid of 961 vertices lost 37.
- **Not selected.** With a point range or a pset (**connect / delaunay / 1,60,1**, **connect / pset,get,name**), the vertices left out are not connected, and they are dudded too.

Only vertices with itp below 20 are connected. Merged (20) and dudded (21) vertices and parent vertices (41; their child vertices on the interface are connected instead) are left out and not counted.

### 5. Points added by connect

Unless **noadd** is given, connect adds vertices in two places.

**Material interfaces.** When the mesh object has geometry (**surface**, **region**, **mregion**) and the vertices several materials, connect finds edges whose two ends are in different materials. It adds a vertex where each such edge crosses the interface, inserts these vertices as in step 3, and repeats until no such edge is left or the number of passes reaches its limit. **check_interface** (3D) also checks edges between two vertices on an interface. The added vertices keep the interface in the mesh, but they can land very close to existing vertices, so the cells near the interface can be thin.

**Small cells.** connect adds a vertex at the centre of each tetrahedron (triangle) whose volume (area) is very small.

**noadd** turns off both. *(after V3.3.4)* **noadd**, **check_interface** and **old_order** may be anywhere in the command, in 2D and 3D. Before, in 3D they were recognised only as the last token, and in 2D **noadd** only as token 18.

### 6. Removing the enclosing cell

The vertices of the enclosing cell and the cells attached to them are removed.

A cell on the convex hull whose circumsphere reaches a vertex of the enclosing cell is removed along with it. This happens for thin cells along nearly flat parts of the hull: three boundary vertices (0,0), (0.5,0.05), (1,0), for example, have a very large circumcircle. The triangle or tetrahedron they would form is lost, and the mesh has a notch where the convex hull should be.

*(after V3.3.4)* In 2D, for vertices in a coordinate plane, connect then fills the hull (src/lg_connect2d_hull.c):

- It walks the outer boundary and adds a triangle at every inward corner.
- It flips edges with the exact in-circle test until every interior edge is Delaunay.
- A corner flatter than 1e-6 (the triangle's height less than 1e-6 of its base) is left open. That is round-off in vertices meant to lie on a straight side, and filling it would add a sliver.
- Edges between triangles of different colour, or along an interface, are not flipped.

connect reports what it did:

```
 connect: added        16 triangles to reach the convex hull
 connect: flipped       6 edges to make the triangulation Delaunay
```

The result is exactly the Delaunay triangulation of the convex hull, so connect no longer runs **recon / 0** in this case. For vertices in a tilted plane, connect still runs **recon / 0** afterwards.

*(after V3.3.4)* In 3D connect then fills the hull too (src/lg_connect3d_hull.c). The tets kept are Delaunay tets of the vertices, so the missing ones are the Delaunay tets of the pockets between the mesh and the convex hull; they are found by gift wrapping. For each boundary face next to a reflex edge (and each face added after it), the tet on its outer side is the face plus the boundary vertex, strictly outside its plane, whose sphere through the face holds no other such vertex (exact in-sphere tests). A face with no vertex outside its plane is a facet of the convex hull. A tet is not added when it is flatter than 1e-6 (height over the face less than 1e-6 of its longest edge: round-off), when the choice is a tie, or when its vertices that are not interface points have different materials. connect reports the tets added and the pockets left open. Without the fill, 1.3% of the hull's volume was missing for random vertices and 7.7% for a 100 x 1 x 1 box (test/suite/test_connect_hull); **connect / nohull** leaves it out.

## The inside test

### Exact tests (2D, vertices in a coordinate plane; after V3.3.4)

The in-circle and orientation tests are done by src/lg_predicates2d.c:

- Each determinant is computed in double precision, after translating the vertices so that one of them is at the origin.
- When the result is larger than the bound on its rounding error (Shewchuk 1997), its sign is certain.
- Otherwise it is computed again in double-double arithmetic, about 106 bits.
- If that cannot decide either (an exact tie, or a near-tie below about 1e-28 of the determinant's size), it is computed exactly in integer arithmetic (src/lg_exact_int.h): every coordinate difference is an integer in units of the smallest last bit among them, and the determinant is formed with 64 x 64 -> 128-bit products, or with multi-word integers when the differences are too long for that.

The answer does not depend on any tolerance, on the size of the box, or on the distance from the origin. A vertex exactly on a circumcircle counts as outside, so on a regular grid either diagonal of a square may be chosen; both are Delaunay. Only vertices that fail the volume tests go to later passes.

### Exact tests in 3D (connect / exact; after V3.3.4)

With **exact**, 3D connect first uses the quick test it already has (the distance to each tetrahedron's stored circumcentre) with a bound on that test's rounding error; only when the result is within the bound does it call src/lg_predicates3d.c. There, as in 2D, the test is done in double precision with Shewchuk's error bounds; if that cannot decide it is done exactly in integers when the coordinate differences are exact in double (points close together: the usual case; small 64-bit integers for a lattice with spacing 10 or 0.5, at any offset, otherwise 128-bit products), and otherwise in double-double arithmetic followed, if that cannot decide either, by multi-word integers. The bound on the quick test is stored with each tetrahedron when it is made, from the vertex farthest from the computed circumcentre (for a thin tetrahedron the computed centre is inaccurate, and the bound must not come from an underestimate of the radius). A vertex exactly on a circumsphere counts as outside in the first pass; a vertex that then fails (cospherical and coplanar at once) is tried again in later passes with ties counted as inside. A new tetrahedron below the volume epsilon is accepted when its exact orientation agrees with the largest new tetrahedron: near-cospherical vertices have Delaunay tetrahedra that are thin but valid. No points are added in small tetrahedra.

The result is the Delaunay tetrahedralization of the vertices. On grids jittered by 1e-12 to 1e-6 (test/suite/test_connect_exact), plain connect leaves hundreds to thousands of faces not Delaunay, and at 1e-9 adds points and leaves vertices out; **connect / exact** adds and loses none and is exactly Delaunay. Delaunay does not mean well shaped: vertices that are meant to be coplanar and cospherical but are off by round-off (the **createpts / rtp** sphere) give tetrahedra that are flat to round-off, valid in exact arithmetic but of zero or negative volume in floating point. Use plain connect for such input.

### The tolerance tests

In 3D, and in 2D for a tilted plane, a vertex is inside a cell's circumsphere when its squared distance from the circumcentre is less than the circumradius squared, by more than a tolerance. The circumcentre is computed with the cell translated so that one of its vertices is at the origin, which avoids losing precision when the coordinates are large.

The tolerance is an area:

- 3D: **smalarea** = *small* x (boxsize_x x boxsize_y x boxsize_z)^(2/3)
- 2D: **smalarea** = 2 x *small* x max(boxsize)^2

Here boxsize is the extent of the vertices in each direction, and *small* = epsilonr x 100,000 x circumsphere_factor (epsilonr is the mesh object's relative epsilon; circumsphere_factor is 1 unless set). For a unit cube, **smalarea** is about 3e-10. It grows with the square of the box size, but not with the spacing of the vertices, so on a long thin domain many tests at the short spacing look like ties.

Each pass loosens the test:

| Pass | Tolerance |
|---|---|
| 1 | 100 x smalarea |
| 2 | smalarea |
| 3 | smalarea / 100 |
| 4 | 0 |
| 5 and later | -100 x smalarea (a vertex slightly outside counts as inside) |

Because the tolerance is absolute, near-ties are decided by it rather than by geometry. Vertices nearly on a common sphere may give a tetrahedralization that is Delaunay only up to the tolerance: on a grid jittered by 1e-9, about 3,000 faces are not exactly Delaunay.

### The volume tests

As a cavity is filled, the new cells must pass two tests:

1. Every new cell has a volume greater than **epsilonv**, the mesh object's volume epsilon: (boxsize_x x boxsize_y x boxsize_z) x epsilonr x 1000. A cell attached to the enclosing cell may be as small as epsilonv x 1e-10.
2. The volumes of the new cells add up to the volume of the cavity, to a relative error of epsilonr x 1e8.

If either fails, the vertex goes into the fail list. To base the epsilons on the geometry, issue **setsize** before **setpts**.

### circumsphere_factor

To make the inside test tighter or looser, add the mesh object attribute **circumsphere_factor**; it multiplies *small*:

```
cmo / addatt / mo / circumsphere_factor / REAL / scalar / scalar
cmo / setatt / mo / circumsphere_factor / 1,0,0 / .125
```

It is needed only in extreme cases: vertices with a very high aspect ratio, warnings about circumsphere problems, or vertices that cannot be connected. It does not affect the exact 2D tests.

## Limitations

- **Non-convex domains.** connect triangulates the convex hull of the vertices. Cells join vertices across a non-convex boundary and must be removed afterwards; the [connect / noadd demo](demos/description2_connect.md) tags them with **interpolate / map** from a hex mesh and removes them.
- **Coincident vertices** are not filtered; they are dudded (step 4). Remove them first with **filter / 1,0,0** and **rmpoint / compress**.
- **3D near-ties** are decided by the tolerance (see above), not exactly, unless **connect / exact** is given.
- **3D hull:** pockets flatter than 1e-6 of their size are left open (step 6); **connect / nohull** leaves every pocket open, as before.
- **Large coordinates in 3D.** For better precision, move the vertices close to the origin before connect, and back afterwards. **trans / zero** and **trans / original** do this without having to know the coordinates:

```
trans / 1,0,0 / zero
connect
trans / 1,0,0 / original
```

  Or give the offset yourself:

```
trans / 1,0,0 / XBIG,YBIG,ZBIG / 0. 0. 0.
connect
trans / 1,0,0 / 0. 0. 0. / XBIG,YBIG,ZBIG
```

  In 2D in a coordinate plane the exact tests do not need this *(after V3.3.4)*.

## Tests

- test/suite/test_connect_2d: every 2D mesh checked with exact rational arithmetic for being Delaunay and covering the convex hull; vertices far from the origin, in the yz and xz planes, stretched, graded and cocircular.
- test/suite/test_connect_options: the options in 2D and 3D.
- test/suite/test_connect_timing: time against the number of vertices, for four vertex orders, 2D and 3D.

## References

- A. Bowyer, "Computing Dirichlet tessellations", The Computer Journal 24 (1981); D. F. Watson, "Computing the n-dimensional Delaunay tessellation with application to Voronoi polytopes", same issue.
- N. Amenta, S. Choi, G. Rote, "Incremental constructions con BRIO", Symposium on Computational Geometry 2003.
- J. R. Shewchuk, "Adaptive precision floating-point arithmetic and fast robust geometric predicates", Discrete & Computational Geometry 18 (1997).
