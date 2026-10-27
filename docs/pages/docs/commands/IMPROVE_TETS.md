---
title: improve_tets
tags: improve_tets, tet, quality, sliver, smooth, flip
---

# IMPROVE_TETS

--------------------

Improves the quality of the worst tets of a tet mesh by moving vertices and reconnecting tets, without adding or removing vertices, so the spatial variation of the mesh resolution is kept. It removes slivers, caps, needles and wedges, for example from a Delaunay mesh connected from vertices generated using 3D Poisson disk (eg. createpts / poisson_disk / 3d_box ...).

## SYNTAX

<pre>
<b>improve_tets</b> / cmo_name / [<b>pset,get,</b> psetname] / [keyword / value] ...
</pre>

`cmo_name` is the tet mesh object (**-def-** for the current one).

**pset,get,** `psetname` limits the work to part of the mesh: only vertices in the pset move, and only tets whose four vertices are all in the pset are reconnected.

| keyword | value | default | meaning |
| :------ | :---- | :------ | :------ |
| **target** | real, 0 &ndash; 0.95 | 0.65 | final target quality (min sine); tets below it are worked on |
| **passes** | integer &ge; 1 | 40 | maximum number of passes (smoothing then flips), all stages |
| **maxmove** | real &gt; 0 | 1.0 | largest distance a vertex may move from where it started, in units of its local spacing (mean length of its edges) |
| **obtuse** | real, 0 &ndash; 1 | 0.75 | weight on the sine of an obtuse dihedral angle (1 = plain min sine; smaller penalizes large angles more) |
| **maxring** | integer &ge; 3 | 7 | largest number of tets around an edge for edge removal |
| **slide** | **on** / **off** | on | boundary and interface vertices slide within their plane or along their line (off: they stay fixed) |
| **smooth** | **on** / **off** | on | vertex smoothing |
| **flip** | **on** / **off** | on | reconnection (edge and face removal) |
| **multiface** | **on** / **off** | on | multi-face removal (off: single-face 2-3 flips only) |
| **global** | **on** / **off** | on | one opening flip pass over all tets before the staged passes |
| **planetol** | real | 1.e-6 | tolerance (sine of the angle) for boundary faces to count as one plane |
| **progress** | **on** / **off** | on | progress lines while the command runs (off: only the final report) |

## DESCRIPTION

**Quality.** The quality of a tet is the smallest sine of its six dihedral angles (the same measure as **[quality](QUALITY.md) / minsine**). A small sine means an angle near 0&deg; or near 180&deg;, so every kind of poorly shaped tet scores low: slivers, caps, needles and wedges. A regular tet has 0.943; 0.17 means the worst angle is 10&deg; or 170&deg;. By default the sine of an obtuse angle is multiplied by **obtuse** = 0.75, which favours removing large angles; large angles matter for accuracy (a-priori error estimates for elliptic problems grow like 1/sin of the angle).

**Method.** The strategy follows Klingner and Shewchuk, "Aggressive Tetrahedral Mesh Improvement" (Proceedings of the 16th International Meshing Roundtable, 2007, pp. 3&ndash;23), restricted to the operations that keep the vertex set:

- *Smoothing.* Each vertex of a poor tet is moved to raise the smallest quality among the tets around it (non-smooth optimisation: the vertex moves along the direction that improves all of the worst tets at once, with a line search). A vertex on the boundary, or on an interface between materials (faces between tets of different itetclr), slides within its plane if all its boundary faces lie in one plane, along its line if they lie in two planes, and stays fixed otherwise (corners, curved surfaces). A vertex never moves further than **maxmove** times its local spacing from where it started.
- *Edge removal.* The tets around an interior edge (a ring of 3 to **maxring** tets) are replaced by the best triangulation of the ring, found by dynamic programming; rings of 3 and 4 are the 3-2 and 4-4 flips.
- *Multi-face removal.* The faces sandwiched between two vertices are removed and the edge between those vertices is made; the best set of faces is found by dynamic programming over the patch; one face is the 2-3 flip.
- *Acceptance.* A move or a reconnection is made only if the worst tet it touches gets better. Boundary faces and interface faces are never changed, tets of different itetclr are never mixed, and the total volume of every reconnection is checked.
- *Schedule.* One flip pass over all tets (**global**), then passes of smoothing and flips over the tets below a target that is raised in stages (those of 0.45, 0.55 and 0.65 below **target**, then **target**), each stage ending when a pass no longer improves the worst tet or the number of tets below the target.

Because no vertex is added or removed and vertices move by a fraction of the local spacing, the resolution of the mesh is kept: on graded 3D Poisson disk meshes the mean edge length at each place stays within a few percent of the input (median), also in the finest region.

**Progress.** While it runs, the command prints what it is doing: the size of the mesh and the starting quality, the opening flip pass, then one line per pass with the target of that pass, the worst quality, the number of tets still below the target, and the vertex moves and flips made in the pass, with the time since the start. Inside a long pass a line like `... pass 14, smoothing, 68% done` appears at most every 15 seconds. For example, on 3D Poisson disk vertices with h = 0.0145 (199,524 vertices, 1,222,701 tets; about 2.5 minutes on one core):

```
improve_tets: 199524 vertices, 1222701 tets
  worst quality 0.0021; 429540 tets below target 0.65
  opening flip pass over all tets ...
    ... opening flips,  65% done     19.2 s
  opening flip pass done: 175627 flips, 29.5 s
  pass  target   worst     below     moves     flips    time (s)
     1    0.45  0.4005        40     12504        55        32.1
     2    0.45  0.4365         4       135         1        32.2
  ...
    ... pass  14, smoothing,  68% done     84.5 s
    14    0.65  0.5354    141955    187623     10771        91.6
  ...
    23    0.65  0.5559      8621     10203        41       158.9
improve_tets: finished, 23 passes, 158.9 s
```

The target rises in stages, so the number below the target jumps when a new stage starts (pass 14 above) and then falls. **progress / off** leaves only the final report.

**Output.** The command reports the worst and mean quality before and after, the smallest and largest dihedral angle before and after, the number of tets below the target, the number of tets, passes, vertex moves, edge and face removals, and how many vertices were free, in a plane, on a line or fixed. The number of tets usually changes (flips do not keep it); the vertices keep their numbers. **jtet** is rebuilt.

It then reports what happened to the resolution. For each vertex, *h* is the mean length of the edges at that vertex in the input mesh (its local spacing; small where the mesh is fine, large where it is coarse). The report gives the distance each vertex moved divided by its *h* (median and largest), and the mean edge length at each vertex after the command divided by *h* (at 1%, median and 99% of the vertices). For example, on a uniform 3D Poisson disk mesh with h = 0.1:

```
  resolution (h = mean length of the edges at a vertex, before):
    vertex move / h:  median  0.154   largest  0.729
    h after / h before:  1%  0.734   median  0.983   99%  1.160
```

A median near 1 and a narrow 1% &ndash; 99% range mean the spacing, and with it the resolution of a graded mesh, is kept. The largest move can never exceed **maxmove**.

A value that is not a number (for a numeric keyword) or not **on** / **off** is refused with a message, and the mesh is left unchanged.

**Limits.** Only tet meshes are accepted. Vertices with a parent/child (doubled) interface chain (isn1 not 0) are held fixed. Cell attributes other than itetclr are not carried over to the new tets. The mesh must have no inverted or flat tets.

The command leaves every other LaGriT command unchanged; its code is in improve_tets.f and improve_tets_core.cpp.

## EXAMPLES

```
define / POI_XMIN / 0.0
define / POI_YMIN / 0.0
define / POI_ZMIN / 0.0
define / POI_XMAX / 1.0
define / POI_YMAX / 1.0
define / POI_ZMAX / 1.0
define / IF_POI_EDGE_BUFFER / 0
define / POI_EDGE_BUFFER / 1.0
createpts / poisson_disk / 3d_box / mo_pts / 0.1 / connect
improve_tets / mo_pts
quality / minsine / y
```
Generate 3D Poisson disk vertices in the unit cube with spacing h = 0.1, connect them (Delaunay), and improve the tets with the defaults. The dihedral angles go from 1.75&ndash;177.09&deg; to about 37&ndash;126&deg;.

```
improve_tets / mo_pts / slide / off
```
The same, with the boundary vertices held fixed (about 29&ndash;134&deg;).

```
pset / p_low / attribute / zic / 1 0 0 / lt / 0.5
improve_tets / mo_pts / pset,get,p_low / target / 0.7
```
Work only below z = 0.5, with a higher target. The report covers the whole mesh, so a poor tet outside the pset still shows as the smallest or largest angle.

```
improve_tets / mo_pts / maxmove / 0.3 / obtuse / 1.0
```
Keep vertices within 0.3 of their local spacing and use the plain min sine.
