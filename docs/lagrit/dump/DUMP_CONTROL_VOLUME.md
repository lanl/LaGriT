---
title: 'DUMP/CONTROL_VOLUME'
tags: dump control_volume del_vor voronoi stor uge tough2 delaunay
---

# DUMP/CONTROL_VOLUME #

------------------------

> **Draft for the LaGriT version of del_vor (phase 6 of 6).** This
> page describes the command as planned; the standalone program
> `del_vor` already does everything below. Kept in the del_vor
> repository (`docs/lagrit/dump/DUMP_CONTROL_VOLUME.md`) until the
> command is in LaGriT, then moved to `docs/pages/docs/commands/dump/`.

**`dump/control_volume`** checks a mesh object and computes its
control-volume geometry with del_vor: the control volume of each
vertex, and for each edge the area of its Voronoi face and its
length. It writes them as FEHM `.stor`, PFLOTRAN `.uge` or TOUGH2
`MESH` files, and keeps the volumes in the vertex attribute
**cv_volume**. It is the new path beside **`dump/stor`**, which is
unchanged (it uses **`matbld2d`** and **`matbld3d`**); both can run on
the same mesh to compare.

Before any geometry **`dump/control_volume`** checks the mesh (cell types, cells
of positive size, conformity, orientation) and tests it for the
Delaunay property, facet by facet. A mesh that fails is not written:
Voronoi coefficients of a mesh that is not Delaunay are wrong. The
facets that fail are written to a file for ParaView.

The mesh is all triangles (2D, or a fracture network in 3D: one
itetclr per fracture) or all tets.

## SYNTAX ##

<pre>
<b>dump</b> / <b>control_volume</b> / mo_name &
     / [<b>stor</b>, file_name] / [<b>uge</b>, file_name] / [<b>tough2</b>, file_name] &
     / [<b>zero_area</b>, <b>remove</b>|<b>keep</b>] / [<b>zero_tol</b>, value] &
     / [<b>compress</b>, <b>on</b>|<b>off</b>] / [<b>cv</b>, <b>voronoi</b>] &
     / [<b>delaunay_tol</b>, value] / [<b>delaunay_fatal</b>, value] &
     / [<b>violations</b>, file_name] &
     / [<b>check_only</b>, <b>on</b>|<b>off</b>] / [<b>report</b>, file_name]
</pre>

mo_name, the mesh object, is required; there is no default (**-def-**
is not taken). After it, each option is a keyword and its value,
written with a comma (or a space) between them; options are separated
by `/` and may come in any order. With no output keyword,
**`dump/control_volume`** writes `voronoi.stor`.

## DESCRIPTIONS ##

**`stor`**, file_name

writes the FEHM `.stor` file (ASCII), in the layout of **`dump/stor`**:
volumes, row pointers, column numbers, value pointers, diagonal
pointers and the values -A(ij)/d(ij).

**`uge`**, file_name

writes the PFLOTRAN explicit unstructured grid file: `CELLS` (one
line per vertex: id, x, y, z, volume) and `CONNECTIONS` (one line per
edge: i, j, the edge midpoint, area).

**`tough2`**, file_name

writes the TOUGH2 `MESH` file: `ELEME` (one record per vertex) and
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

**`cv`**, **`voronoi`**

the control volumes. **`voronoi`** is the only choice for now;
median and hybrid control volumes are planned.

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

### Mesh checks ###

Each of these stops the command and nothing is written:

- cells that are not all triangles or all tets;
- tets with zero or negative volume, triangles with zero area;
- a tet face shared by 3 or more tets; an edge in 3 or more triangles
  of one itetclr;
- two neighboring triangles of one itetclr oriented differently
  (each itetclr may be oriented either way, as long as it is one way).

An edge in triangles of two or more itetclr is a fracture
intersection edge: legal, counted, and its coefficients are the sum
of every fracture's. Tets of different itetclr sharing a face are a
material interface. A vertex in no cell gets a warning.

## EXAMPLES ##

```
dump / control_volume / mo_mesh / stor, mesh.stor
```
check mesh object `mo_mesh` and write `mesh.stor` with the defaults;
the volumes are in **cv_volume**.

```
dump / control_volume / mo_mesh / stor, mesh.stor / uge, mesh.uge / tough2, MESH
```
three outputs from one command.

```
dump / control_volume / mo_mesh / check_only, on / report, mesh_check.txt
```
check the mesh only; keep the messages in `mesh_check.txt`; if the
mesh is not Delaunay, open `violations.vtu` in ParaView.

```
dump / control_volume / mo_mesh / stor, mesh.stor / zero_area, keep / compress, off
dump / stor / mesh_matbld / mo_mesh / ascii / none
```
one matrix entry per mesh edge, every value written, then the same
mesh through LaGriT's own matrix builder, also uncompressed (**none**),
to compare `mesh.stor` with `mesh_matbld.stor`.

```
read / avs / full_mesh.inp / mo_dfn
dump / control_volume / mo_dfn / stor, out.stor / uge, out.uge / zero_area, keep
```
a fracture network from dfnWorks: each fracture is checked on its
own, intersection edges get both fractures' coefficients.

## OPEN QUESTIONS (draft only) ##

Settled (Carl, Nov 8, 2026): the command is **`dump/control_volume`**
(**`dump/stor`** unchanged); the mesh object is required; the volume
attribute is **cv_volume**; the face neighbors (**jtet**) are computed
by del_vor, not taken from LaGriT.

Still open, for phase 6:

1. Is **cv_volume** made on every run, or only when asked for?
2. Parent/child meshes (**settets/parents**): merge children through
   their parent (**isn1**) or keep them apart.
