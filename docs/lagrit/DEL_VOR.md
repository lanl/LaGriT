---
title: DEL_VOR
tags: del_vor voronoi stor uge tough2 delaunay control volume
---

# DEL_VOR #

------------------------

> **Draft for the LaGriT version of del_vor (phase 6 of 6).** This
> page describes the command as planned; the standalone program
> `del_vor` already does everything below. Kept in the del_vor
> repository (`docs/lagrit/DEL_VOR.md`) until the command is in
> LaGriT, then moved to `docs/pages/docs/commands/`.

**`del_vor`** checks the current mesh object and computes its Voronoi
control-volume geometry: the control volume of each vertex, and for
each edge the area of its Voronoi face and its length. It writes
them as FEHM `.stor`, PFLOTRAN `.uge` or TOUGH2 `MESH` files. It is
meant to replace **`matbld2d`** and **`matbld3d`** (used by
**`dump/stor`**); both can run on the same mesh to compare.

Before any geometry **`del_vor`** checks the mesh (cell types, cells
of positive size, conformity, orientation) and tests it for the
Delaunay property, facet by facet. A mesh that fails is not written:
Voronoi coefficients of a mesh that is not Delaunay are wrong. The
facets that fail are written to a file for ParaView.

The mesh is all triangles (2D, or a fracture network in 3D: one
itetclr per fracture) or all tets.

## SYNTAX ##

<pre>
<b>del_vor</b> / [<b>stor</b>, file_name] / [<b>uge</b>, file_name] / [<b>tough2</b>, file_name] &
        / [<b>zero_area</b>, <b>remove</b>|<b>keep</b>] / [<b>zero_tol</b>, value] &
        / [<b>compress</b>, <b>on</b>|<b>off</b>] / [<b>cv</b>, <b>voronoi</b>] &
        / [<b>delaunay_tol</b>, value] / [<b>violations</b>, file_name] &
        / [<b>check_only</b>, <b>on</b>|<b>off</b>] / [<b>report</b>, file_name]
</pre>

Each option is a keyword and its value, written with a comma (or a
space) between them; options are separated by `/` and may come in any
order. With no output keyword, **`del_vor`** writes `voronoi.stor`.

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
a tet face). delta/L < -value is a violation and stops the command;
\|delta/L\| <= value is a tie (co-circular or co-spherical) and is
accepted. Default 1e-10: true ties compute to about 1e-16, real
violations are many orders of magnitude larger. A larger value
accepts meshes that are only close to Delaunay.

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

a copy of everything **`del_vor`** prints, in a file.

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
del_vor / stor, mesh.stor
```
check the current mesh object and write `mesh.stor` with the defaults.

```
del_vor / stor, mesh.stor / uge, mesh.uge / tough2, MESH
```
three outputs from one command.

```
del_vor / check_only, on / report, mesh_check.txt
```
check the mesh only; keep the messages in `mesh_check.txt`; if the
mesh is not Delaunay, open `violations.vtu` in ParaView.

```
del_vor / stor, mesh.stor / zero_area, keep / compress, off
dump / stor / mesh_matbld / -def- / ascii / none
```
one matrix entry per mesh edge, every value written, then the same
mesh through LaGriT's own matrix builder, also uncompressed (**none**),
to compare `mesh.stor` with `mesh_matbld.stor`.

```
del_vor / stor, dfn.stor / delaunay_tol, 1e-8
```
a fracture network from dfnWorks: each fracture is checked on its
own, intersection edges get both fractures' coefficients.

## OPEN QUESTIONS (draft only) ##

To settle with the LaGriT developers before phase 6:

1. The command as above, an option of **`dump/stor`** (the design's
   plan: the new path behind the existing command, **`matbld`**
   staying the default for a while), or both.
2. The mesh: always the current mesh object, or a **`cmo`**, name
   option.
3. The control volumes kept in a vertex attribute (the design returns
   them to LaGriT): its name, and whether it is made by default.
4. Whether LaGriT's own **jtet** is used as is, or rebuilt by
   **`del_vor`** (the standalone program builds it from the cells).
