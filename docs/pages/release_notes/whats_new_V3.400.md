---
title: "What's new in LaGriT V3.4.0"
tags: release notes V3.4.0 whats new cv_dual connect poisson
header_image: control_volume.jpg
header_title: What's new in LaGriT V3.4.0
header_subtitle: Released November 20, 2026. An overview with examples; every change is in the release notes.
---

LaGriT V3.4.0 is the first release since V3.3.3 (January 2024). This page shows what a user will notice, with an example of each. The full list, with every pull request, is in the [V3.4.0 release notes](lagrit_release_notes_V3.400.md).

Some results change from V3.3.3: control-volume files, connect on some point sets, Poisson disk meshes and more digits in output files. Each change, and how to get the old behavior back, is under "Changes in behavior" in the release notes.

## Control volumes with cv_dual

The control volumes and coefficients that **dump / stor**, **fehm**, **pflotran** and **zone_outside** write now come from a new code, cv_dual. Before writing anything, it checks that the mesh is valid and Delaunay. The commands are unchanged, and the Voronoi numbers are the same to rounding:

```
dump / fehm / tet / mo_tet
```

To write the files exactly as V3.3.3 did, add the keyword **matbld**:

```
dump / fehm / tet / mo_tet / matbld
```

The new command **dump / control_volume** gives every cv_dual option: Voronoi, median or hybrid control volumes (hybrid has no negative coefficients), .stor, .uge and TOUGH MESH files, and vertex attributes. For example, this puts each vertex's control volume in an attribute, and its smallest coefficient A/d and the vertex at the other end of that connection in two more:

```
dump / control_volume / mo_tet / stor, tet.stor / cv, hybrid / &
       attribute_cv_volume / ccoef_areabydist_min / ccoef_ij_areabydist_min
```

**See the dual mesh in ParaView.** cv_dual can write the Voronoi (or median, or hybrid) control volumes themselves as a mesh: one polygon or polyhedron per vertex, with the Delaunay mesh beside it. Open `tet_dual.pvd` in ParaView:

```
dump / control_volume / mo_tet / stor, tet.stor / paraview, tet_dual
```

`paraview_faces, on` adds the dual face of every connection, and `paraview_subset` limits the cells to the boundary, the flagged ones, one material or a box. The picture at the top of this page is such a dual mesh, of a dfnWorks fracture network.

- [dump / control_volume](../docs/commands/dump/DUMP_CONTROL_VOLUME.md)
- [cv_dual OR matbld on dump / stor](../docs/commands/dump/DUMP3.md)
- [The cv_dual manual](../docs/cv_dual/cv_dual_manual.md), including the stand-alone program build/cv_dual
- [cv_dual compared with matbld](../docs/cv_dual/cv_dual_vs_matbld.md), on LaGriT's own decks

## Exact Delaunay with connect

In 2D, for points in a coordinate plane, connect now gives the exact Delaunay triangulation of the whole convex hull. In 3D it fills the convex hull too. **connect / exact** makes every 3D test exact and adds no points, so the result is exactly the Delaunay tetrahedralization of the points:

```
connect / exact
```

connect is also much faster on large point sets (256,000 random points: 1.5 s instead of 35 s in 2D, and 2.7 s instead of 14 s in 3D). Its keywords (**noadd**, **check_interface**, **old_order**, **exact**, **nohull**) can be given anywhere on the line.

- [CONNECT](../docs/commands/CONNECT1.md)
- [How connect works](../docs/connect_notes.md)

## Poisson disk sampling in 3D and graded meshes

**createpts / poisson_disk / 3d_box** fills a box with well-spaced vertices and connects them into tets:

```
createpts / poisson_disk / 3d_box / mo_pts / 0.05 / connect
```

With a control file that sets a variable h, an octree adapted to h replaces the uniform lattice. This makes meshes with a wide range of h practical: on one DFN case, 112 s and 0.21 GB instead of 1,152 s and 1.99 GB. The tutorial and examples/dfn_tets build tets graded away from a fracture network's surface.

- [createpts / poisson_disk](../docs/commands/createpts/createpts_poisson.md)
- [Tutorial: Tet Mesh Graded by a Surface's Edge Lengths](../tutorial/dfn_tet_grading/index.md)

## Mesh quality

**improve_tets** improves a mesh's worst tets by smoothing and flips, keeping the vertex set. **quality / minsine** measures each tet by the smallest sine of its dihedral angles, and with **y** stores it as a cell attribute:

```
improve_tets / mo_pts
quality / minsine / y
```

improve_tets flips tets, so the mesh it leaves may no longer be Delaunay. cv_dual then refuses Voronoi and hybrid control volumes, and **dump / stor / ... / median** still writes a file.

Three new vertex attributes hold the shortest, longest or mean length of the edges at each vertex, for any cell type:

```
cmo / addatt / mo_surf / edge_lengths_mean / h_surf
```

**compute / distance_field / ... / facet** gives the exact distance to a surface's triangles, not just to its nearest vertex. crush_thin_tets now measures each tet against its own size and respects a pset.

- [IMPROVE_TETS](../docs/commands/IMPROVE_TETS.md)
- [QUALITY](../docs/commands/QUALITY.md)
- [cmo / addatt](../docs/commands/cmo/cmo_addatt.md)
- [COMPUTE](../docs/commands/COMPUTE.md)

## Input and output

- **17 significant digits:** dump / avs (now avs3), .stor, .uge and .fehmn files write 17 digits, so a value read back is the value written.
- **UTM coordinates:** dump / stor and pflotran now compute volumes and coefficients correctly far from the origin. Before, they lost every digit there.
- **Whole file names:** dump / stor, fehm, pflotran and tecplot no longer cut a file name at 32 characters.
- **Longer input lines:** a command line can be up to 1024 characters. Before, characters past column 80 were dropped without a message.
- **cmo / printatt** prints attribute names in full, up to 32 characters.

## Fixes to know about

These gave wrong results without a message in V3.3.3. If a mesh or a file of yours depends on one of them, make it again with V3.4.0:

- **dump / stor or pflotran at UTM coordinates** wrote wrong volumes and coefficients.
- **dump / stor / ... / coefs** run twice in one session wrote a broken second file.
- **2D connect** made no triangles far from the origin, and left points out on stretched domains.
- **eltset inter and not** with three or more sets gave wrong sets.
- **crush_thin_tets** measured every tet against the wrong size and ignored the pset.
- **Dihedral and solid angles** of flat cells came out as their starting values.

Crashes fixed include 2D and 3D connect writing past or into freed memory, hextotet / 14 on prisms, recon with imt 0, read with a path in the file name, and bad AVS files. They are listed under "Fixes" in the release notes.

## Testing

The new test suite runs 43 suites, each with independent checks and a results page. GitHub runs them on Linux and macOS for every pull request:

```
cd test/suite
JOBS=6 sh run_all.sh
```

Then open `test/suite/summary.html`.
