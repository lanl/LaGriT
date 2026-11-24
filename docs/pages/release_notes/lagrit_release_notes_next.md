LaGriT Release Notes: changes since V3.4.0
====================

These changes are on master and not yet in a release. The file is renamed
for the next release when its version number is chosen.

Previous release  LaGriT V3.4.0  Nov 20, 2026

### New capabilities

- **compute / medial_axis:** for every vertex of a line mesh (2D) or a
  triangle surface (3D), the largest ball tangent there that holds no
  part of the surface. The ball centers become a new mesh object of
  medial points (the medial axis), twice the radius becomes a thickness
  attribute on the surface, and balls on the branches of wide corners
  are flagged so they can be removed. See [COMPUTE](../docs/commands/COMPUTE.md).

- **dump / control_volume / ... / exclude, imt [imt ...]:** the vertices
  of those materials (the ghost generators of a conforming Voronoi mesh)
  are left out of the .stor, .uge and TOUGH files; the control volumes
  and coefficients are computed on the whole mesh first, so the model's
  boundary vertices keep their conforming volumes.

- **dump / fehm / ... / exclude, imt [imt ...]** (and **dump / stor**):
  the complete FEHM file set for a conforming Voronoi mesh without its
  ghost generators, in one command. The .stor as dump / control_volume
  exclude writes it; the .fehmn and the material, interface and
  multi-material zones for the kept vertices, numbered as in the .stor;
  the outside zones and areas from the faces the model shares with the
  ghosts (each face to the side its normal points to most; each side
  adds up to its area).

- **exclude checks** (dump / control_volume, dump / fehm, dump / stor,
  cv_dual -exclude): nothing is written if a kept vertex is on the
  outside of the mesh (the excluded materials do not surround the model;
  `exclude_force` writes anyway); the faces shared with the excluded
  vertices are listed as pieces, each with its area, and a piece that is
  not closed gives a WARNING (a planned hole with its own ghosts: two
  closed pieces). The kept volume is reported.

### Fixes

- **cmo / addatt / edge_lengths_min, _max, _mean on a mesh object with
  no nodes or no cells:** an ERROR and LaGriT goes on; it crashed
  (segmentation fault). The same for edge_connections and
  node_num_diff, which share the code.

- **dump / control_volume / paraview (and cv_dual -paraview), 2D:** a
  polygon's `material` in `root_cells.vtu` is now its vertex's imt1, as
  for polyhedra and as in the .stor and zone files; it was the itetclr
  of one triangle around the vertex, wrong on a material interface
  (81 of 1124 cells in one conforming mesh with a lens) and 0 where
  itetclr was not set. The triangles' color is now the new cell field
  `itetclr`: in a fracture network, color the polygons by `itetclr` to
  see the fractures (a vertex on an intersection has one polygon per
  fracture, each with that vertex's one imt1).

- **dump / control_volume / paraview (and cv_dual -paraview), 3D: a
  crash** (segmentation fault) at a vertex with more than 682 tets
  around it, as at the center of seeds on a sphere (a lens in a
  conforming Voronoi mesh). The buffers of one cell had a fixed size;
  they are now sized from the mesh, and no face is cut short. The .stor
  and FEHM files were not affected. The points of a cell's faces are
  kept one after another in one buffer, so its size grows only with the
  most tets at a vertex (a cylinder with 2000 points per ring: about
  1 MB, where a buffer of faces x points would need 4.6 GB).

### Pull requests

| Date | PR | Description |
| --- | --- | --- |
| 2026-11-24 | #352 | dump / control_volume / paraview, 2D: a polygon's material is its vertex's imt1; new cell field itetclr |
| 2026-11-24 | #351 | dump / control_volume: exclude, the ghost generators' materials left out of the files |
| 2026-11-24 | | docs: Outside zones, how top, bottom, left_w, right_e, front_s, back_n are chosen (OUTSIDE_ZONES.md, linked from DUMP3) |
| 2026-11-23 | #350 | cmo / addatt / edge_lengths_*: an ERROR, not a crash, on a mesh object with no nodes or cells |
| 2026-11-24 | | GitHub checks: a change of documentation only (docs/, .md files) no longer builds and runs the suites |
| 2026-11-24 | | exclude: refused when the excluded materials do not surround the model (exclude_force); pieces of the faces shared with them, each closed |
| 2026-11-23 | #349 | compute / medial_axis: the medial axis and thickness of a surface; suite test_medial_axis |
| 2026-11-24 | | docs: DUMP3, the link to Outside zones was shown as code (an indented paragraph after a code block); now a paragraph with a live link |
| 2026-11-24 | #353 | dump / fehm and dump / stor: exclude, the ghost generators left out of every file; outside zones from the faces shared with the ghosts |
| 2026-11-24 | | dump / control_volume / paraview, 3D: no crash at a vertex with many tets (buffers sized from the mesh); suite case sphere_shell |
| 2026-11-24 | | dump / control_volume / paraview: a cell's face points in one flat buffer (memory linear in the most tets at a vertex); suite case cylinder_axis |
