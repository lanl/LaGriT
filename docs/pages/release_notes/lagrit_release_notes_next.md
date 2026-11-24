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

### Pull requests

| Date | PR | Description |
| --- | --- | --- |
| 2026-11-24 | | dump / control_volume / paraview, 2D: a polygon's material is its vertex's imt1; new cell field itetclr |
| 2026-11-24 | #351 | dump / control_volume: exclude, the ghost generators' materials left out of the files |
| 2026-11-23 | #350 | cmo / addatt / edge_lengths_*: an ERROR, not a crash, on a mesh object with no nodes or cells |
| 2026-11-23 | #349 | compute / medial_axis: the medial axis and thickness of a surface; suite test_medial_axis |
