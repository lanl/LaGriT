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

- **createpts / poisson_disk / 3d_box / ... / face_lattice adaptive |
  uniform:** with the edge buffer on, the vertices on the six box faces
  can now follow the h field. `face_lattice adaptive` (the default with
  h_lookup octree and a control file) makes each face a quad mesh,
  refined with refine (a 2:1 balanced quadtree) until no quad is longer
  than face_lattice_factor x h at its corners (below); the sampler then
  fills the whole box and keeps POI_EDGE_BUFFER x face_lattice_factor x
  h away from the face vertices, so the Voronoi points of the tets on
  the faces lie inside the box and the coupling coefficients are
  positive. On a 10 x 10 x 5 box with h_spacing_scalar 0.05 and h from
  0.1 to 1: 911 vertices and no negative coefficients, against 158,991
  vertices and 13 negative
  coefficients with the lattice at h_spacing_scalar (`face_lattice
  uniform`, the only lattice before; still the default for a uniform h
  and with h_lookup hex_grid). A deck with a control file and the edge
  buffer on now gets the adaptive faces; add `face_lattice uniform` for
  its old vertices.

- **createpts / poisson_disk / 3d_box / ... / face_lattice_factor F:**
  with `face_lattice adaptive`, a face quad is refined while it is longer
  than F x h, and the sampler keeps POI_EDGE_BUFFER x F x h from the face
  vertices. The default 1.5 gives face edges about as long as the tet
  edges inside (F 1, the first adaptive faces: about twice as fine); the
  gap grows with F, which keeps the Voronoi points of the tets on the
  faces inside the box. On the ANALOG fracture network (a 5,979 x 4,098 x
  935 m box): 156,675 vertices instead of 216,527, still no negative
  coupling coefficients.

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

- **createpts / poisson_disk / 3d_box with the edge buffer on**
  (`IF_POI_EDGE_BUFFER` 1, the default): the lattice of vertices on the
  six box faces is now made face by face. It was made by building a hex
  brick over the whole box at `h_spacing_scalar` and keeping its outside
  vertices, so memory grew with the box volume / `h_spacing_scalar`
  cubed, whatever the h field: a 24 x 24 x 14 box with
  `h_spacing_scalar` 0.05 (65 million brick vertices) ran out of memory
  at 6 GB after the sampling had finished. Now it needs 324 MB and
  2.5 s. The face vertices, their order and their itp1 are the same as
  before, bit for bit.

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

- **cv_dual (dump / control_volume, dump / fehm, dump / stor) on a
  mesh from connect / exact with points on common spheres:** tets flat
  to round-off had their circumcenter from round-off (in double
  precision), and cv_dual stopped at its Delaunay check (delta/L of
  -0.8 on a mesh that is exactly Delaunay). connect / exact makes such
  tets wherever four points lie on a circle, as two pairs of mirror
  images always do (the seeds of a conforming Voronoi mesh). A nearly
  flat tet (determinant within 1e-6 of its size) now has its
  circumcenter computed in double-double arithmetic from the
  coordinates as stored, which gives the true center to round-off; a
  tet coplanar even then takes the center of its circle and its facets
  count as ties. Other meshes are unchanged.

### Pull requests

| Date | PR | Description |
| --- | --- | --- |
| 2026-11-24 | #352 | dump / control_volume / paraview, 2D: a polygon's material is its vertex's imt1; new cell field itetclr |
| 2026-11-24 | #351 | dump / control_volume: exclude, the ghost generators' materials left out of the files |
| 2026-11-24 | | docs: Outside zones, how top, bottom, left_w, right_e, front_s, back_n are chosen (OUTSIDE_ZONES.md, linked from DUMP3) |
| 2026-11-23 | #350 | cmo / addatt / edge_lengths_*: an ERROR, not a crash, on a mesh object with no nodes or cells |
| 2026-11-25 | #363 | createpts / poisson_disk / 3d_box: the edge buffer's face lattice made face by face, memory as the face area, not the box volume |
| 2026-11-24 | | GitHub checks: a change of documentation only (docs/, .md files) no longer builds and runs the suites |
| 2026-11-24 | | exclude: refused when the excluded materials do not surround the model (exclude_force); pieces of the faces shared with them, each closed |
| 2026-11-23 | #349 | compute / medial_axis: the medial axis and thickness of a surface; suite test_medial_axis |
| 2026-11-25 | #364 | createpts / poisson_disk / 3d_box: face_lattice adaptive, the box faces follow h (the default with h_lookup octree and a control file); face_lattice uniform for the old lattice |
| 2026-11-25 | #365 | createpts / poisson_disk / 3d_box: face_lattice_factor (default 1.5): face edges about as long as the tet edges inside, the gap to the face vertices grown with it |
| 2026-11-24 | | docs: DUMP3, the link to Outside zones was shown as code (an indented paragraph after a code block); now a paragraph with a live link |
| 2026-11-24 | #353 | dump / fehm and dump / stor: exclude, the ghost generators left out of every file; outside zones from the faces shared with the ghosts |
| 2026-11-24 | | dump / control_volume / paraview, 3D: no crash at a vertex with many tets (buffers sized from the mesh); suite case sphere_shell |
| 2026-11-24 | | dump / control_volume / paraview: a cell's face points in one flat buffer (memory linear in the most tets at a vertex); suite case cylinder_axis |
| 2026-11-24 | | cv_dual: the circumcenter of a nearly flat tet in double-double (connect / exact with points on common spheres); suite case mirror_seeds |
