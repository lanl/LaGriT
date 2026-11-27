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

- **hfield** (new command): a named spacing h built from a few sources,
  for createpts / poisson_disk without a control file. `near, mo` (a
  point set, lines, a surface or a DFN) and `inside, surface / mo` (a
  closed surface, or a closed polygon in 2D), each with h_value or
  h_attribute (for example edge_lengths_mean) x h_factor, h_band and
  h_growth; h = min(h_ceiling, max(h_floor, the smallest h_i)), so h
  can never grow faster than h_growth. Actions create, add, list, eval
  (h at the vertices of any mesh object), dump (a lattice for ParaView)
  and delete. createpts / poisson_disk takes `hfield, hf_name` where
  the control file goes, in 2D and 3D, with every h_lookup and
  face_lattice option; a control file that sets the same h gives the
  same mesh. h_floor below h_spacing_scalar: a WARNING. See
  [HFIELD](../docs/commands/HFIELD.md). Test: test_hfield.

### Fixes

- **extract / surfmesh of a 2D mesh in the x-z or y-z plane** (GitHub
  #4) set z to 0 on the boundary lines; all three coordinates are now
  copied. New suite test_extract_surfmesh.

- **perturb, extract / isosurface and shtgtsfp** gave their temporary
  arrays a memory partition whose name was never set (in perturb the
  line was commented out), so the release at the end could free the
  wrong arrays: level01/createpts_filter (perturb) crashed on one
  GitHub runner. Each sets its name now; valgrind is clean on that
  deck.

- **connect with an interface surface (intrface) on a mesh object with
  no constraint table** crashed (a segmentation fault in
  multi_material.f): when setpts had found no vertices on constrained
  surfaces, the mesh object had no icontab, ncon50 or nconbnd, and the
  first interface vertex was written to address 0. connect (3D and 2D)
  now adds the three attributes when it needs them, and keeps ncon50
  equal to icontab's length as the table grows. The createpts / amr
  demo of the docs (test_createpts cp53) now runs; meshes that already
  had the table are unchanged. The icontab table is described in the
  Mesh Object page.

- **cmo / addatt / edge_lengths_min, _max, _mean on a mesh object with
  no nodes or no cells:** an ERROR and LaGriT goes on; it crashed
  (segmentation fault). The same for edge_connections and
  node_num_diff, which share the code.

- **recon after an interface (intradd.f):** when intradd added the
  interface vertices, the mesh object could be moved in memory, but
  only the vertex arrays were looked up again; the loop that fixes the
  cell connectivity then read and wrote the old, freed arrays. The
  result happened to be right on most machines (level01/recon1 failed
  only under valgrind). The cell arrays are now looked up again too.

- **Whole numbers written as reals** (`3.`, `3.0`, `1e1`) where a
  command reads an integer are now read as that integer; they were read
  as 0, without a message. `quadxy / 3. 3. / ...` made no points;
  `createpts / brick / xyz / 3,2,1e1 / ...` crashed (segmentation
  fault); createpts / brick's flags `1.,1.,1.` meant 0 (cell centers),
  not 1. This changes the output of a deck that relied on the old
  reading: of LaGriT's tests only level01/surface did (its brick flags
  `1.,1.,1.`), and its reference output is updated (the brick now
  fills the box given: volume 1.5e6, was 4.5e5). Other values (`2.5`) are read
  as before. A token's type is unchanged, so a command that checks for
  an integer still refuses `3.`.

- **read / avs: a cell vertex number past the last vertex** (GitHub
  #176) corrupted memory (MMVERIFY "Array epilog overwritten", then a
  crash or wrong cells); it is now an ERROR naming the cell and the
  range of vertex numbers, and nothing is read. A file that does not
  exist (GitHub #112) is an ERROR too, where it was a message and two
  WARNINGs.

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

- **dump / coord and dump / fehm on a quad mesh** (GitHub #289): the
  .fehmn file's elem list was empty; it now lists the quads, 4 vertices
  each, counterclockwise as the triangles. For cells the elem list does
  not take (prisms, pyramids, lines, a hybrid mesh) a WARNING now says
  that no elem list was written. New suite test_dump_coord.

- **cmo / delatt and cmo / DELATT with -all-** (GitHub #163, #108)
  said "Attribute does not exist: -all-". Now `delatt / mo / -all-`
  deletes every temporary attribute and keeps the permanent and
  standard ones; `DELATT / mo / -all-` deletes every attribute but the
  standard ones (those cmo / create makes).
- **cmo / status of a deleted mesh object** (GitHub #159) said the
  current mesh object does not exist; it names the one asked for.
- **math with imt** (GitHub #31) stopped LaGriT with a segmentation
  fault; imt, itp, icr and isn now name imt1, itp1, icr1 and isn1, and
  an attribute that does not exist is an ERROR.
- **cmo / setatt with a word for a number** (GitHub #55), such as an
  undefined define name, set the attribute to garbage (6.95e-310); it
  is an ERROR and nothing is set. New suite test_cmo_commands.

- **dump / control_volume / paraview (and cv_dual -paraview), 2D:** a
  polygon's `material` in `root_cells.vtu` is now its vertex's imt1, as
  for polyhedra and as in the .stor and zone files; it was the itetclr
  of one triangle around the vertex, wrong on a material interface
  (81 of 1124 cells in one conforming mesh with a lens) and 0 where
  itetclr was not set. The triangles' color is now the new cell field
  `itetclr`: in a fracture network, color the polygons by `itetclr` to
  see the fractures (a vertex on an intersection has one polygon per
  fracture, each with that vertex's one imt1).

- **File names up to 132 characters.** A token is kept with 32
  characters, and every file name was cut to 32 without a message (a
  WARNING since V3.4.0), so a longer name wrote or read a different
  file. A command that takes a file name now uses it in full, up to 132
  characters: read and dump (every format, and their short forms),
  dump / control_volume, infile and input, cmo / readatt, pset / write,
  eltset / write, gcolor, massage, read / sheetij and read / gocad. The
  gmv writer read past its 80-character name buffer; dump / stl and
  dump / coord failed on long names. The WARNING for a long token now
  comes after its command, and only for a token the command used cut (a
  mesh object or attribute name, a define's value, a file name over 132
  characters). A define still keeps 32 characters of its value. New
  suite test_long_names.

- **A command no longer reads the values of the previous command.**
  The integer and real value of every token place is now cleared
  before each command is parsed. cmo / addatt took its default value
  from the 11th token even when the command had fewer, so a new
  attribute could get the previous command's value there; after #370
  (whole numbers written as reals), `cmo / addatt / mo / a / vint /
  scalar / nnodes` after a command with `1.` as its 11th token made
  `a` all 1 instead of 0. cmo / addatt now reads its default only when
  the command gives one. level01/stack_layers' reference shows the
  default of layertyp as 0 (it was the 1 of the stack command).

- **dump / control_volume / paraview (and cv_dual -paraview), 3D: a
  crash** (segmentation fault) at a vertex with more than 682 tets
  around it, as at the center of seeds on a sphere (a lens in a
  conforming Voronoi mesh). The buffers of one cell had a fixed size;
  they are now sized from the mesh, and no face is cut short. The .stor
  and FEHM files were not affected. The points of a cell's faces are
  kept one after another in one buffer, so its size grows only with the
  most tets at a vertex (a cylinder with 2000 points per ring: about
  1 MB, where a buffer of faces x points would need 4.6 GB).

- **Docs: control-file lines.** The createpts / poisson_disk page said
  that LaGriT drops the characters of a control-file line past column 80
  without a warning. Since V3.4.0 a line, in a deck or a control file,
  can be up to 1024 characters; the page now says so (and three test
  comments that said the same).

- **An input that ends without finish** (GitHub #22) gave "unexpected
  eof-command ignored" again and again, then "STOP maximum number hit,
  code aborting". Now a WARNING says finish is assumed: LaGriT goes
  back to the input that read the file, or stops as after finish.
- **2D connect's enclosing triangle** (GitHub #67) was printed with
  negative numbers run together; each number now has a blank before
  it, as in 3D.
- **refine (refine_tet_add.f)** kept epsilonl in an integer variable
  (GitHub #242); it is a real now. (It worked only because both take 8
  bytes.)

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
| 2026-11-25 | | docs: createpts_poisson.md, a control-file line can be up to 1024 characters (it said 80) |
| 2026-11-24 | #351 | dump / control_volume: exclude, the ghost generators' materials left out of the files |
| 2026-11-26 | | cmo / delatt and DELATT -all- (GitHub #163, #108); cmo / status of a deleted mesh object (#159); math with imt (#31); setatt with a word for a number (#55); suite test_cmo_commands |
| 2026-11-24 | | docs: Outside zones, how top, bottom, left_w, right_e, front_s, back_n are chosen (OUTSIDE_ZONES.md, linked from DUMP3) |
| 2026-11-26 | | perturb, extract / isosurface, shtgtsfp: their memory partition named (an unset name; a crash on a GitHub runner in level01/createpts_filter) |
| 2026-11-23 | #350 | cmo / addatt / edge_lengths_*: an ERROR, not a crash, on a mesh object with no nodes or cells |
| 2026-11-26 | | connect with an intrface surface: no crash when the mesh object has no constraint table (icontab); the createpts / amr demo runs (test_createpts cp53); icontab described in meshobject.md |
| 2026-11-26 | | extract / surfmesh: z kept for a 2D mesh in the x-z or y-z plane (GitHub #4); suite test_extract_surfmesh |
| 2026-11-25 | | parser: a whole number written as a real (3. 1e1) is also read as an integer, not as 0 |
| 2026-11-26 | | intradd (recon after an interface): the cell arrays looked up again after the mesh object grows (freed memory read and written; valgrind, level01/recon1) |
| 2026-11-25 | #363 | createpts / poisson_disk / 3d_box: the edge buffer's face lattice made face by face, memory as the face area, not the box volume |
| 2026-11-26 | | read / avs: an ERROR for a cell vertex number past the last vertex (GitHub #176) and for a missing file (GitHub #112) |
| 2026-11-24 | | GitHub checks: a change of documentation only (docs/, .md files) no longer builds and runs the suites |
| 2026-11-24 | | exclude: refused when the excluded materials do not surround the model (exclude_force); pieces of the faces shared with them, each closed |
| 2026-11-23 | #349 | compute / medial_axis: the medial axis and thickness of a surface; suite test_medial_axis |
| 2026-11-25 | | dump / coord and dump / fehm: the elem list of a quad mesh (GitHub #289); a WARNING when no elem list is written |
| 2026-11-25 | #364 | createpts / poisson_disk / 3d_box: face_lattice adaptive, the box faces follow h (the default with h_lookup octree and a control file); face_lattice uniform for the old lattice |
| 2026-11-26 | | parser: every token value cleared before each command; cmo / addatt reads its default only when given (after #370 a default could be the previous command's 1.) |
| 2026-11-25 | #365 | createpts / poisson_disk / 3d_box: face_lattice_factor (default 1.5): face edges about as long as the tet edges inside, the gap to the face vertices grown with it |
| 2026-11-25 | | file names up to 132 characters in read, dump, infile and the set writers; WARNING after the command for tokens used cut; suite test_long_names |
| 2026-11-24 | | docs: DUMP3, the link to Outside zones was shown as code (an indented paragraph after a code block); now a paragraph with a live link |
| 2026-11-24 | #353 | dump / fehm and dump / stor: exclude, the ghost generators left out of every file; outside zones from the faces shared with the ghosts |
| 2026-11-26 | | input without finish: finish assumed (GitHub #22); 2D connect's enclosing triangle printed with blanks (#67); refine_tet_add.f epsilonl a real (#242) |
| 2026-11-24 | | dump / control_volume / paraview, 3D: no crash at a vertex with many tets (buffers sized from the mesh); suite case sphere_shell |
| 2026-11-24 | | dump / control_volume / paraview: a cell's face points in one flat buffer (memory linear in the most tets at a vertex); suite case cylinder_axis |
| 2026-11-24 | | cv_dual: the circumcenter of a nearly flat tet in double-double (connect / exact with points on common spheres); suite case mirror_seeds |
