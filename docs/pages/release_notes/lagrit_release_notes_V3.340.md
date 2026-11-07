LaGriT V3.3.4 Release Notes (in development)
====================

This page collects the changes since the V3.3.3 release (tag V3.3.3, January 2024). V3.3.4 is not yet released: the CMake project version is 3.3.4, and entries are added as pull requests merge. Pull requests still in review are listed separately at the end.

Previous release  LaGriT V3.3.3  Jan 4, 2024

### New commands and options

- **improve_tets** (#296): improves the worst tetrahedra of a mesh, by quality = the smallest sine of the dihedral angles, with smoothing and flips that keep the vertex set (Klingner and Shewchuk, IMR 2007). Boundary and interface vertices slide in their plane or line; a pset limits the work. See IMPROVE_TETS.md.
- **quality / minsine** (#296): the smallest dihedral-angle sine of each cell.
- **cmo / addatt / edge_lengths_min, edge_lengths_max, edge_lengths_mean** (#300).
- **createpts / poisson_disk** in 3D (#290, Poisson 3D sampling), with input checks that stop with an error instead of crashing on bad 3D input (#302).
- **Input lines** up to 1024 characters (#299; 80 before). Longer lines are refused, and a token cut at 32 characters gives a WARNING.
- **examples/dfn_tets** (#304): scripts that build 3D Poisson disk tet meshes around a DFN.
- **Tutorial: Tet Mesh Graded by a Surface's Edge Lengths** (docs/pages/tutorial/dfn_tet_grading): tets next to a triangulated surface (a dfnWorks fracture mesh, say) about 1.25 times its triangle edges, growing away from it; a 10-second example, and results on eight dfnWorks meshes.
- **connect / exact** (#319): 3D connect with exact in-sphere and orientation tests and no points added in small tets, so the result is the Delaunay tetrahedralization of the points. On a 12 x 12 x 12 grid jittered by 1e-9, plain connect leaves 3,953 faces not Delaunay, adds 281 points and leaves 4 out; connect / exact adds and loses none. The connect step does about 9% more work for ordinary point sets, about 35% for a lattice with a binary spacing such as 10 or 0.5, and about twice the work for a lattice with a spacing such as 1/63. Off by default.
- **3D connect fills to the convex hull** (#320): the tets lost with the enclosing tetrahedron are put back by gift wrapping with exact tests (src/lg_connect3d_hull.c); with **exact** the mesh is the Delaunay tetrahedralization of the convex hull. Missing before: 1.3% of the hull volume for random points, 7.7% for a 100 x 1 x 1 box; now 0 (pockets flatter than 1e-6 are left open). On by default; **connect / nohull** keeps the old mesh.
- **2D connect: exact, complete and fast** (#312, #313, #314): for points in a coordinate plane, 2D connect gives the exact Delaunay triangulation of the convex hull (src/lg_predicates2d.c, src/lg_connect2d_hull.c), and its time grows linearly (256,000 random points: 1.5 s, was 35 s on a Mac). See [connect_notes](../docs/connect_notes.md).
- **Faster connect** (#314, #316): more than 100 points are inserted in a BRIO order, in 2D and 3D (src/lg_brio_order.c); 3D, 256,000 random points: 2.7 s, was 14.0 s. **connect / old_order** keeps the earlier order.
- **createpts / poisson_disk / 3d_box / ... / h_lookup octree** (#303): an h-adapted octree holds the h field and the neighbour search instead of the uniform lattice (`hex_grid`, still the default), with a maximal fill (any vertex that could still be added lies in the boxes it reports). Graded meshes with a large range of h become practical: on a DFN case with H_MAX = 50 x H_MIN, 112 s and 0.21 GB against 1,152 s and 1.99 GB. Options `octree_k_factor`, `octree_batch`, `octree_dump`. New suite test_poisson_octree.
- **connect options** (#317): **noadd**, **check_interface** and **old_order** may be anywhere in the command, in 2D and 3D, in any combination; `connect / 1,0,0` and `connect / pset,get,name` read the point range as CONNECT1.md shows.

### Fixes

- connect: a negative or huge imt is refused with an error instead of crashing (#297, issue #240).
- hextotet / grid2grid: prism to 14 (issue #214) and pyramid to 4 wrote past their arrays; options that do not fit the cells are refused (#298).
- eltset: inter and not with three or more sets gave wrong results without a message (#301). ELTSET2.md and PSET.md document the set logic.
- triangulate (issues #239, #57), a crash in cmo / copy (issue #6), macOS .stor output, the Poisson sampler, createpts checks (#292).
- interpolate and refine fixes (#293).
- Compiler fixes (#260, clang flag -Wno-error=implicit-int; #264).
- 2D connect made no triangles far from the origin (UTM coordinates; the unit square at x = 1000 of test/level01/connect_errors): the enclosing triangle is now placed inside the box (#312).
- 2D connect left points out on stretched domains (a 1000 x 1 grid: 37 of 961) and was not exactly Delaunay; exact tests fix both, and connect no longer runs recon / 0 for points in a coordinate plane (#313).
- delaunay.f and delaunay2d_lg.f sized a work array by nnodes, but points added by fix_small_tets / fix_small_triangles_lg are numbered above it, so connect wrote past the array (MMVERIFY "Array epilog overwritten"; all points on a sphere) (#316).
- recon accepts vertices with imt 0 (no more "MMGETBLK: ERROR! FATAL ... matrecon") (#315).
- The exact tests are exact in every case: near-ties below about 1e-28 of the determinant's size were taken as ties; they are now decided in integer arithmetic (src/lg_exact_int.h). test_predicates: 341 of 40,000 2D and 147 of 20,000 3D adversarial tests were wrong before, none now (#319).
- read / avs and dump / avs (#306, #307, #308): errors instead of crashes and wrong meshes on bad files; the code restructured; read about 3 times and dump about 2.5 times faster, with byte-identical output. test_avs_io grows to 32 decks.
- TRANS.md had **zero** and **center** swapped and listed a keyword **enter** that does not exist (#318).
- 3D `recon / 1` no longer writes a file `gmv_refine_edge_3d` in the current folder (a debugging dump left in refine_edge_3d.f).
- The 3D hull fill (#320) and the 2D one (#322) no longer resize every array of the mesh object in the middle of connect, which made test/level01/refine_recon_quality and test_createpts cp62 fail on macOS.

### Build and tests

- CMake builds Exodus as an option; src/fc_mangle.h is generated by CMake and no longer tracked (#295).
- New test suite test/suite, with independent checks and a results page per suite, and CI that runs it (#294). `sh test/suite/run_all.sh` runs every suite; the level01 tests run inside it as test_lagrit_upstream. New suites since: test_avs_io (#305), test_hextotet (#298), test_set_logic (#301), test_edge_lengths (#300), test_input_lines (#299), test_improve_tets, test_element_angles and test_crush_thin_tets (#296), test_connect_2d (#313), test_connect_options (#317), test_predicates and test_connect_exact (#319), test_connect_hull (#320), test_recon (#321), test_poisson_octree (#303).
- Each suite's results page is now `summary.html` in the suite's own folder (it was `logs/summary.html`; that file now forwards to it), linked from `test/suite/summary.html`. Every page opens with a "What it does" paragraph and describes each test, and every test that writes a mesh or a vertex set shows a picture of it (new pictures: test_avs_io, test_element_angles, test_improve_tets, test_set_logic, test_connect_options, test_connect_exact, test_connect_hull, test_predicates). New page for test_poisson_command_errors, with LaGriT's message for each deck.
- `run_all.sh` can run less, or faster: **JOBS=N** runs N suites at a time (longest first); **ONLY=tag,...** runs the suites with those tags (connect, poisson, 2d, 3d, ...); **QUICK=1** leaves out the slow ones; **CHANGED=1** (or `CHANGED=<branch>`) runs only the suites the changes can affect, from a map of the source files each suite executes (`suite_sources.txt`, made with a coverage build by `make_suite_sources.py`). CI runs the suites 4 at a time (3 on macOS) and keeps every suite page in its artifact.
- New suite test_recon: `recon / 0` and `recon / 1` in 2D and 3D with exact checks. In 2D both do what RECON.md says. In 3D, `recon / 1` does not bring the Voronoi centres inside the boundary (310 outside before, 454 after on random points in a cube), leaves the mesh far from Delaunay and breaks parent / child chains with two materials, and `recon / 0` cannot flip every face to Delaunay; recorded as known problems, and in RECON.md.

### Changes in behaviour

- Input lines longer than 1024 characters are refused; before, characters past column 80 were dropped without a warning (#299).
- eltset inter and not with three or more sets now give the documented result (#301).
- connect: where points are exactly cocircular or cospherical (a regular grid), a square or cube may be split the other way (either is Delaunay), from the new insertion order (#313, #314, #316); 2D connect adds the triangles along the convex hull (#313), and 3D connect the tets (#320); new lines in connect's output. level01 references updated: connect_errors, intersect, intrp_2D_sizes, pflotran_stor, recon1, addmesh_add, cmo_addatt_normals, connect_cube, hybrid_volume, rmpoint, setpts, smooth.
- read / avs requires a named, empty mesh object; to join meshes, use addmesh / append (#306).
- connect: the coordinates of the enclosing tetrahedron or triangle are deprecated (a WARNING); fewer than 12 (3D) or 9 (2D) numbers after the point range give a WARNING and are ignored; check_interface in 2D gives a WARNING (#317).

### Known issues

- 3D connect (without **exact**) on nearly regular grids (a grid jittered by 1e-6 to 1e-9): a few vertices can be dudded (2 to 4 of 3,375, also in V3.3.4), or many points added by the small-tet pass (419 for 1e-9). V3.3.4 made no tets at all for 1e-9 and crashed for one 1e-6 grid (the work-array overrun fixed in #316). The tolerance in-sphere test decides these near-ties; **connect / exact** avoids them, but on input meant to be cospherical and coplanar (the createpts rtp sphere) its Delaunay tets can be flat to round-off.
- 3D connect without **exact**: not exactly Delaunay where points are nearly cospherical (a grid jittered by 1e-9: 2,977 faces). (Thin tets along the convex hull, lost with the enclosing tetrahedron before #320, are now put back.)
- 2D connect of points in a tilted plane still uses the tolerance tests and recon / 0.
- recon / 1 in 3D does not bring the Voronoi centres inside the boundary and leaves the mesh far from Delaunay (test_recon; see RECON.md).

### Log summary

Pull requests merged since V3.3.3 (dependabot updates of the documentation site not listed):

| Date | PR | Title |
| --- | --- | --- |
| 2026-11-06 | #322 | Brings to master what #303, #306, #307, #308 and #321 were merged into other branches with; the 2D hull fill's macOS fix; no stray recon file |
| 2026-11-04 | #320 | 3D connect fills to the convex hull; connect / nohull; suite pages |
| 2026-11-04 | #319 | connect / exact: exact 3D in-sphere tests; exact predicates in every case |
| 2026-11-04 | #318 | Small fixes: TRANS.md zero/center; 2D interface pass tests |
| 2026-11-04 | #317 | connect: command options in any position; noadd in 2D; deprecate enclosing coordinates |
| 2026-11-04 | #316 | connect (3D): insert the points in a BRIO order |
| 2026-11-04 | #315 | recon: accept vertices with imt 0 |
| 2026-11-04 | #314 | connect (2D): insert the points in a BRIO order |
| 2026-11-04 | #313 | connect (2D): exact Delaunay triangulation of the convex hull |
| 2026-11-04 | #312 | connect (2D): enclosing triangle inside the box far from the origin |
| 2026-11-02 | #305 | test/suite: add test_avs_io, read / avs and dump / avs |
| 2026-11-02 | #304 | examples/dfn_tets: 3D Poisson disk tet meshes around a DFN |
| 2026-11-02 | #302 | poisson_disk: stop with an error instead of crashing on bad 3D input |
| 2026-11-02 | #301 | eltset: inter and not with three or more sets; document set logic |
| 2026-11-02 | #300 | cmo / addatt: edge_lengths_min, edge_lengths_max, edge_lengths_mean |
| 2026-11-02 | #299 | Input lines: use lines up to 1024 characters, refuse longer ones; warn on cut tokens |
| 2026-11-02 | #298 | hextotet / grid2grid: fix prism to 14 (#214) and pyramid to 4 |
| 2026-11-02 | #297 | connect: refuse negative or huge imt instead of crashing (#240) |
| 2026-11-02 | #296 | improve_tets, crush_thin_tets fixes, cell angle fixes, quality / minsine |
| 2026-11-02 | #295 | CMake Exodus build |
| 2026-10-21 | #294 | Test suite in test/suite, and CI that runs it |
| 2026-10-21 | #293 | Interpolate refine fixes |
| 2026-10-20 | #292 | Fixes: triangulate (#239, #57), cmo/copy crash (#6), macOS .stor, Poisson sampler; createpts checks |
| 2026-03-27 | #290 | Poisson 3D sampling |
| 2025-01-09 | #275 | Lagrit site dev |
| 2024-05-06 | #267 | Ehinrichs/testing |
| 2024-05-06 | #266 | Ehinrichs/debug |
| 2024-04-23 | #264 | Updated files to fix compiler issues |
| 2024-04-22 | #262, #263 | Tam debug, and its revert |
| 2024-04-19 | #261 | Ehinrichs/dev |
| 2024-04-17 | #260 | Check clang version and add flag -Wno-error=implicit-int |

```
git log --merges --format="%ad %s %b" --date=short V3.3.3..master
```
