LaGriT V3.4.0 Release Notes
====================

LaGriT V3.4.0 was released on November 20, 2026 (tag V3.4.0). It is the first release since V3.3.3 (January 4, 2024) and brings about 50 pull requests, almost all of them from October and November 2026. The release is source only: GitHub's source archives of the tag.

Previous release  LaGriT V3.3.3  Jan 4, 2024

For an overview with examples, see [What's new in LaGriT V3.4.0](whats_new_V3.400.md).

### Highlights

- **cv_dual, the new default for control volumes:** dump / stor, fehm, pflotran and zone_outside compute Voronoi, median and hybrid control volumes and their geometric coefficients with a new code, cv_dual, which checks the mesh first; `matbld` gives the earlier files. The new command dump / control_volume writes FEHM .stor, PFLOTRAN .uge and TOUGH MESH files with every cv_dual option, vertex attributes that point to negative coefficients, and the dual mesh itself (the control volumes as polygons and polyhedra) as files to view in ParaView.
- **connect:** 2D and 3D connect fill the whole convex hull, can be exactly Delaunay (`connect / exact`), and are 5 to 20 times faster on large point sets.
- **Poisson disk meshes in 3D:** createpts / poisson_disk fills a box in 3D, and an h-adapted octree makes graded meshes with a wide range of h practical.
- **Right numbers at UTM coordinates, with 17 digits:** dump / stor no longer loses every digit far from the origin, and AVS, .stor, .uge and .fehmn files write 17 significant digits, so every double survives a write and a read.
- **New commands for tet quality:** improve_tets improves the worst tets of a mesh, and quality / minsine measures them.
- **A test suite with CI:** 43 suites with independent checks and a results page each, run on Linux and macOS for every pull request.

### Contributors

Carl Gable, Jeffrey Hyman, Terry Miller, Erica Hinrichs and Thyagarajulu Gollapalli. Johannes Krotz and Matthew Sweeney collaborated on the Poisson disk sampling code.

### New commands and options

#### Control volumes: cv_dual (#339 to #345)

- **cv_dual** (src/cv_dual; manual docs/pages/docs/cv_dual/cv_dual_manual.md): computes Voronoi, median or hybrid control volumes and their geometric coefficients from Delaunay triangle and tet meshes (2D, 3D and fracture networks), and writes them as FEHM .stor, PFLOTRAN .uge and TOUGH MESH files with 17 significant digits. It was developed as del_vor, ported from lanl/voronoi without PETSc, and its history is merged into LaGriT (#339).
- **Mesh and Delaunay checks:** before writing anything, cv_dual checks that the mesh is valid and Delaunay; a mesh that is not Delaunay is refused for Voronoi control volumes with an ERROR, and `cv median` still runs on it (#339).
- **Median, blend and hybrid control volumes:** besides Voronoi (the default), cv_dual gives median control volumes (the mesh need not be Delaunay), a fixed blend of the two, and a hybrid that is Voronoi wherever that gives no negative coefficient and moves toward median only in the cells, and only as far, as needed to remove the negative ones (#339).
- **ParaView files:** cv_dual can write the control volumes (all, the boundary ones, the flagged ones, one material or a box) and the coefficient matrix as .vtu files for viewing in ParaView (#339).
- **The program build/cv_dual:** cv_dual is also built as a stand-alone program, `cv_dual -avs mesh.inp -stor mesh.stor`, that reads an AVS mesh file and takes the same keywords as the LaGriT command (#339).
- **dump / control_volume** (new command; DUMP_CONTROL_VOLUME.md): runs cv_dual on a mesh object, using the mesh object's arrays in place; the .stor's second line ends with the command that wrote it (#340, #341). For example:

  ```
  dump / control_volume / mo_name / stor, mesh.stor / uge, mesh.uge / cv, hybrid
  ```

- **attribute_cv_volume:** `dump / control_volume / ... / attribute_cv_volume [, name]` puts each vertex's control volume in a vertex attribute (default name cv_volume) (#340).
- **outside_area:** cv_dual and dump / control_volume can write each boundary vertex's vector area (Ax, Ay, Az) (#342).
- **Keyword cv_dual on dump / stor, fehm, pflotran and zone_outside** (DUMP3.md, "cv_dual OR matbld"): with `cv_dual` the .stor, the .uge and the boundary-area files (`_outside_vor.area`, `_med`, new `_hyb`) come from cv_dual, with `voronoi` (default), `median` or `hybrid` control volumes; `matbld` writes exactly what these commands wrote before. Options cv_dual does not have (vector and area coefficients, binary, pflotran's verbose) leave the dump to matbld with one line saying so, and a mesh cv_dual refuses gives an ERROR with nothing written (#342).
- **cv_dual the default** (DUMP3.md, "cv_dual OR matbld"): without either keyword, dump / stor, fehm, pflotran and zone_outside use cv_dual; zero-area connections are left out of the .stor (`nofilter_zero` keeps them); a mesh whose cells are not all triangles or all tets goes to matbld, with one line saying so; every dump says which code wrote it. See Changes in behavior (#345).
- **ccoef_* keywords on dump / control_volume** (DUMP_CONTROL_VOLUME.md): `ccoef_area_min`, `ccoef_area_max`, `ccoef_ij_area_min`, `ccoef_ij_area_max`, `ccoef_areabydist_min`, `ccoef_areabydist_max`, `ccoef_ij_areabydist_min`, `ccoef_ij_areabydist_max`, `ccoef_distance_min` and `ccoef_distance_max` each put one value per vertex in a vertex attribute, over its connections that are not zero-area: the smallest and largest face area A(ij), coefficient A(ij)/d(ij) and edge length d(ij), and the vertex at the other end of the A and A/d extremes. `ccoef_areabydist_min` and `ccoef_ij_areabydist_min` give what matbld's ccoef and ij_ccoef gave (#345).
- **cv_dual compared with matbld** (docs/pages/docs/cv_dual/cv_dual_vs_matbld.md): a page and a suite run LaGriT's own .stor, .uge and FEHM decks with both codes and explain every difference: the same Voronoi numbers; on LaGriT's Poisson disk mesh matbld's hybrid leaves 43 of 75 negative coefficients and cv_dual's none (#343).

#### connect

- **connect / exact** (3D): exact in-sphere and orientation tests, with no points added in small tets, so the result is the Delaunay tetrahedralization of the points. On a 12 x 12 x 12 grid jittered by 1e-9, plain connect leaves 3,953 faces not Delaunay, adds 281 points and leaves 4 out; connect / exact adds and loses none. The connect step does about 9% more work for ordinary point sets, about 35% for a lattice with a binary spacing such as 10 or 0.5, and about twice the work for a lattice with a spacing such as 1/63. Off by default (#319).
- **2D connect, exact and complete:** for points in a coordinate plane, 2D connect gives the exact Delaunay triangulation of the convex hull (src/lg_predicates2d.c, src/lg_connect2d_hull.c); see [connect_notes](../docs/connect_notes.md) (#313).
- **3D connect fills to the convex hull:** the tets that were lost with the enclosing tetrahedron are put back by gift wrapping with exact tests (src/lg_connect3d_hull.c). Missing before: 1.3% of the hull volume for random points, 7.7% for a 100 x 1 x 1 box; now 0 (pockets flatter than 1e-6 are left open). With **exact** the mesh is the Delaunay tetrahedralization of the convex hull. On by default; **connect / nohull** keeps the old mesh (#320).
- **Faster connect (BRIO order):** more than 100 points are inserted in a randomized spatial order (src/lg_brio_order.c), so the time grows about linearly: 256,000 random points take 1.5 s instead of 35 s in 2D and 2.7 s instead of 14.0 s in 3D. **connect / old_order** keeps the earlier order (#314, #316).
- **connect options anywhere:** **noadd**, **check_interface** and **old_order** may come anywhere in the command, in 2D and 3D, in any combination, and `connect / 1,0,0` and `connect / pset,get,name` read the point range as CONNECT1.md shows (#317).

#### createpts / poisson_disk

- **3D Poisson disk sampling:** `createpts / poisson_disk / 3d_box` fills a box with well-spaced vertices for a tet mesh, with h set by a constant or by the user's control file (#290). Bad 3D input (integer or flat box defines, a box thinner than the edge buffer, bad edge-buffer values) stops with an ERROR instead of crashing, and a WARNING names the edge-buffer defaults when they are not set (#302).
- **h_lookup octree:** an h-adapted octree holds the h field and the neighbor search instead of a uniform lattice (`hex_grid`), with a maximal fill: any vertex that could still be added lies in the boxes LaGriT reports. Graded meshes with a wide range of h become practical: on a DFN case with H_MAX = 50 x H_MIN, 112 s and 0.21 GB against 1,152 s and 1.99 GB. Options `octree_k_factor`, `octree_batch`, `octree_dump`; new suite test_poisson_octree (#303). It is the default with a control file (see Changes in behavior; #327).
- **examples/dfn_tets:** Python scripts that build graded 3D Poisson disk tet meshes around a DFN, one case or a folder of cases, with a results page (#304).
- **Tutorial: Tet Mesh Graded by a Surface's Edge Lengths** (docs/pages/tutorial/dfn_tet_grading): how to make tets next to a triangulated surface (a dfnWorks fracture mesh, say) about 1.25 times its triangle edges, growing away from it; a 10-second example and results on eight dfnWorks meshes (#326).

#### Other commands

- **compute / distance_field / ... / facet:** a sixth word gives the exact distance to the nearest point of the source's triangles, line segments or quads (inside a cell, on an edge or at a vertex) instead of the distance to its nearest vertex, which can be too large by up to about 0.58 of a triangle's edge next to the surface; `vertex`, or no sixth word, keeps the old result. New suite test_distance_field, against brute force (#328).
- **cmo / addatt / edge_lengths_min, edge_lengths_max, edge_lengths_mean:** new vertex attributes holding the shortest, longest or mean length of the edges at each vertex, for any cell type; a triangle surface can then set the resolution of a volume mesh, for example the h field of createpts / poisson_disk (#300).
- **improve_tets** (new command; IMPROVE_TETS.md): improves the worst tets of a mesh, by the smallest sine of their dihedral angles, with smoothing and flips that keep the vertex set (Klingner and Shewchuk, IMR 2007). Boundary and interface vertices slide in their plane or line, and a pset limits the work (#296).
- **quality / minsine:** reports, and with `y` stores as the cell attribute minsine, the smallest sine of each tet's six dihedral angles: one number that catches slivers, caps, needles and wedges (a regular tet has 0.943) (#296).
- **Input lines up to 1024 characters:** a command line can be up to 1024 characters (80 before); longer lines are refused, and a token cut at 32 characters gives a WARNING (#299).

### Fixes

#### Crashes and memory overruns

- **connect, imt:** a negative or huge imt crashed connect; it is now refused with an ERROR before anything changes, in 2D and 3D (#297, issue #240).
- **connect, small-cell pass:** delaunay.f and delaunay2d_lg.f sized a work array by the number of vertices, but the points fix_small_tets and fix_small_triangles_lg add are numbered above it, so connect wrote past the array (MMVERIFY "Array epilog overwritten"; all points on a sphere) (#316).
- **connect, hull fill on macOS:** the 3D hull fill (#320) and the 2D one resized every array of the mesh object in the middle of connect, which made test/level01/refine_recon_quality and test_createpts cp62 fail on macOS; they no longer do (#322).
- **2D connect, freed memory:** multi_material2d_lg and fix_small_triangles_lg can enlarge connect's work arrays, which moves them, and connect2d_lg went on using the old addresses (a heap-use-after-free, found with AddressSanitizer on test_createpts cp62). It was harmless while the freed memory was not reused and wrong otherwise; connect2d_lg now looks its pointers up again after both calls (#344).
- **hextotet / grid2grid:** prism to 14 (issue #214) and pyramid to 4 wrote past their arrays; options that do not fit the cells (14 on hexes, which corrupted memory; 3 on hexes, which made garbage) are now refused, and every option that adds vertices reads only inside the source arrays. New suite test_hextotet (#298).
- **recon with imt 0:** recon on a mesh with vertices of imt 0 stopped with "MMGETBLK: ERROR! FATAL Invalid length request ... matrecon"; it now accepts them (#315).
- **createpts / poisson_disk / 3d_box:** it crashed when the mesh object already existed, and on bad box defines; it now stops with an ERROR (#302).
- **read / avs and dump / avs:** bad files gave crashes or wrong meshes; they now give an ERROR. The code is restructured, and reading is about 3 times and writing about 2.5 times faster, with byte-identical output; test_avs_io grows to 32 decks (#306, #307, #308).
- **read with a path in the file name:** `read / avs / ../dir/file.inp / mo` stopped LaGriT with a Fortran runtime error (exit status 2), because the command is split at each `/` and the file name was the folder `..`. read now gives an ERROR when the file name is a folder, or when the command has more words than its format takes (avs 7, lagrit 5, gmv 5, gocad 4), reads nothing, and LaGriT goes on. File names are given without a path (conventions.md, READ.md); test_avs_io deck ak13 (#325).
- **dump / fehm's sixth token:** it was read as an FEHM .ini file unless it was -def-, so `dump / fehm / root / mo / ascii / keepatt` read a file named keepatt and stopped LaGriT; a keyword there is now a keyword (#342).
- **crush_thin_tets twice in a run:** a second crush_thin_tets in one run could write past the end of an array (a crash in Release builds) (#296).
- **cmo / copy and triangulate:** a crash in cmo / copy (issue #6) and triangulate failures (issues #239, #57) are fixed, with fixes to macOS .stor output, the Poisson sampler and createpts checks (#292).

#### Wrong results without a message

- **dump / stor and dump / pflotran at UTM coordinates:** the 3D default writer (anothermatbld3d.c) computed volumes and circumcenters from absolute coordinates (its translation to the origin had no effect) and lost every digit far from the origin: a 1 x 0.75 x 0.5 box moved by (7e5, 3.2e6, 1.5e3) gave Voronoi volumes adding up to -113.8, with 217 negative volumes. Every writer now works relative to a vertex of each cell (matbld3d_stor.f and the 2D matbld2d_stor.f, which lost about 1e-9 and 3e-7, too), so the same mesh gives the same file at the origin and at UTM coordinates. New suite test_stor_utm (#333).
- **dump / stor / ... / coefs twice in a run:** the second file was broken: the coefficient numbering went on from the first file, so the pointers pointed past the values and some values were never set (src/matrix_values_compress.c) (#333).
- **File names cut at 32 characters:** dump / stor, fehm, pflotran and tecplot cut the file name, root and extension together, without a warning: a 30-character root gave ROOT.s, and a 32-character root gave ROOT with no extension, so dump / pflotran overwrote the .stor of the same root. The names are now built in 256 characters; test_stor_utm deck st08 (#333).
- **2D connect far from the origin:** it made no triangles at UTM coordinates (and for the unit square at x = 1000 of test/level01/connect_errors), because the enclosing triangle was placed 1% of the coordinate away; it is now placed inside the box (#312).
- **2D connect on stretched domains:** it left points out (a 1000 x 1 grid: 37 of 961) and was not exactly Delaunay; exact tests fix both, and connect no longer runs recon / 0 for points in a coordinate plane (#313).
- **Exact predicates on near-ties:** near-ties below about 1e-28 of the determinant's size were taken as ties; they are now decided in integer arithmetic (src/lg_exact_int.h). In test_predicates, 341 of 40,000 2D and 147 of 20,000 3D adversarial tests were wrong before, none now (#319).
- **eltset inter and not:** with three or more sets they gave wrong results; they now give the documented result, and ELTSET2.md and PSET.md document the set logic (#301).
- **createpts / poisson_disk / 3d_box / connect:** vertices connect could not connect (dudded, itp1 = 21, in no tet) were left in the output mesh, 8 of 144,992 in a graded mesh; they are now removed with rmpoint / compress, as in 2D (#324).
- **crush_thin_tets:** every tet was measured against the size of mesh vertices 1 to 4 instead of its own, the cap case was switched off, and the pset was ignored (on a deck with a pset it changed 253 tets outside it; now 0). All are fixed; on a graded Poisson disk mesh the thin tets left below TOLCRUSH 0.1 / 0.2 / 0.3 went from 3 / 36 / 301 to 0 / 15 / 164. New suite test_crush_thin_tets (#296).
- **Angles of flat cells:** in flat or nearly flat cells rounding put a cosine just outside [-1, 1], so the dihedral and solid angle attributes (ang_mind, ang_maxd, ...) came out as their starting values (a flat tet had ang_mind 458.4); solid angles also used a short pi (a cube corner was 90.0000000006 degrees). Both are fixed; new suite test_element_angles (#296).
- **interpolate and refine:** fixes to interpolate and refine (#293).

#### Documentation and stray files

- **Links that gave 404:** 16 links in 10 pages (among them DUMP2.md's link to the dump / control_volume page) gave "404 File not found" on the web site: their text ran across two lines, and GitHub Pages turns a link to a .md page into one to its .html page only when the text is on one line (#345).
- **The web site** (#346): the front page and the manual's landing pages (Online Manual, Commands, Commands by Category, Tutorials, Release Notes, dump / control_volume) have a header band with a mesh picture; the Online Manual is a grid of section cards; the front page has an Online Manual button and a DFN's Delaunay mesh beside its Voronoi dual. errors.md has the anchors the manual's links pointed to.
- **lagrit.lanl.gov:** LaGriT's banner and help, and two messages, point to lanl.github.io/LaGriT and github.com/lanl/LaGriT instead (lagrit.lanl.gov has been shut down). The banner's Manual line is now under the GitHub line, as `Manual: https://lanl.github.io/LaGriT/pages/manual.html`.
- **TRANS.md:** it had **zero** and **center** swapped and listed a keyword **enter** that does not exist (#318).
- **Commands by Category** (commands_cat.md): RZBRICK and SETPTS, and RANKVOLUME, SORT and UPSCALE, ran together on one line each; every command is on its own line again (#323). TRIANGULATE and EXTRACT have clearer descriptions, and DEFINE is described as "assign a value to a character string" (#322, #328).
- **recon / 1 in 3D:** it wrote a debugging file `gmv_refine_edge_3d` in the current folder on every run; it no longer does (#322).
- **ADDMESH.md:** says that addmesh / add writes polygon.inp, which is kept on purpose (#338).

### Changes in behavior

Each item says what changed and, where there is one, how to get the old behavior back.

- **dump / stor, fehm, pflotran and zone_outside use cv_dual** (they used matbld). The control volumes and coefficients are the same to rounding; what changes: the values are the computed ones (not merged within 1e-8) and the small real coefficients matbld dropped are kept; in 2D, the zero-area connections matbld wrote are left out; a mesh that is not Delaunay or has parent/child chains is refused with an ERROR naming `/ matbld` (and `/ median`, the only control volumes cv_dual makes on a mesh that is not Delaunay); `hybrid` is cv_dual's, with no negative coefficients; the .stor's header lines name the control volumes and the command; flagged facets go to ROOT_violations.vtu; matbld's attributes ccoef and ij_ccoef are not made (dump / control_volume's ccoef_* keywords give them). On eight dfnWorks meshes the files agree with matbld's, every difference explained, in 0.54 to 0.81 of matbld's time and about half its memory. Old behavior: add `/ matbld` to the command (#345).

- **dump / avs writes avs3, with 17 digits:** dump / avs now writes avs3, the avs2 layout with the vertex coordinates and real attributes written with 17 significant digits (1pe24.16), so the doubles read back exactly, also at UTM coordinates (at x = 3.2e6, 13 digits moved a vertex by up to 5e-7). A vertex line is 12 characters longer (a tet mesh file about 5-10% larger); programs that read AVS files by white space (read / avs, ParaView, Python) are not affected, but a script that reads fixed columns is. att_node and att_elem write 17 digits too. Old behavior: **dump / avs2** (or **dump / avs1**) writes the old 13-digit formats. test_avs_io's golden files are rewritten (the same values), new deck av21 (#333).
- **dump / stor, pflotran and fehm write 17 digits:** the volumes and coefficients of the ASCII .stor, every number of the .uge (cell and face centers, volumes, areas), and the coordinates of the .fehmn file (and the values of the .ini file it writes) are written with 17 significant digits; at UTM coordinates the .uge centers and .fehmn coordinates were rounded to 1e-6 before. FEHM reads these list-directed and PFLOTRAN by white space, and the .stor and .ini lines stay within 120 characters; test_stor_utm deck st09. Old behavior: not available (#338).
- **dump / stor at UTM coordinates:** .stor and .uge files of meshes far from the origin change, from wrong values to right ones; near the origin they agree with V3.3.3 to about 1e-13 (#333).
- **Long file names:** dump / stor, fehm, pflotran and tecplot use the whole name, so a root of 28 or more characters now gives a file with its full name and extension, where V3.3.3 wrote a shorter name (#333).
- **connect fills the convex hull:** 2D connect adds the triangles along the convex hull (#313) and 3D connect the tets (#320), so a mesh can have more cells than before, and connect prints new lines. Old behavior: **connect / nohull** (3D).
- **connect's insertion order:** where points are exactly cocircular or cospherical (a regular grid), a square or cube may be split the other way (either is Delaunay), from the new insertion order (#313, #314, #316). Old behavior: **connect / old_order**. LaGriT's level01 references were updated: connect_errors, intersect, intrp_2D_sizes, pflotran_stor, recon1, addmesh_add, cmo_addatt_normals, connect_cube, hybrid_volume, rmpoint, setpts, smooth.
- **2D connect without recon / 0:** for points in a coordinate plane connect no longer runs recon / 0 after the triangulation, since the triangulation is already exactly Delaunay (#313). Old behavior: run **recon / 0** after connect.
- **connect's enclosing coordinates:** the coordinates of the enclosing tetrahedron or triangle are deprecated (a WARNING); fewer than 12 (3D) or 9 (2D) numbers after the point range give a WARNING and are ignored; check_interface in 2D gives a WARNING (#317).
- **createpts / poisson_disk / 3d_box, h lookup:** with a control file (a variable h) and no h_lookup, the h lookup is now the octree (it was hex_grid): far less memory and time on graded meshes, and a somewhat denser set of vertices. Without a control file (a uniform h), hex_grid stays the default and the mesh is unchanged; LaGriT says which lookup it used; test_poisson_octree decks po08 and po09. Old behavior: **h_lookup hex_grid** (#327).
- **createpts / poisson_disk / 3d_box / connect:** the vertices connect leaves out are removed, so the mesh can have a few vertices fewer than in V3.3.3 (#324).
- **read / avs needs an empty mesh object:** read / avs requires a named, empty mesh object. To join meshes, read each into its own mesh object and use **addmesh / append** (#306).
- **Input lines:** lines longer than 1024 characters are refused; before, characters past column 80 were dropped without a warning (#299).
- **eltset inter and not with three or more sets:** they give the documented result, which differs from V3.3.3's (#301).
- **crush_thin_tets:** with the fixes above it removes more thin tets (on a graded Poisson disk mesh, those left below TOLCRUSH 0.3 went from 301 to 164; on a uniform one from 27 to 25) and leaves the tets outside a pset alone; the result is valid, with the volume unchanged (#296).
- **Angle attributes:** solid angles change by about 5.6e-10 degrees (full-precision pi), and flat cells get real angles instead of the starting values (#296).
- **hextotet on a quad mesh with no option** splits each quad into 2 triangles, as documented; V3.3.3 used 24 (#298). Old behavior: give the option, **hextotet / 24 / ...**.
- **cmo / printatt** prints attribute names in full, up to 32 characters (it cut them to 17). With **minmax** a name longer than 17 characters is on a line of its own, its numbers on the next line in the same columns; names of up to 17 characters print as before (#347).
- **Code comments and messages** that described the earlier behavior as "V3.3.4 and earlier" now say "V3.3.3 and earlier" (the last released version); "V3.3.4" was never released.

### Known issues

- **dump / stor with matbld drops small coefficients with their volume shares:** dump / stor / ... / matbld leaves out connections whose coefficient is below compress_eps (1e-8) times the largest, and the control volumes of their vertices lose those connections' shares. On random points in a box (test_stor_utm st04, 308 vertices) 8 of 2,017 connections are left out and 15 volumes change, by up to 3e-5 of the vertex's volume (the volumes add up to 0.50000057 instead of 0.5). To keep them, give the mesh object a smaller compress_eps before dump / stor, or use cv_dual, the default, which keeps every connection (DUMP3.md):

  ```
  cmo / addatt / mo / compress_eps / REAL / scalar / scalar / constant / permanent / / 1.0e-14
  ```

- **matbld (named) on meshes cv_dual refuses:** on a mesh that is not Delaunay dump / stor (matbld) writes Voronoi coefficients without a warning, and on a mesh with parent/child chains it prints "Error: There are parential issues involved!!!!!!!" for many vertices and writes a .stor with zero volumes, which LaGriT does not count as an error. With `cv_dual` both are refused with an ERROR (cv_dual_vs_matbld.md).
- **matbld's hybrid leaves negative coefficients:** on test_poisson_3d's mesh, 43 of 75 negative coefficients remain with `dump / stor / ... / hybrid`; cv_dual's hybrid leaves none (cv_dual_vs_matbld.md).
- **dump / stor / ... / none in 3D** (matbld3d_stor) writes |A|/d, so negative coefficients come out positive.
- **The graph-compressed .stor** (matbld3d_gstor) has no title lines.
- **TOUGH MESH:** cv_dual writes the AHTX column equal to the volume, as lanl/voronoi did.
- **3D connect without exact on nearly regular grids** (a grid jittered by 1e-6 to 1e-9): a few vertices can be dudded (2 to 4 of 3,375, also in V3.3.3), or many points added by the small-tet pass (419 for 1e-9); V3.3.3 made no tets at all for 1e-9 and crashed for one 1e-6 grid (the work-array overrun fixed in #316). The tolerance in-sphere test decides these near-ties; **connect / exact** avoids them, but on input meant to be cospherical and coplanar (the createpts rtp sphere) its Delaunay tets can be flat to round-off.
- **3D connect without exact is not exactly Delaunay** where points are nearly cospherical (a grid jittered by 1e-9: 2,977 faces).
- **2D connect of points in a tilted plane** still uses the tolerance tests and recon / 0.
- **Links to lagrit.lanl.gov:** lagrit.lanl.gov has been shut down, and links to it in the documentation pages, old test outputs and the front page's Contact Us button are broken. Use https://lanl.github.io/LaGriT and https://github.com/lanl/LaGriT/issues.
- **recon in 3D:** recon / 1 does not bring the Voronoi centers inside the boundary and leaves the mesh far from Delaunay, it breaks parent/child chains with two materials, and recon / 0 cannot flip every face to Delaunay (test_recon; see RECON.md).

### Build and tests

- **test/suite:** a new test suite with independent checks and a results page per suite, and CI that runs it (#294). `sh test/suite/run_all.sh` runs every suite, and the level01 tests run inside it as test_lagrit_upstream; 43 suites in this release.
- **Suites added since #294:** test_avs_io (#305), test_hextotet (#298), test_set_logic (#301), test_edge_lengths (#300), test_input_lines (#299), test_improve_tets, test_element_angles and test_crush_thin_tets (#296), test_connect_2d (#313), test_connect_options (#317), test_predicates and test_connect_exact (#319), test_connect_hull (#320), test_recon (#322), test_poisson_octree (#303), test_distance_field (#328), test_stor_utm (#333), 16 test_cv_dual_* suites (#339 to #343).
- **Results pages:** each suite's page is `summary.html` in the suite's own folder (it was `logs/summary.html`, which now forwards to it), linked from `test/suite/summary.html`. Every page opens with a "What it does" paragraph and describes each test, and every test that writes a mesh or a vertex set shows a picture of it (new pictures: test_avs_io, test_element_angles, test_improve_tets, test_set_logic, test_connect_options, test_connect_exact, test_connect_hull, test_predicates). A new page for test_poisson_command_errors shows LaGriT's message for each deck (#320).
- **run_all.sh options:** **JOBS=N** runs N suites at a time (longest first); **ONLY=tag,...** runs the suites with those tags (connect, poisson, 2d, 3d, ...); **QUICK=1** leaves out the slow ones; **CHANGED=1** (or `CHANGED=<branch>`) runs only the suites a change can affect, from a map of the source files each suite executes (`suite_sources.txt`, made with a coverage build by `make_suite_sources.py`) (#322).
- **CI:** GitHub runs the suites on Linux (gcc 13 and 14) and macOS for every pull request and every push to master, 4 suites at a time (3 on macOS), and keeps every suite page in its artifact (#294).
- **CMake Exodus option:** CMake can build LaGriT with Exodus; src/fc_mangle.h is generated by CMake and no longer tracked (#295).
- **The version:** CMake's project version is 3.4.0, and LaGriT's banner says V3.4.0.
- **.mailmap:** git shortlog and GitHub show one name and email per contributor.
- **Compiler fixes:** a clang flag, -Wno-error=implicit-int (#260), and fixes for compiler issues (#264).

### Log summary

Pull requests merged since V3.3.3 (dependabot updates of the documentation site not listed):

| Date | PR | Title |
| --- | --- | --- |
| 2026-11-20 | #347 | cmo / printatt: attribute names in full, up to 32 characters |
| 2026-11-20 | #346 | docs: header band and section cards for the manual landing pages; new front-page band |
| 2026-11-20 | #345 | dump / stor, fehm, pflotran, zone_outside: cv_dual by default; dump / control_volume ccoef_* keywords |
| 2026-11-18 | #344 | connect (2D): look the work arrays up again after they may move |
| 2026-11-18 | #343 | cv_dual and matbld compared: suite test_cv_dual_vs_matbld and a report |
| 2026-11-17 | #342 | dump / stor, fehm, pflotran, zone_outside: keyword cv_dual \| matbld |
| 2026-11-17 | #341 | cv_dual test pages and manual: commands in grey scrolling boxes |
| 2026-11-17 | #340 | dump / control_volume: cv_dual on a mesh object (attribute_cv_volume); suite test_cv_dual_control_volume |
| 2026-11-17 | #339 | cv_dual (formerly del_vor) in LaGriT, with its history: src/cv_dual, the program build/cv_dual, 13 suites test_cv_dual_* |
| 2026-11-15 | #338 | dump / stor, pflotran, fehm: 17 significant digits; ADDMESH.md polygon.inp |
| 2026-11-10 | #333 | dump / stor right at UTM coordinates; whole file names; dump / avs with 17 digits (avs3) |
| 2026-11-09 | #328 | compute / distance_field: facet option, the exact distance to cells |
| 2026-11-06 | #327 | poisson_disk 3d_box: h_lookup octree by default with a control file |
| 2026-11-06 | #326 | Tutorial: tet mesh graded by a surface's edge lengths |
| 2026-11-06 | #325 | read: an ERROR, not a crash, for a file name that is a folder or a path |
| 2026-11-06 | #324 | poisson_disk 3d_box connect: remove the vertices connect leaves out |
| 2026-11-06 | #323 | commands_cat: one command per line |
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

#303 (h_lookup octree), #306 to #308 (read / dump avs) and #321 (run_all.sh options, test_recon) were merged into other branches and reached master in #322. The list comes from:

```
git log --merges --format="%ad %s %b" --date=short V3.3.3..V3.4.0
```
