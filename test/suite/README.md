# LaGriT test suite

Tests that run LaGriT decks and judge the results against answers worked out
independently (exact spacing, exact interpolated values, documented point
distributions, valid and Delaunay meshes), not only against a stored copy of
an earlier output. Each suite is a folder with its own `00README`, runner,
checker and a results page with pictures.

This suite is meant to replace the older `test/level01`–`level03` tests run by
`test/runtests.py`. Until every level01 test has been converted, the suite
`test_lagrit_upstream` runs all of level01 exactly as `runtests.py` judges it
(plus checks for crashes, unfinished runs and one-line differences), so
nothing is lost in the meantime.

## Quick start

From this folder, after building LaGriT in `build/` at the top of the
checkout:

```
sh run_all.sh                 # every suite, about 5 to 11 minutes
JOBS=4 sh run_all.sh          # the same, 4 suites at a time
BIG=1 sh run_all.sh           # also the slow parts, about 10 minutes more
sh run_all.sh /path/to/lagrit # a different LaGriT
```

Then open `summary.html` in this folder: one row per suite, linking to each
suite's own page. Every runner can also be run alone from its folder (for
example `sh test_triangulate_2d_polygon/run_triangulate_tests.sh`) and writes
`summary.html` there, in the suite's folder (the checker writes it in `logs/`
and `lift_summary.py`, run by every runner on exit, moves it up one folder,
fixing its links into `logs/`; `logs/summary.html` is left as a page that
forwards to it).

Each runner uses `build/lagrit` of this checkout if it exists, else `lagrit`
on `PATH`, unless a path is given as the first argument. Requires: a POSIX
shell and Python 3 (standard library only).

Run `BIG=1 sh run_all.sh` after any change to `src/triangulate_lg.f` (tri21,
40,004 vertices) or to connect (the connect timing suite).

## Running less: side by side, by group, by what changed

During development most changes can affect only a few suites. Three settings
(they combine) make `run_all.sh` run less, or faster:

```
JOBS=4 sh run_all.sh             # 4 suites at a time (longest first); about
                                 # a third of the time on a 4-core machine
ONLY=connect sh run_all.sh       # the suites tagged connect (or a name: ONLY=test_recon)
ONLY=2d,quality sh run_all.sh    # any of several tags
QUICK=1 sh run_all.sh            # leave out the slow suites (20 s or more)
CHANGED=1 sh run_all.sh          # only the suites what changed since master can affect
CHANGED=connect3d-hull JOBS=4 sh run_all.sh   # ... since another branch or commit
```

**Tags** (in the `SUITES` list of `run_all.sh`): `poisson`, `createpts`,
`triangulate`, `connect`, `recon`, `interpolate`, `quality`, `io`, `sets`,
`parser`, `upstream`, `errors`, `2d`, `3d`; `slow` (QUICK=1 leaves these out)
and `big` (BIG=1 only).

**CHANGED** compares the working tree (committed or not, and new files) with
the base, and `select_changed.py` picks the suites:

- a file in `src/` selects the suites whose runs execute at least one line of
  it, from `suite_sources.txt` (and suites that compile it themselves, like
  test_predicates); an include file selects the suites of every file that
  includes it;
- a file in a suite's folder selects that suite, and any suite that uses it
  (the letters suite uses test_poisson_3d's checker);
- a change to `test/level01` selects test_lagrit_upstream;
- docs, `.md` files, `.github`, PyLaGriT and examples select nothing;
- anything else (CMake files, a source file the map has never seen,
  `run_all.sh` and the other shared scripts) runs everything.

The run prints the reason for each choice. `suite_sources.txt` is made by
running every suite on a LaGriT built with coverage counting (gcc and
gfortran): see the top of `make_suite_sources.py` (about 5 minutes on Linux).
Make it again now and then, and after adding a suite (until then the new
suite always runs). Before a pull request, run everything once: `JOBS=4 sh
run_all.sh`.

## The suites

| suite | what it tests | time |
|---|---|---|
| `test_poisson_command_errors` | `createpts / poisson_disk`: 31 bad commands and 6 warning cases, each reported clearly, no crash | 5 s |
| `test_poisson_2d` | the original 2D Poisson deck: uniform and variable h; spacing, h tracking, triangle quality | 6 s |
| `test_poisson_2d_shapes` | square at four h, hexagon, circle, ellipses: spacing, containment, quality | 4 s |
| `test_poisson_nonconvex` | sawtooth outlines across a sweep of h: where the clip stops reproducing the notches (stress test; some degraded results expected) | 7 s |
| `test_poisson_3d` | the original 3D Poisson deck (reduced box): spacing, tet quality, `.stor` coefficients | 60 s |
| `test_poisson_3d_refine_letters` | a 3D box graded from h = 0.16 at the letters "LaGriT" to 1.0 | 45 s |
| `test_poisson_sampler` | the 2D and 3D Poisson disk samplers against the exact h field (uniform, linear, narrow band; 3 seeds) | 45 s |
| `test_poisson_octree` | `3d_box` with `h_lookup octree`: spacing against the exact h, maximality, the h field in batches, seeds, `octree_k_factor`, the octree file read back, octree against `hex_grid` (time, memory, tet angles) | 15 s |
| `test_createpts` | `createpts` xyz, rtz, rtp, line, brick and error cases against the documentation | 30 s |
| `test_triangulate_2d_polygon` | `triangulate` on 41 polygons: collinear runs, dense boundaries, near-touching geometry, far-from-origin coordinates, combs, spirals, holes | 20 s |
| `test_connect_2d` | 2D `connect`: the same 400 points at the origin, at UTM-sized and negative offsets and in the yz and xz planes, a unit square at x = 1000, graded, stretched, round-off and cocircular point sets; every point connected, the same triangles everywhere, exactly Delaunay and covering the convex hull (exact rational checks) | 5 s |
| `test_lagrit_upstream` | all 43 `test/level01` tests, judged as `runtests.py` does, plus crash and one-line-difference checks (`LEVEL2=1`: the Exodus tests too) | 30 s |
| `test_interpolate_compute` | `interpolate` voronoi / continuous / map and `compute` distance fields against independent answers, for every source cell type; keepatt with dudded vertices; points on edges and faces | 10 s |
| `test_distance_field` | `compute / distance_field` with the new `facet` option (exact distance to triangles, line segments, quads) against brute force on a triangle, spheres, a warped quad surface, a helix and a mixed mesh; the vertex option unchanged; bad sources refused | 30 s |
| `test_crush_thin_tets` | `crush_thin_tets` on planted needle, wedge, cap and sliver tets (inside and on the boundary), a jittered lattice and 3D Poisson disk meshes: quality before, trip by trip and after, and why each thin tet left survives | 15 s |
| `test_element_angles` | `cmo/addatt` dihedral and solid angle attributes (`ang_mind` ... `ang_maxr_solid`) and `quality` aspect / edge_ratio / edge_min / edge_max on 213 tets of known shape, very thin slivers, hex, prism, pyramid, triangle and quad, against an independent computation | 1 s |
| `test_improve_tets` | the new command `improve_tets` (improve the worst tet quality by smoothing and flips, vertex set kept) on uniform and graded 3D Poisson disk meshes: quality, validity, the box, the resolution, fixed boundary, pset, materials, refused commands | 6 s |
| `test_avs_io` | `read / avs` and `dump / avs`: every cell type, hybrid meshes, 0-based numbering, comments, each attribute section, the dump options; meshes compared with their files, dumps byte for byte with `golden/`; errors and warnings for bad files | 2 s |
| `test_stor_utm` | `dump / stor` (every writer: the 3D default with compression all, graph and hybrid; the older `matbld3d_stor` with none and coefs; 2D) and `dump / pflotran` on the same mesh at the origin and at UTM coordinates (7e5, 3.2e6, 1.5e3): the files agree, the Voronoi volumes add up to the cells' volume, and match an independent computation; file-name roots of up to 32 characters keep their extensions | 2 s |
| `test_hextotet` | `hextotet` and `grid2grid`: every option on hex, prism, quad and pyramid meshes (issue #214), defaults, and options that do not fit the cells refused | 1 s |
| `test_set_logic` | `eltset` and `pset` union / inter / not with 1 to 4 sets, every member against the definitions | 1 s |
| `test_edge_lengths` | `cmo / addatt` edge_lengths_min / max / mean against an independent computation | 1 s |
| `test_input_lines` | input lines up to 1024 characters, longer lines refused, cut tokens warned | 2 s |
| `test_connect_options` | `connect` options in 2D and 3D: `noadd`, `check_interface`, `old_order` in any position, the point range with or without `delaunay`, the deprecated enclosing tet / triangle coordinates | 2 s |
| `test_connect_exact` | 3D `connect` and `connect / exact` on grids, grids jittered by 1e-12 to 1e-3, random points, a thin box and an rtp sphere; every mesh checked with exact rational arithmetic (Delaunay, valid, nothing added or missing) | 25 s |
| `test_predicates` | the exact orientation, in-circle and in-sphere tests of 2D and 3D connect against rational arithmetic on adversarial inputs (compiles two small C drivers; no lagrit) | 10 s |
| `test_connect_hull` | 3D `connect` fills to the convex hull: random, graded, thin, UTM-sized and jittered-grid point sets with plain connect, `exact` and `nohull`; validity, the hull volume covered (an independent hull), exact Delaunay | 20 s |
| `test_recon` | `recon / 0` and `recon / 1` in 2D and 3D, one and two materials: Delaunay after the flips, obtuse boundary angles, where added vertices go, the boundary, the area or volume of each material (exact rational checks); KNOWN: `recon / 1` in 3D breaks Delaunay and writes a stray GMV file, a mesh without parent / child vertices stops LaGriT | 10 s |
| `test_connect_timing` | `BIG=1` only: connect time against vertex count for four vertex orders; every mesh checked | 5 min |

Times are for a Release build on a recent Mac.

## Conventions

Every runner
- takes the LaGriT path as its only argument (default above);
- deletes every output it expects before running, so a stale file can never
  pass as a new result;
- keeps each run's stdout, `lagrit.log` and `lagrit.out` under `logs/`;
- exits nonzero on any FAIL.

Every checker
- is Python 3, standard library only;
- prints one `key value` line per quantity;
- separates HARD checks, which decide PASS or FAIL, from MEASURED quantities,
  which are reported but not judged;
- defines each measure in its opening comment.

Results: PASS, FAIL, and in some suites KNOWN (a documented failure; the deck
says why in `# known:` lines) and FIXED (a KNOWN deck that now passes: the
run fails until its `# known:` lines are removed), REFUSED (LaGriT correctly
rejects the input) and REPORTED (an open question, not judged).

Meshes have VERTICES and CELLS. Older text in the Poisson suites still says
nodes and elements.

## Adding a test

Copy a deck in the suite closest to what you are testing, change it, and add
its expected result the way that suite's `00README` describes (in several
suites, header lines at the top of the deck say what to check). Prefer an
answer the checker can compute itself over a stored output. Generated inputs
come from the suite's `make_*.py` script with fixed seeds, so they are the
same on every machine.

## Not run by run_all.sh

`test_poisson_3d_refine_letters/timing/time_letters.sh` is not a test: it
splits the letters demo's run time into field build, sampling and connect.

## What git keeps

Only decks, inputs, scripts and READMEs. Everything a run writes (`work/`,
`logs/`, pictures, `summary.html`, `run_all.log`) is listed in `.gitignore`.
