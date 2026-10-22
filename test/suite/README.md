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
sh run_all.sh                 # every suite, about 3 to 5 minutes
BIG=1 sh run_all.sh           # also the slow parts, about 10 minutes more
sh run_all.sh /path/to/lagrit # a different LaGriT
```

Then open `summary.html` in this folder: one row per suite, linking to each
suite's own page. Every runner can also be run alone from its folder (for
example `sh test_triangulate_2d_polygon/run_triangulate_tests.sh`) and writes
`logs/summary.html` there.

Each runner uses `build/lagrit` of this checkout if it exists, else `lagrit`
on `PATH`, unless a path is given as the first argument. Requires: a POSIX
shell and Python 3 (standard library only).

Run `BIG=1 sh run_all.sh` after any change to `src/triangulate_lg.f` (tri21,
40,004 vertices) or to connect (the connect timing suite).

## The suites

| suite | what it tests | time |
|---|---|---|
| `test_poisson_command_errors` | `createpts / poisson_disk`: 19 bad commands and 4 warning cases, each reported clearly, no crash | 4 s |
| `test_poisson_2d` | the original 2D Poisson deck: uniform and variable h; spacing, h tracking, triangle quality | 6 s |
| `test_poisson_2d_shapes` | square at four h, hexagon, circle, ellipses: spacing, containment, quality | 4 s |
| `test_poisson_nonconvex` | sawtooth outlines across a sweep of h: where the clip stops reproducing the notches (stress test; some degraded results expected) | 7 s |
| `test_poisson_3d` | the original 3D Poisson deck (reduced box): spacing, tet quality, `.stor` coefficients | 60 s |
| `test_poisson_3d_refine_letters` | a 3D box graded from h = 0.16 at the letters "LaGriT" to 1.0 | 45 s |
| `test_poisson_sampler` | the 2D and 3D Poisson disk samplers against the exact h field (uniform, linear, narrow band; 3 seeds) | 45 s |
| `test_createpts` | `createpts` xyz, rtz, rtp, line, brick and error cases against the documentation | 30 s |
| `test_triangulate_2d_polygon` | `triangulate` on 41 polygons: collinear runs, dense boundaries, near-touching geometry, far-from-origin coordinates, combs, spirals, holes | 20 s |
| `test_lagrit_upstream` | all 43 `test/level01` tests, judged as `runtests.py` does, plus crash and one-line-difference checks (`LEVEL2=1`: the Exodus tests too) | 30 s |
| `test_interpolate_compute` | `interpolate` voronoi / continuous / map and `compute` distance fields against independent answers, for every source cell type; keepatt with dudded vertices; points on edges and faces | 10 s |
| `test_crush_thin_tets` | `crush_thin_tets` on planted needle, wedge, cap and sliver tets (inside and on the boundary), a jittered lattice and 3D Poisson disk meshes: quality before, trip by trip and after, and why each thin tet left survives | 15 s |
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
