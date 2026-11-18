# cv_dual's test suites

Tests that judge cv_dual's results against answers worked out
independently, not only against a stored copy of an earlier output.
Each suite is a folder `test/suite/test_cv_dual_*` with its own
`00README`, runner, checker and a results page with pictures, in the
layout of LaGriT's suites. This folder (`cv_dual_tools`) holds what
they share: `pagekit.py` (the results pages), `svgdraw.py` (the
pictures), `exact_check.py` (the exact Delaunay oracle),
`cv_dual_selftest.F90` (the kernel checks) and `stor_compare.py`.

## Quick start

LaGriT's `run_all.sh` runs them, from `test/suite`, after building
LaGriT in `build/` at the top of the checkout (cv_dual is built with
it):

```
ONLY=cv_dual JOBS=6 sh run_all.sh    # cv_dual's 13 suites, about 15 s
JOBS=6 sh run_all.sh                 # with every LaGriT suite
BIG=1 ONLY=test_cv_dual_timing sh run_all.sh   # timing (development)
```

Then open `test/suite/summary.html`: one row per suite, linking to
each suite's own page. Every runner can also be run alone from its
folder (for example `sh test_cv_dual_kernels/run_kernel_tests.sh`) and
writes `summary.html` there.

Each runner uses `build/cv_dual` of this checkout if it exists, else
`cv_dual` on `PATH`, unless a path is given as the first argument;
given LaGriT's `lagrit` (as `run_all.sh` passes it), it uses the
`cv_dual` beside it.
`test_cv_dual_kernels` runs `cv_dual_selftest` and `test_cv_dual_entry` runs
`cv_dual_entry_test`, both built next to `cv_dual`.
Requires: a POSIX shell and Python 3 (standard library only).

## The suites

| suite | what it tests | time |
|---|---|---|
| `test_cv_dual_kernels` | the triangle and tet kernels against independent formulas (2000 random cells): cotangent formula in 2D, a right-triangle formula and perpendicular pieces in 3D, vertex shares that add up to the cell for any dual points, the same tet at UTM-like coordinates, closed dual cells on three meshes | 1 s |
| `test_cv_dual_voronoi_gold` | the three lanl/voronoi test meshes written as `.stor` (every edge kept, compressed or not; the defaults), `.uge` and TOUGH `MESH`, against LaGriT's and voronoi's gold files, the `.stor` layout, the volume balance, the zero-area connections left out, and the formats against each other | 1 s |
| `test_cv_dual_options` | the keyword options (outputs, `zero_area`, `zero_tol`, `compress`, `cv`, `alpha`, `hybrid_tol`, `alpha_min`, `hybrid_iter`, `alpha_file`, `delaunay_tol`, `violations`, `delaunay_fatal`, `check_only`, `report`, `timing`, `paraview` and its options, lanl/voronoi's `-type -o -compress`) the vertex material names, `matrix`, `matrix_bins`, the `.stor` header and the options line of the report, and refused commands, 70 runs | 1 s |
| `test_cv_dual_poisson_lagrit` | five Poisson disk meshes from LaGriT's suites (`test_poisson_3d` cases 1 and 2, the three `test_poisson_2d` meshes) against the `.stor` files LaGriT wrote: coefficients, volumes, negative and zero-area connections, the exact volume | 2 s |
| `test_cv_dual_validity` | the mesh checks made before any geometry: 12 meshes that each break one rule (cell types, positive size, conformity, orientation, parent/child chains) must stop with the right message and write nothing; 6 legal but unusual meshes must run (LaGriT's wrapped vertex data; CR LF, tabs and D exponents) | 1 s |
| `test_cv_dual_delaunay` | the Delaunay check against exact rational arithmetic on 17 runs: ties, violations (flipped edges and faces), a near tie (a warning; fatal with `delaunay_fatal` 1e-10; a tie with a larger `delaunay_tol`), boundary circumcenters outside, UTM coordinates, the violations file facet by facet | 3 s |
| `test_cv_dual_dfn` | fracture networks: intersection edges, orientation fracture by fracture, every coefficient and area against the cotangent formula, a turned-over triangle caught; the 4-fracture dfnWorks example 4_user_rects_uniform (60 intersection edges) | 2 s |
| `test_cv_dual_parity` | cv_dual's `.stor` against LaGriT's `dump / stor` on 14 meshes (2D, 3D, DFN, two materials, UTM coordinates; 15 with dfnWorks TPL_nonuniform outside git); `tools/stor_compare.py` must explain every difference | 10 s |
| `test_cv_dual_policy` | the control-volume policies (`cv median`, `blend`, `hybrid`) on 27 runs: every coefficient and volume against the suite's own Python dual geometry; the hybrid leaves no negative coefficient (LaGriT's Poisson mesh, random points in a square and a cube, a fracture network) and keeps alpha = 1 away from the boundary; the dual faces close; median volumes exact; blend linear in 2D; UTM coordinates; median on non-Delaunay meshes (LaGriT's 3-material hex-to-tet demo) | 8 s |
| `test_cv_dual_matrix` | the matrix file (`-matrix FILE`, `-matrix_bins N`) read back: one point per `.stor` entry at (j, -i) in the selected rows, both halves and the diagonal; every value against the `.stor` and the mesh (coefficient, distance, area, class, flags, materials, alpha from the mesh file, angle from the faces file); the report's bandwidth and profile; the image's pixels against the points | 3 s |
| `test_cv_dual_entry` | the LaGriT entry point (`cv_dual_lagrit_entry`) called by a harness the way LaGriT will (fixed-form Fortran with LaGriT's flags, no interface, the mesh arrays in place): every file, the screen output (through the log hook) and the returned volumes equal the program's; bad options, bad meshes, parent/child chains and arrays in the wrong layout come back in `ierr` with a message, write nothing, and the next call works | 2 s |
| `test_cv_dual_control_volume` | LaGriT's command `dump / control_volume` on meshes LaGriT makes or reads (tets, 2D triangles, a fracture network, two materials, Poisson meshes): every file equals the cv_dual program's on the mesh the deck wrote, the `.stor`'s second line ends with the command, cv_dual's lines reach LaGriT's log, `attribute_cv_volume` holds the `.stor`'s volumes; bad commands, a hex mesh, a mesh that is not Delaunay and parent/child chains give an ERROR, write nothing, and LaGriT goes on | 2 s |
| `test_cv_dual_dump_keyword` | the keyword `cv_dual` / `matbld` on `dump / stor`, `fehm`, `pflotran` and `zone_outside`: each `.stor` and `.uge` equals the program's; the geometry and zone files equal matbld's; the boundary areas (`_outside_vor`, `_med`, `_hyb.area`) equal the program's `outside_area` list, matbld's for Voronoi and median, and add up to each side of a box; fallbacks to matbld and errors | 2 s |
| `test_cv_dual_vs_matbld` | LaGriT's own decks that write `.stor`, `.uge` and FEHM files (level01, level03, `test_stor_utm`), run with matbld and with `cv_dual`: every difference between the two runs' files explained (`stor_compare`'s labels), hybrids' negative coefficients counted, meshes cv_dual refuses listed | 4 s |
| `test_cv_dual_paraview` | the ParaView files (`-paraview ROOT`) read back by the suite's own reader (`vtu_read.py`): every control volume (polygon in 2D and fracture networks, polyhedron in 3D) closed, outward, with the `.stor` volume measured from its own faces; Voronoi faces on their bisector planes; the subsets; the dual faces' areas; the negative coefficients; both polyhedron layouts, text and binary | 5 s |
| `test_cv_dual_timing` | for development, only with `BIG=1` (tag `big`): cv_dual's time per step and peak memory on meshes of growing size made by LaGriT, the log-log slope (1 = linear), and LaGriT's `dump / stor` on the same meshes | minutes |

## Conventions

Every runner
- takes the cv_dual path as its only argument (default above);
- deletes every output it expects before running, so a stale file can
  never pass as a new result;
- keeps each run's screen output under `logs/`;
- exits nonzero on any FAIL.

Every checker
- is Python 3, standard library only, run with `python3 -I`;
- prints one `key value result` line per quantity;
- separates HARD checks (PASS or FAIL) from MEASURED quantities, which
  are reported but not judged;
- defines each measure in its opening comment;
- draws its pictures as hand-written SVG (`svgdraw.py`), each showing
  the mesh or cell tested and what is measured on it.

Meshes have VERTICES and CELLS.

## What git keeps

Only inputs, scripts and READMEs. Everything a run writes (`work/`,
`logs/`, `summary.html`) is listed in LaGriT's `test/suite/.gitignore`.
