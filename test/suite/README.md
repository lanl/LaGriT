# del_vor test suite

Tests that judge del_vor's results against answers worked out
independently, not only against a stored copy of an earlier output.
Each suite is a folder with its own `00README`, runner, checker and a
results page with pictures. The layout follows LaGriT's `test/suite`.

## Quick start

From this folder, after building del_vor in `build/` at the top of the
checkout:

```
sh run_all.sh                 # every suite but test_timing, a few seconds
JOBS=4 sh run_all.sh          # the same, 4 suites at a time
ONLY=gold sh run_all.sh       # the suites tagged gold (or a name)
ONLY=timing sh run_all.sh     # test_timing (development; see its 00README)
sh run_all.sh /path/to/del_vor
```

Then open `summary.html` in this folder: one row per suite, linking to
each suite's own page. Every runner can also be run alone from its
folder (for example `sh test_kernels/run_kernel_tests.sh`) and writes
`summary.html` there.

Each runner uses `build/del_vor` of this checkout if it exists, else
`del_vor` on `PATH`, unless a path is given as the first argument.
`test_kernels` runs `del_vor_selftest`, built next to `del_vor`.
Requires: a POSIX shell and Python 3 (standard library only).

## The suites

| suite | what it tests | time |
|---|---|---|
| `test_kernels` | the triangle and tet kernels against independent formulas (2000 random cells): cotangent formula in 2D, a right-triangle formula and perpendicular pieces in 3D, vertex shares that add up to the cell for any dual points, the same tet at UTM-like coordinates, closed dual cells on three meshes | 1 s |
| `test_voronoi_gold` | the three lanl/voronoi test meshes written as `.stor` (every edge kept, compressed or not; the defaults), `.uge` and TOUGH2 `MESH`, against LaGriT's and voronoi's gold files, the `.stor` layout, the volume balance, the zero-area connections left out, and the formats against each other | 1 s |
| `test_options` | the keyword options (outputs, `zero_area`, `zero_tol`, `compress`, `cv`, `delaunay_tol`, `violations`, `delaunay_fatal`, `check_only`, `report`, `timing`, lanl/voronoi's `-type -o -compress`) and refused commands, 39 runs | 1 s |
| `test_poisson_lagrit` | five Poisson disk meshes from LaGriT's suites (`test_poisson_3d` cases 1 and 2, the three `test_poisson_2d` meshes) against the `.stor` files LaGriT wrote: coefficients, volumes, negative and zero-area connections, the exact volume | 2 s |
| `test_validity` | the mesh checks made before any geometry: 11 meshes that each break one rule (cell types, positive size, conformity, orientation) must stop with the right message and write nothing; 6 legal but unusual meshes must run (LaGriT's wrapped vertex data; CR LF, tabs and D exponents) | 1 s |
| `test_delaunay` | the Delaunay check against exact rational arithmetic on 17 runs: ties, violations (flipped edges and faces), a near tie (a warning; fatal with `delaunay_fatal` 1e-10; a tie with a larger `delaunay_tol`), boundary circumcenters outside, UTM coordinates, the violations file facet by facet | 3 s |
| `test_dfn` | fracture networks: intersection edges, orientation fracture by fracture, every coefficient and area against the cotangent formula, a turned-over triangle caught; Carl's 4-fracture dfnWorks example (60 intersection edges) | 2 s |
| `test_parity` | del_vor's `.stor` against LaGriT's `dump / stor` on 14 meshes (2D, 3D, DFN, two materials, UTM coordinates; 15 with dfnWorks TPL_nonuniform outside git); `tools/stor_compare.py` must explain every difference | 10 s |
| `test_timing` | for development, not run by default (`ONLY=timing sh run_all.sh`): del_vor's time per step and peak memory on meshes of growing size made by LaGriT, the log-log slope (1 = linear), and LaGriT's `dump / stor` on the same meshes | minutes |

## Conventions

Every runner
- takes the del_vor path as its only argument (default above);
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
`logs/`, `summary.html`, `run_all.log`) is listed in `.gitignore`.
