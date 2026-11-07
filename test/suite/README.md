# del_vor test suite

Tests that judge del_vor's results against answers worked out
independently, not only against a stored copy of an earlier output.
Each suite is a folder with its own `00README`, runner, checker and a
results page with pictures. The layout follows LaGriT's `test/suite`.

## Quick start

From this folder, after building del_vor in `build/` at the top of the
checkout:

```
sh run_all.sh                 # every suite, a few seconds
JOBS=2 sh run_all.sh          # the same, 2 suites at a time
ONLY=gold sh run_all.sh       # the suites tagged gold (or a name)
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
| `test_voronoi_gold` | the three lanl/voronoi test meshes written as `.stor` (plain and compressed), `.uge` and TOUGH2 `MESH`, against LaGriT's and voronoi's gold files, the `.stor` layout, the volume balance and the formats against each other | 1 s |

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
