# cv_dual

Voronoi control volumes from Delaunay meshes.

cv_dual reads a triangle mesh (2D, including fracture networks in 3D)
or a tet mesh (3D) and computes the Voronoi control-volume geometry:
the volume of each vertex's control volume, the area of each Voronoi
face and the connection distances. These are the geometric
coefficients of two-point-flux finite-volume codes, written as FEHM
`.stor`, PFLOTRAN `.uge` and TOUGH `MESH` files.

One core library, two front ends: the standalone program `cv_dual`
(AVS UCD `.inp` input) and the LaGriT entry point
`cv_dual_lagrit_entry`, which takes a mesh object's arrays (the
command `dump / control_volume`, next). It is meant to replace
`matbld2d` and `matbld3d` in LaGriT later.

cv_dual was developed as del_vor (Carl Gable, November 2026) in
[github.com/cwgable/del_vor](https://github.com/cwgable/del_vor), now
archived; its whole history is in LaGriT's (`git log --follow` on any
of its files). Where del_vor's notes say `del_vor_`, read `cv_dual_`.

## Status

Phase 6b of 6: in LaGriT. Phase 1: the lanl/voronoi kernels and writers ported
to one edge-based sparse structure, without PETSc or MPI, with
keyword options for the outputs, zero-area connections and
compression. Phase 2: the mesh is checked before any geometry (cell
types, positive size, conformity, orientation) and tested for the
Delaunay property; facets that fail are written for ParaView.

Phase 5: ParaView files (`-paraview ROOT`): the control volumes as
polygons (2D, one per vertex per fracture in a network) or closed
polyhedra (3D), drawn from the dual points the coefficients came
from, with the mesh, the negative coefficients and optionally the
dual faces; subsets; binary or text; VTK's new or legacy polyhedron
layout. The coefficient matrix itself can be written for ParaView
(`-matrix FILE`): a point per `.stor` entry at (j, -i), with its
values, and with `-matrix_bins N` an N x N image of the whole matrix.
Fracture networks (triangles of several itetclr crossing along shared
edges) are handled. Phase 3: `test/suite/cv_dual_tools/stor_compare.py` compares any two
`.stor` files and explains every difference; cv_dual agrees with
LaGriT's `dump / stor` on 14 meshes and dfnWorks networks; a faster
reader; timing of every step (linear in the mesh size up to 10
million cells; 2.4 times faster than LaGriT's read + `dump / stor`
there, with 1.5 to 2.2 times less memory); slightly non-Delaunay
facets (nearly co-spherical, as LaGriT's `connect` leaves a few) give
a warning, not an error (`delaunay_fatal`).

Phase 4: control-volume policies (`cv`): median (centroids; the mesh
need not be Delaunay), blend (one alpha everywhere) and the adaptive
hybrid, which moves dual points toward the centroid only where a
coefficient would be negative (stage A, a closed-form clamp at the
boundary; stage B, cell-by-cell correction), with an alpha file for
ParaView.

Phase 6a: the run is a library routine that never stops, and LaGriT
can call it through `cv_dual_lagrit_entry` with its own mesh arrays
in place (manual, section 17); a mesh with parent/child chains
(`itp1` = 41) is refused. Next: cv_dual moves into LaGriT, renamed
cv_dual (6b), and becomes `dump / control_volume` (6c).

The manual: [`cv_dual_manual.md`](cv_dual_manual.md).
The LaGriT command page (a draft until phase 6c):
[`DUMP_CONTROL_VOLUME.md`](../commands/dump/DUMP_CONTROL_VOLUME.md).

## Build

cv_dual is built with LaGriT (LaGriT's `CMakeLists.txt`, from the
top of the LaGriT checkout):

```
cmake -S . -B build -D CMAKE_BUILD_TYPE=Release
cmake --build build --parallel 6
```

This builds `build/lagrit` and, beside it, `build/cv_dual` (the
program), and `build/cv_dual_selftest` and `build/cv_dual_entry_test`
(for the test suites). cv_dual itself needs no LaGriT library: only a
Fortran 2008 compiler (gfortran 9 or later, or Intel Fortran).

## Run

```
cv_dual -avs MESH.inp [-stor FILE] [-uge FILE] [-tough FILE]
        [-zero_area remove|keep] [-zero_tol X] [-compress on|off]
        [-cv voronoi|median|blend|hybrid] [-alpha X]
        [-hybrid_tol X] [-alpha_min X] [-hybrid_iter N]
        [-alpha_file FILE] [-delaunay_tol X] [-violations FILE]
        [-check_only on|off] [-report FILE] [-paraview ROOT]
        [-paraview_faces on|off] [-paraview_subset S]
        [-vtu_format binary|ascii] [-vtu_layout new|legacy]
        [-matrix FILE] [-matrix_bins N]
```

Options are keyword / value pairs in any order. Several outputs can
be written in one run; the geometry is computed once.

| keyword | value | default | meaning |
|---|---|---|---|
| `avs` | file | | the mesh: AVS UCD, all triangles or all tets |
| `stor` | file | `voronoi.stor` if no output is given | FEHM `.stor` |
| `uge` | file | | PFLOTRAN `.uge` |
| `tough` | file | | TOUGH `MESH` (`tough2`, the older name, also works) |
| `zero_area` | `remove`, `keep` | `remove` | `remove`: connections whose Voronoi face has zero area are left out of every file. `keep`: the coefficient matrix has one connection per mesh edge |
| `zero_tol` | number >= 0 | `1e-12` | a connection has zero area when \|A\| <= zero_tol d² (tets) or zero_tol d (triangles), d the edge length |
| `compress` | `on`, `off` | `on` | `.stor`: each distinct A/d value is written once and shared through the pointer block |
| `cv` | `voronoi`, `median`, `blend`, `hybrid` | `voronoi` | where each cell's dual point goes: circumcenter; centroid; one alpha everywhere; per cell, toward the centroid only as far as needed for no negative coefficient |
| `alpha` | 0 to 1 | | `cv blend` only (required): 1 = Voronoi, 0 = median |
| `hybrid_tol` | number >= 0 | `0` | `cv hybrid` only: how negative a coefficient may stay, relative to the sum of its pieces |
| `alpha_min` | 0 to 1 | `0` | `cv hybrid` only: the lowest alpha it may give a cell |
| `hybrid_iter` | whole number >= 1 | `100` | `cv hybrid` only: the most correction passes |
| `alpha_file` | file | none | the mesh with alpha and stage per cell, for ParaView |
| `paraview` | root | none | ParaView files: `ROOT.pvd` (open this), the mesh, the control volumes, the negative coefficients |
| `paraview_faces` | `on`, `off` | `off` | also the dual faces (tets) |
| `paraview_subset` | `all`, `boundary`, `flagged`, `material N`, `box x1 y1 z1 x2 y2 z2` | `all` | the vertices that get a cell |
| `vtu_format` | `binary`, `ascii` | `binary` | |
| `vtu_layout` | `new`, `legacy` | `new` | VTK's polyhedron layout: ParaView 6, or ParaView 5 |
| `matrix` | file | none | the coefficient matrix for ParaView: a point per `.stor` entry at (j, -i), both halves and the diagonal |
| `matrix_bins` | whole number >= 1 | none | with `matrix`: also an N x N image of the matrix (the file's name with `.vti`) |
| `delaunay_tol` | number >= 0 | `1e-10` | a tie within ±delaunay_tol; below, a facet is not Delaunay (see the manual) |
| `delaunay_fatal` | number >= 0 | `1e-2` | δ/L down to -delaunay_fatal: a warning (nearly co-spherical) and the run goes on; below it: fatal |
| `violations` | file | `violations.vtu` | the facets that fail, for ParaView; written only when some facet is flagged |
| `check_only` | `on`, `off` | `off` | `on`: the checks only, no coefficient file |
| `report` | file | none | a copy of everything cv_dual prints |
| `timing` | `on`, `off` | `off` | development only: the time of each step and peak memory |

The zero-area filter changes only which connections are written; the
control volumes are never changed. lanl/voronoi's options `-type
fehm|pflotran|tough`, `-o FILE` and `-compress` (no value) also work.

The LaGriT command (a later phase), `dump / control_volume`, will
take a mesh object and then the same keywords, as pairs in any order,
written with a comma inside each pair:

```
dump / control_volume / mo_dfn / stor, out.stor / uge, out.uge / zero_area, keep
```

A run prints the mesh checks, the Delaunay check and a summary: sizes,
the volume balance (control volumes against the mesh volume), and the
numbers of zero-area and negative coefficients. Exit status 0 on
success; 1 on any error (a bad option or file, a mesh that fails a
check, a mesh that is not Delaunay), with a message saying what and
where, and then no coefficient file is written.

## Tests

cv_dual's 13 suites (`test/suite/test_cv_dual_*`) run with LaGriT's:

```
cd test/suite
ONLY=cv_dual JOBS=6 sh run_all.sh     # cv_dual's suites alone
JOBS=6 sh run_all.sh                  # every LaGriT suite
```

then open `test/suite/summary.html`. `test/suite/cv_dual_tools/README.md`
lists cv_dual's suites. LaGriT's CI (`.github/workflows/test-lagrit.yml`)
runs them on every pull request.

## Source

| file | contents |
|---|---|
| `src/cv_dual/cv_dual_kinds.F90` | integer and real kinds (64-bit integers) |
| `src/cv_dual/cv_dual_log.F90` | all screen output, so LaGriT can redirect it (`log_set_sink`) |
| `src/cv_dual/cv_dual_options.F90` | the keyword options, shared by every front end |
| `src/cv_dual/cv_dual_mesh.F90` | mesh arrays with LaGriT names (`xic`, `itet`, `itetoff`, `itetclr`, `imt1`, `itp1`, `isn1`, `jtet`) |
| `src/cv_dual/cv_dual_read_avs.F90` | AVS UCD reader (vertex data `imt1`, `itp1`, `isn1`) |
| `src/cv_dual/cv_dual_scan.F90` | fast text scanner for the reader: blocks of the file, numbers parsed in place (`strtod`) |
| `src/cv_dual/cv_dual_timing.F90` | wall and CPU time per step and peak memory (`timing on`, development only) |
| `src/cv_dual/cv_dual_topology.F90` | the edge CSR: one row per vertex, upper triangle; vertex-to-cell incidence |
| `src/cv_dual/cv_dual_jtet.F90` | face neighbors in LaGriT's `jtet` encoding; conformity and orientation counts |
| `src/cv_dual/cv_dual_validity.F90` | the mesh checks: cell types, positive size, conformity, orientation, unused vertices |
| `src/cv_dual/cv_dual_delaunay.F90` | the Delaunay check: δ/L per interior facet, boundary circumcenters, slivers |
| `src/cv_dual/cv_dual_write_vtu.F90` | the violations file (VTK XML, ASCII) |
| `src/cv_dual/cv_dual_geometry.F90` | circumcenters (relative to a vertex) and the cell kernels |
| `src/cv_dual/cv_dual_policy.F90` | where each cell's dual points go: the blend, face weights, stage A's closed form |
| `src/cv_dual/cv_dual_hybrid.F90` | the adaptive hybrid: stage A, stage B, boundary edges |
| `src/cv_dual/cv_dual_tess.F90` | the control volumes as polygons and polyhedra, the mesh and negative-coefficient grids, for ParaView |
| `src/cv_dual/cv_dual_vtu.F90` | a .vtu writer (text or appended binary; VTK's new and legacy polyhedron layouts) and the .pvd |
| `src/cv_dual/cv_dual_assemble.F90` | cell-by-cell accumulation into the CSR |
| `src/cv_dual/cv_dual_write_stor.F90` | FEHM `.stor`, LaGriT's layout |
| `src/cv_dual/cv_dual_write_uge.F90` | PFLOTRAN `.uge` |
| `src/cv_dual/cv_dual_write_tough.F90` | TOUGH `MESH` |
| `src/cv_dual/cv_dual_report.F90` | the Delaunay report and the run summary |
| `src/cv_dual/cv_dual_run.F90` | one run on a mesh in memory (the library): checks, policies, coefficients, every output file; never stops |
| `src/cv_dual/cv_dual_entry.F90` | the LaGriT entry point `cv_dual_lagrit_entry`: LaGriT's mesh arrays in place, the options as one string |
| `src/cv_dual/cv_dual.F90` | the standalone program: options, the AVS file, then `cv_dual_run` |
| `test/suite/test_cv_dual_entry/harness/` | `cv_dual_entry_test`: calls the entry point as LaGriT will (used by `test_cv_dual_entry`) |
| `test/suite/cv_dual_tools/cv_dual_selftest.F90` | kernel checks used by `test/suite/test_cv_dual_kernels` |
| `test/suite/cv_dual_tools/stor_compare.py` | compares two `.stor` files and labels every difference with what explains it |

## Origin and license

The kernels and writers are ported from
[lanl/voronoi](https://github.com/lanl/voronoi) (LANL C19012), and
the test meshes and gold files in `test/suite/test_cv_dual_voronoi_gold/inputs`
are copied from it. BSD-3; see `src/cv_dual/LICENSE.md`.
