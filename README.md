# del_vor

Voronoi control volumes from Delaunay meshes.

del_vor reads a triangle mesh (2D, including fracture networks in 3D)
or a tet mesh (3D) and computes the Voronoi control-volume geometry:
the volume of each vertex's control volume, the area of each Voronoi
face and the connection distances. These are the geometric
coefficients of two-point-flux finite-volume codes, written as FEHM
`.stor`, PFLOTRAN `.uge` and TOUGH2 `MESH` files.

One core library, two front ends: the standalone program `del_vor`
(AVS UCD `.inp` input) and, later, a LaGriT entry point that replaces
`matbld2d` and `matbld3d`. The design, with the reasons for each
choice, is in the design document (see `notes` in the project folder).

## Status

Phase 1 of 6: the lanl/voronoi kernels and writers ported to one
edge-based sparse structure, without PETSc or MPI. Voronoi control
volumes only; median and hybrid control volumes, the Delaunay check
and ParaView output come in later phases.

## Build

CMake 3.16 or later and a Fortran 2008 compiler (gfortran 9 or later,
or Intel Fortran). No libraries.

```
cmake -S . -B build -D CMAKE_BUILD_TYPE=Release
cmake --build build --parallel 6
```

This builds `build/del_vor` and the kernel self test
`build/del_vor_selftest`.

## Run

```
del_vor -avs MESH.inp [-type fehm|pflotran|tough2] [-o FILE]
        [-compress] [-cv voronoi]
```

| option | meaning |
|---|---|
| `-avs MESH.inp` | the mesh: AVS UCD, all triangles or all tets |
| `-type` | `fehm` (`.stor`, the default), `pflotran` (`.uge`), `tough2` (`MESH`) |
| `-o FILE` | output file; default `voronoi.stor`, `pflotran.uge` or `MESH_VORONOI` |
| `-compress` | `.stor` only: write each distinct coefficient once |
| `-cv voronoi` | control volumes; only `voronoi` for now |

These are the options of lanl/voronoi. A run prints a summary: sizes,
the volume balance (control volumes against the mesh volume), and the
numbers of zero-area and negative coefficients. Exit status 0 on
success, 1 on any error.

## Tests

```
cd test/suite
sh run_all.sh
```

then open `test/suite/summary.html`. See `test/suite/README.md`.

## Source

| file | contents |
|---|---|
| `src/del_vor_kinds.F90` | integer and real kinds (64-bit integers) |
| `src/del_vor_log.F90` | all screen output, so LaGriT can redirect it |
| `src/del_vor_mesh.F90` | mesh arrays with LaGriT names (`xic`, `itet`, `itetoff`, `itetclr`, `imt1`) |
| `src/del_vor_read_avs.F90` | AVS UCD reader |
| `src/del_vor_topology.F90` | the edge CSR: one row per vertex, upper triangle |
| `src/del_vor_geometry.F90` | circumcenters (relative to a vertex) and the cell kernels |
| `src/del_vor_policy.F90` | where each cell's dual points go (Voronoi for now) |
| `src/del_vor_assemble.F90` | cell-by-cell accumulation into the CSR |
| `src/del_vor_write_stor.F90` | FEHM `.stor`, LaGriT's layout |
| `src/del_vor_write_uge.F90` | PFLOTRAN `.uge` |
| `src/del_vor_write_tough.F90` | TOUGH2 `MESH` |
| `src/del_vor_report.F90` | the run summary |
| `src/del_vor.F90` | the standalone program |
| `test/del_vor_selftest.F90` | kernel checks used by `test/suite/test_kernels` |

## Origin and license

The kernels and writers are ported from
[lanl/voronoi](https://github.com/lanl/voronoi) (LANL C19012), and
the test meshes and gold files in `test/suite/test_voronoi_gold/inputs`
are copied from it. BSD-3; see `LICENSE.md`.
