---
title: 'cv_dual and matbld compared'
tags: cv_dual matbld stor fehm pflotran voronoi hybrid comparison
---

# cv_dual and matbld compared

LaGriT computes control volumes and their geometric coefficients for
`dump / stor`, `dump / fehm` and `dump / pflotran` with its sparse
matrix code, matbld (`matbld2d_stor`, `matbld3d_stor`,
`anothermatbld3d`), until V3.4.0; since then with cv_dual by default,
and with matbld when the command says **`matbld`**
([cv_dual OR matbld](../commands/dump/DUMP3.md#cv_dual-or-matbld); cv_dual's
[manual](cv_dual_manual.md)). This page compares the two: where their
files agree, every way they differ and why, and what changed when
cv_dual became the default.

The evidence comes from three test suites in `test/suite`, each
rerunnable:

- **test_cv_dual_vs_matbld**: LaGriT's own decks that write `.stor`,
  `.uge` and FEHM files (level01 `hybrid_volume`, `io_agf_simple`,
  `pflotran_stor`, `zone_outside`; level03 `stor_precision_diff`; the
  nine decks of `test_stor_utm`; and two of its own: a Poisson mesh
  with both hybrids, and parent/child chains), each run as written and again with
  `/ cv_dual` on every dump command. Every file of the two runs is
  compared, and every difference must be explained.
- **test_cv_dual_parity**: the cv_dual program against `dump / stor` on
  14 more meshes (lanl/voronoi's, Poisson meshes, a two-material mesh,
  two fracture networks; dfnWorks TPL_nonuniform, 114,739 triangles,
  outside git).
- **test_cv_dual_timing**: time and memory from 9,000 to 10 million
  cells.

The comparisons use `stor_compare.py` (manual, [section 15](cv_dual_manual.md#15-comparing-stor-files-and-parity-with-lagrit)),
which labels every difference between two `.stor` files with its
cause.

## Summary

- **Voronoi control volumes: the same numbers.** On every deck the
  volumes and coefficients agree to the printed digits or to
  round-off, apart from differences that come from how matbld stores
  them: values its compression merged, zero-area connections one
  writer keeps and the other leaves out, small coefficients it drops. Both codes find the same negative
  coefficients.
- **Hybrid: cv_dual's removes the negative coefficients; matbld's
  only some.** On LaGriT's Poisson disk mesh, Voronoi has 75 negative
  coefficients: matbld's `hybrid` leaves 43, cv_dual's none. On random
  points in a box, 349: matbld's leaves 339, cv_dual's none.
- **cv_dual checks the mesh; matbld does not.** A mesh that is not
  Delaunay gets Voronoi coefficients from matbld without a warning;
  cv_dual refuses it and says where. Neither writes hex meshes.
- **cv_dual is 2.4 times faster at 10 million cells and needs 1.5 to
  2.2 times less memory.**
- **matbld has options cv_dual does not**: vector and area
  coefficients, binary `.stor` files, `dump / pflotran`'s `verbose`.
  These leave the dump to matbld, with one line saying so.

## Voronoi: where the files differ, and why

The `.stor` files of test_cv_dual_vs_matbld's decks (cv_dual's
connections / matbld's; the largest differences relative to the
largest value; the labels that explain them):

| deck | vertices | connections | coefficients | volumes | explained by |
|---|---|---|---|---|---|
| st01 (moved lattice, origin and UTM) | 378 | 1988 / 1988 | 2.8e-13 | 6.4e-16 | digits, round_off, merge 5 |
| st03 (`coefs`, `none`: matbld3d_stor) | 378 | 1988 / 2241 | 4.3e-16 | 7.4e-16 | digits, round_off; 253 zero_area |
| st04 (random points in a box) | 308 | 2017 / 2009 | 9.8e-9 | 5.0e-9 | merge 125; 8 dropped (15 volumes) |
| st05 (Poisson disk mesh) | 857 | 5083 / 5083 | 5.4e-9 | 8.6e-16 | digits, round_off, merge 1 |
| vm01 (Poisson disk mesh, test_poisson_3d case 1) | 1647 | 10235 / 10233 | 9.9e-9 | 6.8e-9 | merge 7; 2 dropped (4 volumes) |
| st06 (2D triangles) | 1271 | 3670 / 3670 | 5.1e-16 | 2.9e-16 | digits, round_off |
| level01 pflotran_stor (sample tets) | 60 | 255 / 255 | 3.4e-16 | 4.4e-16 | digits, round_off |
| level01 pflotran_stor (2D triangles, 3 x 4) | 12 | 17 / 23 | 1.5e-16 | 1.7e-16 | digits, round_off; 6 zero_area |
| level01 hybrid_volume (lattice, default writer) | 1331 | 3630 / 3630 | 0 | 1.0e-15 | digits |
| level01 hybrid_volume (lattice, `none`, `coefs`) | 1331 | 3630 / 7884 | 6e-17 | 1.3e-15 | digits, round_off; 4254 zero_area |

The `.uge` files (`dump / pflotran`) and the boundary areas
(`_outside_vor.area`) agree the same way, and every other file
(`.fehmn`, the zone files) is the same byte for byte.

What the labels mean, in the order they matter:

- **digits, round_off.** The same number, or two sums of the same
  pieces added in a different order (within 64 u, 1.4e-14, of the
  largest value). Both codes write 17 significant digits since V3.4.0.
- **merge.** matbld's compression (`all`, the default, and every `.uge`)
  stores one value for all values within 1e-8 of the largest, so a
  coefficient can move by up to 1e-8 of the largest. cv_dual merges
  only values within 1e-14, so its numbers are the computed ones.
- **zero_area.** A lattice, or any mesh with four or more vertices on
  one sphere, has edges whose Voronoi face has zero area. cv_dual
  leaves them out (`nofilter_zero` keeps them, one connection per mesh
  edge), as matbld's default 3D writer does; matbld's other writers
  (`coefs`, `none`, and 2D) write them. The control volumes are the
  same either way. Even with
  `nofilter_zero`, matbld leaves out some zero-area connections (7 of
  pflotran_stor's hex-derived mesh, 34 of stor_precision_diff's).
- **dropped.** matbld also leaves out real coefficients below 1e-8 of
  the largest, and the volumes of their vertices lose those pieces: on
  st04's random points, 8 connections and 15 volumes (differences up to
  5e-9 of the largest volume). cv_dual keeps them.
- **nstor_magnitude.** In 3D, `dump / stor ... / none` uses matbld's
  older writer, which stores \|A\|/d: a negative coefficient comes out
  positive (all 75 of test_poisson_3d case 1's, test_cv_dual_parity).
  cv_dual keeps the sign.
- **UTM-size coordinates.** Before V3.4.0, matbld's 3D writer computed
  tet volumes from absolute coordinates and lost every digit there (a
  box of volume 0.032 got -100.5). Fixed in V3.4.0 (#333, #338); the
  `_u` decks of test_stor_utm now agree with cv_dual as their origin
  twins do.

matbld also adds two attributes, `ccoef` and `ij_ccoef`, to the mesh
object when it finds negative coefficients. cv_dual never changes the
mesh object, except the attributes `dump / control_volume` is asked
for: `attribute_cv_volume`, and the `ccoef_*` keywords, of which
`ccoef_areabydist_min` and `ccoef_ij_areabydist_min` give what
`ccoef` and `ij_ccoef` gave (equal on every vertex with a negative
coefficient of test_poisson_3d case 1, test_cv_dual_control_volume).

## Negative coefficients and the hybrids

A Voronoi coefficient is negative where a cell's circumcenter lies
outside the mesh or a facet is close to not Delaunay. Both codes find
the same ones. The hybrids differ: matbld's moves the dual point of
each boundary tet whose circumcenter is outside onto the tet's
surface; cv_dual's starts the same way, then checks every edge and
moves the points of the cells involved toward their centroids, only
as far as needed, until no coefficient is negative.

| deck | Voronoi (both) | matbld `hybrid` | cv_dual `hybrid` |
|---|---|---|---|
| vm01 (Poisson disk mesh of test_poisson_3d case 1, 1647 vertices) | 75 | 43 | 0 |
| st04 (random points in a 1 x 1 x 0.5 box, 308 vertices) | 349 | 339 | 0 |
| st05 (Poisson disk mesh, 857 vertices; Voronoi only) | 51 | | |
| hybrid_volume (lattices) | 0 | 0 | 0 |

On vm01, cv_dual's first step alone (each boundary tet's point put on
its boundary face when its circumcenter is outside) removes all 75
(manual, [section 9](cv_dual_manual.md#9-control-volumes-and-coefficients));
matbld's, which looks for the same tets, leaves 43. matbld's hybrid
works only in 3D and with the default options; cv_dual's in 2D, 3D
and fracture networks, with every option.

Both hybrids put the face area along the edge into the coefficient
(the component of the dual face's vector area in the edge's
direction), so a coefficient means the same thing in both.

## Meshes

| mesh | matbld | cv_dual |
|---|---|---|
| not Delaunay (st09: vertices moved after `connect`, 45 facets beyond `delaunay_fatal`, delta/L to -0.35) | writes Voronoi coefficients, no warning | refuses: an ERROR and nothing written; the facets in `violations.vtu`. `median` goes on (it does not need a Delaunay mesh) |
| nearly Delaunay (ties, as `connect` leaves a few) | writes | writes, with a warning and the facets counted |
| hex, prism, mixed cells | writes nothing ("does not qualify") | refuses with an ERROR |
| parent/child chains (`settets / parents`; vm02: 384 vertices) | prints "Error: There are parential issues involved!!!!!!!" 3,126 times, then writes a `.stor` in which 203 of the 384 vertices have zero volume | refuses: an ERROR, nothing written, and names `resetpts / parent` and `rmpoint / compress`; after them both codes write the same control volumes |
| fracture networks (triangles in 3D) | writes | writes; fracture intersection edges get every fracture's share |

## Speed and memory

From test_cv_dual_timing on an Intel Mac (6 cores), cv_dual's program
against LaGriT's read + `dump / stor` (manual, [section 16](cv_dual_manual.md#16-speed-and-memory)):

| mesh | cv_dual | peak | LaGriT read + `dump / stor` | peak |
|---|---|---|---|---|
| 1,019,162 tets | 7.2 s | 275 MB | 14.8 s | 424 MB |
| 10,404,079 tets | 74.8 s | 2,317 MB | 175.7 s | 3,459 MB |
| 9,990,450 triangles | 60.4 s | 2,981 MB | 143.2 s | 6,433 MB |

Both grow linearly with the mesh; cv_dual a little more slowly.

## cv_dual the default (V3.4.0)

Since V3.4.0 `dump / stor`, `dump / fehm`, `dump / pflotran` and
`dump / zone_outside` use cv_dual unless the command says `matbld`.
What changed for users, from the comparison above:

- **The `.stor` holds the same connections as matbld's default 3D
  writer** (zero-area ones left out; `nofilter_zero` keeps them), with
  **the computed values**, not values merged within 1e-8, and the small
  real coefficients matbld drops kept. In 2D, matbld wrote the
  zero-area connections; cv_dual leaves them out there too.
- **A mesh that is not Delaunay is refused** for Voronoi control
  volumes, where matbld wrote a file; the message says where the mesh
  fails, `ROOT_violations.vtu` (beside the dump's files) shows it, and
  names `median`, `hybrid` and `matbld`, which get a file. A mesh with
  parent/child chains is refused too, naming `matbld`, where matbld
  wrote zero volumes.
- **A mesh of other cells** (hexes, prisms, mixed) goes to matbld, as
  before, with one line saying so.
- **`hybrid` gives no negative coefficients** instead of matbld's
  boundary fix.
- **The `.stor`'s two header lines** name the control volumes and the
  command that wrote the file.
- **`ccoef` and `ij_ccoef` are not made**: `dump / control_volume`
  with `ccoef_areabydist_min` and `ccoef_ij_areabydist_min` gives them
  (and eight more), or `matbld`.
- **Each dump says which code wrote it**, in one line.

Nothing else changes: the other FEHM files are the same, options
cv_dual does not have still go to matbld (with a line saying so), and
`matbld` gives the earlier files byte for byte. On eight
[dfnWorks](https://dfnworks.lanl.gov) fracture-network meshes (up to
479,021 triangles; run outside the test suite) the Voronoi `.stor`
files agree with matbld's, every difference explained, the control
volumes add up to the same total, and cv_dual took 0.54 to 0.81 of
matbld's time and about half its memory.
