# dfn_tets: 3D tet meshes around a DFN

Builds a 3D Poisson disk tet mesh of a box around a DFN (a triangle mesh of
the fractures, such as dfnWorks' `full_mesh.inp`). Next to the fractures the
tets have the DFN's resolution; away from them they get coarser. The tets do
not conform to the fractures.

Two scripts, Python 3.8 or later, standard library only:

| script | use it for |
|---|---|
| `dfn_tets.py` | one DFN: your actual work |
| `dfn_tets_suite.py` | a folder of cases: runs `dfn_tets.py` on each, one page for all |

Both need a LaGriT with `improve_tets` (PR #296) and
`cmo / addatt / edge_lengths_*` (the edge-lengths PR); `--h-lookup octree`,
and so `octree.ini`, needs poisson_disk's `h_lookup octree` (PR #303).

## In this folder

| | |
|---|---|
| `dfn_tets.py`, `dfn_tets_suite.py` | the scripts |
| `octree.ini` | the recommended settings with `h_lookup octree` |
| `cases/` | two small dfnWorks DFNs, uniform and nonuniform (below) |
| `results/` | reference results on all eight dfnWorks test cases at full size |

## Try it (under a minute)

```
cd examples/dfn_tets
python3 dfn_tets_suite.py cases --settings octree.ini --lagrit /path/to/lagrit --out /tmp/dfn_tets_try
open /tmp/dfn_tets_try/summary.html
```

Both cases should give a near-DFN ratio of about 1.00 (see `results/`).
The nonuniform case checks that the tets follow the DFN's own changes in
resolution.

## The test cases

`cases/` holds `4_user_rects_uniform` and `4_user_rects_nonuniform`
(`full_mesh.inp` and `metadata.json`, 0.5 MB together). They come from
J. Hyman's dfn_mesh_suite: four dfnWorks examples (4_user_rects,
ade_example, constant, TPL), each meshed by dfnWorks' `mesh_network` once
uniform (`uniform_mesh=True`) and once with its nonuniform defaults
(`min_dist 0.5`, `max_dist 10`, `max_resolution_factor 10`), all with
h = 0.1, from dfnWorks commit `16b16b8a` (branch `regression-suite`).
`metadata.json` records this for each case. The other six cases (5 to
103 MB each) are not in the repository because of their size; ask the
dfnWorks team for the suite, or regenerate it from the dfnWorks examples
with those settings.

## One DFN

```
python3 dfn_tets.py --dfn full_mesh.inp --json metadata.json --lagrit /path/to/lagrit --out my_mesh
open my_mesh/summary.html
```

The mesh is `my_mesh/tets.inp`. With no `metadata.json`, give the box
corners instead: `--box X0 Y0 Z0 X1 Y1 Z1`.

**Check the cost first.** `--plan-only` reads the DFN and prints the mesh
size, LaGriT's memory and time (measured on a 2.6 GHz Intel i7), and
runs nothing:

```
python3 dfn_tets.py --dfn full_mesh.inp --json metadata.json --lagrit /path/to/lagrit --plan-only
```

A run predicted to need more memory than `max_memory_gb` is not started.
The default is 60% of the computer's memory.

## Changing settings

`python3 dfn_tets.py --help` lists every setting with its default. Keep the
ones you use in a settings file:

```
python3 dfn_tets.py --write-settings my.ini      # every setting, explained
(edit my.ini: lagrit, c, slope, ...)
python3 dfn_tets.py --settings my.ini --dfn full_mesh.inp --json metadata.json
```

The command line wins over the settings file, and the file wins over the
defaults. Each page lists the settings used and where each came from.

| setting | default | what it does |
|---|---|---|
| `stat` | mean | h_dfn is the mean (or min) length of the DFN edges at each DFN vertex |
| `c` | 0.6 | h = C × h_dfn next to the DFN. poisson_disk's h is the smallest spacing allowed, and tet edges come out about 1.67 h in 3D. 0.6 with `mean` gives tet edges equal to the DFN's. |
| `slope` | 0.2 | how fast h grows away from the fractures. 0.2 or less keeps tets well shaped. |
| `hmax_factor`, `min_cells` | 10, 5 | the largest h: hmax_factor × the smallest h, but at most the box size / min_cells. Raise hmax_factor for fewer vertices far from the fractures. |
| `hscalar_factor` | 2 | sizes poisson_disk's lookup lattice, which sets the memory. 2 is the most allowed: 8× less memory than 1, for a few percent in accuracy. |
| `seed` | 1 | poisson_disk's random seed |
| `improve` | yes | run improve_tets |
| `max_memory_gb` | 60% of memory | do not start a run predicted to need more |
| `keep_hfield` | no | also write the h lookup lattice (large) |

## A folder of cases

```
python3 dfn_tets_suite.py dfn_mesh_suite --lagrit /path/to/lagrit
open dfn_mesh_suite_dfn_tets/summary.html
```

A case is a folder holding `full_mesh.inp` and `metadata.json`. Any
`dfn_tets.py` setting given here goes to every case, for example
`--slope 0.15` or `--settings my.ini`. `--only CASE ...` runs just those
cases, and `--plan-only` sizes them all without running any. Each case runs
in its own process into `OUT/<case>/`, so one failure doesn't stop the rest.

## Quick runs (about 4 minutes for the eight dfnWorks cases)

`--sub-box SIDE` meshes only a cube of that side in each case's box, placed
where the DFN is densest (or at 0 with `--sub-center origin`). The whole DFN
is still read, so the h field inside the cube is the full case's, fractures
outside it included. A case whose box is no bigger than SIDE runs whole.

```
python3 dfn_tets_suite.py dfn_mesh_suite --sub-box 2.5 --lagrit /path/to/lagrit
open dfn_mesh_suite_quick_dfn_tets/summary.html
```

Use it for sweeps (`--slope`, `--c`, `--seed`) and before and after code
changes, and compare its numbers only with other quick runs. For one DFN,
`dfn_tets.py --box X0 Y0 Z0 X1 Y1 Z1` does the same.

## Sweeps

`--sweep SETTING=V1,V2,...` runs the suite once for each value of one
setting, into `OUT/SETTING_V/`, and writes `OUT/sweep.html`, which compares
the values case by case. For example:

```
python3 dfn_tets_suite.py dfn_mesh_suite --lagrit /path/to/lagrit \
    --only constant_nonuniform --sweep hmax_factor=10,20,50 --min-cells 2
```

For `hmax_factor`, `min_cells` also caps H_MAX at the box size / min_cells,
and each page says which of the two set it. Remember that the slope limits h
too: in a box of side L, h can never pass about SLOPE × (the largest distance
from the DFN). The sweep page shows that distance and how many tet vertices
sit at H_MAX. A large H_MAX makes poisson_disk much slower (see the note
`--plan-only` prints).

## h_lookup octree (with a LaGriT that has it)

`--h-lookup octree` makes poisson_disk look h up in an octree adapted to h,
instead of a uniform lattice over the box (`hex_grid`, this script's
default; the script always gives `h_lookup`, since poisson_disk itself
now uses the octree by default when a control file sets h). The
octree also replaces poisson_disk's uniform neighbour grid. Memory and time
then follow the number of tet vertices, not the box volume / h_spacing_scalar³,
so a large H_MAX (`--hmax-factor 50`) or a big box is practical. Its fill
is maximal (no vertex could be added), so for the same h it makes more,
shorter edges than hex_grid: use `c 0.74` with `flat 3`, as `octree.ini`
does, for edges matching the DFN's. Example:

```
python3 dfn_tets_suite.py dfn_mesh_suite --sub-box 2.5 \
    --lagrit /path/to/lagrit --h-lookup octree --out quick_octree
```

`--octree-k-factor K` (default 0.75): an octree leaf is at most h / K;
larger K is a finer, more accurate octree. `--octree-dump yes` also writes
the leaves as `octree_leaves.inp` (AVS hex, with `h_field_att` and the level
`itetlev`). `--keep-hfield` is for hex_grid only.

## What the page shows

Every number is computed by LaGriT on every tet and tet vertex; the script
only bins them.

- **Near-DFN ratio.** At tet vertices within one local h of the fractures:
  the mean length of the tet edges there / the DFN's mean edge length. 1 means
  the same resolution as the DFN.
- **Grading.** Mean tet edge length against distance from the DFN, with the
  h asked for.
- **Angles.** Smallest and largest dihedral angle of each tet, before and
  after improve_tets.
- **A slice** coloured by edge length, with the fracture traces.
- **Predicted and actual** LaGriT time and peak memory.

## Files in an output folder

| file | |
|---|---|
| `tets.inp` | the mesh (AVS) |
| `summary.html` | this run's page |
| `result.json` | the page's numbers, for scripts |
| `run.lgi`, `h_dfn.mlgi` | the LaGriT deck and h-field file: rerun by hand with `lagrit < run.lgi` in that folder |
| `run.out` | LaGriT's output |
| `angles_*.table`, `vertex_stats.table` | LaGriT's tables behind the page |
| `octree_leaves.inp` | with `--octree-dump yes`: the octree leaves |
| `section.html` | the page's case section (the suite joins these) |

## Results on the dfnWorks test cases

With `octree.ini` on all eight cases at full size (16 GB Mac): near-DFN
ratio medians 0.98 to 1.02, angles after improve_tets at least 31° except
where a box edge passes near a fracture (21.4° in 4_user_rects_uniform),
and at most 2.6 GB and 34 minutes for the largest case that ran with the
suite (ade_example_uniform, 761k tet vertices); TPL_uniform, 2.0 million
tet vertices, took 82 minutes and 6.1 GB. Every number, and a second run with
`flat 2` and `hmax_factor 20`, is in `results/`.

With hex_grid and the defaults (`c 0.6`, `flat 0`, Oct 28) six cases ran;
TPL_uniform and TPL_nonuniform needed 15 to 18 GB for hex_grid's lookup
lattice, which `h_lookup octree` replaces.
