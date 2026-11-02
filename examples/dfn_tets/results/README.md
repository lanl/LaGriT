# Reference results (Oct 31 2026)

`dfn_tets_suite.py` on the eight dfnWorks test cases at full size, run by
Carl Gable on a 16 GB Mac (2.6 GHz 6-core Intel i7), with LaGriT on the
branch of PR #303 (`h_lookup octree`). Each file is the suite's
`summary.json`: for every case the plan (predicted size, memory and
time), the results and the settings used. Paths have been shortened.

Common settings: `stat mean`, `c 0.74`, `slope 0.2`, `hscalar_factor 2`,
`seed 1`, `h_lookup octree`, `octree_k_factor 0.75`, improve_tets on.

The near-DFN ratio is, at tet vertices within one local h of the
fractures, the mean tet edge length there / the DFN's mean edge length.

## flat3_hmax10.json: `flat 3`, `hmax_factor 10` (= octree.ini)

| case | tet vertices | near-DFN ratio median (5–95%) | angles after improve_tets | LaGriT time | peak memory |
|---|---:|---|---|---:|---:|
| 4_user_rects_nonuniform | 2,489 | 0.99 (0.81–1.17) | 36.0–126.8° | 6 s | 15 MB |
| 4_user_rects_uniform | 4,336 | 0.98 (0.81–1.16) | 21.4–150.8° | 9 s | 23 MB |
| TPL_nonuniform | 319,171 | 1.02 (0.86–1.21) | 32.5–131.2° | 33 min * | 1.2 GB |
| ade_example_nonuniform | 119,901 | 1.01 (0.85–1.19) | 35.0–130.1° | 5 min | 555 MB |
| ade_example_uniform | 760,961 | 0.99 (0.85–1.14) | 31.3–130.6° | 34 min | 2.6 GB |
| constant_nonuniform | 83,414 | 1.00 (0.85–1.18) | 36.0–127.7° | 4 min | 372 MB |
| constant_uniform | 373,899 | 0.99 (0.84–1.15) | 35.8–128.8° | 13 min | 1.4 GB |

\* The Mac slept during this run; it takes about 10 minutes awake.
TPL_uniform ran on its own (2.03 million tet vertices, 82 min, 6.1 GB)
and is not in the file.

## flat2_hmax20.json: `flat 2`, `hmax_factor 20`

| case | tet vertices | near-DFN ratio median (5–95%) | angles after improve_tets | LaGriT time | peak memory |
|---|---:|---|---|---:|---:|
| 4_user_rects_nonuniform | 2,243 | 1.00 (0.83–1.18) | 38.7–122.6° | 5 s | 14 MB |
| 4_user_rects_uniform | 3,842 | 0.98 (0.81–1.19) | 32.6–132.3° | 8 s | 21 MB |
| TPL_nonuniform | 252,044 | 1.06 (0.89–1.25) | 33.4–131.6° | 12 min | 1.1 GB |
| ade_example_nonuniform | 101,014 | 1.04 (0.88–1.24) | 36.0–128.3° | 4 min | 471 MB |
| ade_example_uniform | 646,306 | 1.01 (0.87–1.17) | 32.7–133.3° | 28 min | 2.4 GB |
| constant_nonuniform | 67,682 | 1.02 (0.87–1.21) | 36.0–125.9° | 3 min | 318 MB |
| constant_uniform | 302,621 | 1.01 (0.86–1.18) | 35.0–130.0° | 11 min | 1.2 GB |

15–21% fewer tet vertices than flat 3, with the ratio a little above 1
(C of about 0.71 would bring it back to 1).

## Known: thin tets at a box edge

The 21.4° in 4_user_rects_uniform (flat 3) is where a box edge passes
close to a fracture. poisson_disk samples the box edges at 0.81 h, and
improve_tets does not move vertices on the box edges.
