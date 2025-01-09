
## ABOUT THE TEST SUITE

The LaGriT Test Suite is located on the github repo at LaGriT/test/level01.
Each directory contains input files and input.lgi with commands to run.

For each test directory there is a reference directory with expected output. Differences between the "outx3dgen" and "reference/outx3dgen" files will indicate why a test fails. The reference directory also contains all out* mesh files written during the run.

## TO RUN AND CHECK A SINGLE TEST:

-  Go into the individual directory (ie addmesh_add).
-  Run lagrit exectuable and compare to files in reference directory.
```
     lagrit -out outx3dgen < input.lgi
     diff outx3dgen reference/outx3dgen
```
-  Or use default output file names:
```
     lagrit < input.lgi
     diff lagrit.out reference/outx3dgen
```

[addmesh_add](https://github.com/lanl/LaGriT/tree/master/test/level01/addmesh_add)
[addmesh_append](https://github.com/lanl/LaGriT/tree/master/test/level01/addmesh_append)
[addmesh_doublemesh](https://github.com/lanl/LaGriT/tree/master/test/level01/addmesh_doublemesh)
[addmesh_merge](https://github.com/lanl/LaGriT/tree/master/test/level01/addmesh_merge)
[cmo_addatt_normals](https://github.com/lanl/LaGriT/tree/master/test/level01/cmo_addatt_normals)
[cmo_readatt](https://github.com/lanl/LaGriT/tree/master/test/level01/cmo_readatt)
[connect_cube](https://github.com/lanl/LaGriT/tree/master/test/level01/connect_cube)
[connect_errors](https://github.com/lanl/LaGriT/tree/master/test/level01/connect_errors)
[createpts](https://github.com/lanl/LaGriT/tree/master/test/level01/createpts)
[createpts_filter](https://github.com/lanl/LaGriT/tree/master/test/level01/createpts_filter)
[eltset](https://github.com/lanl/LaGriT/tree/master/test/level01/eltset)
[filter](https://github.com/lanl/LaGriT/tree/master/test/level01/filter)
[grid2grid](https://github.com/lanl/LaGriT/tree/master/test/level01/grid2grid)
[hextotet](https://github.com/lanl/LaGriT/tree/master/test/level01/hextotet)
[hybrid_volume](https://github.com/lanl/LaGriT/tree/master/test/level01/hybrid_volume)
[interp_continuous](https://github.com/lanl/LaGriT/tree/master/test/level01/interp_continuous)
[interp_map](https://github.com/lanl/LaGriT/tree/master/test/level01/interp_map)
[interp_voronoi](https://github.com/lanl/LaGriT/tree/master/test/level01/interp_voronoi)
[intersect](https://github.com/lanl/LaGriT/tree/master/test/level01/intersect)
[intrp_2D_sizes](https://github.com/lanl/LaGriT/tree/master/test/level01/intrp_2D_sizes)
[io_agf_simple](https://github.com/lanl/LaGriT/tree/master/test/level01/io_agf_simple)
[pflotran_stor](https://github.com/lanl/LaGriT/tree/master/test/level01/pflotran_stor)
[pset](https://github.com/lanl/LaGriT/tree/master/test/level01/pset)
[pset_readwrite](https://github.com/lanl/LaGriT/tree/master/test/level01/pset_readwrite)
[quad_quality](https://github.com/lanl/LaGriT/tree/master/test/level01/quad_quality)
[quality](https://github.com/lanl/LaGriT/tree/master/test/level01/quality)
[read_gocad](https://github.com/lanl/LaGriT/tree/master/test/level01/read_gocad)
[recon1](https://github.com/lanl/LaGriT/tree/master/test/level01/recon1)
[refine_octree](https://github.com/lanl/LaGriT/tree/master/test/level01/refine_octree)
[refine_octree_prd](https://github.com/lanl/LaGriT/tree/master/test/level01/refine_octree_prd)
[refine_recon_quality](https://github.com/lanl/LaGriT/tree/master/test/level01/refine_recon_quality)
[rmpoint](https://github.com/lanl/LaGriT/tree/master/test/level01/rmpoint)
[rotateln](https://github.com/lanl/LaGriT/tree/master/test/level01/rotateln)
[setpts](https://github.com/lanl/LaGriT/tree/master/test/level01/setpts)
[single_triangle](https://github.com/lanl/LaGriT/tree/master/test/level01/single_triangle)
[smooth](https://github.com/lanl/LaGriT/tree/master/test/level01/smooth)
[smooth_massage](https://github.com/lanl/LaGriT/tree/master/test/level01/smooth_massage)
[sort](https://github.com/lanl/LaGriT/tree/master/test/level01/sort)
[stack_layers](https://github.com/lanl/LaGriT/tree/master/test/level01/stack_layers)
[surface](https://github.com/lanl/LaGriT/tree/master/test/level01/surface)
[triangulate_polygon](https://github.com/lanl/LaGriT/tree/master/test/level01/triangulate_polygon)
[zone_outside](https://github.com/lanl/LaGriT/tree/master/test/level01/zone_outside)
[zone_outside_minmax](https://github.com/lanl/LaGriT/tree/master/test/level01/zone_outside_minmax)
