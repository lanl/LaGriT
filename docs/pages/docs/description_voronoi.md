---
title: Example interpolate/voronoi
---

 

# interpolate/voronoi



## Example 1: show results from using the **`interpolate/voronoi`** (nearest node) method.


 For each point in the sink grid, the nearest node is found from the
 source or background grid. The attribute value associated with the
 sink point is assigned the attribute value from the source node.

 The output consists of GMV files.

 The input consists of AVS and GMV files. The input command file has 15 Examples of the interpolate command.
 
 [lagrit_input_voronoi](lagrit_input_voronoi.md) View LaGriT command file.
 
 [lagrit_input_voronoi](lagrit_input_voronoi) Download LaGriT command file.


The following **interpolate/voronoi** Examples are in the LaGriT command file.

## Example 1


Copy node attribute values from a source coarse hex mesh to each sink node of a refined triangle mesh. 
The image shows the imt attribute colors of the source hex mesh and the refined triangle sink mesh.

```
intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/ 
```
<img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor1.gif">
 

## Example 2

Copy node attribute values from a source quad mesh to each sink node of a quad mesh.
The image shows the source quad mesh and the sink refined quad mesh.

```
intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/ 
```
<img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor1.gif">
 

[source and sink quad grids](image/vor_rand.gif">
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/vor_rand_TN.GIF)](image/vor_rand.gif">

 The objective is to test the ability of **interpolate** to find the
 nearest node in a tet sphere and copy to a 2D triangle sphere surface.
 The 2D surface has a radius slightly smaller than the 3D sphere. Image
 on left is the 3D tet sphere. Image on right is the triangulated
 surface with imt values copied from the 3D source sphere. The ends
 have been cutaway to show the surface elements.
[Source 3D tet grid](image/vor_sphere_src.gif">
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/vor_sphere_src_TN.GIF)](image/vor_sphere_src.gif">
[Sink 2D tri grid](image/vor_sphere_cut.gif">
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/vor_sphere_cut_TN.GIF)](image/vor_sphere_cut.gif">



[Back to main page.](commands/main_interpolate.md)

