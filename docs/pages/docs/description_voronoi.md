---
GENERATOR: 'Mozilla/4.75 [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 'interpolate / voronoi'
---

 

interpolate / voronoi
---------------------





Example 1: show results from using the **voronoi** (nearest node) option
of interpolate.




 For each point in the sink grid, the nearest node is found from the
 source or background grid. The attribute value associated with the
 sink point is assigned the attribute value from the source node.

 The output consists of GMV files.

 The input consists of AVS and GMV files. The input deck for this
 example is
 [lagrit\_input\_voronoi](lagrit_input_voronoi)





Results from Example 1:




 The objective is to test the ability of **interpolate** to find the
 nearest node in a coarse hex grid to each point of a refined triangle
 grid. The imt value of each found node is copied to the sink imt
 attribute. Image shows the imt colors of the coarse hex grid and the
 refined triangle sink grid.
[source hex and sink tri grids](image/vor1.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/vor1_TN.GIF)](image/vor1.gif">

 The objective is to test the ability of **interpolate** to find the
 nearest node in a quad grid and copy imt values to sink quad grid.
 Image shows both the irregular quad grid and the refined quad sink
 grid.
[source and sink quad grids](image/vor_rand.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/vor_rand_TN.GIF)](image/vor_rand.gif">

 The objective is to test the ability of **interpolate** to find the
 nearest node in a tet sphere and copy to a 2D triangle sphere surface.
 The 2D surface has a radius slightly smaller than the 3D sphere. Image
 on left is the 3D tet sphere. Image on right is the triangulated
 surface with imt values copied from the 3D source sphere. The ends
 have been cutaway to show the surface elements.
[Source 3D tet grid](image/vor_sphere_src.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/vor_sphere_src_TN.GIF)](image/vor_sphere_src.gif">
[Sink 2D tri grid](image/vor_sphere_cut.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/vor_sphere_cut_TN.GIF)](image/vor_sphere_cut.gif">








[Back to main page.](commands/main_interpolate.md)



