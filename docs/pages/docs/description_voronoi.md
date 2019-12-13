---
title: interpolate/voronoi examples
---

# interpolate/voronoi

Show examples using the **`interpolate/voronoi`** (nearest node) method. For each point in the sink grid, the nearest node is found from the source or background grid. The attribute value associated with the sink point is assigned the attribute value from the source node. 

The full set of 15 examples and input.lgi command file are found in test/level01/interp_voronoi/
 
[lagrit_input_voronoi](lagrit_input_voronoi.md) View LaGriT command file.
 
[lagrit_input_voronoi](demos/input/lagrit_input_voronoi.txt) Download LaGriT command file.



## Example 1

Copy node attribute values from a source coarse hex mesh to each sink node of a refined triangle mesh. 

```
read gmv input_3d_hex.gmv   cmo_src
read gmv input_2d_hires.gmv cmo_sink
intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/
```
<img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor1.gif">

The image shows the imt attribute colors of the source hex mesh (coarse lines) and the nodes of the refined triangle sink mesh colored by the nearest source node values.

## Example 2

Copy node attribute values from a source quad mesh to each sink node of a quad mesh.

```
read gmv input_random500_quad.gmv cmo_src
read gmv input_500_quad.gmv cmo_sink
intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/
```
<img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor_rand.gif">

The image shows the source irregular shaped quad edges and the sink high resolution quad mesh colored by nearest source node values.
 

## Example 3

Copy node attribute values from a source tet sphere to each sink node of a triangle surface.
The triangle surface is slightly inside the radius of the 3D sphere.

```
read gmv input_sphere3d.gmv cmo_src
read gmv input_sphere2d.gmv cmo_sink
intrp/voronoi/cmo_sink imt1/1,0,0/cmo_src imt1/
```
<img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor_sphere_src.gif"> <img width="400" src="https://lanl.github.io/LaGriT/assets/images/vor_sphere_cut.gif">

The image shows the source 3D tet sphere mesh and the sink triangle surface with interpolated values.




[Back to main page.](commands/main_interpolate.md)

