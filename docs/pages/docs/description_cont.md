---
title: interpolate/continuous
---

# interpolate/continuous


Show examples using the **interpolate/continuous** method. For each point in the sink mesh, the enclosing element is found from the source mesh. The sink node or element (centroid) is assigned the interpolated attribute values from the element nodes. WARNING! continuous interpolation from a hex is not supported. Use hextotet to convert the mesh from hex to tet elements.

The full set of 5 examples and input.lgi command file are found in test/level01/interp_continuous/

 
[lagrit_input_continuous](lagrit_input_map.md) View LaGriT command file.
 
[lagrit_input_continuous](lagrit_input_map) Download LaGriT command file.



## Example 1

Interpolate source node values to sink nodes.
Source is 3 high, the bottom set of tets are a flat bottom xval is 0. top xval is 500.
The sink mesh is a single hex with bottom nodes located at bottom of source mesh.

```
read avs input_tet3x3_flat.inp cmo_src 
read avs input_hex1_med.inp    cmo_sink
intrp/continuous/cmo_sink xval /1,0,0/ cmo_src xval
```
Interpolate from cmo_src node attribute xval to cmo_sink node attribute xval.

<img width="400" src="https://lanl.github.io/LaGriT/assets/images/con01_src.gif">
 
Image shows the tall source tet mesh in same view with the sink hex element positioned in the lower left corner.


<img width="350" src="https://lanl.github.io/LaGriT/assets/images/con01_sink.gif">

This image shows the sink hex element with the interpolated values written to the node attribute called xval.



## Example 2

Interpolate node values from a set of large triangles to a high resolution quad mesh.

```
read avs input_random500_tri.inp cmo_src
read avs input_500_quad.inp      cmo_sink
intrp/continuous/cmo_sink numreal/1,0,0/cmo_src numreal
```
Interpolate from cmo_src triangle attribute numreal to cmo_sink node attribute numreal.


<img width="380" src="https://lanl.github.io/LaGriT/assets/images/con02_src.gif">

Image shows the source triangles colored by the node attribute numreal. Source mesh is an irregular triangle mesh with 12 nodes.


<img width="400" src="https://lanl.github.io/LaGriT/assets/images/con02_sink.gif">

Image shows the higher resolution sink quad mesh with the interpolated
 values at the quad nodes. As seen by this image, the sink mesh does
 not extend as far as the source triangles. It has nodes outside the
 source mesh. These nodes are flagged with the dark red values (1 greater than the max of numreal attribute).



[Back to main page.](commands/main_interpolate.md)

