---
title: interpolate/map examples
---


# interpolate/map

Show examples using the **`interpolate/map`** method. For each node or element centroid in the sink mesh, find the enclosing element source or background mesh. The attribute value associated with the sink point is assigned the attribute value from the source element attribute. 

The full set of 12 examples and input.lgi command file are found in test/level01/interp_map/

 
[lagrit_input_map](lagrit_input_map.md) View LaGriT command file.
 
[lagrit_input_map](lagrit_input_map) Download LaGriT command file.



# Example 1

Copy element attribute values from a source coarse hex mesh to each sink node of a refined triangle mesh. 
The itetclr value of each found element is copied to the sink imt attribute. 
This shows the result of a tie where there are multiple candidates found in the search.
The top nodes of the sink tri mesh are located within two source elements, on shared edge. 
A single source element is chosen by the tiebreaker option which selects either the mininum or maximum source element value as detirmined by the keywords tiemin and tiemax.

```
read gmv input_3d_hex.gmv   cmo_src
read gmv input_2d_hires.gmv cmo_sink
interpolate/map/cmo_sink imt1/1,0,0/cmo_src itetclr
```
The tie breaker for this command is the default tiemax.

```
intrp/map/cmo_sink imt1/1,0,0/cmo_src itetclr/tiemin, min
```
The tie breaker for this command is set using the keyword tiemin. Additionally the interpolation function is set to min.

<img width="400" src="https://lanl.github.io/LaGriT/assets/images/view_map01.gif">

This image shows the coarse source hex mesh in same view as the higher resolution triangle mesh.
The itetclr values are 1(blue), 2(green), and 3(red).

<img width="400" src="https://lanl.github.io/LaGriT/assets/images/map01_max.gif">   <img width="400" src="https://lanl.github.io/LaGriT/assets/images/map01_min.gif">

These two images show the sink mesh after interpolation of hex itetclr to sink node attribute imt1. 

The left image shows the result of using default tiemax option, with top nodes set to value 3.
The right image shows the result of using the tiemin option, with top nodes set to value 2.



## Example 2

This example shows how the flag option is used when sink nodes are located outside the elements of the source mesh. 

```
read gmv input_3d_hex.gmv   cmo_src
read gmv input_2d_hires.gmv cmo_sink
intrp/map/cmo_sink imt1/1,0,0/cmo_src itetclr/ plus1
```
This examples uses the plus1 option to flag outside nodes. For each sink node not inside a source element, the value 1 + max value in the itetclr attribute will be assigned to sink node imt1. The flag value plus1 can be left off the command line as it is the default.

```
intrp/map/cmo_sink imt1/1,0,0/cmo_src itetclr/nearest,imt1/mintie
```
This second call uses the option nearest with attribute imt to tag outside node values. The mintie option will break any tie canidates by choosing the min value.


<img width="400" src="https://lanl.github.io/LaGriT/assets/images/view_map02.gif">

This image shows the source hex mesh with itetclr values 1(blue), 2(green), and 3(red). The high resolution sink triangles are shown in the same view to see the sink nodes located outside the source mesh and will not to have enclosing source elements.
 
 <img width="400" src="https://lanl.github.io/LaGriT/assets/images/map02_plus1.gif"> 
 
This image shows results from the first call using plus1 option. It shows the sink triangles after imt values are assigned from source itetclr values using the flag option plus1.  The points outside the source mesh are flagged with the value of maximum itetclr value, plus one, which in this case is 4. 
 
 
 <img width="400" src="https://lanl.github.io/LaGriT/assets/images/map02_nearest.gif">

This image is the second call which is the same command, except that outside nodes are flagged with the nearest point imt1 value of the source nodes. Note that the keyword nearest must be followed with a node attribute name to be used for the flagged points. The tiemin option is used for any tie candidate values.



# Example 3

The example shows how to map values from source elements to sink elements (centroid point).

```
read gmv input_3d_hex.gmv cmo_src
read gmv input_tet24.gmv cmo_sink
intrp/map/cmo_sink itetclr /1,0,0/cmo_src tet_id / tiemin, min
```
The sink attribute is of type element, so the centroid point of each element will be used as the sink point. The tie breaker for this command is set using the keyword tiemin. Additionally the interpolation function is set to min.

<img width="400" src="https://lanl.github.io/LaGriT/assets/images/map03_src.gif"> <img width="400" src="https://lanl.github.io/LaGriT/assets/images/map03_view.gif">

The left image shows the hex source mesh colored by element itetclr numbers 1 through 36.
The right image are both the hex source mesh and the sink tet mesh in same view.


 <img width="400" src="https://lanl.github.io/LaGriT/assets/images/map03_sink.gif">

This image is the interpolated tetrahedra sink mesh with elements colored from the source hex mesh. If the sink attribute is element type, centroids for each element are calculated and used as the sink location points. 
This sink image has the nine materials exploded for better viewing.



[Back to main page.](commands/main_interpolate.md)

