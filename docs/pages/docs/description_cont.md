---
title: 'interpolate / continuous'
---

interpolate / continuous
------------------------

Example 3: show results from using the **continuous** option of
interpolate.


 For each point in the sink grid, the enclosing element is found from
 the source grid. The attribute value associated with the sink point is
 assigned the interpolated attribute values from the element nodes.
 WARNING! continuous interpolation from a hex is not supported. Use
 hextotet to convert the source grid.

 The output consists of GMV files.

 The input consists of AVS and GMV files. The input deck for this
 example is [LaGriT\_input\_continuous](lagrit_input_continuous)


Results from Example 3:


 The objective is to test the ability of **interpolate** to use
 **continuous** interpolation from source element vertices on to sink
 points.

 Image on left shows the source tet grid and location of the sink hex
 element in the lower left corner of the tet grid. Values from the
 enclosing tet nodes are interpolated on to each hex point based on the
 point location in the found source element.

 The image on the right is the sink hex element with the interpolated
 values written to node attribute called xval.

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/con01_src_TN.GIF"> 

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/con01_sink_TN.GIF">

 The objective is use the **continuous** method to interpolate node
 values from a triangle grid to a high resolution quad grid.

 Image on the left shows the source triangles colored by the node
 attribute numreal.

 Image on the right shows the sink quad grid with the interpolated
 values at the quad nodes. As seen by this image, the sink grid does
 not extend as high as the source grid, and has nodes outside the
 source grid. These nodes are flagged with the dark red values.

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/con02_src_TN.GIF">

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/con02_sink_TN.GIF">

[Back to main page.](commands/main_interpolate.md#DEMOS)

