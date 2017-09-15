---
GENERATOR: 'Mozilla/4.75 [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 'interpolate / map'
---

 

interpolate / map
-----------------


Example 2: show results from using the **map** option of interpolate.


 For each point in the sink grid, the enclosing element is found from
 the source grid. The attribute value associated with the sink point is
 assigned the attribute value from the associated source element.

 The output consists of GMV files.

 The input consists of AVS and GMV files. The input deck for this
 example is [lagrit\_input\_map](../lagrit_input_map)


Results from Example 2:


 The objective is to test the ability of **interpolate** to find the
 enclosing element in a coarse hex grid associated with each point of a
 refined triangle grid. The itetclr value of each found element is
 copied to the sink imt attribute.

 The left image shows the source hex grid with itetclr values 1(blue),
 2(green), and 3(red). The high resolution sink triangle grid is in the
 image to show relative location.

 The right two images show the sink grid after using interpolate to
 color sink imt1 from the source itetclr values. The top nodes of the
 sink tri grid are located within two source elements, on shared edge.
 A single element is chosen by the tiebreaker option which selects
 either the mininum or maximum source element value.

 The middle image shows the result of using default **tiemax** option.

 The right image shows the result of using the **tiemin** option.
[source and sink grids](image/view_map01.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/view_map01_TN.GIF)](image/view_map01.gif">
[tiebreaker tiemax](image/map01_max.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map01_max_TN.GIF)](image/map01_max.gif">
[tiebreaker tiemin](image/map01_min.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map01_min_TN.GIF)](image/map01_min.gif">

 Demo 2 shows how the flag option is used for points outside the source
 grid. The sink grid has been translated so that part of the grid lies
 outside the source grid. The left image shows the course source grid
 and the refined sink grid together to show relative position. The
 source grid itetclr values are 1(blue) thru 3(red).

 The middle image shows the sink grid after it has had imt values
 assigned from source itetclr values using the flag option **plus1**.
 The sink points located outside the source grid were found not to have
 any enclosing elements. These points are flagged with the value of
 maximum itetclr value, plus one, which in this cas e is 4.

 The image on the right is the same intrp run, except that outside
 points are flagged with the **nearest** point imt1 value of the source
 grid. Note that the keyword **nearest** must be followed with a node
 attribute name of source values to be used for the flagged points.
[source and sink grids](image/view_map02.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/view_map02_TN.GIF)](image/view_map02.gif">
[flag with plus1](image/map02_plus1.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map02_plus1_TN.GIF)](image/map02_plus1.gif">
[flag with nearest](image/map02_nearest.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map02_nearest_TN.GIF)](image/map02_nearest.gif">

 Demo 3 shows how to map values from source elements to sink elements
 (centroid point). Image on the left shows the hex source grid colored
 by element id numbers 1 through 36. The image on the right is the
 resulting tetrahedra sink grid with elements colored from the source
 grid. If the sink attribute is element type, centroids for each
 element are calculated and used as the sink points. The sink image has
 the nine materials exploded for better viewing.
[Source hex grid](image/map03_src.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map03_src_TN.GIF)](image/map03_src.gif">
[Sink and Source grids](image/map03_view.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map03_view_TN.GIF)](image/map03_view.gif">
[Sink tet grid](image/map03_sink.gif">
[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/map03_sink_TN.GIF)](image/map03_sink.gif">



[Back to main page.](main_interpolate.md#DEMOS)

