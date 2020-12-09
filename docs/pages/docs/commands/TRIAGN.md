---
title: TRIANGULATE
tags: triangulate 
---
 
# TRIANGULATE

---------------


**`triangulate`** will take an ordered set of nodes in the current 2d mesh
object that define a perimeter of a polygon and create a trangulation of
the polygon.
<br>
The nodes are assumed to lie in the xy plane; the z coordinate is ignored.
<br>
The code will exit early if orientation or node ordering is not correct. 


Use [**`REFINE`**](REFINE.md) and [**`SMOOTH`**](SMOOTH.md) to break triangles into desired resolution.
 

No checks are performed to verify that the nodes define a legal perimeter (i.e. that segments of the perimeter do not
cross). The code will connect the last node to the first node to complete the perimeter.


This code supports triangulation of self-intersecting polygons (polygon with holes), assuming that the order of the nodes are correct. Moreover
the connectivity of the polyline must also be defined correctly. No checks are made.
<br>
One disadvantage of the algorithm for triangulating self-intersecting polygons is that it does not always work. For example, if the holes have
complicated shapes, with many concave vertices, the code might fail. In this case, the user may try to rotate the order of the nodes:


[*Issue 57*](https://github.com/lanl/LaGriT/issues/57) is a known difficulty in the case where polygon points have a large number of co-linear points. 


## SYNTAX

<pre>
<b>triangulate</b> / 

<b>triangulate</b> / <b>clockwise</b> or <b>counterclockwise</b> 
</pre>

**`clockwise`** or **`counterclockwise`** (default clockwise) Orientation is defined with the viewer above the xy plane.



## EXAMPLES

```
triangulate
triangulate/clockwise
```
Both examples will triangulate the points in the clockwise direction.


```
cmo/create/2dmesh /// tri
read/avs/2dfile.avs

triangulate/counterclockwise
refine/rivara///edge/1 0 0/ 0.05 ///inclusive
```
Triangulate a polygon and then use refine/rivara to refine mesh to desired element size.


[Click here for demos](../demos/description_tri.md)
