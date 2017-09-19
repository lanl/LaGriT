 ---
 title: TRIAGN
 tags: ok
 ---
 
 **TRIANGULATE**

**triangulate** will take an ordered set of nodes in the current 2d mesh
object that define a perimeter of a polygon and create a trangulation of
the polygon.  The nodes are assumed to lie in the xy plane; the z
coordinate is ignored.  No checks are performed to verify that the nodes
define a legal perimeter (i.e. that segments of the perimeter do not
cross).  The code will connect the last node to the first node to
complete the perimeter.

This code support triangulation of self-intersecting polygons (polygon
with holes), assuming that the order of the nodes are correct. Moreover
the connectivity of the polyline must also be defined correctly. No
checks are made.

One disadvantage of the algorithm for triangulating self-intersecting
polygons is that it does not always work. For example, if the holes have
complicated shapes, with many concave vertices, the code might fail. In
this case, the user may try to rotate the order of the nodes:

NODE\_ID:

1 -&gt; 2
2 -&gt; 3
...
N -&gt; 1

**FORMAT:**

**triangulate** **/clockwise** OR **counterclockwise** 

clockwise is the default.  Orientation is defined with the viewer above
the xy plane.
 
**EXAMPLES:**

	triangulate
	triangulate/counterclockwise

	mo/create/2dmesh///ri

	read/avs/2dfile.avs

	triangulate

[Click here for demos](../demos/main_tri.md)
