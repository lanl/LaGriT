---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: TRANS
---

 **TRIANGULATE**

**triangulate** will take an ordered set of nodes in the current 2d mesh
object that define a perimeter of a polygon and create a trangulation of
the polygon.  The nodes are assumed to lie in the xy plane; the z
coordinate is ignored.  No checks are performed to verify that the nodes
define a legal perimeter (i.e. that segments of the perimeter do not
cross).  The code will connect the last node to the first node to
complete the perimeter.

**FORMAT:**

**triangulate** [**/clockwise**  **counterclockwise**]

clockwise is the default.  Orientation is defined with the viewer above
the xy plane.

 

**EXAMPLES:**

**triangulate**

**triangulate/counterclockwise**

**cmo/create**/2dmesh//**/tri**

**read** **/avs**/2dfile.avs

**triangulate**

** ** 

** **


[Click here for demos](demos/triangulate/md/main_tri.md)
[](demos/triangulate/test/test/md/main_tri.md)
