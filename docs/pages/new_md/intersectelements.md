---
title: INTERSECTELEMENTS
---

**INTERSECT\_ELEMENTS**

 This command takes two meshes and creates an element-based attribute
 in mesh1 that contains the number of elements in mesh2 that
 intersected the respective element in mesh1.

 We define intersection as two elements sharing any common point.

FORMAT:

 **intersect\_elements** / mesh1 / mesh2 / 
[attrib\_name
]

NOTES:

 
[attrib\_name
] specifies the name of the element based attribute in
 mesh1 that is created by this command. The default name for this
 attribute is in\_&lt;mesh2&gt;. For example, if the comand syntax was:

  **intersect\_elements**/cmo\_strat/cmo\_well/

 the element based attribute that stores the number of intersections
 would be named in\_cmo\_well. It is worth noting that GMV does not
 take kindly to names that are longer than eight characters and will
 truncate them without even thinking twice, resulting in the name used
 in our example being changed to in\_cmo\_w. Therefore, it is good
 practice to use your own attribute names less than eight characters if
 possible.

 This code has been slightly modified to work with AMR grids produced
 in X3D. This modification depends on an element based attribute that
 X3D creates called **itetkid**. If this attribute is not present,
 **intersect\_elements** will **NOT** be able to recognize the AMR
 grid, and will intersect all elements of the octree. With the itetkid
 attribute present, only leaves of the octree which intersect will be
 flagged.

 **intersect\_elements** is not designed to work with every
 element-element combination, but it is pretty thorough. The following
 table shows what element/element intersetion capabilities are
 available. An **X** in the box means that the intersection is
 supported.

   ------- ------- ------- ------- ------- ------- ------- -------
           point   line    tri     quad    tet     pyr     hex
   point   **X**   **X**   **X**   **X**   **X**   **X**   **X**
   line    **X**   **X**   **X**   **X**   **X**           **X**
   tri     **X**   **X**   **X**   **X**   **X**           **X**
   quad    **X**   **X**   **X**   **X**   **X**           **X**
   tet     **X**   **X**   **X**   **X**   **X**           **X**
   pyr     **X**                                            
   hex     **X**   **X**   **X**   **X**   **X**           **X**
   ------- ------- ------- ------- ------- ------- ------- -------

 For example, this means that if you have a mesh that has hexes and
 tets in it, you could intersect it with a mesh that has anything but
 pyramids in it.

 Finally, **intersect\_elements** is based on a k-D-R tree
 implementation to improve performance in many circumstances.
 Unfortunately, there is no way to improve performance if the elements
 being intersected have many candidate elements in their bounding
 boxes. As such, there are situations where running time may be
 improved by refining mesh2 such that its elements are of comparable
 size with those of mesh1.

EXAMPLES:

 **intersect\_elements**/cmo\_grid/cmo\_sphere/

 **intersect\_elements**/cmo\_grid/cmo\_well/obswell
