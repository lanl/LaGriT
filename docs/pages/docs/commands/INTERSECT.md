---
title: INTERSECT
tags: intersect
---


# INTERSECT

-------------------

Creates a new Mesh Object from the intersection of two existing Mesh
  Objects. The existing Mesh Objects have to be topologically 2D and
  geometrically 3D. The created Mesh Object will be topologically 1D
  and geometrically 3D. 
  
  
## SYNTAX

<pre>
<b>intersect</b>/cmo_out/cmo_1_in/cmo_2_in
</pre>


`cmo_out` is the new mesh object.  
  
Node quantities for the new Mesh Object will
  be created by interpolation on the corresponding node quantities of
  the first input Mesh Object `cmo_1_in` from the second mesh object `cmo_2_in`.

This command will also apply the **line_graph** option of the
  **`sort`** command on the new Mesh Object. This will sort the elements
  (which will be line segments) into a reasonable order based on their
  connectivity, and will also create element attributes **ctype, cid, loop_id. 
  
**`intersect`** will create a temporary sort key and use
  that to reorder the elements, so there is no need to use your own
  sort key. For more details on the sorting and on the created element
  attributes, please see [sort](SORT.md).

 
