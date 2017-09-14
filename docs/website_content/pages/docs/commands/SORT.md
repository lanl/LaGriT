---
title: sort
---

sort
----

 The **sort** command creates a sort key for chosen node or element
 attributes. Sort can be in ascending or descending order and will not
 alter current mesh object values (though the **line\_graph** option
 will create three new attributes; see below). One can perform a sort
 on a single attribute or in the case of **index** or **rank** sorting,
 one can perform a multi-key sort. The **line\_graph** sort does not
 sort on an attribute but instead sorts line segment elements into a
 reasonable order based on connectivity. In each case the sort key that
 is created can be used in the **reorder** command to change the node
 or element ordering of the mesh object.

 The command parameters include the cmo\_name, followed by the
 sort\_type. The ordering is indicated by **ascending** or
 **descending**. A new attribute is created with the sort\_key\_name.
 For the **bins**, **index**, and **rank** options, sorting is
 performed on the sort\_attributes which can be a single attribute name
 or a list of attribute names. This list is formed with attribute names
 in\_att1, in\_att2, through in\_attn. The **line\_graph** option does
 not take any attributes as arguments because it sorts based on the
 connectivity of the elements, which must be line segments.

cmo\_name: The choices for first parameter are the name of a valid mesh
object or **-def-** which select the currently active mesh object.



sort\_type: The sorting methods include **bins**, **index**, **rank**
and **line\_graph**.

 **bins** A single-key sort which assigns each in\_att1 value a bin
 number. If in\_att1 is an integer then bins each have a unique integer
 value associated with them. If in\_att1 is a real number, bins are
 created with values which are within +-epsilon of each other, where
 epsilon=1.e-10
*abs(real\_bin\_value). If all array values are unique,
 then the maximum value of the index array will equal the number of
 entries in the sorted list. Otherwise, the maximum value of the index
 array will be less than the number of entries in the sorted list but
 will equal the number of unique entries in the list.

 With the **bins** method, an optional user specified epsilon
 multiplier, **epsilon\_user**, will override the default value of
 1.e-10.

 

 **index** Constructs a single or multi-key index table such that
 in\_att1(ikey(1)) is the first entry in the sorted array,
 in\_att1(ikey(2)) is the second, etc.


 **rank** Constructs a single or multi-key rank table such that the
 tables ith entry give the rank of the ith entry of the sorted arrays.
 The rank table is derived from the index table by:

 foreach j = 1,N

 rank(index(j)) = j

 end

 **line\_graph** This option requires all elements to be line segments,
 and it arranges them in a reasonable order. In particular, it makes
 the following guarantees:

 -   Each connected component will be arranged together.
 -   Polylines (chains of line segments with no branching or loops)
     will be in order from one end to the other.
 -   Polygons will be in order starting from one segment and looping
     back around to the same place.

 The sorted order for components which are not polylines or polygons is
 unspecified, but it will usually be reasonable because the underlying
 algorithm visits the edges via depth first search.

 The **line\_graph** option for **elements** also generates the
 following three integer element attributes:

 -   cid: A component id for distinguishing separate connected
     components. Each connected component receives a unique positive
     integer starting from one. This allows you to identify all the
     edges in a particular component by selecting all elements with a
     particular component id.
 -   ctype: The component type, represented as an integer from 1 to 5.

     1 (Polyline)
     :   A connected chain of segments with no branches or loops.

     2 (Tree)
     :   A connected acyclic component.

     3 (Polygon)
     :   A component consisting solely of a single loop.

     4 (Shared Edges)
     :   A component which has a pair of cycles with a shared edge.

     5 (Other)
     :   Anything which does not fit into the above categories.

 -   loop\_id: This is a unique positive integer assigned to each
     simple cycle. Edges that are not part of a cycle receive a default
     value of zero. If an edge is shared (i.e. part of more than one
     cycle) then it will be labeled with only one of its cycles. In
     this case, the cycle corresponding to the label is not fully
     specified because there is more than one right answer.

 The **line\_graph** option for **nodes** is based on the option for
 elements, except that it does not create extra attributes. Based on
 the sorted elements, the nodes will be reordered in the same sequence.
 This is necessary for triangulation as "TRIANGULATE" routine requires
 the nodes to be in clockwise/counterclockwise order.



sort\_order: Choose between **ascending** or **descending**

 **ascending** Sort sort\_attributes in **ascending** order

 **descending** Sort sort\_attributes in **descending** order

The **line\_graph** sort will ignore this option, but it still expects
the field to be present for consistency with the other sort variations.



sort\_key\_name: The name for an integer vector (VINT) which will hold
the output sort key values. If the name exists it will be used, if it
does not exist it will be created. If no name is given for
sort\_key\_name A name will be created which will be the concatination
of **'ikey\_**' and the first attribute name in sort\_attributes (i.e.
/-def-/imt will produce a sort key named ikey\_imt). For the
**line\_graph** option, the default key will be called
**ikey\_line\_graph**.



sort\_attributes: The name of one or more existing attribute names. Each
attribute will be used as a node of element based array upon which the
sorting routine will sort. Multi-key sorts can have an arbitrary number
of input attributes. Attribute in\_att1(n) has priority over in\_att2(n)
in breaking ties. Note: all attributes are put into a real
*8 work array
before being sent to the sort routine.

SINGLE KEY bins **FORMAT:**

 **sort** / cmo\_name / **bins** / [ **ascending  descending** ] /
 sort\_key\_name / sort\_attribute / [epsilon\_user]

MULTI-KEY **FORMAT:**

 **sort**/ cmo\_name / **index  rank** / [ **ascending  descending**
 ] / sort\_key\_name / in\_att1, in\_att2, in\_att3 ...

LINE GRAPH **FORMAT:**

 **sort** / cmo\_name / **line\_graph** / [ **ascending  descending**
 ] / sort\_key\_name / [**elements  nodes**]

**EXAMPLES:**

 **sort** / cmo / **index / ascending** / ikey / imt zic yic xic

 Multi-key sort first by imt then to break ties consider z coordinate,
 then if there are further ties, use y coordinate. Use x coordinate as
 final tie breaker.

 **sort** / cmo **/ rank / descending** / ikey / yic

 Produce ranking of nodes based on y coordinate in descending order.

 **sort** / cmo / **index /-def-/-def-**/ xic yic zic

 Produce index of noded coordinates. This would be like a line sweep
 sort where the sweep is first along x coordinate then y then z.

 **sort** / cmo / **bins / ascending** / i\_index / xic

 **sort**/ cmo / **bins / ascending** / j\_index / yic

 **sort** / cmo / **bins / ascending** / k\_index / zic

 If the cmo were a finite difference grid of points, the above three
 commands would produce the finite difference indexing. All points with
 the same x value would be in the same i\_index bin, all points with
 the same y value would be in the same j\_index bin, etc.

 **sort** / cmo / **line\_graph** / **ascending** / ikey / elements

 Sort the line segment elements into a reasonable order based on
 connectivity. This also creates attributes cid, ctype, and loop\_id
 (see above).

 **sort / xyz / bins**

 Old version no longer supported but syntax will work. Result is the
 same as previous three commands.

LINKS:

 [Example 1 for sort and reorder](../sort_lagrit_input_1)

 [Example 2 for sort and reorder](../sort_lagrit_input_2)

BEGIN OLD FORMAT - No longer supported but syntax will still work.

Old Format - **sort / xyz / [ index  bins  rank** ]

 sort/xyz/index - sorts the x,y,z coordinate integer arrays i\_index,
 j\_index, k\_index such that xic(i\_index(i)) i=1,..nnodes lists the
 coordinate in ascending order.

 sort/xyz/bins - sorts the x,y,z coordinates and assigns each i\_index,
 j\_index, k\_index values in ascending order of the bin number of the
 sorted list.

 sort/xyz/rank - sorts the x,y,z coordinates and assigns each i\_index,
 j\_index, k\_index values the ranking of the node in the sorted list.

 If all array values are unique, then the maximum value of the index
 array will equal the number of entries in the sorted list. Otherwise,
 the maximum value of the index array will be less than the number of
 entries in the sorted list but will equal the number of unique entries
 in the list.

 For example given x = 0, 1, 2, 1, 0

 sort/xyz/index returns i\_index = 5, 1, 4, 2, 3

 sort/xyz/bins returns i\_index = 1, 2, 3, 2, 1

 sort/xyz/rank returns i\_index = 2, 4, 5, 3, 1

END OLD FORMAT - No longer supported but syntax will still work.

