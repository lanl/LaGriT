---
GENERATOR: 'Mozilla/4.79 
[en
] (X11; U; Linux 2.4.18-3 i686) 
[Netscape
]'
Generator: Microsoft Word 98
title: sort
---

sort
----

 The **sort** command creates a sort key for choosen node or element
 attributes. Sort can be in ascending or decending order and will not
 alter current mesh object values. One can perform a sort on a single
 attribute or in the case of **index** or **rank** sorting, one can
 perform a multi-key sort. The sort key that is created can be used in
 the **reorder** command to change the node or element ordering of a
 mesh object.




 The command parameters include the cmo\_name, followed by the
 sort\_type. The ordering is indicated by **ascending** or
 **decending**. A new attribute is created with the sort\_key\_name.
 Sorting is performed on the sort\_attributes which can be a single
 attribute name or a list of attribute names. This list is formed with
 attribute names in\_att1, in\_att2, through in\_attn.





cmo\_name: The choices for first parameter are the name of a valid mesh
object or **-def-** which select the currently active mesh object.



sort\_type: The sorting methods include **bins**, **index** or **rank**.

 **bins** A single-key sort which assigns each in\_att1 value a bin
 number. If in\_att1 is an integer then bins each have a unique integer
 value associated with them. If in\_att1 is a real number, bins are
 created with values which are within +-epsilon of each other, where
 epsilon=1.e-10
*abs( in\_att1(1)). If all array values are unique,
 then the maximum value of the index array will equal the number of
 entries in the sorted list. Otherwise, the maximum value of the index
 array will be less than the number of entries in the sorted list but
 will equal the number of unique entries in the list.

 **index** Constructs a single or multi-key index table such that
 in\_att1(ikey(1)) is the first entry in the sorted array,
 in\_att1(ikey(2)) is the second, etc.

 **rank** Constructs a single or multi-key rank table such that the
 tables ith entry give the rank of the ith entry of the sorted arrays.
 The rank table is derived from the index table by:

 foreach j = 1,N

 rank(index(j)) = j

 end



sort\_order: Choose between **ascending** or **decending**

 **ascending** Sort sort\_attributes in **ascending** order

 **decending** Sort sort\_attributes in **decending** order



sort\_key\_name: The name for an integer vector (VINT) which will hold
the output sort key values. If the name exists it will be used, if it
does not exist it will be created. If no name is given for
sort\_key\_name A name will be created which will be the concatination
of **'ikey\_**' and the first attribute name in sort\_attributes. (i.e.
/-def-/imt will produce a sort key named ikey\_imt)



sort\_attributes: The name of one or more existing attribute names. Each
attribute will be used as a node of element based array upon which the
sorting routine will sort. Multi-key sorts can have an arbitrary number
of input attributes. Attribute in\_att1(n) has priority over in\_att2(n)
in breaking ties. Note: all attributes are put into a real
*8 work array
before being sent to the sort routine.

SINGLE KEY FORMAT:

 **sort** / cmo\_name / **bins** / 
[ **ascending  decending** 
] /
 sort\_key\_name / sort\_attribute

MULTI-KEY FORMAT:

 **sort**/ cmo\_name / **index  rank** / 
[ **ascending  decending**
 
] / sort\_key\_name / in\_att1, in\_att2, in\_att3 ...

EXAMPLES:

 **sort** / cmo / **index / ascending** / ikey / imt zic yic xic

 Multi-key sort first by imt then to break ties consider z coordinate,
 then if there are further ties, use y coordinate. Use x coordinate as
 final tie breaker.

 **sort** / cmo **/ rank / decending** / ikey / yic

 Produce ranking of nodes based on y coordinate in decending order.

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

 **sort / xyz / bins**

 Old version no longer supported but syntax will work. Result is the
 same as previous three commands.

LINKS:

 [Example 1 for sort and reorder](sort_lagrit_input_1)

 [Example 2 for sort and reorder](sort_lagrit_input_2)

BEGIN OLD FORMAT - No longer supported but syntax will still work.

Old Format - **sort / xyz / 
[ index  bins  rank** 
]

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

