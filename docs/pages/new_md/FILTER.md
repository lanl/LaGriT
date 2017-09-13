---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: FILTER
---

 

 **FILTER**

  **Filter** is used in either node or element mode.  In 'node' mode
  it is used to mark for deletion nodes that lie within a distance,
  epsilon, of one another.  In 'element' mode it either marks or
  deletes elements which have the exact same set of nodes. The test is
  on node numbers, not the geometric position of nodes so for this to
  work one should filter the nodes
  first.[]{style="font-weight: bold;"

  **

  Node Mode:**

  Used to filter (mark for deletion) points that are geometricly
  close, (mesh object epsilon value)[, or if the
  ]{style="font-family: times;"[tolerance]{style="font-family: monospace;"[
  parameter is given, closer than the tolerance
  ]{style="font-family: times;"specified by the user. This command
  changes the node type of the deleted points to type 'dudded'
  (itp=21) but does not remove them from the point list. Note that at
  least one point must be specified in the point range
  (ifirst,ilast,istride) in order for this command to work properly.
  Dudded points (itp=21) can be removed from the mesh object by
  calling
  [[rmpoint/compress](RMPOINT.md).]{style="font-weight: bold;"

  

  **Element Mode:**

  Search a mesh object for duplicate elements. A duplicate element is
  defined as having the exact same set of nodes in the element
  connectivity list (itet). The order of the nodes in the connectivity
  does not matter. The element with the larger itetclr value (master)
  will be kept. The duplicate element will have its material color
  (itetclr) changed to max(itetclr) + 1. Two new element attributes
  (iclr1, iclr2) are added to the mesh object to keep track of the
  correspondence of master(retained)/duplicate(removed) elements and
  their original material id (itetclr). 


<div style="margin-left: 80px;">

For all elements the values of iclr1 are set their original itetclr
values.

For any element that is neither master nor duplicate, the value of icr2
is set to its itetclr value.

For an element that is a master, icrl2 is set to the original itetclr
value of its duplicate.

For an element that is a duplicate, icrl2 is set to the original itetclr
value of its master.




  For example, consider the 4 element mesh, where element 2 and 3 are
  duplicates and:

  <div style="margin-left: 40px;">
 
  Element 1,2 itetclr = 1

  Element 3,4 itetclr = 2

  then after:

  [filter]{style="font-weight: bold;"/[element]{style="font-weight: bold;"/10/[nodelete   ]{style="font-weight: bold;"                                                  

  

  Maximum material id max(itetclr)          =         
  2                         

  Duplicate Elements will be set to itetclr =         
  3                         

  search\_range                              =        
  10                         

  nelements searched                        =         
  4                         

  Number of duplicate element found         =         
  1                         

  

  cmo/printatt/cmohex1/itetclr/1 0
  0                                             

  Attribute:
  itetclr                                                             

           1         
  1                                                          

           2         
  3                                                          

           3         
  2                                                          

           4         
  2                                                          

  

  cmo/printatt/cmohex1/iclr1/1 0
  0                                               

  Attribute:
  iclr1                                                               

           1         
  1                                                          

           2         
  1                                                          

           3         
  2                                                          

           4         
  2                                                          

  

  cmo/printatt/cmohex1/iclr2/1 0
  0                                               

  Attribute:
  iclr2                                                               

           1         
  1                                                          

           2         
  2                                                          

           3         
  1                                                          

           4         
  2                                                          

 
  
 
  

  All elements are tested. The search for each element's duplicate
  does not occur over the entire element list. The default for
  search\_range is 10 and results in looking at the 10 elements in the
  element list sequentially above and 10 elements sequentially below
  the test element.  In the example given below the elements are
  sorted so that elements that are physically close to each other will
  be close to each other in the element list.

  

  The [search\_range]{style="font-family: monospace;" can be set by
  the user. Setting [search\_range]{style="font-family: monospace;"
  to a number larger than the number of elements will cause all
  elements to be searched.

  

  The algorithm will only detect one duplicate element per element. If
  there are more than two elements with the same connectivity, they
  can be found by calling **filter/element** multiple times.

  

  The default behavior is to not delete the duplicate elements. 
  However the duplicate elements will be deleted from the mesh if the
  parameter [delete ]{style="font-weight: bold;"is specified.

  

  In general if you are merging together two meshes and then want to
  delete duplicate elements the commands might be:

  

  
* Merge two mesh objects

  **addmesh **/ merge / cmohex / cmohex1 / cmohex2

  
* Create an attribute with the median x,y,z coordinate of each
  element

  [createpts]{style="font-weight: bold;" / [median

  ]{style="font-weight: bold;"
* Sort and reorder the elements based
  on the median points. This will insure that elements that occupy
  the

  
* same location will have element numbers near one another.

  [sort]{style="font-weight: bold;" / -def- /
  [index]{style="font-weight: bold;" /
  [ascending]{style="font-weight: bold;" / ikey / xmed ymed zmed

  [reorder]{style="font-weight: bold;"/ -def- /ikey

  
* Filter and remove duplicate nodes.

  [filter]{style="font-weight: bold;" / 1 0 0

  [rmpoint]{style="font-weight: bold;" / [compress

  ]{style="font-weight: bold;"
* Filter and remove duplicate
  elements.

  [filter]{style="font-weight: bold;" /
  [element]{style="font-weight: bold;" / /
  [delete]{style="font-weight: bold;"

  


 **FORMAT:**

  **filter** / ifirst,ilast,istride / 
[tolerance
]

  

  **filter** / [element]{style="font-weight: bold;" /
  
[[search\_range]{style="font-family: monospace;"[
]]{style="font-family: courier new,courier,monospace;"
  / 
[ **nodelete**  [delete]{style="font-weight: bold;" 
]


  **EXAMPLES:**


  **filter**

  Filter all nodes and delete duplicates with epsilon tolerance is set
  automaticly.

  

  **filter** / 1 0 0 / 1.e-3

  Filter all nodes and delete duplicates where epsilon tolerance is
  set by user to 1.e-3.

  

  **filter** / [pset]{style="font-weight: bold;"
  [get]{style="font-weight: bold;" point\_set

  Filter a subset of the nodes and delete duplicates with epsilon
  tolerance is set automaticly.

  

  **filter** / [element]{style="font-weight: bold;"

  Filter all elements and set itetclr of duplicates to max(itetclr) +
  1. Assign values to iclr1 and iclr2 arrays.

  

  **filter** / [element]{style="font-weight: bold;" / /
  [nodelete]{style="font-weight: bold;"

  Filter elements and set itetclr of duplicates to max(itetclr) + 1.
  Assign values to iclr1 and iclr2 arrays.

  

  **filter** / [element]{style="font-weight: bold;" / /
  [delete]{style="font-weight: bold;"

  Filter elements and delete duplicate elements. Assign values to
  iclr1 and iclr2 arrays.

  

  **filter** / [element]{style="font-weight: bold;" / 1e20 /
  [delete]{style="font-weight: bold;"

  Filter all elements (assuming there are less than 1e20)  with an
  exhaustive search and delete duplicate elements. Assign values to
  iclr1 and iclr2 arrays.


 



