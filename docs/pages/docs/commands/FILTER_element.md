---
title: FILTER/ELEMENT 
tags: filter element 
---


# FILTER/ELEMENT

--------------------

## SYNTAX

<pre>
<b>filter</b>/[ <b>element</b>]/[search_range]/[<b>nodelete</b>|[<b>delete</b>]
</pre>
  

  Search a mesh object for duplicate elements. A duplicate element is
  defined as having the exact same set of nodes in the element
  connectivity list (itet). The order of the nodes in the connectivity
  does not matter. The element with the larger itetclr value (master)
  will be kept. The duplicate element will have its material color
  (itetclr) changed to max(itetclr) + 1. Two new element attributes
  (iclr1, iclr2) are added to the mesh object to keep track of the
  correspondence of master(retained)/duplicate(removed) elements and
  their original material id (itetclr). 

For all elements the values of iclr1 are set their original itetclr
values.

For any element that is neither master nor duplicate, the value of icr2
is set to its itetclr value.

For an element that is a master, icrl2 is set to the original itetclr
value of its duplicate.

For an element that is a duplicate, icrl2 is set to the original itetclr
value of its master.


  For example, consider the 4 element mesh, where element 2 and 3 are:
 
  Element 1,2 itetclr = 1

  Element 3,4 itetclr = 2

  then after:

  [filter]{style="font-weight: bold;"/[element]/10/[nodelete]                              

  

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

  

  The [search\_range] can be set by
  the user. Setting [search\_range] 
  to a number larger than the number of elements will cause all
  elements to be searched.

  

  The algorithm will only detect one duplicate element per element. If
  there are more than two elements with the same connectivity, they
  can be found by calling **filter/element** multiple times.

  

  The default behavior is to not delete the duplicate elements. 
  However the duplicate elements will be deleted from the mesh if the
  parameter [delete] is specified.

  

  In general if you are merging together two meshes and then want to
  delete duplicate elements the commands might be:

  

  
* Merge two mesh objects

  **addmesh **/ merge / cmohex / cmohex1 / cmohex2

  
* Create an attribute with the median x,y,z coordinate of each
  element

  [createpts] / [median] 
* Sort and reorder the elements based
  on the median points. This will insure that elements that occupy
  the

  
* same location will have element numbers near one another.

  [sort] / -def- /
  [index] /
  [ascending] / ikey / xmed ymed zmed

  [reorder] / -def- /ikey

  
* Filter and remove duplicate nodes.

  [filter]  / 1 0 0

  [rmpoint] / [compress]
* Filter and remove duplicate
  elements.

  [filter]  /
  [element] / 
  [delete]

 **EXAMPLES:**

 

 **filter** / [element] 

 Filter all elements and set itetclr of duplicates to max(itetclr) + 1.
 Assign values to iclr1 and iclr2 arrays.

 

 **filter** / [element] / [nodelete] 

 Filter elements and set itetclr of duplicates to max(itetclr) + 1.
 Assign values to iclr1 and iclr2 arrays.

 

 **filter** / [element]/ [delete] 

 Filter elements and delete duplicate elements. Assign values to iclr1
 and iclr2 arrays.

 

 **filter** / [element] / 1e20 / [delete] 

 Filter all elements (assuming there are less than 1e20)  with an
 exhaustive search and delete duplicate elements. Assign values to
 iclr1 and iclr2 arrays.

