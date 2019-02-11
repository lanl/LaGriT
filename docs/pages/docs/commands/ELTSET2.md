---
title: ELTSET
tags: element set eltset
---

# ELTSET (Element Set) #

Associate a name with a element set based on various selection criteria and logical operators. Output element sets.
  
  
By convention, *`ifirst,ilast,istride`* syntax represents a set selection defined either by a set of elements from ifirst to ilast, with increments of istride (1 by default). A set selection can also be defined with **eltset,get,** *eset_name* where *eset_name* has been defined by the following **eltset** commands. Most commands with the syntax *ifirst,ilast,istride* can also use **eltset,get,** *elset_name*.


## SYNTAX ##

<pre>
<b>eltset</b>/eset_name/element_attribute_name/ <b>eq</b> OR <b>ne</b> OR <b>lt</b> OR <b>gt</b> OR <b>le</b> OR <b>ge</b> /value/

<b>eltset</b>/eset_name/ <b>union</b> <b>inter</b> <b>not</b> / eset_list/
  
<b>eltset</b>/eset_name/ <b>delete</b>/eset_name/

<b>eltset</b>/eset_name/ <b>inclusive</b> <b>exclusive</b> <b>face</b> /<b>pset,get</b>, pset_name/

<b>eltset</b>/eset_name/ <b>region</b> <b>mregion</b> / region_name OR mregion_name/

<b>eltset</b>/eset_name/ <b>volume</b>/ <b>eq</b> OR <b>ne</b> OR <b>lt</b> OR <b>gt</b> OR <b>le</b> OR <b>ge</b> /value

<b>eltset</b>/eset_name/ <b>aspect</b>/ <b>eq</b> OR <b>ne</b> OR <b>lt</b> OR <b>gt</b> OR <b>le</b> OR <b>ge</b> /value

<b>eltset</b>/eset_name/ <b>list</b>

<b>eltset</b>/eset_name/ <b>write</b> /file_name[.cellset]/ <b>ascii</b> OR <b>binary</b>

<b>eltset</b>/-all- / <b>write</b> /file_name[.cellset]/ <b>ascii</b> OR <b>binary</b>
  
</pre>

 
  1.  **eq** equal to, **gt** greater than, **lt** less than, **ge**
      greater than or equal, **le** less than or equal to, **ne** no
      equal to value of an element attribute
  2.  **inclusive** pset membership - all elements any of whose nodes
      is in pset
  3.  **exclusive** pset membership - all elements all of whose nodes
      are in pset
  4.  **face** pset membership - elements if all nodes of a face are
      in pset
  5.  **union, inter, not**, boolian comparison with other eltsets
  6.  **region** or **mregion** membership
  7.  quality criteria (**volume** or **aspect** ratio)
  8.  **list** - list element set members to screen, or list the names
      of element sets if no 'eltset\_name' is specified.
  9.  **write** - write element set members to a file
  10. **delete** - remove element set from mesh object
 
  The **mregion** and **region** form of this command calculate the
  center of mass of the element and determine which mregion or region
  the center lies in. It is possible if the interface surfaces are
  curved that the center will not lie in the same mregion or region as
  the vertices. Using itetclr will give the better result.

 
## EXAMPLES ##

```
eltset/element_set1/itetclr/eq/4 

eltset/element_set2/face/pset/get/mypset

eltset/element_set3/inclusive/pset/get/mypset

eltset/element_set4/region/upper 

eltset/element_set5/volume/lt/3.0 

eltset/element_set5/delete

eltset / /list 
```

```
cmo / select / mo_hex
intersect_elements / mo_hex / mo_wells / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset,get,e_refine
cmo / setatt / mo_hex / if_inter / 1,0,0 / 0
eltset / e_refine / delete
```

These commands will intersect mesh object named mo_hex with mesh object named mo_wells. The attribute if_inter is created by **intersect_elements** and has a non-zero value for elements intersected by mo_wells. The selected set is refined. It is good practice to clean up if using names and attributes over again. Here we set if_inter values to 0 and remove the element set when we are done.

