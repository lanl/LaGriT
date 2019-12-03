---
title: ELTSET
tags: element set eltset
---

# ELTSET (Element Set) 

-------------

Associate a name with a element set based on various selection criteria and logical operators. Output element sets.
  
  
By convention, *`ifirst,ilast,istride`* syntax represents a set selection defined either by a set of elements from ifirst to ilast, with increments of istride (1 by default). A set selection can also be defined with **eltset,get,** *eset_name* where *eset_name* has been defined by the following **eltset** commands. Most commands with the syntax *ifirst,ilast,istride* can also use **eltset,get,** *elset_name*.


The following are the syntax and parameter definitions, click on options in table or scroll down the page.



|    |    |    |   
| :------ | :---------- | :------ | 
| [`attribute_name`](#attributename) |  [**`delete`**](#delete) | [**`list`**](#list) |
| [**`logicals`**](#logicals) |  [**`pset`**](#pset) | [**`region`**](#region) |
| [quality **`volume`**](#volume) | [quality **`aspect`**](#volume) |  [**`write`**](#write) |


<hr>


### `attribute_name` <a name="attributename"></a>
<pre>
<b>eltset</b>/eset_name/ attribute_name /<b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> or <b>le</b> or <b>ge</b> /value/
</pre>
forms an eltset from elements in `attribute_name` that compare to `value`. The comparators are **eq** equal to, **gt** greater than, **lt** less than, **ge** greater than or equal, **le** less than or equal to, **ne** not equal.


### **`delete`** <a name="delete"></a>
<pre>
<b>eltset</b>/eset_name/ <b>delete</b>
</pre>
removes a previously define element set from current mesh object



### **`list`** <a name="list"></a>
<pre>
<b>eltset</b>/eset_name/ <b>list</b>

<b>eltset</b> / / <b>list</b>
</pre>
list all elements in `eset_name`. If the 2nd argument is empty, list all names all of eltsets for the mesh object



### **`logicals`** <a name="logicals"></a>
<pre>
<b>eltset</b>/eset_name/ <b>union</b> <b>inter</b> <b>not</b> / eset_list/
</pre>
logical operations **`union`**, **`inter`** and **`not`** act on previously defined eltsets. The definition of the unary operator **`not`** is extended such that **`not`**/e1/e2 means e1 and (not(e2)).


### **`pset`** <a name="pset"></a>
<pre>
<b>eltset</b>/eset_name/ <b>inclusive</b> or <b>exclusive</b> or <b>face</b> /<b>pset,get</b>, pset_name/
</pre>
forms an eltset from nodes in a pset with name `pset_name`. The following criteria are used to define which elements node set are included. The selection criteria is required.

- **inclusive**  all elements any of whose nodes is in pset, if a node is on any element, include it. Generally the largest selection.
- **exclusive**  all elements all of whose nodes are in pset, all nodes of an element must be in the set. Generally a smaller selection.
- **face**  elements if all nodes of a face are in pset



### **`region`**  **`mregion`** <a name="region"></a>
<pre>
<b>eltset</b>/eset_name/ <b>region</b> or <b>mregion</b> / region_name /
</pre>
will return all nodes that are in the specified geometry **region** or material **mregion** given by its `region_name`.

This command calculates the center of mass of the element and determine which **mregion** or **region** the center lies in. It is possible if the interface surfaces are curved that the center will not lie in the same material or geometry region as the vertices. Using values of **itetclr** may give the better result.


### **`volume`** **`aspect`** <a name="volume"></a>
<pre>
<b>eltset</b>/eset_name/ <b>volume</b>/ <b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> or <b>le</b> or <b>ge</b> /value

<b>eltset</b>/eset_name/ <b>aspect</b>/ <b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> or <b>le</b> or <b>ge</b> /value
</pre>
forms an eltset based on the quality criteria **volume** or **aspect** ratio. The `value` and comparator detirmines the set selection.


### **`write`** <a name="write"></a>
<pre>
<b>eltset</b>/eset_name or <b>-all-</b> / <b>write</b> /file_name[.cellset]/ <b>ascii</b> or <b>binary</b>
</pre>
write or dump an element list with name `eset_name` to a file, options are **ascii** or **binary**. The argument **-all-** will write all eltsets in the mesh object. 



 
## EXAMPLES 

```
eltset/element_set1/itetclr/eq/4 

eltset/element_set2/face/pset/get/mypset

eltset/element_set3/inclusive/pset/get/mypset

eltset/element_set4/region/upper 

eltset/element_set5/volume/lt/3.0 

eltset/element_set5/delete

eltset /  / list 
```
These are various examples of the eltset command.


```
cmo / select / mo_hex
intersect_elements / mo_hex / mo_wells / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset,get,e_refine
cmo / setatt / mo_hex / if_inter / 1,0,0 / 0
eltset / e_refine / delete
```

This set of commands will intersect mesh object named mo_hex with mesh object named mo_wells. The attribute **if_inter** is created by **`intersect_elements`** and has a non-zero value for elements intersected by mo_wells. The selected set is refined. It is good practice to clean up if using names and attributes over again. Here we set **if_inter** values to 0 and remove the element set when we are done.



