---
title: RMPOINT
tags: rmpoint
---

# RMPOINT

---------------------


Tag or remove nodes and elements from a mesh.   


Dudded nodes, or nodes marked for removal have  their  itp array mareked with **ifitpdud** (21).
Dudded elements have a negative value for the first entry in the **itet** vertex list.  

The tagged mesh object nodes and elements are treated as invisible until they are removed from the mesh object. 


## SYNTAX

<pre>
<b>rmpoint</b>/ifirst,ilast,istride/[<b>exclusive</b> or <b>inclusive</b> ]

<b>rmpoint</b>/<b>compress</b>/

<b>rmpoint/zero_volume</b>/threshold

<b>rmpoint/element</b> [tet_list or <b>eltset,get</b>,esetname]

<b>rmpoint/womesh</b> 

<b>rmpoint/sparse</b> 

</pre>


`/ifirst,ilast,istride` / [**exclusive** or **inclusive** ] does not remove but marks the selected nodes and elements for removal.
If  **exclusive** (default), an element is marked only if all of its nodes are in the selection. If  **inclusive**, any element with a node from the selected set  will be marked.  



**`compress`** remove and update all tagged nodes and elements. This will update arrays  and material-wise resequences all remaining nodes. This will change the node ordering and numbers of the mesh.

 

**`zero_volume`** will remove elements whose volumes are less than or equal to the specified `threshold`. 


**`element`** will remove all marked (negative **itet**) elements from the mesh. Elements can be specified by a `tet_list` or **eltset,get**,esetname.


**`womesh`** will delete stray nodes that are not connected to any element and that are not parent nodes. 


**`sparse`** is to be used with caution and requires reconnection when done.






## EXAMPLES

```
rmpoint/ pset,get,pset1
rmpoint/compress
```
Mark all the nodes in pset1 for removal.  Remove elements all of whose vertices are members of pset1.
Remove all marked nodes and update the mesh object arrays. 


```
rmpoint/zero_volume/1.e-16
rmpoint/compress
```
Remove all elements with volumes less than 1.e-16

```
rmpoint/element/27 259 1009
rmpoint/compress
```
Remove the three specified elements

```
rmpoint/element/eltset,get,e_mat1
rmpoint/compress
```
Remove the elements in the element set named e_mat1
