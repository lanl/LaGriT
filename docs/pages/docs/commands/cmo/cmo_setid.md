---
title: "cmo/set_id"
tags: cmo set_id
---

This command creates integer attributes that contain the node and/or element number. This is useful if later operations delete nodes or elements causing renumbering, these attributes will contain the original node or element number.

# cmo/set_id #

<pre>
 <b>cmo/set_id</b> /mo_name/ <b>both</b>  <b>node</b>  <b>element</b> /[attribute_name1]/[attribute_name2]
</pre>

`mo_name` is the name of the mesh object.

**both** or **node** or **element** indicate which attributes to create, node numbers, element numbers, or both. If no attribute name is given on the command line, the default attribute names *`id_node`* or  *`id_elem`* will be used.
 
  
## EXAMPLES


```
cmo/create/cmo1///hex
createpts/brick/xyz/11,11,11/0.,0.,0./1.,1.,1./1,1,1

cmo/set_id/cmo1/node
cmo/set_id/cmo1/element
cmo/set_id/cmo1/both
cmo/set_id/cmo1/node/id_node2
cmo/set_id/cmo1/element/id_elem2
```

These **`set_id`** commands will add node and element attributes to the mesh object *`cmo1`*. Node and element id numbers will be written to default node attribute names  *`id_node`*  *`id_elem`* and another set of id numbers will be written to user attributes with names *`id_node2`*  *`id_elem2`*


```
cmo/set_id/cmo1/both/id_node1/id_elem1

pset/p_xgthalf/geom/xyz/1,0,0/.5,0,0/1,1,1
  rmpoint/pset,get,p_xgthalf
  rmpoint/compress
  dump/set_id.inp/cmo1
 ```
 
This example copies node id numbers into node attribute id_node1 and element id numbers into element attribute id_elem1. A set of points are selected within a box shaped geometry and then deleted. The **rmpoint/compress** will remove the nodes tagged for removal and reorder the mesh nodes and element to their new size. The original node and element id numbers are preserved in the added attributes.
 

  

  

  

