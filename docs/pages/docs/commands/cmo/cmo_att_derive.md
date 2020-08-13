---
title: 'cmo/addatt - add an attribute to a mesh object'
tags: cmo addatt
---

# cmo/attribute_derive

# cmo/attribute_union

------------


 

 
## SYNTAX

<pre>
 <b>cmo/attribute_derive</b> / sink_mo_name /  [ src_mo_name ]
 
 <b>cmo/attribute_union</b> / sink_mo_name /  src_mo_name 
</pre>

**cmo/attribute_derive** is used to give one mesh object
 (at least) the same set of attributes as another mesh object. This is
 useful, for example, for merging two mesh objects. Specifically, it
 looks at the set of attributes present in the source mesh, compares it
 to the set of attributes in the sink mesh, and adds to the sink mesh
 any attributes that it is missing.

**cmo/attribute_union** is a wrapper and executes **attribute_derive** twice, once in each direction and is used to give two mesh objects the same set of attributes as each other. This is useful, for example,
 for merging two mesh objects. Specifically, it looks at the set of
 attributes present in each mesh, compares it to the set of attributes
 in the other mesh, and makes it so each mesh posesses the union of the
 two sets of attributes. 

  
`sink_mo_name` is the new or modified mesh object.

`src_mo_name` is mesh object to derive from. If no source mesh is
 given, it will use the current mesh object. The **attribute_union** command needs both the sink and source meshes to be designated.



## EXAMPLES

```
cmo/attribute_derive/cmo_sink/cmo_src
cmo/attribute_derive/empty_cmo

cmo/attribute_union/ cmo2 / cmo1
```


 

 

 
