---
Title: GEOMETRY
tags: geometry, regions
---

# GEOMETRY

------------------

Initialize a geometry or change the name of the current geometry.

To associate this geometry with a mesh object
  use the [cmo/geometry](cmo/cmo_geom.md) command.  See [Geometries](../geometries.md) for a
  discussion of geometry.

<pre>
<b>geometry/create</b> /geom_name
 
<b>geometry/release</b> /geom_name
</pre>

**`create`** Initialize a geometry called `geom_name`. 
 
**`release`** Change the name of the current geometry to `geom_name`. Release all data structures related to geometry, geom_name and remove geom_name from the list of geometries.


## EXAMPLES

```
geometry/create/new_geom/

geometry/release/old_geom/
```

   

   

 
