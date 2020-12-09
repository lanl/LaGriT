---
Title: "cmo/geometry"
Tags: cmo geometry
---

# cmo/geometry

------------

Associate a geometry with a mesh object. The geometry is created with [surface](../SURFACE.md) and [region](..REGION.md) commands.

 All geometry information will be updated to the geometry information of the named geometry.  This
  includes number of surfaces, regions, material regions, current geometry name, and definitions of active surfaces, regions and material regions.  [Geometries](../../geometries.md) for a discussion of geometry.  The [cmo/constraint](cmo_constraint.md) command might also be required.

## SYNTAX

<pre>
<b>cmo/geometry</b> /cmo_name / geom_name
</pre>

`cmo_name` is the mesh object whose attribute **geometry_name** will be assigned `geom_name`. The mesh object and geometry must have been previously created. 

  

## EXAMPLES

```
geometry/create/blobgeom/
cmo/create/mo1

cmo/geometry/mo1/blobgeom/
cmo/constraints/mo_sink/cmo_src
```
