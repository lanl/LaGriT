---
title: MODE
tags: mode
---

# MODE

----------------------


The MODE Command  sets up several optimization options.


# SYNTAX

<pre>
<b>mode/discrete</b>/surface_cmo/tolldamage

<b>mode/adaption_field</b>/field_name

<b>mode/recon/geom</b> <b>delaunay adaption</b>
</pre>
  

### Discrete optimization

**`discrete`**/surface_cmo/tolldamage
 
If this mode is set, **refine**, **smooth**, **merge** will
   require any operation that involves nodes on the specified surface
   to result in a mesh whose surface nodes are also members of the
   `surface_cmo`.

A mesh object attribute associated with the 3d mesh named
   discrete_optimize will be created and its value will be the name
   of the surface mesh object.
 
  
### Error adaption

**`adaption_field`** / field_name
 
If this mode is set, optimization operations will be based on
   reducing error.  A mesh object attribute associated with the 3d
   mesh named 'adaption_field' will be created and it's value will
   be the name of the field.
 
  
### Reconnection

**`recon/geom`**

**`recon/delaunay`**

**`recon/adaption`**
 
 Setting this mode will determine the criterion used to
   [reconnect](RECON.md) the mesh.  The default mode is
   **delaunay** and setting mode to **delaunay** will cause recon to
   attempt to create a [delaunay mesh](CONNECT1.md).  Setting mode
   to **geom** will reconnect to increase inscribed radii of
   elements.  Setting mode to adaption will reconnect to reduce
   solution error.  The parameter `field_name` must be set with the
   **`mode/adaption_field`** command.

 

## EXAMPLES

```
mode/adaption_field/solution

mode/recon/adaption
```
  

 
