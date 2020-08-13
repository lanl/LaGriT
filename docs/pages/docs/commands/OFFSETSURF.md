---
title: OFFSETSURF
tags: offsetsurf
---


# OFFSETSURF

-----------------------


This command is used to offset a triangulated surface in the direction of the surface
  outward normal, i.e., normal surface motion. 
For each node a 'synthetic' unit outward normal **N** is computed based on the
weighted average angle of the normals of the triangles shared by that node. 

To add the nodes in the new surface to the main mesh object use the
  **`copypts`** command. To add the new surface to the main mesh object use a **`surface`** command with `new_cmo` 
as the sheet name (e.g.  **surface**/s2d/bndy_type)




## SYNTAX

<pre>
<b>offsetsurf</b>/ new_cmo / old_cmo / dist / [keywords]
</pre>


`new_cmo` is the name of the new surface.


`old_cmo` is the surface to be used in generating the offset surface. 


`dist` is the distance to offset the surface. The new
  node coordinates, R_new, are computed using the formula: R_new = R_old + dist times N_node


 
The following keywords are available:


**`keepatt, keep_angle`** Compute node angle weighted normals and keep the vector components in three scalar attributes **x_n_norm y_n_norm z_n_norm**


**`keep_area`** Compute node area weighted normals and keep the vector components in three scalar attributes **x_n_norm y_n_norm z_n_norm**


**`xzero`** Compute the full offset direction vector but set x component to zero


**`yzero`**  Compute the full offset direction vector but set y component to zero


**`zzero`**  Compute the full offset direction vector but set z component to zero


**`xy, xz, yx, yz, zx, zy`** - these keywords come after the 'dist' value. They constrain the offset to
   be parallel to the specified plane. These arguments can be used with a line type mesh object to constrain the offset to a particular plane.



 

## EXAMPLES

```
offsetsurf/mo_out/cmo_in/d
```
offset a triangulated surface a distance d using angle weighted normals


```
offsetsurf/cmo_out / cmo_in / d / keep_area 
```
offset using area weighted normals


```
offsetsurf/mo_out/cmo_in/d/[xy,xz,yx,yz,zx,zy]
```
offset a line cmo a distance d in a direction parallel to the specified plane


```
offsetsurf/mo_out/cmo_in/d/x y z
```
offset a line cmo a distance d in the direction specified by the vector (x,y,z)

 

 

