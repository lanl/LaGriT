---
title: OFFSETSURF
tags: ok
---

 **OFFSETSURF**

  Offsets triangulated surfaces in the direction of the surface
  outward normal, i.e., normal surface motion. For each node a
  'synthetic' unit outward normal **N** is computed based on the
  weighted average angle of the normals of the triangles shared by
  that node. old\_cmo is the surface to be used in generating the
  offset surface. new\_cmo is the name of the new surface.

  To add the nodes in the new surface to the main mesh object use the
  **copypts** command. To add the new surface to the main mesh object
  use a **surface** command with new\_cmo as the sheet name (e.g.
  **surface**/s2d/bndy\_typedist is given in user coordinates (i.e.
  whatever units the old\_cmo mesh object was defined in.) The new
  node coordinates, R\_new, are computed using the formula:
 
   R\_new = R\_old + dist * N\_node
 
  Various keywords control the behavior of the command:
 
   The following keywords can appear in the 5th, 6th, 7th or 8th
   argument position.

   **keepatt, keep\_angle** -  Compute node angle weighted normals and
   keep the vector components in three scalar attributes x\_n\_norm,
   y\_n\_norm, z\_n\_norm

   **keep\_area** - Compute node area weighted normals and keep the
   vector components in three scalar attributes, x\_n\_norm,
   y\_n\_norm, z\_n\_norm

   **xzero**  - Compute the full offset direction vector but set x
   component to zero

   **yzero**  - Compute the full offset direction vector but set x
   component to zero

   **zzero**  - Compute the full offset direction vector but set x
   component to zero

   
   The following keywords can appear in the 5th argument position.

   **xy, xz, yx, yz, zx, zy** - these keywords constrain the offset to
   be parallel to the specified plane. These arguments can be used
   with a line type mesh object to constrain the offset to a
   particular plane.

**FORMAT:**
 

**offsetsurf**/new\_cmo/old\_cmo/dist/keyword

 

**EXAMPLES:**

    offsetsurf/mo_out/cmo_in/d
    
offset a triangulated surface a distance d using angle weighted normals

    offsetsurf/cmo_out / cmo_in / d / keep_area 

offset using area weighted normals

    offsetsurf/mo_out/cmo_in/d/[xy,xz,yx,yz,zx,zy]

offset a line cmo a distance d in a direction parallel to the specified
plane

    offsetsurf/mo_out/cmo_in/d/x y z

offset a line cmo a distance d in the direction specified by the vector
(x,y,z)

 

 

