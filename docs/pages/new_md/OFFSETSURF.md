---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: OFFSETSURF
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
 
   R\_new = R\_old + dist 
* N\_node

**FORMAT:**

 

**offsetsurf**/new\_cmo/old\_cmo

** ** 

** ** 

**EXAMPLES:**

**offsetsurf**/cmo\_out/cmo\_in/d

          offset a triangulated surface a distance d

**offsetsurf**/cmo\_out/cmo\_in/d/[xy,xz,yx,yz,zx,zy]

offset a line cmo a distance d in a direction parallel to the specified
plane.

**offsetsurf**/cmo\_out/cmo\_in/d/x y z

offset a line cmo a distance d in the direction specified by the vector
(x,y,z)
