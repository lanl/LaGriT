---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: FIELD
---

 

 **MODE**

  The MODE Command  sets up several optimization options

  Currently implemented are:

  (1) discrete optimization:

  **mode/discrete**/surface\_cmo/tolldamage
 
   if this mode is set, **refine,** **smooth**, **merge** will
   require any operation that involves nodes on the specified surface
   to result in a mesh whose surface nodes are also members of the
   surface\_cmo.

   A mesh object attribute associated with the 3d mesh named
   discrete\_optimize will be created and its value will be the name
   of the surface mesh object.
 
  
(2) error\_adaption

  **mode** **/adaption\_field**/field\_name
 
   if this mode is set, optimization operations will be based on
   reducing error.  A mesh object attribute associated with the 3d
   mesh named 'adaption\_field' will be created and it's value will
   be the name of the field.
 
  
(3) reconnection

  **mode/recon** **/geom**

  **mode/recon** **/delaunay**

  **mode/recon** **/adaption**
 
   Setting this mode will determine the criterion used to
   [reconnect](RECON.md) the mesh.  The default mode is
   **delaunay** and setting mode to **delaunay** will cause recon to
   attempt to create a [delaunay mesh](CONNECT1.md).  Setting mode
   to **geom** will reconnect to increase inscribed radii of
   elements.  Setting mode to adaption will reconnect to reduce
   solution error.  Field\_name must be set with the
   **mode** **/adaption\_field** command.

 **FORMAT:**

  **mode/discrete**/surface\_cmo/tolldamage

  **mode** **/adaption\_field**/field\_name

  **mode/recon** **/geom** **delaunay** **adaptio**n

   

 **EXAMPLES:**

  **mode** **/adaption\_field**/solution

  **mode** **/recon** **/adaption**
  All optimization including **[massage](MASSAGE.md)** commands that
  follow will be performed to reduce error in the user defined field
  solution.

 
