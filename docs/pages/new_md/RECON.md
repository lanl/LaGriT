---
GENERATOR: 'Mozilla/4.79C-SGI [en] (X11; U; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: RECON
---

 

 **RECON**

  This command flips connections in the mesh to get restore the
  Delaunay criterion or to improve element shapes. The option 1
  (recommended for 2D meshes only) adds points on the boundaries if
  needed. The option 0 (default) specifies that no points are to be
  added on the boundaries. The standard method used by recon is to
  flip connections based on the in-sphere test (the circumsphere of a
  tetrahedral element should contain no other nodes). Additional
  flipping criteria are available. The Minimum Error Gradient Adaption
  ([mega](RADAPT.md)) can be invoked by changing the value of the
  code variable [ivoronoi](meshobject.md)
  (**cmo/setatt**//ivoronoi/-2). The effect of this option is to
  generate well shaped elements; however the grid will not be
  Delaunay. If the user has a function to used for adaptive
  reconnection this option is available by setting the code variable
  ivoronoi to 2 (**cmo/setatt**//ivoronoi/2). The user will have to
  supply an external function.

  If damage is specified then flips on exterior boundaries are checked
  to verify that the maximum depth of deformation of the external
  boundary does not exceed the value of damage. The default value of
  damage is 1% of the problem size. This setting prevents connecting
  across corners if the external boundary is a reflective box.
 
  If the keyword **checkaxy** is provided, then 2D flips are
  suppressed if the new triangles

  would have xy-projected areas less than EPSILONA.
 
  **recon** is called by other LaGriT commands such as **massage**. 
  To disable recon set ivoronoi to 5 (**cmo/setatt**//ivoronoi/5).
 
  **recon** will by default reconnect across interface edges.  To
  restrict reconnection to interior faces and exterior boundary faces,
  set [iopt2to2](meshobject.md) to 0 (**cmo/setatt**//iopt2to2/0)

 **FORMAT:**

  **recon**/[**1** **0**]/[damage]/[**checkaxy**]

 **EXAMPLES:**

  **recon**   attempt to restore Delaunay
 
  **cmo/setatt**//ivoronoi/-2
 
  **recon **   attempt to improve geometric mesh quality
 
  **recon** **/1**  for 2d meshes add nodes on boundaries to guarantee
  Delaunay
 
  **recon**//.001  reconnect limit interface and boundary damage to a
  maximum of .001.
 
  **recon**/0/.001**/checkaxy**  for 2d meshes reconnect, limiting
  damage to a maximum of .001

                                           and preventing creation of
  any negatively oriented or small triangles

                                           (with respect to the
  xy-plane).

   

   

 [Click here for demos](demos/2d_recon/test/md/main_2d_recon.md)

   

