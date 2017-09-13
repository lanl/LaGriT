---
GENERATOR: 'Mozilla/4.75 [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
---
[]{#CREATEPTS/VORONOI**CREATEPTS/VORONOI**

 This routine creates new mesh object attributes called
 **xvor**,**yvor** and **zvor** if they do not already exist.  These
 attributes have length **nelements** and rank **scalar**.  They
 contain the x,y,z coordinates of the voronoi point of each element in
 the mesh.  The voronoi point is defined only for triangular and
 tetrahedral meshes.
