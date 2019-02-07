---
Title: "CREATEPTS/VORONOI"
---


# CREATEPTS / VORONOI #


This routine creates new mesh object attributes called **xvor**, **yvor**, and **zvor** if they do not already exist. They
 contain the x,y,z coordinates of the voronoi point of each element in
 the mesh.  These attributes have length **nelements** and rank **scalar**.   
 
The voronoi point is defined only for triangular and tetrahedral meshes.

 
 ## SYNTAX ##
<pre>
<b>createpts</b> / <b>voronoi</b>
</pre>

## EXAMPLES ##
<pre>
define/NX/3
define/NY/3
define/NZ/3
define/MINX/0.0
define/MAXX/1.0
define/MINY/0.0
define/MAXY/1.0
define/MINZ/0.0
define/MAXZ/1.0

cmo/create/cmo_hex ///hex
createpts/brick/xyz/NX,NY,NZ/MINX,MINY,MINZ/MAXX,MAXY,MAXZ/1,1,1
hextotet / 6 / cmo_tet / cmo_hex

cmo / select / cmo_tet
createpts / voronoi
cmo/ printatt / cmo_tet / -all- / minmax
</pre>
