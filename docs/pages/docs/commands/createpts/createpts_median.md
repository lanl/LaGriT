---
Title: "CREATEPTS/MEDIAN"
---

# CREATEPTS / MEDIAN #

 This routine creates new mesh object attributes called **xmed**,**ymed** and **zmed** if they do not already exist. They
contain the x,y,z coordinates of the median point of each element in the mesh. These attributes have length **nelements** and rank **scalar**. The median point is defined for all supported element types.

## SYNTAX ##
<pre>
<b>createpts</b> / <b>median</b>
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

cmo/create/cmo_quad///quad
quadxy NX NY/MINX MINY 0./MAXX MINY 0./MAXX MAXY 0./MINX MAXY 0.
rzbrick/xyz/NX,NY,1/1,0,0/connect

cmo / select / cmo_quad
createpts / median
cmo/ printatt / cmo_quad / -all- / minmax
</pre>
