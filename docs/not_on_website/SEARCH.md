---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: SEARCH
---

 **SEARCH**

The preferred command is search/delaunay. This option assures the
resulting grid meets the insphere delaunay criterion. The algorithm may
refuse to insert points if the points will result in near-zero volume
elements. The near-zero test is based on the geometry. If you notice
problems with search refusing to add all points, issue the setsize
command before setpts. This will adjust the volume epsilon to the
geometry. The user may also adjust both epsilonl and epsilonv with the
assign command; this is recommended for expert users only.

This other form of the command is:

isrchopt -

0 =&gt; Set up the mesh for specified points. If points are not
specified, set up the mesh for the entire problem. Also, remove the
enclosing tetrahedron after generating the mesh.

1 =&gt; Same as 0 except do not remove tetrahedra associated with the
enclosing tetrahedron.

2 =&gt; Add specified points to the existing mesh and remove tetrahedra
associated with the enclosing tetrahedron.

3 =&gt; Add specified points to the existing mesh and do not remove
tetrahedra associated with the enclosing tetrahedron.

4 =&gt; Just remove tetrahedra associated with the enclosing
tetrahedron.

FORMAT:

**search** **/delaunay**

**search**/isrchopt/ifirst,ilast,istride/
