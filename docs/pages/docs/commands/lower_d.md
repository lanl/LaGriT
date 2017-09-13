---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: FIELD
---

 

 **LOWER\_D**

  This suite of commands creates and handles the lower dimension
  structures associated with a mesh.  The existing mesh is labeled
  'd0'.  The next lower dimension mesh 'd1' and so on.  For an
  original 3D mesh, the d1 structures are the surfaces (2D) separating
  material regions, the d2 structures are the lines separating the d1
  surfaces and the d3 structures are the nodes at the ends of the d2
  lines.

  Several new attributes are created which belong the original mesh
  object:

   
 
    ------------------------ ------------------------ ------------------------
    name                     type                     length
 
    d0\_nnodes\_topo         VINT, nnodes             0 = interior

                                                      1 = surface node

                                                      2 = line node

                                                      3 = line end node
 
    d1\_nnodes               INT, scalar              number of nodes in this
                                                      structure
 
    d1\_elements             INT, scalar              number of elements in
                                                      this structure
 
    d1\_nef\_cmo             INT, scalar              number of facets/element
                                                      in this structure
 
    d1\_nee\_cmo             INT, scalar              number of edges/element
                                                      in this structure
 
    d1\_nen\_cmo             INT, scalar              number of nodes/element
                                                      in this structure
 
    d1\_jtet\_cycle\_max     INT, scalar              the longest jtet cycle
                                                      in this structure
 
    d1\_itettyp              VINT d1\_elements        element type
 
    d1\_itetclr              VINT d1\_elements        element selection number
 
    d1\_itet off             VINT d1\_elements        offset to d1\_itet
 
    d1\_jtet off             VINT d1\_elements        offset to d1\_jtet
 
    d1\_itet                 VINT d1\_elements        list of nodes for each
                             xd1\_neu\_cmo            element
 
    d1\_jtet                 VINT d1\_elements        list of face neighbors
                             xd1\_nef\_cmo            
 
    d1\_elm\_d0              VINT d1\_elements        elements face 
# in
                                                      original mesh that this
                                                      element came from
 
    d2\_nnodes               INT, scalar              number of nodes in this
                                                      structure
 
    d2\_elements             INT, scalar              number of elements in
                                                      this structure
 
    d2\_nef\_cmo             INT, scalar              number of facets/element
                                                      in this structure
 
    d2\_nee\_cmo             INT, scalar              number of edges/element
                                                      in this structure
 
    d3\_nen\_cmo             INT, scalar              number of nodes/element
                                                      in this structure
 
    d2\_jtet\_cycle\_max     INT, scalar              the longest jtet cycle
                                                      in this structure
 
    d2\_itettyp              VINT d2\_elements        element type
 
    d2\_itetclr              VINT d2\_elements        element material number
 
    d2\_itet off             VINT d2\_elements        offset to d2\_itet
 
    d2\_jtet off             VINT d2\_elements        offset to d2\_jtet
 
    d2\_itet                 VINT d2\_elements        list of nodes for each
                             xd2\_neu\_cmo            element
 
    d2\_jtet                 VINT d2\_elements        list of face neighbors 
                             xd2\_nef\_cmo            
 
    d2\_elm\_d1              VINT d2\_elements        element & face that this
                                                      element came from in
                                                      next higher level
                                                      structure
 
    d3\_nnodes               INT, scalar              number of nodes in this
                                                      structure
 
    lower\_d\_flag           INT, scalar              0= no lower d structure
                                                      exist

                                                      =1 lower\_d structures
                                                      exist and are valid

                                                      =2 lower\_d structures
                                                      not valid
    ------------------------ ------------------------ ------------------------
 
  The above set of attributes are created if the original mesh is 3D. 
  If the original mesh is 2D then the d1 structures are created, but
  the d2 structures are simply the d2\_nnodes.  If the original mesh
  is 1D, then only the d1\_nnodes structure is created.
 
  At the time the lower\_d structures are created color table
  attributes: d0\_clrtab, d0\_nclrs, .. are also created.

 FORMAT:

  **lower\_d** / reate**/ 
[cmo\_name
]

    create lower\_d structures in mesh object

  **lower\_d** / **release**/ 
[cmo\_name
]

   release lower\_d structures

  **lower\_d** / **extract**/ 
[cmo\_name/cmo1/cmo2/cmo2
]

   create lower\_d structures into named mesh objects cmo1, cmo2,
  cmo3.

  **lower\_d** / **filter**/ 
[cmo\_name
] /
[iclr1  itp  **imt** 
  lr** 
] / value 
[ **and**  **or**  **new** 
]

    these commands are advised for expert users only.

 EXAMPLES:

 
