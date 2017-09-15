---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#GENERIC%20MESH%20REGION%20QUERIES:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](MeshVertex.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](Geom.md)

------------------------------------------------------------------------

------------------------------------------------------------------------

  **GENERIC REGION TYPE OPERATORS**

------------------------------------------------------------------------

 *int* **RType\_NumVerts**(*ElType* type);

Number of vertices for a generic element of a particular type

------------------------------------------------------------------------

*int* **RType\_NumEdges**(*ElType* type);

Number of edges for a generic element of a particular type

------------------------------------------------------------------------

*int* **RType\_NumFaces**(*ElType* type);

Number of faces for a generic element of a particular type

------------------------------------------------------------------------

*void* **RType\_EdgeVertMap**(*ElType* type, *int* ienum, *int*

*evnums);

Get the local vertex numbers for local edge 'ienum' of element type.

------------------------------------------------------------------------

*void* **RType\_FaceVertMap**(*ElType* type, *int* ifnum, *int* 
*nfv,
*int* 
*
*fvnums);

Get the local vertex numbers for local facee 'ifnum' of element type.

------------------------------------------------------------------------

*void* **RType\_FaceEdgeMap**(*ElType* type, *int* ifnum, *int* 
*nfe,
*int* 
*
*fenums);

Get the local edge numbers for local face 'ifnum' of element type.

------------------------------------------------------------------------

*void* **RType\_EdgeVertMap**(*ElType* type, *int* ienum, *int*

*evnums);

Get the local vertex numbers for local edge 'ienum' of element type.

 

 <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#GENERIC%20MESH%20REGION%20QUERIES:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](MeshVertex.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](Geom.md)
