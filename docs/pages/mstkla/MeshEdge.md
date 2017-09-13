---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX64 6.5 IP28) 
[Netscape
]'
---

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20EDGE:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshFace.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshVertex.md)

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MESH EDGES:**

------------------------------------------------------------------------

Since LaGriT does not really store mesh edges, the mesh edge data

structure (*MeshEdge*, *PMeshEdge*) is created on the fly whenever edge

information is requested. Therefore after utilizing the edge(s) get

information, it (they) should be destroyed by calling ME\_Delete

operator.

------------------------------------------------------------------------

*int* **ME\_Number**(*PMeshObj* mesh, *PMeshEdge* e);


*
* This will return 0 for now since I haven't yet decided on a good


*
* way to assign a unique number to the (virtual) edges of the mesh.

------------------------------------------------------------------------

*void* **ME\_Delete**(*PMeshEdge* e);

Free the data structure associated with this edge created on the fly.

(There is an operator for creation but I cannot yet imagine needing to

make that a publicly available operator).

<img height="300" width="300" src="../images/bullet12.gif">"27" "31"
*
* IMPORTANT 
*
*

ME\_Delete will not remove any connections in the mesh. It just destroys
the

temporary data structure used to store the edge info

------------------------------------------------------------------------

*int* **ME\_IsParent**(*PMeshObj* mesh, *PMeshEdge* pe);

If both vertices of the edge are parents, the edge is a parent.

------------------------------------------------------------------------

*PMeshEdge* **ME\_Parent**(*PMeshObj* mesh, *PMeshEdge* pe);

The parents of each vertex of the edge are obtained and a new 'parent'

edge is constructed from them.

------------------------------------------------------------------------

*void* **ME\_Children**(*PMeshObj* mesh, *PMeshEdge* pe, *int* 
*nec,
*PMeshEdge* 
*
*ec);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
*
* NOT YET
IMPLEMENTED 
*
*
*

Child vertices of the face are found and appropriate combinations of
these vertices are used to construct the new 'child' faces. For a
combination to be appropriate, the vertices of the child face has to
point to the same material region.

------------------------------------------------------------------------

*int* **MEs\_areSame**(*PMeshEdge* e1, *PMeshEdge* e2);

Compare the data in two MeshEdge data structures and see if they

represent the same connection in the mesh. This operator is needed

since the on-the-fly *PMeshEdge* pointers may not be the same and they

do not directly point to any persistent "object" in the database.

------------------------------------------------------------------------

*PGeomEntity* **ME\_GMentity**(*PMeshObj* mesh, *PMeshEdge* e);

Return the geometric model entity the edge is on. In the absence of
explicit

edge representation in the database, this may be problematic in a few
cases

cases where multiple classifications are possible.

------------------------------------------------------------------------

*GType* **ME\_GMtype**(*PMeshObj* mesh, *PMeshEdge* e);

Return the type of geometric model entity the edge is on. This (I

think) is more deterministically found from the classifications of its

vertices but still there may be some incorrect cases. The way around

is complicated and will not be implemented for now.

------------------------------------------------------------------------

*PMeshVert* **ME\_Vertex**(*PMeshObj* mesh, *PMeshEdge* e, *int* i);

Return the i'th vertex of the mesh edge. i can only be 0 or 1.

Parent edges return parent vertices and child edges return child
vertices.

If a vertex is not on an interface the vertex itself is returned.

------------------------------------------------------------------------

<img height="300" width="300" src="../images/new1.gif">"31" "12"*PMeshVert*
**ME\_OtherVertex**(*PMeshObj* mesh, *PMeshEdge* e, *PMeshVert* v);

Return the other vertex of the mesh edge (obviously the one that is not
v).

------------------------------------------------------------------------

*void* **ME\_Faces**(*PMeshObj* mesh, *PMeshEdge* e, *int* 
*nef,
*PMeshFace* 
*
*efaces);

Get the "faces" of an "edge". The faces data structures are also

constructed on the fly since they do not exist in the LaGriT

database. Also, as with other upward connectivity queries, this can

execute usefully only if the routine MESH\_BldUpAdj is called

beforehand. Also, see note on freeing the *PMeshFace* structures listed

under MV\_Edges and MV\_Faces.

The behavior of this operator for parent and child edges is similar to

MV\_Edges.

------------------------------------------------------------------------

*void* **ME\_Regions**(*PMeshObj* mesh, *PMeshEdge* e, *int* 
*ner,
*PMeshRegn* 
*
*eregions);

Get the regions connected to an edge. MESH\_BldUpAdj must be called
before this operator can be called usefully.

The behavior of this operator for child and parent edges is similar to

that of MV\_Regions.

<img height="300" width="300" src="../images/bullet12.gif">"27" "31"
*
* IMPORTANT 
*
*

Since MESH\_BldUpAdj builds explicit upward adjacency information, this

routine does not require a seed region to be provided. Also, this

routine will work regardless of whether there is a complete cycle of

regions around the edge and whether the edge is an arbitrary type of

non-manifold edge!!

------------------------------------------------------------------------

*int* **ME\_EntOnBdry**(*PMeshObj* mesh, *PMeshEdge* pe, *PMeshVert*e
pv);

Check if vertex is on the boundary of edge. In other words, is this

vertex one of the edges vertices.

 

 

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20EDGE:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshFace.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshVertex.md)
