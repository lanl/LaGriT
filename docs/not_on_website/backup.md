---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX64 6.5 IP28) 
[Netscape
]'
---

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md)         <img height="300" width="300" src="../images/arrow3.gif">"30"
"30"         <img height="300" width="300" src="../images/arrow4.gif">"30" "30"

****

**MESH OPERATORS**

 

These operators work on variables of type *PMeshObj.*

------------------------------------------------------------------------



*PMeshObj* **MESH\_Create**();

Creates a default mesh object and returns it - At this point all the
info

in it is useless.

------------------------------------------------------------------------

*void* **MESH\_InitFromFile**(*PMeshObj* mesh, *char 
**filename, *char

**format, *int* iotype, *char 
*
**name);

Read a mesh in from "*filename*" and initialize the "*mesh*".

The file is in the format "*format*" ("*lagrit*" for LaGriT dumps,
"*gmv*" for GMV files, "*avs*" for AVS files).

*'iotype'* is 1 for ascii and 0 for binary.

'*name*' is the name of the mesh that is read and returned

If a LaGriT file is read, the geometric model info will be read as well.

------------------------------------------------------------------------



*void* **MESH\_InitFromCMO**(*PMeshObj* mesh, *char* 
*cmoname);

Initialize the mesh object from a given cmo. If '*cmoname*' is NULL (say
sent in as the character string ), the mesh object is initialized from
the default mesh object. If not the given cmo is made is active and the
mesh object is

initialized from that.

<img height="300" width="300" src="../images/note1.gif">"31" "32"This implies that
sometime in the middle of a LaGriT subroutine, one can call a C routine
which initializes a mesh object and then uses the functionality of
MSTKLA!! (Cool!)

------------------------------------------------------------------------



*int* **MESH\_Select**(*PMeshObj* mesh);

Make a particular mesh object active

------------------------------------------------------------------------

*void* **MESH\_Delete**(*PMeshObj* mesh);

Delete a particular mesh object.

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* NOT YET IMPLEMENTED

*
* WILL BE IN SOON

------------------------------------------------------------------------

*void* **MESH\_WriteToFile**(*PMeshObj* mesh, *char* 
*filename, *char*

*format, *int* iotype);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* NOT YET IMPLEMENTED

*
* WILL BE IN SOON 
*
*

------------------------------------------------------------------------

*PMeshObj* **MESH\_Duplicate**(*PMeshObj* mesh);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* NOT YET IMPLEMENTED

*
*

------------------------------------------------------------------------

*int* **MESH\_NumVertices**(*PMeshObj* mesh);

Number of vertices (nodes) in the mesh.

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT 
*
*
*

This count may include deleted vertices, parent vertices and other

types of vertices that should be ignored. So when looping through

vertices of a mesh, one should check if the vertex is to be ignored.

------------------------------------------------------------------------

*int* **MESH\_NumRegions**(*PMeshObj* mesh);

Number of regions (elements) in the mesh.

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT 
*
*
*

This count may include deleted regions. While looping through the

regions of a mesh, one should check if the region is to be ignored.

------------------------------------------------------------------------



*PMeshVert* **MESH\_Vertex**(*PMeshObj* mesh, *int* i);

Fetch the i'th vertex (node) of the mesh (This is C so remember i

starts from 0) The returned mesh vertex object can be queried for

information. The returned vertex must always be checked for its type

to see if it deleted or must otherwise be ignored.

------------------------------------------------------------------------

*PMeshRegn* **MESH\_Region**(*PMeshObj* mesh, *int* i);

Fetch the i'th region (element) of the mesh. The returned mesh region

can be queried for information. The returned region must be checked if

it is a deleted region.

------------------------------------------------------------------------



*void* **MESH\_BldUpAdj**(*PMeshObj* mesh);

Build the vertex to region connectivity. This will allow upward

adjacency operator MV\_Regions to be called. In fact those operators

will function properly only after this is called.

------------------------------------------------------------------------



*void* **MESH\_DelUpAdj**(*PMeshObj* mesh);

Delete the vertex to region connectivity information to save

memory. It is not possible for MSTKLA to determine when this

information is not needed and call this routine automatically.

------------------------------------------------------------------------



*void* **MESH\_IncrNodeLists**(*PMeshObj* mesh);

Increment all the node lists.

<img height="300" width="300" src="../images/note1.gif">"31" "32"(
*
* NOT FULLY TESTED

*
*)

------------------------------------------------------------------------



*void* **MESH\_IncrElemLists**(*PMeshObj* mesh);

Increment all the element lists.

<img height="300" width="300" src="../images/note1.gif">"31" "32"(
*
* NOT FULLY TESTED

*
*)

------------------------------------------------------------------------



*void* **MESH\_CmprNodeLists**(*PMeshObj* mesh);

Compress all the element lists.

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
*
* NOT YET
IMPLEMENTED 
*
*
*

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT !!! 
*
*
*

There is one side effect of compression of lists. If one is storing a

list of node numbers or PMeshVerts (which for the LaGriT interface are

just integers cast as pointers), then after compression, the node

numbers or PMeshVert "addresses" may not be referring to the nodes you

think they are.

------------------------------------------------------------------------



*void* **MESH\_CmprElemLists**(*PMeshObj* mesh);

Compress all the element lists.  
*
*
* NOT YET IMPLEMENTED 
*
*
*

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT !!! 
*
*
*

There is one side effect of compression of lists. If one is storing a

list of element numbers or PMeshRegns (which for the LaGriT interface

are just integers cast as pointers), then after compression, the

element numbers or PMeshRegn "addresses" may not be referring to the

elements you think they are.

 

------------------------------------------------------------------------

------------------------------------------------------------------------



   **MESH VERTEX OPERATORS**

 

------------------------------------------------------------------------



*int* **MV\_Number**(*PMeshObj* mesh, *PMeshVert* v);

Return an ID number for the mesh vertex.

 

------------------------------------------------------------------------

*VType* **MV\_Type**(*PMeshObj* mesh, *PMeshVert* v);

Return the mesh vertex type. Most useful in checking if a mesh vertex

has been deleted or if it is a parent vertex on an interface

------------------------------------------------------------------------

*PGeomEntity* **MV\_GMEntity**(*PMeshObj* mesh, *PMeshVert* v);

Return the geometric model entity the mesh vertex is classified on.

------------------------------------------------------------------------

*GType* **MV\_GMtype**(*PMeshObj* mesh, *PMeshVert* v);

Return the type of geometric model entity the mesh vertex is

classified on (GREGION, GFACE, GEDGE, GVERTEX, GUNKNOWN).

------------------------------------------------------------------------

*void* **MV\_Coords**(*PMeshObj* mesh, *PMeshVert* v, *double* 
*xyz);

Coordinates of mesh vertex

------------------------------------------------------------------------

*int* **MV\_IsParent**(*PMeshObj* mesh, *PMeshVert* v);

<img height="300" width="300" src="../images/note1.gif">"31" "32"A vertex not on an
interface is its own parent - return value will be 1

------------------------------------------------------------------------

*int* **MV\_IsChild**(*PMeshObj* mesh, *PMeshVert* v);

<img height="300" width="300" src="../images/note1.gif">"31" "32"A vertex not on an
interface is its own child - return value will be 1

------------------------------------------------------------------------

*PMeshVert* **MV\_Parent**(*PMeshObj* mesh, *PMeshVert* v);

<img height="300" width="300" src="../images/note1.gif">"31" "32"For a vertex not on an
interface, the vertex itself is returned

------------------------------------------------------------------------

*void* **MV\_Children**(*PMeshVert* mesh, *PMeshVert* v, *int* 
*nvc,
*PMeshVert* 
*
*vc)

<img height="300" width="300" src="../images/note1.gif">"31" "32"For a vertex not on an
interface, a 1 item array with the vertex

pointer itself is returned and nvc = 1

 


------------------------------------------------------------------------

*void* **MV\_Edges**(*PMeshObj* mesh, *PMeshVert* v, *int* 
*nve,
*PMeshEdge* 
*
*vedges);

List of mesh edges connected to vertex. For this routine to work

properly, one must call MESH\_BldAdj first. If not the routine will

just return 0.

If the vertex is a parent vertex, then the operator will return all

the edges connected to all the child vertices of the vertex. The edge

returned will be of type PARENT. Each of these edges will be made up

of 'v' and any other parent vertex it is connected to. If it is a

child vertex, then it will return edges connected only to itself.  In

this case all the edges will be of type CHILD. Each edge will be made

up of the child vertex 'v' and any other child vertex it is connected

to. Note that vertices on 2-manifold (single material) boundaries and

in the interior can be considered a parent or a child as required.

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT 
*
*
*

When finished with the list of edges, call ME\_Delete on each of the

edges and free the vedges list. This is important since the MeshEdge

data structures are created on the fly and the MESH object does not

keep track of them.

 

------------------------------------------------------------------------

*void* **MV\_Faces**(*PMeshObj* mesh, *PMeshVert* v, *int* 
*nvf,
*PMeshFace* 
*
*vfaces);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* NOT YET TESTED 
*
*

List of mesh faces connected to vertex. For this routine to work

properly, one must call MESH\_BldAdj first. If not the routine will

just return 0.

The behavior of the operator for parent and child vertices is the same

as it is for MV\_Edges.

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
*
* IMPORTANT 
*
*
*

When finished with the list of faces, call MF\_Delete on each of the

faces and free the vfaces list. This is important since the MeshFace

data structures are created on the fly and the MESH object does not

keep track of them.

------------------------------------------------------------------------

*int* **MV\_numRegions**(*PMeshObj* mesh, *PMeshVert* v);

Number of mesh regions connected to mesh vertex. For this routine to

work properly, one must call MESH\_BldUpAdj first. If not the routine

will just return 0.

 

------------------------------------------------------------------------

*void* **MV\_Regions**(*PMeshObj* mesh, *PMeshVert* v, *int* 
*nr,
*PMeshRegn* 
*
*regns);

List of mesh regions connected to mesh vertex. For this routine to

work properly, one must call MESH\_BldUpAdj first. If not the routine

will just return 0 for the number of regions and a NULL pointer for

the region list.

 

For a parent vertex, mesh regions in all the material regions

connected to all its children are returned. For a child vertex, only

mesh regions in the particular material region pointed to by the child

are returned.

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

<img height="300" width="300" src="bullet12.gif">"27" "31"
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

*void* ME\_Children(*PMeshObj* mesh, *PMeshEdge* pe, *int* 
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

<img height="300" width="300" src="bullet12.gif">"27" "31"
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

 

------------------------------------------------------------------------

------------------------------------------------------------------------



    **MESH FACES:**


------------------------------------------------------------------------



Since LaGriT does not really store mesh faces, the mesh face data

structure (MeshFace, *PMeshFace*) is created on the fly whenever face

information is requested. Therefore after utilizing the face(s) get

information, it (they) should be destroyed by calling MF\_Delete

operator.

------------------------------------------------------------------------

*int* **MF\_Number**(*PMeshObj* mesh, *PMeshFace* f);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* This will return 0
for now since I haven't yet decided on a good


*
* way to assign a unique number to the (virtual) faces of the mesh.

------------------------------------------------------------------------

*void* **MF\_Delete**(*PMeshFace* f);

Free the data structure associated with this face created on the fly.

(There is an operator for creation but I cannot yet imagine needing to

make that a publicly available operator).

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
* IMPORTANT 
*
*

MF\_Delete will not remove any connections in the mesh. It just

destroys the temporary data structure used to store the face info

------------------------------------------------------------------------

*int* **MF\_IsParent**(*PMeshObj* mesh, *PMeshFace* pf);

If all vertices of the face are parents, the face is a parent.

------------------------------------------------------------------------

*PMeshFace* **MF\_Parent**(*PMeshObj* mesh, *PMeshFace* pf);

The parents of all vertices of the face are obtained and a new

'parent' face is constructed from them.

------------------------------------------------------------------------



*void* **MF\_Children**(*PMeshObj* mesh, *PMeshFace* pf, *int* 
*nfc,
*PMeshFace* 
*
*fc);

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

*int* **MFs\_areSame**(*PMeshFace* f1, *PMeshFace* f2);

Compare the data in two MeshFace data structures and see if they

represent the same connection in the mesh. This operator is needed

since the on-the-fly *PMeshFace* pointers may not be the same and they

do not directly point to any persistent "object" in the database.

------------------------------------------------------------------------

*PGeomEntity* **MF\_GMentity**(*PMeshObj* mesh, *PMeshFace* f);

Return the geometric model entity the face is on. In the absence of

explicit face representation in the database, this may be problematic

in a few cases cases where multiple classifications are possible.**

------------------------------------------------------------------------

*GType* **MF\_GMtype**(*PMeshObj* mesh, *PMeshFace* f);

Return the type of geometric model entity the face is on. This (I

think) is more deterministically found from the classifications of its

vertices but some problems may still occur. See Comments in
ME\_GMtype.**

------------------------------------------------------------------------

*PMeshVert* **MF\_Vertices**(*PMeshObj* mesh, *PMeshFace* f, *int*

*nfv, *PMeshVert*ex 
*fverts);

Return the vertices of the mesh face. The vertices are in no

particular order.  Parent faces return parent vertices and child faces

child vertices. If a vertex is not on an non-manifold boundary

(interface) the vertex itself is returned.**

------------------------------------------------------------------------

*PMeshVert* **MF\_VerticesOrd**(*PMeshObj* mesh, *PMeshFace* f, *int*

*nfv, *PMeshVert*ex 
*fverts);

<img height="300" width="300" src="../images/note1.gif">"31" "32"
*
* NOT YET IMPLEMENTED

*
*

Return the vertices of the mesh face. The vertices are ordered such

that the smallest one is the first. Also, the sequence is such that

the face points out of the region connected to it or in case there are

two regions connected to it, such that it points out of the region

with the lower ID Number. The behavior of the operator for parent and

child faces is the same as MF\_Vertices**

------------------------------------------------------------------------

*void* **MF\_Edges**(*PMeshObj* mesh, *PMeshFace* f, *int* 
*nef,
*PMeshFace* 
*
*efaces);

Get the "edges" of an "face". The edges data structures are

constructed on the fly since they do not exist in the LaGriT

database. See note on freeing the *PMeshFace* structures listed under

MV\_Edges and MV\_Faces. Parent faces return parent edges and child

faces return child edges.**

------------------------------------------------------------------------

*void* **MF\_Regions**(*PMeshObj* mesh, *PMeshFace* f, *PMeshRegn*
fregions
[2
]);

Get the regions connected to an face. MESH\_BldUpAdj must be called

before this operator can be called usefully. The regions will be

returned such that the face 'f' as defined will point out fregions
[0
]

(if it exists) and into fregions
[1
] (if it exists).

A parent face will return both regions connected to it while a child

face will return a region only on its side of the interface. If a

"child" face constructed from interior nodes and child nodes on an

interface, both regions connected to it are returned.

<img height="300" width="300" src="bullet12.gif">"27" "31"
*
* IMPORTANT 
*
*

Since MESH\_BldUpAdj builds explicit upward adjacency information, this

routine does not require a seed region to be provided.**

------------------------------------------------------------------------

*int* **MF\_EntOnBdry**(*PMeshObj* mesh, *PMeshFace* f, MType mtype,
PMeshEntity pent);

Check if mesh entity of type 'mtype' is on the boundary of mesh

face. mtype can only be MEDGE or MVERTEX. In other words, check if

mesh face is made up of mesh edge or vertex in question.

 

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MESH REGIONS**:


------------------------------------------------------------------------

 *int* **MR\_Number**(*PMeshObj* mesh, *PMeshRegn* r);

Number ID for mesh region

------------------------------------------------------------------------

*PGeomEntity* **MR\_GMEntity**(*PMeshObj* mesh, *PMeshRegn* r);

Geometric entity (can only be a region) mesh region is classified on**

------------------------------------------------------------------------

*ElType* **MR\_Type**(*PMeshObj* mesh, *PMeshRegn* r);

Type of element that mesh region is (TET, HEX etc)**

------------------------------------------------------------------------

*int* **MR\_NumVerts**(*PMeshObj* mesh, *PMeshRegn* r);

Number of vertices in the element**

------------------------------------------------------------------------

*void* **MR\_Vertices**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*nv,
*PMeshVert* 
*
*verts);

List of vertices of a mesh region**

------------------------------------------------------------------------

*void* **MR\_VerticesE**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshEdge* e,
*PMeshVert* 
*rverts);

<img height="300" width="300" src="../images/note1.gif">"31" "32"THIS OPERATOR IS
APPLICABLE ONLY TO TETS

Return the vertices of the given tet w.r.t to the given edge. The

vertices v1 and v2 are vertices of the edge (maybe (0,1) or (1,0)) -

these are returned as rverts
[0
] and rverts
[1
]. The vertices
rverts
[2
]

(call it k1) and rverts
[3
] (k2) are returned such that if the
vertices

are above the edge then looking from vertex 0 to vertex 1 of the edge

k2 is to the left of k1. Mathematically,

   (v2k1 X v2k2) . v2v1 &gt; 0

where v2k1 is the vector from v2 to k1, etc.**

------------------------------------------------------------------------

*void* **MR\_VerticesLocE**(*PMeshObj* mesh, *PMeshRegn* r, *int* enum,
*PMeshVert* 
*rverts);

<img height="300" width="300" src="../images/note1.gif">"31" "32"THIS OPERATOR IS
APPLICABLE ONLY TO TETS

Same as MR\_VerticesE (above) but the edge is specified by a local edge

number of the region instead of an explicit pointer.**

------------------------------------------------------------------------

*int* **MR\_NumFaces**(*PMeshObj* mesh, *PMeshRegn* r);

Number of faces of element**

------------------------------------------------------------------------

*void* **MR\_Edges**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*num,
*PMeshEdge* 
*
*edge);

List of edges of a mesh region**

------------------------------------------------------------------------

*void* **MR\_Faces**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*num,
*PMeshFace* 
*
*faces);

List of faces of a mesh region**

------------------------------------------------------------------------

*int* **MR\_LocEdgeNum**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshEdge* e);

Local edge number of the given edge in the given region**

------------------------------------------------------------------------

*int* **MR\_LocFaceNum**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshFace* f);

Local face number of the given face in the given region.**

------------------------------------------------------------------------

*int **MR\_EntOnBdry***(*PMeshObj* mesh, *PMeshRegn* r, MType mtype,
PMeshEntity pent);

Check if mesh entity of type 'mtype' is on the boundary of mesh

region. mtype can be MFACE, MEDGE or MVERTEX. In other words, the

operator checks if mesh region is made up of mesh face, edge or vertex

in question.

 


------------------------------------------------------------------------

*PMeshRegn* **MR\_FaceNebr**(*PMeshObj* mesh, *PMeshRegn* r, *int*
lfnum);

Return the mesh region adjacent to face number 'lfnum' of region 'r'

The local face numbering starts from 0 in C style.

 

------------------------------------------------------------------------

------------------------------------------------------------------------

  **GENERIC REGION TYPE OPERATORS**


------------------------------------------------------------------------

 *int* **RType\_NumVerts**(*ElType* type);

Number of vertices for a generic element of a particular type**

------------------------------------------------------------------------

*int* **RType\_NumEdges**(*ElType* type);

Number of edges for a generic element of a particular type**

------------------------------------------------------------------------

*int* **RType\_NumFaces**(*ElType* type);

Number of faces for a generic element of a particular type**

------------------------------------------------------------------------

*void* **RType\_EdgeVertMap**(*ElType* type, *int* ienum, *int*

*evnums);

Get the local vertex numbers for local edge 'ienum' of element type.**

------------------------------------------------------------------------

*void* **RType\_FaceVertMap**(*ElType* type, *int* ifnum, *int* 
*nfv,
*int* 
*
*fvnums);

Get the local vertex numbers for local facee 'ifnum' of element type.**

------------------------------------------------------------------------

*void* **RType\_FaceEdgeMap**(*ElType* type, *int* ifnum, *int* 
*nfe,
*int* 
*
*fenums);

Get the local edge numbers for local face 'ifnum' of element type.**

------------------------------------------------------------------------

*void* **RType\_EdgeVertMap**(*ElType* type, *int* ienum, *int*

*evnums);

Get the local vertex numbers for local edge 'ienum' of element type.

 

------------------------------------------------------------------------

------------------------------------------------------------------------

   UTILITIES:

 

------------------------------------------------------------------------

 *void* **ReportError**(*char* 
*modulename, *char* 
*message, ErrType
severity);

Report an error message. The severity may be specified as

**MESSG**: Print a message to the screen

**WARNING**: Print message with attention grabbing text

**ERROR**: Print message with additional attention grabbing text

**FATAL**: Print attention grabbing message and exit.

 

------------------------------------------------------------------------

------------------------------------------------------------------------

 **GEOMETRIC MODEL INITIALIZATION:**

 

------------------------------------------------------------------------



*void* **GMTKLA\_Init**(*void*);

Initialize the geometric model interface (does nothing for now but

include it for completeness sake).

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MODEL OPERATORS:**

 

------------------------------------------------------------------------

 *int* **GM\_NumRegions**(*void*);

Number of model regions. Note this has nothing to do with number of

material attributes. That is the analysis preprocessing concern not

the mesh generator's or the geometric modeler's. The number of model

regions refers to the number of distinct volumes in the geometric

model.**

------------------------------------------------------------------------

*int* **GM\_NumFaces**(*void*);

Number of model faces. For now, this just returns the number of

surfaces in the geometric model although they are strictly not the

same.**

------------------------------------------------------------------------

*PGeomRegn* **GM\_Region**(*int* i);

Return the i'th geometric model region**

------------------------------------------------------------------------

*PGeomFace* **GM\_Face**(*int* i);

Return the i'th geometric model face**

------------------------------------------------------------------------

*void* **GM\_Regions**(*int* 
*nr, *PGeomRegn* 
*
*gregs);

Get all the model regions**

------------------------------------------------------------------------

*void* **GM\_Faces**(*int* 
*nf, *PGeomFace* 
*
*gfaces);

Get all the model faces 

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MODEL REGION OPERATORS:**  

------------------------------------------------------------------------

*void* **GR\_Faces**(*PGeomRegn* gr, *int* 
*nf, *PGeomFace* 
*faces);

Get the model faces that form the boundary of a model region.**

------------------------------------------------------------------------

*int* **GR\_FaceDir**(*PGeomRegn* gr, *PGeomFace* gf);

Get the sense in which a model region is using a face, i.e., is the

face normal pointing into the region (return value 0), out of the

region (return value 1), both (return value 2) or none (return value

-1). 
*
*
*
*
* MUST VERIFY THiS WITH CODE VALUES 
*
*
*
*
*
***

------------------------------------------------------------------------

*int* **GR\_OnBoundary**(*PGeomRegn* this, *PGeomEntity* gent);

 

 


------------------------------------------------------------------------

------------------------------------------------------------------------



   **MODEL FACE OPERATORS:**


------------------------------------------------------------------------

 *void* **GF\_Regions**(*PGeomFace* gf, *PGeomRegn* gr
[2
]);

Get the regions on either side of the face. gr
[0
] is the region on
the

opposite side of the normal while gr
[1
] is the region on the same
side

of the normal.  In principle (not in LaGriT), both entries of gr may

be filled \_AND\_ gr
[0
] may be equal to gr
[1
]. Also in principle,
both

entries of gr may be NULL.

 

------------------------------------------------------------------------

------------------------------------------------------------------------

**MODEL ENTITY OPERATORS:**


------------------------------------------------------------------------

*GType* **GEnt\_Type**(*PGeomEntity* gent);

Type of a geometric model entity (GREGION, GFACE, GEDGE, GVERTEX,

GUNKNOWN).  Typically can be used in conjunction with the operator to

get mesh entity classification. So one can get the model entity a mesh

vertex is on, query what type it is and do something based on the

response (e.g. reposition the vertex only if it is classified in a

model region).

 

------------------------------------------------------------------------

*int* **GEnt\_Number**(*PGeomEntity* gent);

ID Number of geometric model entity. Can be called instead of

GR\_Number, GF\_Number, GE\_Number and GV\_Number.

 

------------------------------------------------------------------------

*int* **GEnt\_OnBoundary**(*PGeomEntity* g1, *PGeomEntity* g2);

Check if g2 is on the boundary of g1 (Check if a face is on the

boundary of a region, an edge is on the boundary of a face, a vertex

is on the boundary of a region)

 

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md)         <img height="300" width="300" src="../images/arrow3.gif">"30"
"30"         <img height="300" width="300" src="../images/arrow4.gif">"30" "30"
