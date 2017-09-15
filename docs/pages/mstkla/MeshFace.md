---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20FACE:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshEdge.md)

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

<img height="300" width="300" src="../images/bullet12.gif">"27" "31"
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

*int* **MFs\_areSame**(*PMeshObj* mesh, *PMeshFace* f1, *PMeshFace* f2);

Compare the data in two MeshFace data structures and see if they

represent the same connection in the mesh. This operator is needed

since the on-the-fly *PMeshFace* pointers may not be the same and they

do not directly point to any persistent "object" in the database.

------------------------------------------------------------------------

*PGeomEntity* **MF\_GMentity**(*PMeshObj* mesh, *PMeshFace* f);

Return the geometric model entity the face is on. In the absence of

explicit face representation in the database, this may be problematic

in a few cases cases where multiple classifications are possible.

------------------------------------------------------------------------

*GType* **MF\_GMtype**(*PMeshObj* mesh, *PMeshFace* f);

Return the type of geometric model entity the face is on. This (I

think) is more deterministically found from the classifications of its

vertices but some problems may still occur. See Comments in ME\_GMtype.

------------------------------------------------------------------------

*PMeshVert* **MF\_Vertices**(*PMeshObj* mesh, *PMeshFace* f, *int*

*nfv, *PMeshVert*ex 
*
*fverts);

Return the vertices of the mesh face. The vertices are in no

particular order.  Parent faces return parent vertices and child faces

child vertices. If a vertex is not on an non-manifold boundary

(interface) the vertex itself is returned.

------------------------------------------------------------------------

*PMeshVert* **MF\_VerticesOrd**(*PMeshObj* mesh, *PMeshFace* f, *int*

*nfv, *PMeshVert*ex 
*
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

child faces is the same as MF\_Vertices

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

faces return child edges.

------------------------------------------------------------------------

*void* **MF\_Regions**(*PMeshObj* mesh, *PMeshFace* f, *PMeshRegn*
fregions[2]);

Get the regions connected to an face. MESH\_BldUpAdj must be called

before this operator can be called usefully. The regions will be

returned such that the face 'f' as defined will point out fregions[0]

(if it exists) and into fregions[1] (if it exists).

A parent face will return both regions connected to it while a child

face will return a region only on its side of the interface. If a

"child" face constructed from interior nodes and child nodes on an

interface, both regions connected to it are returned.

<img height="300" width="300" src="../images/bullet12.gif">"27" "31"
*
* IMPORTANT 
*
*

Since MESH\_BldUpAdj builds explicit upward adjacency information, this

routine does not require a seed region to be provided.

------------------------------------------------------------------------

*int* **MF\_EntOnBdry**(*PMeshObj* mesh, *PMeshFace* f, MType mtype,
PMeshEntity pent);

Check if mesh entity of type 'mtype' is on the boundary of mesh

face. mtype can only be MEDGE or MVERTEX. In other words, check if

mesh face is made up of mesh edge or vertex in question.

 

 

------------------------------------------------------------------------

[]{#MF-Attribs*int* **MF\_GetAttVal**(*PMeshObj* mesh, *PMeshFace* f,
*char 
**attname, *int 
**ival,

*double 
**rval, *char 
**cval, *void 
*
**pval, *AttType 
**atype);

Depending on the type of the attribute, the appropriate field will be

filled on succesful completion. Arrays are returned in pval, integer in

ival and real number in rval (I don't think lagrit supports character

attributes)

<img height="300" width="300" src="../images/bullet12.gif">"27" "31" Will only work for
surface meshes

------------------------------------------------------------------------

*int* **MR\_SetAttVal**(*PMeshObj* mesh, *PMeshFace* f, *char

**attname, *int* ival,

*double* rval, *char* cval, *void 
**pval);

This is similar to the GetAttVal routine - depending on the type and

rank, one sends in a value with the right variable

*
*
* PERHAPS these routines need an additional variable, "rank" but
for

now.....

 

<img height="300" width="300" src="../images/bullet12.gif">"27" "31" Will only work for
surface meshes
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20FACE:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshEdge.md)
