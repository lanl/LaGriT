---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20VERTEX:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshEdge.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/GenRegion.md)

 

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

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/note1.gif">"31" "32"A vertex not on an
interface is its own parent - return value will be 1

------------------------------------------------------------------------

*int* **MV\_IsChild**(*PMeshObj* mesh, *PMeshVert* v);

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/note1.gif">"31" "32"A vertex not on an
interface is its own child - return value will be 1

------------------------------------------------------------------------

*PMeshVert* **MV\_Parent**(*PMeshObj* mesh, *PMeshVert* v);

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/note1.gif">"31" "32"For a vertex not on an
interface, the vertex itself is returned

------------------------------------------------------------------------

*void* **MV\_Children**(*PMeshVert* mesh, *PMeshVert* v, *int* 
*nvc,
*PMeshVert* 
*
*vc)

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/note1.gif">"31" "32"For a vertex not on an
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

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/bullet12.gif">"27" "31"
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

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/note1.gif">"31" "32"
*
* NOT YET TESTED 
*
*

List of mesh faces connected to vertex. For this routine to work

properly, one must call MESH\_BldAdj first. If not the routine will

just return 0.

The behavior of the operator for parent and child vertices is the same

as it is for MV\_Edges.

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/bullet12.gif">"27" "31"
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

[]{#MV-Attribs*int* **MV\_GetAttVal**(*PMeshObj* mesh, *PMeshVert* v,
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

 

------------------------------------------------------------------------

*int* **MV\_SetAttVal**(*PMeshObj* mesh, *PMeshVert* v, *char

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

 

 
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20VERTEX:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshEdge.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/GenRegion.md)
