---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX64 6.5 IP28) 
[Netscape
]'
---

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20REGION:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/Mesh.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshFace.md)

 

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MESH REGIONS**:

------------------------------------------------------------------------

 *int* **MR\_Number**(*PMeshObj* mesh, *PMeshRegn* r);

Number ID for mesh region

------------------------------------------------------------------------

*PGeomEntity* **MR\_GMEntity**(*PMeshObj* mesh, *PMeshRegn* r);

Geometric entity (can only be a region) mesh region is classified on

------------------------------------------------------------------------

*ElType* **MR\_Type**(*PMeshObj* mesh, *PMeshRegn* r);

Type of element that mesh region is (TET, HEX etc)

------------------------------------------------------------------------

*int* **MR\_NumVerts**(*PMeshObj* mesh, *PMeshRegn* r);

Number of vertices in the element

------------------------------------------------------------------------

*void* **MR\_Vertices**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*nv,
*PMeshVert* 
*
*verts);

List of vertices of a mesh region

------------------------------------------------------------------------

*void* **MR\_VerticesE**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshEdge* e,
*PMeshVert* 
*rverts);

<img height="300" width="300" src="../images/note1.gif">"31" "32"**THIS OPERATOR IS
APPLICABLE ONLY TO TETS**

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

where v2k1 is the vector from v2 to k1, etc.

------------------------------------------------------------------------

*void* **MR\_VerticesLocE**(*PMeshObj* mesh, *PMeshRegn* r, *int* enum,
*PMeshVert* 
*rverts);

<img height="300" width="300" src="../images/note1.gif">"31" "32"**THIS OPERATOR IS
APPLICABLE ONLY TO TETS**

Same as MR\_VerticesE (above) but the edge is specified by a local edge

number of the region instead of an explicit pointer.

------------------------------------------------------------------------

*int* **MR\_NumFaces**(*PMeshObj* mesh, *PMeshRegn* r);

Number of faces of element

------------------------------------------------------------------------

*void* **MR\_Edges**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*num,
*PMeshEdge* 
*
*edge);

List of edges of a mesh region

------------------------------------------------------------------------

*void* **MR\_Faces**(*PMeshObj* mesh, *PMeshRegn* r, *int* 
*num,
*PMeshFace* 
*
*faces);

List of faces of a mesh region

------------------------------------------------------------------------

*int* **MR\_LocEdgeNum**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshEdge* e);

Local edge number of the given edge in the given region

------------------------------------------------------------------------

*int* **MR\_LocFaceNum**(*PMeshObj* mesh, *PMeshRegn* r, *PMeshFace* f);

Local face number of the given face in the given region.

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



[]{#MR-Attribs*int* **MR\_GetAttVal**(*PMeshObj* mesh, *PMeshRegn* r,
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
solid meshes

------------------------------------------------------------------------



*int* **MR\_SetAttVal**(*PMeshObj* mesh, *PMeshRegn* r, *char

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
solid meshes

 

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#MESH%20REGION:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/Mesh.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/MeshFace.md)
