---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#typedefs) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](parent-child.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](Mesh.md)

------------------------------------------------------------------------

------------------------------------------------------------------------

  **TYPEDEFS FOR MESH AND MODEL ENTITIES**

------------------------------------------------------------------------

 

A mesh object (actually a pointer to it) in the interface can be
declared as

 *PMeshObj* mesh1, mesh2;

 

Mesh entity types are as follows:

 /
* Mesh Regions 
*/

 *PMeshRegn* reg1, oreg, regarr[50];

 

 /
* Mesh Vertices (nodes) 
*/

 *PMeshVert* vert, 
*verts. vert[20][30];

 /
* Mesh Face 
*/

 *PMeshFace* rface;

 /
* Mesh Edge 
*/

 *PMeshEdge* face\_edges[4];

 /
* Type of mesh region or element       
*/

 /
* This is an enum decl which can be    
*/

 /
* TET, PYRAMID, PRISM, HEX or RDELETED 
*/

 *ElType* reg\_type;

 /
* Type of mesh vertec 
*/

 /
* Can be VIGNORE, VPARENT, VDELETED 
*/

 *VType* vert\_type;

 

 

Geometric model entity types are:

 /
* Model entity - can be a region, face, edge or a vertex 
*/

 *PGeomEntity*  gent;

 /
* Model region 
*/

 *PGeomRegn* greg;

 

 /
* Model face 
*/

 *PGeomFace* gfaces[20], 
*
*gfaces2, interface;

 /
* Model edge - not used at this point 
*/

 *PGeomEdge* geds[], 
*gfeds;

 /
* Model vertex - not used at this point 
*/

 *PGeomVert* gv1, gv2;

 /
* Type of model entity - can be GREGION, GFACE, GEDGE, GVERTEX 
*/

* GType* geomtype;

 

 [<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#typedefs) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](parent-child.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](Mesh.md)
