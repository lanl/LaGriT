---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---

 <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#EXAMPLE) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](utilities.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](linking.md)

/
*

==========================================================================

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

  Example program with MSTKLA

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

==========================================================================

*

#include &lt;stdio.h&gt;

#include &lt;stdlib.h&gt;

#include "mstkla.h"

#include "gmtkla.h"

int main(int argc, char 
*argv[]) {

  char 
*meshname;

  PMeshObj mesh;

  PMeshVert vert, rverts[8];

  PMeshRegn reg, oreg;

  ElType type;

  int nv, nf, nr, nrv, i, j, 
*rfside, nrf, ngr, ngf;

  double xyz[3];

  PGeomEntity gent;

  PGeomFace 
*rflist, 
*gflist;

  PGeomRegn 
*grlist;

 

  ***/
* Initialization 
***/*

 

  MSTKLA\_Init();

  GMTKLA\_Init();

 

  ***/
* Create a mesh object and fill in the data after reading from a
file 
***/*

  mesh = MESH\_Create();

  MESH\_InitFromFile(mesh, argv[1], argv[2], 1, &meshname);

 

  ***/
* Model info 
***/*

 

  ngr = GM\_NumRegions();

  fprintf(stderr,"Number of model regions: %d

n",ngr);

 

  ngf = GM\_NumFaces();

  fprintf(stderr,"Number of model faces: %d

n",ngf);

 

  ***/
* Get all the regions in the model 
***/*

  GM\_Regions(&ngr, &grlist);

 

  ***/
* Loop through the regions and print out info about the faces***

***     bounding this region***

***     
***/*

 

  for (i = 0; i &lt; ngr; i++) {

    fprintf(stderr,"Region %-d

n:

n",(i+1));

 

    ***/
* Faces of the region 
***/*

 

    GR\_Faces(grlist[i],&nrf,&rflist,&rfside);

 

    fprintf(stderr,"  %d Faces

n",nrf);

    for (j = 0; j &lt; nrf; j++) {

      fprintf(stderr,"Face %-d  Dir %-d Type %-d

n",

       GF\_Number(rflist[j]),rfside[j],GF\_Type(rflist[j]));

    

  

  for (i = 0; i &lt; ngr; i++)

    GR\_Delete(grlist[i]);

  free(grlist);

  **/*
* Just get the all the faces of the model 
***/*

***  /
* Delete the objects containing that info - the surfaces
themselves***

***     don't get deleted***

***     
***/*

  GM\_Faces(&ngf, &gflist);

  for (i = 0; i &lt; ngf; i++)

    GF\_Delete(gflist[i]);

  free(gflist);

 

  ***/
* Mesh Info 
***/*

  fprintf(stderr,"Number of nodes in
mesh:

t%d

n",MESH\_NumVertices(mesh));

  fprintf(stderr,"Number of elements in
mesh:

t%d

n",MESH\_NumRegions(mesh));

  ***/
* Loop through each mesh vertex and print some info about it

***/*

  nv = MESH\_NumVertices(mesh);

  for (i = 0; i &lt; nv; i++) {

    vert = MESH\_Vertex(mesh,i);

    if (MV\_Type(mesh,vert) == VDELETED  MV\_Type(mesh,vert) ==
VIGNORE)

      continue;

    ***/
* Get the classification (model entity that mesh entity is on)
of***

***       the mesh vertex***

***       
***/*

    gent = MV\_GMentity(mesh,vert);

    ***/
* Get the coordinates of the mesh vertex 
***/*

    MV\_Coords(mesh, vert, xyz);

    if (gent)

      fprintf(stderr,"V%-d [ G%-d (order %1d) ---  %lf %lf %lf

n",

       MV\_Number(mesh,vert),GEnt\_Number(gent), MV\_GMtype(mesh,vert),

       xyz[0],xyz[1],xyz[2]);

    else

      fprintf(stderr,"V%-d [ G
* (order %1d) ---  %lf %lf %lf

n",

       MV\_Number(mesh,vert),MV\_GMtype(mesh,vert),

       xyz[0],xyz[1],xyz[2]);

  

  fprintf(stderr,"

n

n");

  nr = MESH\_NumRegions(mesh);

  ***/
* Loop through each mesh regions and print some info about it

***/*

  for (i = 0; i &lt; nr; i++) {

    reg = MESH\_Region(mesh,i);

    ***/
* Region type - TET, PYRAMID, PRISM, HEX? 
***/*

    type = MR\_Type(mesh,reg);

 

    ***/
* Vertices of region aka nodes of element 
***/*

    MR\_Vertices(mesh, reg, &nrv, (PMeshVert 
*) rverts);

 

    ***/
* Classification of mesh region i.e. what model region it is in

***/*

    gent = MR\_GMentity(mesh,reg);

    if (GEnt\_Type(gent) != GREGION)

      fprintf(stderr,"Mesh Region not classified on model
region???

n");

 

    fprintf(stderr,"R%-d [ G%-d --- ", MR\_Number(mesh,reg),
GEnt\_Number(gent));

    for (j = 0; j &lt; nrv; j++)

      fprintf(stderr,"%d  ", MV\_Number(mesh,rverts[j]));

    fprintf(stderr,"

n");

  

 

  ***/
* Loop through the regions of the mesh and print the face
neighbor info 
***/*

  for (i = 0; i &lt; nr; i++) {

    reg = MESH\_Region(mesh,i);

    fprintf(stderr,"Region %-d neighbors: ", MR\_Number(mesh,reg));

    type = MR\_Type(mesh,reg);

    nf = RType\_NumFaces(type);

    for (j = 0; j &lt; nf; j++) {

      ***/
* Find the other region sharing face 'j' of 'reg' 
***/*

      oreg = MR\_FaceNebr(mesh,reg,j);

      if (oreg) { ***/
* a region exists on the other side of face,
print info 
***/*

         fprintf(stderr,"R%-d ",MR\_Number(mesh,oreg));

      

    

    fprintf(stderr,"

n");

  

 

 

 

 <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#EXAMPLE) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](utilities.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](linking.md)
