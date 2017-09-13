---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: CONNECT
---

 

 

 **CONNECT**

  

  Connect the nodes into a Delaunay tetrahedral or triangle grid. The
  Delaunay criterion requires that the circumsphere (circumcircle)
  defined by each tetrahedron (triangle) contains no mesh nodes in its
  interior. At present only the **delaunay** option is implemented;
  this option will be the default. The delaunay algorithm used
  requires that a "big tet" be constructed that contains all nodes in
  its interior. The user has the option of providing the coordinates
  of this "big tet". The user also has the option of selecting a
  subset of nodes to connect.   Connect will by default detect
  material interfaces and will look for edges that intersect the
  interfaces.  Nodes will be added to the mesh at these intersections
  to create a conforming mesh.  This activity may be turned off by
  using the **noadd** option.

  

  The **check\_interface** option is more expensive but does a more
  exhaustive job of making sure there are no edges of the mesh that
  cross a material boundary.

  

  **Connect** may refuse to add nodes that will result in near
  zero-volume tetahedra. The volume tests are based on the mesh object
  epsilons. To ensure that these epsilons are based on the geometry,
  issue a
  **[setsize](http://lagrit.lanl.gov/SETSIZE.md)**command
  before **[setpts](http://lagrit.lanl.gov/SETPTS.md)**. 
  Expert users may adjust the epsilons with the 
  [cmo/setatt](http://lagrit.lanl.gov/cmo_setatt.md) 
  command.  **Connect** will generate a 2D triangular mesh if both
  [**ndimensions\_geom** and
  **ndimenions\_topo**](http://lagrit.lanl.gov/meshobject.md)
  are 2.  In this case all nodes must lie in a plane.

  [Click here for more details on the connect
  algorithm](http://lagrit.lanl.gov/connect_notes.md).
 
  The following instructions are for connecting points on a planar
  surface.  The mesh must have **ndimensions\_topo**=2 and
  **ndimensions\_geom**=2.
 
  **cmo****/create**/trimesh//**/tri**

  **cmo****/modatt**/**/ndimensions\_geom****/default**/2

  ...

  **connect**
 
  an alternate way to achieve this is:

  **cmo****/create**/trimesh//**/triplane**

      ...

  **connect**

   

 FORMAT:

  **connect**/
[**delaunay**
]/ifirst,ilast,istride/big\_tet\_coordinates

  **connect****/noadd

  connect****/check\_interface**

 EXAMPLES:

  **connect**

  Create the Delaunay tetrahedral connectivity of all nodes in the
  mesh. Add nodes to break multi-material connections.

  **

  connect****/delaunay**/

  Create the Delaunay tetrahedral connectivity of all nodes in the
  mesh. Add nodes to break multi-material connections.

  **

  connect/delaunay**/1,0,0/
  0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./

  **connect**/1,0,0/
  0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./noadd

  **connect/delaunay**/1,0,0/
  0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./noadd

  **connect/delaunay**/1,0,0/
  0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./check\_interface

  Create the Delaunay tetrahedral connectivity of all nodes in the
  mesh and specify explicitly the coordinates of the enclosing
  tetrahedron

  **

  **connect/pset get points

  ****Create the Delaunay tetrahedral connectivity of a subset of
  nodes.****

  

  **connect/noadd**

  Create the Delaunay tetrahedral connectivity of  all nodes in the
  mesh and disregard material interfaces.

  **

  connect**/check\_interface****

  Create the Delaunay tetrahedral connectivity of  all nodes in the
  mesh with added checking of edges that have both nodes tagged as
  itp='intrface' to be sure that the edge does not cross a material
  interface. This option is more expensive but may fix situations
  where multi-material edges do not get refined because they connect
  an 'intrface' node to an 'intrface' node.


 [Click here for 2D
 demos](http://lagrit.lanl.gov/demos/2d_connect/test/md/main_2d_connect.md)

 [Click here for 3D
 demos](http://lagrit.lanl.gov/demos/connect/test/md/main_connect.md)



 

