---
title: CONNECT
tags: connect tet
--- 

# CONNECT #

Connect the nodes into a Delaunay tetrahedral or triangle grid. The Delaunay criterion requires that the circumsphere (circumcircle) defined by each tetrahedron (triangle) contains no mesh nodes in its interior. If the target simulator is one that uses two-point flux approximation and Voronoi control volumes (FEHM, PFLOTRAN, TOUGH2), use **connect** to create the mesh, then **dump/stor** to compute and output geometric coefficients file.

[Click here for a description of Delaunay](https://en.wikipedia.org/wiki/Delaunay_triangulation)
  
[Click here for more details on the connect algorithm](https://lanl.github.io/LaGriT/pages/docs/connect_notes.html)

  
## SYNTAX ##

<pre>
<b>connect</b>/[ <b>delaunay</b>/ ifirst,ilast,istride / big_tet_coordinates ]

<b>connect</b> / <b>noadd</b>

<b>connect</b> / <b>check_interface</b>
</pre>


## OPTIONS
 
 **`delaunay`** is the default algorithm and requires that a "big tet" be constructed that contains all nodes in
  its interior.  For mesh points with multi-materials (imt with multi-values), connect will detect material interfaces and will look for edges that intersect the interfaces.  connect will add points at these intersections to create a conforming mesh, note that mesh quality may be impacted.  
  
  
*`ifirst,ilast,istride`* The user has the option of selecting a subset of nodes to connect.


*`big_tet_coordinates`* The user has the option of providing the coordinates of this "big tet". 


**`noadd`** This option will turn off material interface detection.


**`check_interface`** option does a more exhaustive job of making sure there are no edges of the mesh that cross a material boundary.




## USAGE


For 2D and connecting points on a planar surface, the mesh must have **ndimensions_topo**=2 and **ndimensions_geom**=2. These commands will set the mesh object for connect.

```
cmo/create/ cmotri / triplane
```
or
```
cmo / create / cmotri / / / tri
cmo/setatt/cmotri/ndimensions_geom/1 0 0/2
```


The connect command does not filter out coincident vertices (sqrt[(xi-xj)<sup>2</sup> + (yi-yj)<sup>2</sup> + (zi-zj)<sup>2</sup>] < epsilon). Use the following commands before connect to remove duplicate points.
```
filter/ 1,0,0
rmpoint/compress
```

For better precision where large coordinate numbers are being used, translate the points close to zero before using connect. The following will move points, connect, then translate back to original position.
```
trans 1,0,0 /XBIG,YBIG,ZBIG/ 0. 0. 0./
connect
trans 1,0,0 / 0. 0. 0./ XBIG,YBIG,ZBIG
```

The connect algorithm creates a triangulation or tetrahedralization of the convex hull. If the domain to be meshed is not convex, there is no guarantee that all of its faces and edges will be within the specified mesh boundary. Connections will be formed across non-convex boundaries. 


A convex geometry is not guaranteed. A point distribution over a large region where spacing varies from very small to very large can result in high aspect ratios with small concavities formed on mesh boundaries. You can mitigate the impact by adjusting the mesh resolution. Generally high aspect ratio tets (long dimension along the external boundary) are more of a problem. This means that mesh refinement that brings the mesh closer to unit aspect ratio will help.

  
The **connect** command may refuse to add nodes that will result in near
  zero-volume tetahedra. The volume tests are based on the mesh object
  epsilons. To ensure that these epsilons are based on the geometry,
  issue a **setsize** command before **setpts**. Expert users may adjust the epsilons with the **cmo/setatt** command. 



## EXAMPLES ##

```
connect
```

```
connect/delaunay/
```
These two commands are the same, they create the Delaunay tetrahedral connectivity of all nodes in the mesh. Add nodes to break multi-material connections.

```
cmo / create / mo_tri / / / triplane
createpts / xyz / 4 4 1 / 0. 0. 0. / 1. 1. 0. / 1 1 1
cmo / setatt / mo_tri / imt / 1 0 0 / 1
connect
cmo / setatt / mo_tri / itetclr / 1 0 0 / 1
resetpts / itp
```
Create a triplane mesh object (ndimensions_topo=2 and ndimensions_geom=2) with 4 x 4 points on plane with Z = 0. Connect into triangles and set node and element materials to 1. The resetpts/itp command will define the boundary nodes for this mesh.

```
connect/delaunay/1,0,0/0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./

connect/1,0,0/ 0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./noadd

connect/delaunay/1,0,0/ 0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./noadd

connect/delaunay**/1,0,0/0.,0.,0./1000.,0.,0./500.,1000.,0./500.,500.,10./check_interface
```

Create the Delaunay tetrahedral connectivity of all nodes in the mesh and specify explicitly the coordinates of the enclosing tetrahedron. 

```
connect /pset, get, mypoints
```

Create the Delaunay tetrahedral connectivity of a subset of nodes with the name mypoints.

```
connect/noadd
```

Create the Delaunay tetrahedral connectivity of  all nodes in the  mesh and disregard material interfaces.
  
```
# create a 2D triangle mesh
cmo / create / mo_tri / / / triplane

# Make some points at the four corners
createpts / xyz / 5 5 1 / 0. 0. 0. / 1. 1. 0. / 1 1 1

# Add some random points and delete duplicate points
createpts / random / xyz / 0.4 / 0.1 0.1 0. / 0.9 0.9 0.
filter / 1 0 0
rmpoint / compress

# set some defaults for the connect routine
cmo / setatt / mo_tri / imt / 1 0 0 / 1
cmo / setatt / mo_tri / itp / 1 0 0 / 0

connect

# set default materials and boundary tags
cmo / setatt / mo_tri / itetclr / 1 0 0 / 1
resetpts / itp
```

Create a simple 2D triangle mesh. Set defaults for material and boundary tags for nice behavior. This starts with points at the corner of a box and fills with random points. The connect command will create the triangulation and the result is a single material mesh with inside/outside boundary points tagged with the itp attribute.

```
connect/check_interface
```

Create the Delaunay tetrahedral connectivity of  all nodes in the
  mesh with added checking of edges that have both nodes tagged as
  itp='intrface' to be sure that the edge does not cross a material
  interface. This option is more expensive but may fix situations
  where multi-material edges do not get refined because they connect
  an 'intrface' node to an 'intrface' node. 
  

 [Click here for 2D demos](../demos/main_2d_connect.md)

 [Click here for 3D demos](../demos/main_connect.md)
