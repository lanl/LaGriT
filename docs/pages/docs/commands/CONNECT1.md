---
title: CONNECT
tags: connect, tet, Delaunay
--- 

# CONNECT #

-----------------------


Connect the nodes into a Delaunay tetrahedral or triangle grid. The Delaunay criterion requires that the circumsphere (circumcircle) defined by each tetrahedron (triangle) contains no mesh nodes in its interior. If the target simulator is one that uses two-point flux approximation and Voronoi control volumes (FEHM, PFLOTRAN, TOUGH2), use **connect** to create the mesh, then **dump/stor** to compute and output geometric coefficients file.

[Click here for a description of Delaunay](https://en.wikipedia.org/wiki/Delaunay_triangulation)
  
[Click here for more details on the connect algorithm](https://lanl.github.io/LaGriT/pages/docs/connect_notes.html)

  
## SYNTAX ##

<pre>
<b>connect</b> / [ <b>delaunay</b> ] / [ ifirst,ilast,istride ] / [ <b>noadd</b> ] [ <b>check_interface</b> ] [ <b>old_order</b> ]
</pre>

The keywords **noadd**, **check_interface** and **old_order** may be given in any position after **connect**, in any combination.


## OPTIONS
 
 **`delaunay`** is the algorithm (the only one; it may be left out). It starts from an enclosing triangle or tetrahedron that contains all nodes and inserts the nodes one at a time. For mesh points with several materials (imt values separated by material interfaces from **surface** / **region** / **mregion**), connect then looks for edges that cross an interface and adds points where they cross, to make a conforming mesh; mesh quality may suffer. It also adds a point in the middle of each triangle or tetrahedron of very small area or volume.
  
*`ifirst,ilast,istride`* (or **pset,get,** *name*) connects only those nodes. It may follow **connect** directly: `connect/1,0,0` is `connect/delaunay/1,0,0`.

**`noadd`** (2D and 3D) adds no points: neither at material interfaces nor in small triangles or tetrahedra. The mesh connects exactly the given nodes; edges may cross material interfaces.

**`check_interface`** (3D) does a more exhaustive job of making sure there are no edges of the mesh that cross a material boundary: it also checks edges between two nodes tagged itp='intrface'. In 2D it gives a warning and is ignored.

*`big_tet_coordinates`* (deprecated) The 12 coordinates of the enclosing tetrahedron (3D) or 9 of the enclosing triangle (2D), after the point range. They are used with a WARNING that the option will be removed; connect places the enclosing tetrahedron or triangle itself. Fewer numbers give a WARNING and are ignored. (LaGriT V3.3.4 and earlier read 12 numbers after the point range whatever was there, so `connect/delaunay/1,0,0/noadd` made no tetrahedra.)

**`old_order`** (2D and 3D; may be added anywhere in the command) inserts the points in the order used by LaGriT V3.3.4 and earlier, a fixed prime stride through the point list. By default connect inserts more than 100 points in a Biased Randomized Insertion Order (BRIO; Amenta, Choi and Rote, 2003): the points are split at random into rounds that double in size, each round is sorted along a Morton (Z-order) curve (in 2D on the two coordinates of the points' plane, in 3D on all three), and the rounds are inserted smallest first. Each insertion then starts close to the new point, so connect time grows about linearly with the number of points, whatever order they are given in (256,000 random points: 2D about 23 times faster, 3D about 5 times faster than V3.3.4). The random choices use a fixed seed, so a run is reproducible. The mesh is the same in either order except where points are exactly cocircular or cospherical (a regular grid, say: in 3D a cube splits into 5 or 6 tets), where either choice is Delaunay; **old_order** reproduces the choices of older versions.




## USAGE


For 2D and connecting points on a planar surface, the mesh must have **ndimensions_topo**=2 and **ndimensions_geom**=2. This command will create a mesh object with settings appropriate for the connect command.
*Note: the command will fail if you forget the spaced slashes acting a place holders for other options.*
```
cmo/create/ cmotri / / / triplane
```



The connect command does not filter out coincident vertices (sqrt[(xi-xj)<sup>2</sup> + (yi-yj)<sup>2</sup> + (zi-zj)<sup>2</sup>] < epsilon). Use the following commands before connect to remove duplicate points.
```
filter/ 1,0,0
rmpoint/compress
```


In 2D the points may lie in any plane. When they lie in a coordinate plane (every z the same, or every y, or every x), the triangulation is exactly Delaunay and covers the convex hull of the points: the in-circle and orientation tests are exact (they do not depend on tolerances or on how far the points are from the origin), and the triangles along the hull that the construction leaves out are added at the end. A hull corner flatter than 1e-6 (the triangle's height less than 1e-6 of its base) is not filled: that is round-off in points meant to lie on a straight side, and the sliver would only harm the mesh. Points in a tilted plane are connected as in LaGriT V3.3.4 and earlier, followed by **recon/0**.

In 3D, for better precision where large coordinate numbers are being used, translate the points close to zero before using connect. The following will move points, connect, then translate back to original position.
```
trans 1,0,0 /XBIG,YBIG,ZBIG/ 0. 0. 0./
connect
trans 1,0,0 / 0. 0. 0./ XBIG,YBIG,ZBIG
```

The connect algorithm creates a triangulation or tetrahedralization of the convex hull. If the domain to be meshed is not convex, there is no guarantee that all of its faces and edges will be within the specified mesh boundary. Connections will be formed across non-convex boundaries. 


In 3D (and in 2D in a tilted plane) a convex geometry is not guaranteed. A point distribution over a large region where spacing varies from very small to very large can result in high aspect ratios with small concavities formed on mesh boundaries. You can mitigate the impact by adjusting the mesh resolution. Generally high aspect ratio tets (long dimension along the external boundary) are more of a problem. This means that mesh refinement that brings the mesh closer to unit aspect ratio will help.

  
The connect command may refuse to add nodes that will result in near
  zero-volume tetahedra. The volume tests are based on the mesh object
  epsilons. To ensure that these epsilons are based on the geometry,
  issue a **setsize** command before **setpts**. Expert users may adjust the epsilons with the **cmo/setatt** command. 


The connect command will report if it is successful, check to see if there are any problems. For instance, connect may finish but may not be able to include all points due to a non-convex boundary or a poor point distribution. For instance:
```
 Dudding      7181 points that have no associated tetrahedra.

 The mesh is complete but could not include all points.                         
 Number of points that could not be connected:       7181                       
 
LaGriT FINISH: connect        
```

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
connect/delaunay/1,0,0/noadd
connect/noadd/check_interface
connect/old_order/noadd
```

Keywords in any order: connect all nodes without adding points; the same with the extra interface check (3D); without adding points, in the insertion order of LaGriT V3.3.4 and earlier.

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

The **connect** command replaces the LaGriT V1 command **search/delaunay**.

