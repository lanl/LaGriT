---
title: CONNECT
tags: connect tet
--- 

# CONNECT #

  Connect the nodes into a Delaunay tetrahedral or triangle grid. The
  Delaunay criterion requires that the circumsphere (circumcircle)
  defined by each tetrahedron (triangle) contains no mesh nodes in its
  interior.  
  
## SYNTAX ##

<pre>
<b>connect</b>/[ <b>delaunay</b>/ ifirst,ilast,istride / big_tet_coordinates ]

<b>connect</b> / <b>noadd</b>

<b>connect</b> / <b>check_interface</b>
</pre>
 
 **`delaunay`** is the default algorithm and requires that a "big tet" be constructed that contains all nodes in
  its interior.  For mesh points with multi-materials, connect will detect material interfaces and will look for edges that intersect the interfaces.  Nodes will be added to the mesh at these intersections to create a conforming mesh. 
  
*`ifirst,ilast,istride`* The user has the option of selecting a subset of nodes to connect.

*`big_tet_coordinates`* The user has the option of providing the coordinates of this "big tet". 



**`noadd`** This option will turn off material interface detection.

**`check_interface`** option does a more exhaustive job of making sure there are no edges of the mesh that cross a material boundary.



The **connect** command may refuse to add nodes that will result in near
  zero-volume tetahedra. The volume tests are based on the mesh object
  epsilons. To ensure that these epsilons are based on the geometry,
  issue a **setsize** command before **setpts**. 


The **connect** command does not like duplicate points. Use the following commands before connect to remove duplicate points.

```
filter/ 1,0,0
rmpoint/compress
```

  
Expert users may adjust the epsilons with the **cmo/setatt** command.  **Connect** will generate a 2D triangular mesh if current mesh object attributes **ndimensions_geom** and **ndimenions_topo** are 2.  In this case all nodes must lie in a plane. The following instructions are for connecting points on a planar surface.  The mesh must have **ndimensions_topo**=2 and **ndimensions_geom**=2.
  
```
cmo / create / cmotri / / / tri
cmo/setatt/cmotri/ndimensions_geom/1 0 0/2
```
or
```
cmo/create/ cmotri / triplane
```


[Click here for more details on the connect algorithm](https://lanl.github.io/LaGriT/pages/docs/connect_notes.html)
 

## EXAMPLES ##

```
connect
```

```
connect/delaunay/
```

The two commands are the same, they create the Delaunay tetrahedral connectivity of all nodes in the mesh. Add nodes to break multi-material connections.

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
