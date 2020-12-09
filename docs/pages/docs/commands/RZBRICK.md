---
title: RZBRICK
tags: rzbrick
---

# RZBRICK

-------------------

This command is deprecrated, see [CREATEPTS](createpts.md).

Builds a brick mesh and generates a nearest neighbor connectivity matrix. This command is similar to the rz command format except here we
have symmetry flags to input. A second format specifies that a mesh be created and connected.


Use the connect option with **quadxyz** to connect logically rectangular grids.


## SYNTAX
<pre>
<b>rzbrick /xyz rtz rtp</b>/ni,nj,nk/xmin,ymin,zmin/xmax,ymax,zmax/ &
           iiz,ijz,ikz/[iirat,ijrat,ikrat/xrz,yrz,zrz/isym,jsym,ksym]

<b>rzbrick /xyz rtz rtp</b>/ni,nj,nk/pset,get,pset_name/<b>connect</b>/
</pre>

### Geometry Options:
 
  **xyz** specifies Cartesian coordinates (default).

  **rtz** specifies cylindrical coordinates.

  **rtp** specifies spherical coordinates.

### Distribution Options:

  
`ni,nj,nk` are the number of points to be created in each direction.

`xmin,ymin,zmin` are the minimums for coordinates.

`xmax,ymax,zmax` are the maximums for coordinates.

`iiz,ijz,ikz` 0 or 1 switches: 
  
* if = 0 then mins and maxs are used as cell centers

* if = 1 then mins and maxs are used as cell vertices
    

`iirat,ijrat,ikrat` ratio zoning switches 0=off (default), 1=on


`xrz,yrz,zrz` ratio zoning value - distance is multiplied by this value for each subsequent point.

`pset,get,pset_name` point set selection given by name 

`isym,jsym,ksym`  symmetry flags - not documented


<hr>

**Warning: This command does not create a 2D grid, it has memory errors:**

``
rzbrick/xyz/5,10,1/0. 0. 0./10. 20. 0. /1,1,1
``

for 2D this will work:
```
cmo create cmo1///quad 
quadxy/ 5 5/ 0. 0. 0. / 20. 0. 0./20. 20. 0. / 0. 20. 0. 
rzbrick/xyz/5,5,1/1,0,0/connect
```





## EXAMPLES


	rzbrick/xyz/3,2,3/0.,0.,0./1.,1.,1./1,1,1

creates a hex grid 2x1x2 cells in the unit cube


	quadxyz/5,7,5/0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/-1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/setpts
	rzbrick/xyz/5,7,5/1,0,0/connect

creates a hex grid inside the hexahedral specified by the 8 corners passed to quadxyz
