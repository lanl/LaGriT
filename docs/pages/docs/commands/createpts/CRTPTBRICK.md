---
title: CREATEPTS/BRICK
tags: createpts/brick
---

# CREATEPTS/BRICK

------------------

Builds a set nodes (logically rectangular) and create finite element hexahedral connectivity. This command is similar to the deprecated **rz** command format except that symmetry flags are included. A second format specifies that a mesh be created and connected. These commands require a hex or quad cmo to be defined. See also [**`createpts`**](CRTPTSRZ.md) command without the brick option which will create an unconnected point distribution.


This command can be used to create finite element hexahedral connectivity on a logically rectangular set of nodes created by another method (for example, with **quadxyz** ).


## SYNTAX

<pre>
<b>createpts/brick</b> / <b>xyz</b> or <b>rtz</b> or <b>rtp</b> / ni,nj,nk / xmin,ymin,zmin / xmax,ymax,zmax / iiz,ijz,ikz / [ iirat,ijrat,ikrat / xrz,yrz,zrz / isym,jsym,ksym ]

<b>createpts/brick</b> / <b>xyz</b> or <b>rtz</b> or <b>rtp</b>/ ni,nj,nk / 1,0,0 / <b>connect</b> /
</pre>

*Note these actions apply to a hex or quad mesh object as defined with the create command or generated from another command.*


### Distribution Types:


**`xyz`** specifies Cartesian coordinates.

**`rtz`** specifies cylindrical coordinates.

**`rtp`** specifies spherical coordinates.


### Distribution Options:


`ni,nj,nk` are the number of points to be created in each direction.

`xmin,ymin,zmin` are the minimums for coordinates.

`xmax,ymax,zmax` are the maximums for coordinates.

`iiz,ijz,ikz`  if = 0 then mins and maxs are used as cell centers if =1 then mins and maxs are used as cell vertices. The default is 1,1,1

`iirat,ijrat,ikrat` are optional and set the ratio zoning switches (0=off,1=on)

`xrz,yrz,zrz` are optional and are the ratio zoning value - distance is multiplied by the value for each subsequent point.

`isym,jsym,ksym` symmetry flags - not documented

`1,0,0` is the point set selection indicated by start,stride,stop or pset,get,pset_name where 1,0,0 are all points.

<hr>

## EXAMPLES:

```
cmo/create/cmohex / / / hex
createpts/brick/xyz / 3,2,3 /0.,0.,0./1.,1.,1./1,1,1
```
Creates a set of hex points (3x2x3) and hex elements (2x1x2 hex) in the unit cube. The connect option should not be used in this case because the hex brick connectivity is created. 

```
cmo/create/cmohex / / / hex
quadxyz /5,7,5/ 0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/ &
         -1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/
createpts/brick/xyz/5,7,5/1,0,0/connect
```
These commands create a 3D set of nodes using **quadxyz** and then creates hex connectivity with the **createpts/brick** command. Be careful that the *nx, ny, nz* values for both the **quadxyz** and **createpts/brick** command are the same.
 
```
cmo/create/cmoquad/ / / quad
quadxy/ 11, 11 / 0. 0. 0. / 1. 0. 0. / 1. 1. 0. / 0. 1. 0.
createpts/brick/xyz/11,11,1/1 0 0 / connect
```
The first line creates an empty quad cmo. The second line calls **quadxy** to create a 2D set of nodes. The last line uses **createpts/brick** to connect the points into quad elements.

```
define RTOP 20.
define NRADIAL 3
define NRAYS  20
define NRINGS 12
define WRADIUS 2.
define CIRDEG 360.
cmo/create/mo_cylinder / / / hex
createpts/brick/rtz/NRADIAL,NRAYS,NRINGS/0. 0. 0./ &
         WRADIUS CIRDEG RTOP /1,1,1
```
Use the **define** commands to set variables for making a cylinder mesh.   
This hex mesh has height of 20.
There are 3 points from center to rim, there are 20 points around and 12 points vertical. 
Width of radius from center is 2 and the cylinder is a full 360 degrees.


