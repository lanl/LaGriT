---
title: CREATEPTS/BRICK
tags: createpts/brick
---

# CREATEPTS/BRICK

------------------

Builds a set nodes (logically rectangular) and creates finite element connectivity. This command is similar to the **quadxyz** command except that symmetry flags are included and elements are created. 


The **brick** option specifies that a mesh is connected into elements. This command can be used to create finite element connectivity on a logically rectangular set of nodes created by another method. See  [**`quadxyz`**](../QUADXYZ1.md) or [**`createpts`**](CRTPTSRZ.md) command without the **brick** option.


Make the mesh object with the cell type the brick makes, using **cmo/create**. For instance,  
``` cmo/create/mo_name / / / hex``` or ``` cmo/create/mo_name / / / quad```.

The brick sets the cell type of the mesh object itself: hex (ni,nj,nk all greater than 1), quad (one of them 1) or line (ni,1,1). A mesh object with no cells yet, of any type (for instance one made with ``` cmo/create/mo_name``` and no type, which is tet), becomes a hex, quad or line mesh object, and the result is the same as in a mesh object of that type. Adding a brick to a mesh object that already has cells of another type (tets, say, or hexes for a quad brick) is an ERROR: the two kinds of cells cannot share one mesh object.


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

`isym,jsym,ksym` are optional periodic (wrap-around) switches, default 0,0,0. They come after the ratio zoning tokens, so those must be given too, even if ratio zoning is off. With **`jsym`** = 1, the last layer of cells in the j direction is joined to the FIRST layer of vertices (j = 1) instead of the last (j = nj). **`isym`** does the same in i, and **`ksym`** in k. Use a switch only where the first and last vertices of that direction are at the same place, as in **`rtz`** with theta from 0 to 360 (`jsym`), or **`rtp`** with phi from 0 to 360 (`ksym`). The seam is then closed in the connectivity without **filter**. The vertices of the last layer (j = nj) are still created, but no cell uses them. In any other case the wrap makes cells that reach back across the whole mesh and overlap the others. For example, an **`xyz`** brick with `isym` = 1 reports a total volume larger than its box.

When a switch is used:

* the cells next to the axis of an **`rtz`** brick still use several vertices at the same place on the axis. Only **filter** merges those;
* **filter / 1,0,0** followed by **rmpoint / compress** merges the unused seam vertices and the axis vertices together, and gives the same mesh as without the switch.

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

```
cmo/create/mo_cyl / / / hex
createpts/brick/rtz/5,17,5/0.,0.,0./2.,360.,2./1,1,1/0,0,0/1.,1.,1./0,1,0
```
The same kind of cylinder, with `jsym` = 1: the cells from 337.5 to 360 degrees are joined to the vertices at 0 degrees, so the seam is closed without **filter**. The 25 vertices at 360 degrees (5 radii x 5 heights) are left unused. The ratio zoning tokens `0,0,0 / 1.,1.,1.` (off) are needed to reach the switches.

*LaGriT reads only the first 80 characters of an input line. A line that reaches the symmetry switches is easily longer than that, and then the switches are silently lost. Write such a command without spaces, as above, or continue it on the next line with **&**.*


