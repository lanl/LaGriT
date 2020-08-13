---
title: RM
tags: rm, remove
---

# RM

------------------

Removes any points that are within the specified point range and specified volume of space. 
This command is seldom used but included here for backward compatibility.

A more typical set of commands for removing points is the combination of [**`PSET`**](PSET.md) with [**`RMPOINT`**](RMPOINT.md)/pset,get,pname.


Note that the points that are removed become dudded out (point **itp** set to 21) and are not removed from the mesh object data arrays.


## SYNTAX

<pre>
<b>rm</b> / <b>xyz</b> /ifirst,ilast,istride/xmin,ymin,zmin/xmax,ymax,zmax/ [xcen,ycen,zcen]

<b>rm</b> / <b>rtp</b> /ifirst,ilast,istride/ r1,t1,p1 / r2/t2/p2/ [xcen,ycen,zcen]  

<b>rm</b> / <b>rtz</b> /ifirst,ilast,istride/ r1,t1,z1 / r2,t2,z2/ [xcen,ycen,zcen] 
</pre>


### Geometry Options


**`xyz`** Cartesian coordinates defined by minimum and maximum of the coordinates.


**`rtp`** Spherical coordinates defined by the center point. 
The sperical shell or sperical section given by radius r1 to r2, and angles theta t1 to t2 and angles phi p1 to p2.
 The value of theta is the angle with respect to the Z-axis and phi is the angle in the XY-plane with respect to the X-axis. 


**`rtz`** Cylindrical coordinates defined by the center line points.  
The  cylinder or cylindrical shell given by radius r1 to r2, angle theta t1 to t2 and height z1 to z2.
The value of theta is the angle in the XY- plane with respect to the x-axis.


In cylindrical coordinates the cylinder always lines up along the z axis; use the **coordsys** command before issuing the **rm** command if the points
  to be removed are not aligned with the z-axis; then issue a final **coordsys** command to return to normal. 



`ifirst,ilast,istride` is the selected point set range where 1,0,0 means all. The **pset,get**, pname convention can be used.


 



## EXAMPLES

```
rm/ xyz /1,0,0/ 2.,2.,2./4.,4.,4./  0.,0.,0.

rm/ rtz /1,0,0/ 0.,0.,0./1.,360.,10./ 0.,0.,0.
```
