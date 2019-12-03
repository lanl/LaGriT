---
title: QUADXYZ
tags: quadxyz point distribution
---


# QUADXYZ

-----------------------


Define arbitrary, logical hexahedrons of points in 3D(xyz) space.


## SYNTAX

<pre>
<b>quadxyz</b>/ nx,ny,nz / &
    x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4 / &
    x5,y5,z5/x6,y6,z6/x7,y7,z7/x8,y8,z8 
</pre>

`nx ny nz` specifies the number of points between the 1st and last point along each axis. The number of points will be 1 more than the number of spaces. 

`x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4` are the coordinates counter clockwise around the bottom. 

`x5,y5,z5/x6,y6,z6/x7,y7,z7/x8,y8,z8` are the coordinates counter clockwise around the top. 


See example and image for the point order.


## EXAMPLES

```
define NPTS 2

cmo/create/mohex
quadxyz/NPTS NPTS NPTS/ &
  0. 0. 0./1. 0. 0.02 / 1. 1. 0. /0. 1.  .1 / & 
  0. 0. 1./1. 0. 1./ 1. 1. 1. /0. 1. 1.1 

createpts/brick/xyz/NPTS NPTS NPTS/1,0,0/connect
```
Create a 2x2x2 point distribution and connect into a single hex element with 8 points as shown in the image.


<img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxyz_hex.png" alt="quadxyz">

