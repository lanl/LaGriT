---
title: QUADXYZ
tags: quadxyz point distribution
---


# QUADXYZ

-----------------------



Define arbitrary, logical set of points in 3D(xyz) space, no elements are created. <br>
The set of points can be connected into hexahedrals by using the command [**`createpts/brick`**](createpts/CRTPTBRICK.md) as shown in the example and image below.


This point distribution is defined by 6 points along the xyz axis. This differs from **createpts/brick/xyz/** which generates a logicially rectangular distribution defined by 2 points at the mininum and maximum corners of the domain and then generates connectivity for elements.




## SYNTAX

<pre>
<b>quadxyz</b>/ nx,ny,nz / &
    x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4 / &
    x5,y5,z5/x6,y6,z6/x7,y7,z7/x8,y8,z8 
</pre>


`nx ny nz` specifies the number of points between the 1st and last point along each X, Y, Z axis. The number of points will be 1 more than the number of spaces. 


`x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4` are the coordinates counter clockwise around the bottom quad face. 


`x5,y5,z5/x6,y6,z6/x7,y7,z7/x8,y8,z8` are the coordinates counter clockwise around the top quad face. 




## EXAMPLES

```
define NPTS 2

cmo/create/mohex
quadxyz/NPTS NPTS NPTS/ &
  0. 0. 0./1. 0. 0.02 / 1. 1. 0. /0. 1.  .1 / & 
  0. 0. 1./1. 0. 1./ 1. 1. 1. /0. 1. 1.1 

createpts/brick/xyz/NPTS NPTS NPTS/1,0,0/connect
```
Create a 2x2x2 point distribution (mesh object with 0 elements). Then use createpts/brick to create connectivity. The result is a single hex with 8 points as shown in the image.

Click on image for full size.

<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxyz_hex.png"> <img width="500" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxyz_hex.png" alt="quadxyz"> </a>


