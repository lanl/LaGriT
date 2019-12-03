---
title: QUADXY
tags: quadxy 2D point distribution
--- 

# QUADXY

----------------

Define an arbitrary, logical quad of points in 3D space with NDIM1 x NDIM2 number of nodes.

The four corners of the quad surface are defined in counter clockwise order ( the normal to the quad points is defined using the right hand rule and the order of the points).

The nodes can be connected using the **`createpts/brick/xyz`** command.

## SYNTAX

<pre>
<b>quadxy</b>/ndim1, ndim2 /x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4
</pre>

## EXAMPLES

```
cmo/create/moquad/ / / quad
createpts/brick/xyz/ 3 5 1 /1,0,0/connect
```
Create a 3 x 5 grid on the XY plane where Z = 0.



Input LaGriT command file for 3 examples shown in images: [ex_quadxy.lgi](https://lanl.github.io/LaGriT/pages/docs/demos/input/ex_quadxy.lgi.txt)


<img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example1.png" alt="quadxy"> <img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example2.png" alt="quadxy"> <img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example3.png" alt="quadxy"> 




