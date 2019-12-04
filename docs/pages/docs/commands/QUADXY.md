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
quadxy/ 3 5 /0. 0. 0./10. 0. 0./10. 20. 0./0. 20. 0. 
createpts/brick/xyz/ 3 5 1 /1,0,0/connect
```
XY Flat, as shown in first image. Create a 3 x 5 quad grid on the XY plane where Z = 0.

```
define X1 2.0
define Y1 40.0
define Z1 4.0
define X2 2.0
define Y2 1.0
define Z2 1.0
define X3 50.
define Y3 20.
define Z3 40.
define X4 55.
define Y4 15.
define Z4 1.

define / NX / 6
define / NY / 1
define / NZ / 24

cmo/create/moquad/ / / quad
quadxy/NX NZ/ X1 Y1 Z1 / X2 Y2 Z2 /X3 Y3 Z3 / X4 Y4 Z4
createpts/brick/xyz/NX NY NZ/1,0,0/connect


```
XZ Twisted, as shown in last image. Create a 6 x 24 quad grid with a twist.



Input LaGriT command file for 3 examples: [ex_quadxy.lgi](https://lanl.github.io/LaGriT/pages/docs/demos/input/ex_quadxy.lgi.txt)


<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example1.png"><img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example1.png" alt="quadxy"> </a> 

<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example2.png"><img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example2.png" alt="quadxy"> </a> 

<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example3.png"><img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/quadxy_example3.png" alt="quadxy"> </a> 



