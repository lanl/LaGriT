---
title: ROTATEPT
tags: rotatept
---

# ROTATEPT

---------

Rotates a point distribution about a point. 




<pre>
<b>rotatept</b>/ifirst,ilast,istride/[<b>no</b>]<b>copy</b> /xcen,ycen,zcen/theta/phi 
</pre>


`ifirst,ilast,istride` is the node range selected by node numbers or pset,get,pname.


**`nocopy`** keeps only the rotated points, the node count does not change.


**`copy`** keeps a copy of the original unrotated points, as well as the rotated points.
The new points will have the rotated coordinate values but no other mesh attributes will be set for these points.


`xcen,ycen,zcen` is the point center of rotation.


`theta` in degrees is the angle of rotation toward the negative z-axis.


`phi` in degrees is the angle of rotation of the XY plane around the Z-axis, where positive phi is measured from the positive x-axis toward the positive y-axis. 

 

## EXAMPLES

```
define x1 38.
define x2 170.
define y1 17. 
define y2 81.
cmo/create/cmosink/ / /quad
quadxy/ 133 65 / x1 y1 0./ x2 y1 0./ x2 y2 0. / x1 y2 0.

rotatept/1,0,0/nocopy/ x2 y1  0./0. -16.5
```
rotate from right lower corner xmax and ymin

```
define XTRANS 55.
define YTRANS 30.
define ROT 30.

trans/1,0,0/0. 0. 0./ XTRANS YTRANS  0./                                
rotatept/1,0,0/nocopy/0. 0. 0./0. ROT  
```
translate then rotate a grid

<hr>

[Click here for demos](../demos/description_rotatept.md)
