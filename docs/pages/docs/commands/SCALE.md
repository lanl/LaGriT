---
title: SCALE
tags: scale
---

# SCALE

-------------------------------------

Scale a point distribution according to the scale factors. 

## SYNTAX

<pre>
<b>scale</b>/range/ <b>relative</b> /<b>xyz rtz rtp</b>/iscale,jscale,kscale/xcen,ycen,zcen

<b>scale</b>/range/ <b>absolute</b> /<b>xyz rtz rtp</b>/iscale,jscale,kscale/
</pre>


`range` is the selection of points designated by node numbers for ifirst,ilast,istride or **pset**,get,pname. 1,0,0 will select all.


**`absolute`** scaling factors are constants added on to the existing coordinates.  That is, absolute is really a translation rather than a rescale.


**`relative`** scaling factors are unitless multipliers with reference to geometric center `xcen,ycen,zcen`. 


`iscale,jscale,kscale` are the scale factors according to `geometry` type:
- **`xyz`** Cartesian coordinates. iscale,jscale,kscale = x, y, z
- **`rtz`** Cylindrical coordinates. iscale,jscale,kscale =  radial, theta,  z 
- **`rtp`** Spherical coordinates. iscale,jscale,kscale = radial, theta, phi 



## EXAMPLES

```
scale/1,0, 0/ relative / xyz / 0.3048 0.3048 0.3048
```
Convert coordinates from feet to meters.


```
scale / 1 0 0 / relative / xyz / 10. 10. 1.
math/sin/CMO_OBJ/zic/1,0,0/CMO_OBJ/yic

scale / 1 0 0 / relative / xyz / 0.1 0.1 0.1
trans / 1 0 0 / 0. 0. 0. / 0. 0. 0.5
cmo / printatt / -def- / -xyz- / minmax
```
Use the math math and scale commands to set the z coordinates to make the shape of a sin() wave. 
First use scale to make x and y coordinates to 0-10 so the sin(y) goes through complete 2pi cycles.
Set z(i)=sin(y(i)). A final scale factor is applied and the surface is translated up so all z values are above 0.

