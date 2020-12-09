---
title: MREGION
tags: mregion, region, surface
---

# MREGION

------------------

Define a material region from a set of surfaces by logically combining surface names and region names. 
**`MREGION`** is used together with  [**`SURFACE`**](SURFACE.md) and [**`REGION`**](REGION.md) to set mesh object geometry and materials. 
<br>
See Examples below.

Defining a mregion will cause the information associated with this material region to be stored under the name of the [geometry](../geometries.md) of the current mesh object.  Releasing the region will remove this information.
The defined mesh object geometry can be named with the [**`GEOMETRY`**](geometry.md) command.




## SYNTAX

<pre>
<b>mregion</b>/ material_region_name/ surface_and_region_operators

<b>mregion</b>/ mregion_name/ <b>release</b>
</pre>

Defining a material region will cause the information associated with
this material region to be stored under the name of the [geometry](../geometries.md) of the current mesh object.
Releasing the material region will remove this information.
<br>To define a material region, use operators with surfaces and regions according to the following rules.

<br> The operators **or, and**, and **not** mean union, intersection, and complement respectively and are applied to surfaces and regions.
<br> Parentheses are operators and are used for nesting.
<br> Spaces are required to separate operators and parentheses.
<br>
<br>
The operators **lt, le, gt**, and **ge** are applied to mesh object surfaces according to the following rules:


- **lt** if the surface following is a volume then **lt** means inside not including the surface of the volume. If the surface is a plane or a
sheet **lt** means the space on the side of the plane or sheet opposite to the normal not including the plane or sheet itself.

- **le**  if the surface following is a volume then **le** means inside including the surface of the volume. If the surface is a plane or a
sheet **le** means the space on the side of the plane or sheet opposite to the normal including the plane or sheet itself.

- **gt**  if the surface following is a volume then **gt** means outside not including the surface of the volume. If the surface is a plane or a
sheet **gt** means the space on the same side of the plane or sheet as the normal not including the plane or sheet itself.

- **ge**  if the surface following is a volume then **ge** means outside including the surface of the volume. If the surface is a plane or a
sheet **ge** means the space on the same side of the plane or sheet as the normal including the plane or sheet itself.

Internal interfaces should be excluded when defining material regions, use **lt** and **gt**. External boundaries should be included, use **le** and **ge**.



## EXAMPLES

```
mregion/mat1/ le box1 and ( lt sphere1 and ( lt plane1 or gt plane2 ) ) 
```
Define a material mat1 that is inside or on a box shaped surface, and inside surface called sphere1, and relative to plane1 and plane2. 


```
mregion/mat2/ regiona or regionb 
```
Define a material region based on earlier defined regions such that material mat2 are all nodes found inside  regiona or regionb.


```
cmo/create/cmo
surface/outside/reflect/box/0,0,0/3,2,1
region/all/le outside
mregion/all/le outside
createpts/xyz/3,3,3/0,0,0/3,2,1/1,1,1
setpts
connect
```
Very simple application of the region and mregion commands to create a single material box shaped mesh.


```
# Define geometry

cmo / select / CMO_IN
geometry / create / stack_geom

# define surfaces for CMO_IN

cmo / select / CMO_IN
surface / s0 /reflect / sheet / mo0
surface / s1 /reflect / sheet / mo1
surface / s2 /reflect / sheet / mo2
surface / s3 /reflect / sheet / mo3
surface / s4 /reflect / sheet / mo4
surface / s5 /reflect / sheet / mo5
surface / s6 /reflect / sheet / mo6

# define geometry regions
# all internal interfaces are defined
# and are included once only

region / r_below / le s0
region / r1 / ge s0 and lt s1
region / r2 / ge s1 and lt s2
region / r3 / ge s2 and lt s3
region / r4 / ge s3 and lt s4
region / r5 / ge s4 and lt s5
region / r6 / ge s5 and lt s6
region / r_above / ge s6

# define material regions
# internal interfaces not included 

mregion / mr_below / lt s0
mregion / mr1 / gt s0 and lt s1
mregion / mr2 / gt s1 and lt s2
mregion / mr3 / gt s2 and lt s3
mregion / mr4 / gt s3 and lt s4
mregion / mr5 / gt s4 and lt s5
mregion / mr6 / gt s5 and lt s6
mregion / mr_above / gt s6

# Finished building geometry

```
Define geometry and material regions based on surface grids read into mesh objects named mo0, mo1, ... mo6.
The result are regions stacked in layers such that nodes found equal to surface s0 are the interface nodes between regions r1 and r2.
Nodes between surfaces s0 and s1 are in region r1 and have the  material labeled mr1. And so on.
This mesh geometry has the name stack_geom.


 
