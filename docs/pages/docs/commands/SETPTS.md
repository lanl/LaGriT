---
title: SETPTS
tags: setpts, imt
---

# SETPTS

-----------------


Set point types and **imt** material by calling **surfset** and **regset** routines. 

Assumes mesh object has geometry [REGION](REGION.md) and material [MREGION](MREGION.md) defined by [SURFACE](SURFACE.md) commands. 


The node material **imt** attribute is set based on the previously defined **mregion** commands. 
Interior and external boundary nodes should be assigned to exactly one **mregion**;
these nodes will be assigned an **imt** value that corresponds to the **mregion.** 

Node types are assigned based on whether a node is on a surface and what type the surface(s) is.
A constraint table is generated and **icr** updated if constraint type surfaces are defined. 


## SYNTAX

<pre>
<b>setpts</b>

<b>setpts/no_interface</b>

<b>setpts/closed_surfaces/reflect</b>
</pre>

**`setpts`**

With no arguments, this command sets point types and **imt** materials by previously defined regions.  
Nodes will be assigned an **imt** value that corresponds to the **mregion.** A node which is on an interface will be assigned to any
**mregion** and will be given an **imt** value of "**interface**" (an integer equal to one more than the number of material regions.) 

Node types are assigned based on whether a node is on a surface and what type the surface(s) is.

This command must be executed before **connect.**


**`setpts`** / **`no_interface`** 

This allows one to set the **imt** values of nodes without getting any nodes labeled **interface**. 
This is useful if you do not want **settets** to create parent child chains at interface points. 
The actual **imt** value given may be determined by roundoff error so should not be used in cases
where there are a large number of interface points. 

This is useful for setting **imt** values of an rzbrick mesh in which interface points only occur due to the coincidental point very near the geometry defining surface.

**`setpts`** / **`closed_surfaces/reflect`** 

This  option works with geometries in which all **regions** and **mregions** are defined by closed surfaces. 
The nodes that fall on more than one surface are labeled as interface nodes. 
Nodes which fall on exactly one surface are labeled external reflective boundary nodes. 

The **resetpts/itp** must be called to correct the point types for external boundaries. 


<hr>

## EXAMPLES

```
# define surfaces and regions
cmo/create/mo_tet
surface/s1/reflect/box/0,0,0/1,1,1
surface/p1/intrface/plane/0,0,.5/1,0,.5/1,1,.5
surface/p2/intrface/plane/.5,0,0/.5,1,0/.5,1,1
surface/p3/intrface/plane/.75,0,0/.75,1,0/.75,1,1
region/r0/ le s1 and ge p1 and gt p3 /
mregion/m0/ le s1 and gt p1 and gt p3 /
region/r1/ le s1 and ge p1 and le p3 /
mregion/m1/ le s1 and gt p1 and lt p3 /
region/r2/ le s1 and le p1 and ge p2 /
mregion/m2/ le s1 and lt p1 and gt p2 /
region/r3/ le s1 and le p1 and lt p2 /
mregion/m4/ le s1 and lt p1 and lt p2 /

# create point distribution
createpts/xyz/17,17,17/0,0,0/1,1,1/1,1,1

# set point types and materials
setpts

# connect into tet mesh
connect

# set element types and materials
settets
resetpts/itp
```
In general, it is good practice to use filter to remove duplicate points, setpts to set point attributes, settets to set elements, and a final resetpts/itp to set boundary.



```
cmo/create/s1///tri
read/avs/surfaces
cmo/create/3dmesh
surface/surface1/intrcons/sheet/s1
copyts/3dmesh/s1/
cmo/release/s1/
cmo/create/s2///tri/
read/avs/surf2.avs
surface/surface2/intrcons/sheet/s2
copypts/3dmesh/s2
cmo/release/s2/

region/r1/le surface1
region/r2/le surface2
mregion/mr1/lt surface1
mregion/mr2/lt surface2

setpts/closed_surfaces/reflect

connect
settets
resetpts/itp
```
example using the closed_surfaces command
