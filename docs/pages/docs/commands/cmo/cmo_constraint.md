---
title: "cmo/constraint"
tags: cmo constraint
---

# cmo/constraint

Associate the surface constraint information of the mesh object
 cmo_src with cmo_sink.  The number of constraints and the constraint
 table ( nconbnd, icontrab) are copied from the source mesh object to
 the sink mesh object.  
 
 Since **nconbnd** and **icontab** are mesh object
 attributes, they must be explicity copied if a new mesh is to use an existing geometry.
 See [Mesh Object](../../meshobject.md)  for descriptions.


## SYNTAX

<pre>
<b>cmo/constraint</b>/ cmo_sink/ cmo_src
</pre>
        

 
## EXAMPLES

```
cmo/create/cmotet 
geometry/create/boxg 
surface/s1/ 
surface/s2 
region 
mregion 
... 
cmo/create/cmohex///hex 
cmo/geometry/boxg 
cmo/constraint/cmohex/cmotet
```
This example will use the geometry (surfaces regions and material regions) defined when creating cmotest and apply the geometry to the mesh object cmohex.
