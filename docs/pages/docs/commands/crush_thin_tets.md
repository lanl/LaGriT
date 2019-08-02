---
title: crush_thin_tets
---

# CRUSH_THIN_TETS

This algorithm can be used to reduce sliver shaped tetrahedrals from a mesh.

## SYNTAX

<pre>
<b>crush_thin_tets</b>/ cmo_name / [ TOLCRUSH ] / [pset,get,psetname]
</pre>

*`TOLCRUSH`* is the optional value providing a measurement selection to crush, the default value is .1

**pset,get,** *`psetname`* is the optional node selection for part of the mesh.



**`crush_thin_tets`** loops thru tets of a volume mesh
and looks for elements that, relative to the characteristic
length established by the normalized root mean square length
of the edges, are thinner than TOLCRUSH. 

In the case that thinness is defined as the minimum tet altitude, then this relative length
is actually an aspect ratio, because it goes to zero if
the tet is degenerate and has a maximal value of 1 for a regular tet.  

Given the relative length scale, we can measure thinness
not just of the altitudes, but of any distance measurement in the tet.
In particular there are four type of thin situations
possible for a bad tet and we take appropriate actions for each case:

1) If an element has relative edge length shorter than TOLCRUSH, it is merged.

2) If a normal point-to-edge projection has relative length < TOLCRUSH,
    a node is added to the edge, so that a type (1) merge can take place.

3) If a normal point-to-face projection has relative length < TOLCRUSH,
     a node is added to the face, so that a type (1) merge can take place.

4) If a normal mutual diagonal-to-diagonal projection has relative length < TOLCRUSH, 
    a node is added to one diagonal, so that a type (2) situation is created
    (which leads to another refinement and a type (1) merge).

Since there are situations when a type (1) action would be barely
rejected on tolerance, but the more complicated type (2) action
would be barely accepted on tolerance, we loosen the tolerance
slightly for type (1) actions relative to type (2) actions to avoid this.
Similarly, there are situations when a type (2) action would be
barely rejected on tolerance, but the more complicated type (3)
or type (4) actions would be barely accepted on tolerance.
To avoid this we loosen the tolerance of type (2) actions relative
to type (3) and type(4) actions.

Currently we are using these effective tolerances (on relative length):
- type 1 : 1.2*TOLCRUSH
- type 2 : 1.1*TOLCRUSH
- type 3 : TOLCRUSH
- type 4 : TOLCRUSH


## EXAMPLES

```
crush_thin_tets / cmotet / 0.05
rmpoint / compress
resetpts / itp

crush_thin_tets / cmotet
rmpoint / compress
resetpts / itp


```
