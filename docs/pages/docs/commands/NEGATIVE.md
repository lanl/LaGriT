---
title: NEGATIVE_AIJ
tags: negative_aij, coefficients
---
 

# NEGATIVE_AIJ

---------------------

This command tests all edges of all boundary faces for negative coupling coefficients.
It adds three attributes to the mesh object:

**num_neg_coup_coeff** is the number of negative coupling coefficients

**neg_coup_coeff** is the value of coupling coefficients (type VDOUBLE  length = num_neg_coup_coeff ) 

**ietet_aij** is a vector with length = num_neg_coup_coeff.  for each negative coupling coefficient:
<br>
i: ietet_aij(1,i) contains the tetrahedron number which contributes the most negative portion to the coupling coefficient
<br>
ietet_aij(2,i) contains the local face number that contains the local edge 
<br>
(ietet_aij(3,i)) which has the negative coupling coefficient.
<br>

## SYNTAX

<pre>
<b>negative_aij</b>

<b>negative_aij/refine</b>

<b>negative_aij/refine</b>/maxiter

<b>negative_aij/eltset</b>/eltset_name

<b>negative_aij/rivara</b>
</pre>

The attributes  **num_neg_coup_coeff**, **neg_coup_coeff**, and **ietet_aij** can be used to generate a set of points to be added to
the mesh in an attempt to reduce the number of negative coupling coefficients by using the **`refine`** option. The points added are created by
projecting the fourth node of the tetrahedron onto the identified face and then projecting this projection onto the identified edge. If the
final projected point lies between the end points of the identified edge, this edge is refined. The identification and refinement steps may
be iterated up to `maxiter` times. Alternatively the attribute may be used to create an **`eltset`** of the identified elements.

The **`rivara`** option uses a rivara refinement method to add nodes on exterior boundary edges until all coupling coefficients are positive or until a maximum number of iterations has been exceeded.



## EXAMPLES

```
negative_aij

negative_aij/refine 
```

for both examples, only one iteration will be performed

```
negative_aij/refine/ 5
```
a maximum of 5 iterations will be performed

```
negative_aij/eltset/ bad_tets 
```
an element set called bad_tets will be created, no refinement will be performed

```
negative_aij/rivara
```
refinement method to add nodes on exterior boundary edges


[Click here for demo of the command above](../demos/main_rivara.md)
