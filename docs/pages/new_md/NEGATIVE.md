---
GENERATOR: 'Mozilla/4. [en] (Win98; U) [Netscape]'
Generator: Microsoft Word 98
title: 'NEGATIVE\_AIJ'
---

 

 **NEGATIVE\_AIJ**

  This command tests all edges of all boundary faces for negative
  coupling coefficients.

It adds three attributes to the mesh object:

num\_neg\_coup\_coeff, type=INT, length=scalar -number of negative
coupling coefficients

neg\_coup\_coeff, type=VDOUBLE, length=num\_neg\_coup\_coeff - value of
coupling coefficient

ietet\_aij, type=VINT, length=num\_neg\_coup\_coeff,rank=vector -

for each negative coupling coefficient, i: ietet\_aij(1,i) contains the
tetrahedron number which contributes the most negative portion to the
coupling coefficient, ietet\_aij(2,i) contains the local face number
that contains the local edge (ietet\_aij(3,i)) which has the negative
coupling coefficient

These attributes can be used to generate a set of points to be added to
the mesh in an attempt to reduce the number of negative coupling
coefficients by the **refine** option. The points added are created by
projecting the fourth node of the tetrahedron onto the identified face
and then projecting this projection onto the identified edge. If the
final projected point lies between the end points of the identified
edge, this edge is refined. The identification and refinement steps may
be iterated up to maxiter times. Alternatively the attribute may be used
to create an **eltset** of the identified elements.

The **rivara** option uses a rivara refinement method to add nodes on
exterior boundary edges until all coupling coefficients are positive or
until a maximum number of iterations has been exceeded.

**FORMAT:**

**negative\_aij**

**negative\_aij** **/refine**

**negative\_aij** **/refine**/maxiter

**negative\_aij/eltset**/eltset\_name

**negative\_aij/rivara**

**EXAMPLES:**

**negative\_aij**

**negative\_aij/refine** only one iteration will be performed

**negative\_aij/refine/5** a maximum of 5 iterations will be performed

**negative\_aij** **/eltset**/bad\_tets an element set called bad\_tets
will be made no refinement will be performed/ **negative\_aij/rivara**

** ** 
[Click here for demos](demos/refine_rivara/test/md/main_rivara.md)
