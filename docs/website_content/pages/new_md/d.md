---
Generator: Microsoft Word 98
title: d
---

** ****

d
. Mesh Object Connectivity

The Mesh Object attributes, **itettyp**, **itetoff**, **jtetoff**,
**itet**, and **jtet** along with the variables contained in the include
file **local\_element.h** completely describe the mesh connectivity. The
following discussion is based on the concept of local facets and local
edges for an element. The nodes comprising a given element are always
specified in a well-defined order; hence when one references the 'second
facet' of an element, one references a pre-defined set of points.
Consider a tetrahedral element, with nodes labeled as in the diagram:

<img height="300" width="300" src="Image232.gif">"196" "188"

The points are oriented so that the triple product
<img height="300" width="300" src="Image233.gif">"70" "15" is positive, and the volume of
the tet is one-sixth of the triple product. The local facets are defined
as follows:

  ---- --- ---- ---- ---- ---
  F1   =   I2   I3   I4    
  F2   =   I1   I4   I3    
  F3   =   I1   I2   I4    
  F4   =   I1   I3   I2    
  ---- --- ---- ---- ---- ---

The local edges for a tetrahedral are defined as follows:

E1 I1 I2

E2 I1 I3

E3 I1 I4

E4 I2 I3

E5 I2 I4

E6 I3 I4

Similarly, local facets and local edges are defined for all element
types.

**itettyp(it)** gives the element type of element **it.**

** **

itetoff(it) gives the offset to the first node in element **it.**

**itet(itetoff(it)+j)** gives the jth node of element it.

**nelmnen(itettyp(it))** gives the number of nodes of element it.

To loop through all the nodes of all elements in the mesh:

do it=1,ntets

do j=1,nelmnen(itettyp(it))

k=itet(itetoff(it)+j)

enddo

enddo

**nelmnef(itettyp(it))** gives the number of facets of element it.

**ielmface0(iface,itettyp(it))** gives the number of nodes on facet
**iface** of element it.

**ielmface1(local\_node,iface,itettyp(it))** gives the increment to the
node number (**local\_node**) on facet **iface** of element it.

To loop through all the nodes, **k,** of all elements in the mesh by
facets:

do it=1,ntets

do i=1,nelmnef(itettyp(it))

do j=1,ielmface0(i,itettyp(it))

k=itet(itetoff(it)+

ielmface1(j,i,itettyp(it)))

enddo

enddo

enddo

**nelmnee(itettyp(it))** gives the number of edges of element it.

**ielmface2(inode,iface,itettyp(it))** gives the edge number associated
with inode on facet **iface** of element it.

**ielmedge1(12,iedge,itettyp(it))** gives the node offset associated
with edge **iedge** of element it.

To loop through all pairs of edge nodes (**i1,i2**) of all elements in
the mesh :

do it=1,ntets

do i=1,nelmnee(itettyp(it))

i1=itet(itetoff(it)+

ielmedge1(1,i,itettyp(it)))

i2=itet(itetoff(it)+

ielmedge1(2,i,itettyp(it))

enddo

enddo

To loop through all pairs of edge nodes (**i1,i2**) of all elements in
the mesh by facets:

do it=1,ntets

do i=1,nelmnef(itettyp(it))

do j=1,ielmface0(i,itettyp(it))

ie=ielmface2(j,i,itettype(it))

i1=itet(itetoff(it)+

ielmedge1(1,ie,itettyp(it)))

i2=itet(itetoff(it)+

ielmedge1(2,ie,itettyp(it)))

enddo

enddo

enddo

** **

jtet(itetoff(it)+j) gives the element number and local facet number of
the neighbor to element **it**, facet j.

To loop to find all neighbors of elements ( **jt** is neighbor element
number, **jf** is facet of neighboring element) (if **jt** is a neighbor
of element **i**t and local face **if** of **it** is the same as local
face **jf** of **jt** then **jtet** (**jtetoff**(**it**) + **if**) =
**nef\_cmo** 
* (**jt**-1)+**jf**, where **nef\_cmo** is the number of
faces per element. Similarily, **jtet** (**jtetoff**(**jt**) + **jf**)=
**nef\_cmo** 
* (**it** -1) + **if**.) (**mbndry** is the value added to
**jtet** if element **it** is on a boundary or interface; the **jtet**
value of an element **it** with facet **j** on an exterior boundary will
be exactly **mbndry**; ** ** the **jtet** value of an element **it** with
facet **j** on an interior interface will be **mbndry** + the **jtet**
value calculated from the neighboring element number and neighbor
element local\_facet number):

c get number of faces per element for this mesh object

call cmo\_get\_info(‘faces\_per\_element’,cmo\_name,

nef\_cmo,ilen,ity,ics)

do it=1,ntets

do i=1,nelmnef(itettyp(it))

c check if element face is on an external boundry

if(jtet(jtetoff(it)+i).eq.mbndry) then

jt=0

jf=0

c check if element face is on an internal boundry

elseif(jtet(jtetoff(it)+i).gt.mbndry) then

jt=1+(jtet(jtetoff(it)+i)-mbndry-1)/nef\_cmo

jf=jtet(jtetoff(it)+i)-mbndry-nef\_cmo
*(jt-1)

C Volume element

else

jt=1+(jtet(jtetoff(it)+i)-1)/nef\_cmo

jf=jtet(jtetoff(it)+i)-nef\_cmo
*(jt-1)

endif

enddo

enddo

** **
