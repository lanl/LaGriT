---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: c
---

 

 **c. FORTRAN Interface**

Mesh Object attribute data are accessed through a set of subroutines. An
example of accessing an existing Mesh Object and creating a new mesh
object is given in [Section IV.d](accessing.md); that example should
be used as a template when operating with Mesh Objects. The subroutine
set includes:

[cmo\_get\_name](meshob.md#cmo_get_name) retrieve active mesh object
name

[cmo\_set\_name](meshob.md#cmo_set_name) set active mesh object name

[cmo\_get\_info](meshob.md#cmo_get_info)retrieve mesh object pointer

[cmo\_get\_intinfo](meshob.md#cmo_get_intinfo) retrieve mesh object
integer data

[cmo\_get\_attinfo](meshob.md#cmo_get_attinfo) retrieve mesh real or
character data

[cmo\_set\_info](meshob.md#cmo_set_info) set mesh object integer data

[cmo\_set\_attinfo](meshob.md#cmo_set_attinfo) set mesh real or
character data

[cmo\_newlen](meshob.md#cmo_newlen) adjust the lengths of mesh object
data based on the values of number of nodes and number of elements which
are stored as integer mesh object attributes (nnodes, nelements)

Only data from the activeMesh Objectmay be retrieved; calling
**cmo\_set\_name** will make the referenced Mesh Object active. Scalar
quantities are retrieved and stored using **cmo\_get\_intinfo**,
**cmo\_get\_attinfo**,  and **cmo\_set\_info**. Vector quantities are
referred to by their pointers. The length of the vectors is calculated
internal to LaGriT based on the values of the scalar mesh object
attributes. Memory allocation for a new mesh object or for a mesh object
which will grow in size is accommodated by first setting the appropriate
scalars for the Mesh Object by using  **cmo\_set\_intinfo** with the new
number of elements and/or nodes and then calling**cmo\_newlen**. These
two steps must be taken before adding to the size of a Mesh Object.

Mesh object parameters are retrieved with the
[cmo\_get\_attparam](meshob.md#cmo_get_attparam) subroutine.

See [IV.e.2](meshob.md) for a list of  mesh object subroutines .
