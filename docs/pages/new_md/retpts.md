---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
---

 

**  Retrieving Point Sets and Element Sets**

 

 **eltlimc** returns an array of element numbers where the elements
 belong to the **eltset** given in the argument list. **** Eltsets must
 be specified by name. On return the array pointed to by ipmpary will
 contain the mpno element numbers that belong to the eltset.

  **eltlimc**(ich1,ich2,ich3,ipmary,mpno,ntets,xtetwd)
 
   ich1,ich2,ich3       **eset,get,**eltset\_name (character
*32)

   ipmpary                  pointer to array of elements of
   eltset\_name that is filled on output

   mpno                      integer number of elements in
   eltset\_name (output)

   ntets                        integer number of elements in mesh
   object (input)

   xtetwd                    array of eltset membership information
   (input) see [See III.a](meshobject.md)

 **pntlimc,pntlimn** return an array of node numbers where the nodes
 belong to the **pset** given in the argument list. On return the array
 pointed to by ipmpary will contain mpno node numbers. These numbers
 are the nodes that belong to the **pset**.

  **pntlimc**(ich1,ich2,ich3,ipmary,mpno,npoints,isetwd,itp1)

ich1,ich2,ich3     **pset,get,**pset\_name (character
*32)

ipmpary                pointer to array of node number of pset\_name
that is filled on output

mpno                     integer number of nodes in pset\_name (output)

npoints                  integer number of nodes in mesh object (input)

isetwd                   array of pset membership information [See
III.a](meshobject.md)

itp1                       array of point types [See
III.a](meshobject.md)



**pntlimn**(ifirst,ilast,istride,ipmary,mpno,npoints,isetwd,itp1)

ifirst,ilast,istride  point range: first node, last node, stride between
nodes (integers)

ipmpary                pointer to array of node number of pset\_name
that is filled on output

mpno                     integer number of nodes in pset\_name (output)

npoints                  integer number of nodes in mesh object (input)

isetwd                   array of pset membership information [See
III.a](meshobject.md)

itp1                       array of point types [See
III.a](meshobject.md)
