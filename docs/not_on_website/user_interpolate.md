---
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
Generator: Microsoft Word 98
title: 3
---

**   User\_interpolate**

 **user\_interpolate(**cmo\_sink,cmo\_src,cmolength,cname,nlist,list,ierror\_return)

  Supplying this subroutine and linking it in when building the
  executable allows the user to supply the interpolation formula for
  an attribute whose interpolations been set to **user.**

  

 ** ** cmo\_sink         is the SINK mesh object

   cmo\_src           is the SOURCE mesh object

   cmolength        is "nnodes" or "nelements"

   cname              is the name of the attribute

   nlist                  number of items to interpolate

   list(nlist)         the list of items for which new interpolated
 values are to be produced           

   ierror\_return    0 = no error

                           &gt;1 = error
