---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
title: 'cmo/copyatt'
---

 

cmo/copyatt
-----------

 The mo/copyatt** command is used to copy one attribute field to
 another. There is currently no provision for indexed sets.







FORMAT:

 mo/copyatt**/ cmo\_sink / cmo\_src / attnam\_sink / attnam\_src



cmo\_sink / cmo\_src : are the mesh object names to write to(sink) and
from (source). The two names can represent the same mesh object.



attnam\_sink / attnam\_src : are the mesh object attributes to write
to(sink) and from (source). If the two attributes differ in type or
length, a messege will be written. The routine does allow the values of
an element attribute to be written to the nodes of that belong to each
element. To create a mesh object where each element has its own unique
set of nodes, create parent-child chains for each element. This can be
done using commands mo/set\_id** and **settets**.







EXAMPLES:

 mo/copyatt** cmo1/cmo2/ itetclr / itetclr

 mo/copyatt** cmo1/cmo2/ itetclr
 Both versions will copy itetclr from cmo2 to cmo1
 

 mo/addatt** cmotet/elevation

 mo/copyatt** cmotet cmotet/ elevation zic
 In the mesh object cmotet, attribute zic is copied to attribute
 elevation
 

 mo/copyatt** cmotri cmotri/ itetsav itetclr

 mo/set\_id** cmotri/element/itetclr

 **settets**

 mo/copyatt** cmotri cmotri/ imt itetsav
 Copy element itetclr values into attribute itetsav. Create
 parent-child chains so each element has its own set of nodes. Copy the
 saved itetsav values into node attribute imt for each element. Each
 element will have nodes where the imt values match element itetsav
 values.
 

