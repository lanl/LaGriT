---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

 

     **INTERSECT\_ELEMENTS**

 

 The Purpose of this subroutine is to take the intersection of the two
 mesh objects and note in which elements of mesh object one the
 elements of mesh object two intersect. A new cell-based attribute is
 created with default name xsect\_cm. This attribute holds the number
 of times the sink element was intersected by the source element. A
 value of 0 indicates there was no intersection for that elem.



     FORMAT:

 ** intersect\_elements**/sink\_mo/source\_mo/
[attrib\_name
]

  
 Currently works on lines, tris, quads, or tets
