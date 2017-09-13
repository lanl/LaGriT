---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

mo/constraint**

         mo/constraint**/cmo\_sink/cmo\_src

 Associate the surface constraint information of the mesh object
 cmo\_src with cmo\_sink.  The number of constraints and the constraint
 table ( nconbnd, icontrab) are copied from the source mesh object to
 the sink mesh object.  Since nconbnd and icontab are mesh object
 attributes, they must be explicity copied if a new mesh is to use an
 existing geometry.  [See III.A](../../meshobject.md)  for a
 description of nconbnd and incontab.



    EXAMPLES:

 mo/create**/cmotet

 **geometry/create**/boxg

 **surface/s1**/

 **surface/s2**

 **region**

 ** mregion**

 ...

 mo/create/mohex**///hex**

 mo/geometry**/boxg

 mo/constraint/mohex/cmotet
 This example will use the geometry (surfaces regions and material
 regions) defined when creating cmotest and apply the geometry to the
 mesh object cmohex.
