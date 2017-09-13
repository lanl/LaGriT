---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: SETSIZE
---

 **RANKVOLUME**

  

  

  RANKVOLUME prints out the lowest volume elements from a mesh, ranked
  in increasing order.  The default is to print out the 100 lowest
  volume elements, but this number can be changed by specifying it as
  an optional second argument to the command. Also printed are the
  number of exterior boundary faces and number of interfaces faces for
  each of these elements.

 

FORMAT:

**rankvolume**/
[number\_of\_elements\_to\_rank
]

EXAMPLE:

 

rankvolume/10

elt. no.       volume     
#ext.bound.faces 
#int.bound.faces

    343660  0.105844E-05      0          1

    567342  0.105844E-05      0          0

    567266  0.105844E-05      0          1

    283659  0.105844E-05      0          0

    687334  0.105844E-05      0          0

    450784  0.105844E-05      0          0

    730990  0.105844E-05      0          0

    146725  0.105844E-05      0          0

    785111  0.105844E-05      0          0

    450711  0.105844E-05      0          0
