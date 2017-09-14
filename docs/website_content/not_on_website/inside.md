---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: 
    The inside set of subroutines test whether a query point is strictly
    inside, strictly outside or on the surface of the specified element
---

 

 **INSIDE**

  The **inside** set of subroutines that test whether a query point is
  strictly inside, strictly outside or on the surface of the specified
  element. The value returned in iflag is 0 if the query point is
  inside the element, -1 if outside, or is set to the local face
  number containing the query point. Coordinates of the query point
  are in xq, yq, zq. Coordinates of the vertices of the element are in
  x1, y1, z1, x2..... The coordinates of the element must be specified
  in the correct order (see the section on Mesh Object Connectivity).

   

  **inside\_pyr**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)
 
  **inside\_pri**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,x6,y6,z6xq,yq,zq,iflag)
 
  **inside\_hex**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
 
  xq,yq,zq,iflag)
 
  **inside\_te**t(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)
 
  **inside\_quad2d**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)
 
  **inside\_tri2d**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)

   

 These routines should not be confused with

  **inside\_quad**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)
 
  **inside\_tri**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)
 
  which return in iflag: 1 if the query point is on the plane of the
  element, 0 if below the plane and -1 if above.
