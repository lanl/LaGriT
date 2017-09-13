---
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
Generator: Microsoft Word 98
title: QUADXYZ
---

 

 **QUADXYZ**

  Define an arbitrary, logical hexahedron of points in 3D(xyz) space.
  nx specifies the number of points between the 1st and 2nd point.  ny
  specifies the number of points between the 1st and 4th point and nz
  specifies the number of points between the 1st and 5th point.  The
  eight corners of the hex are then listed as two sets of quads, each
  set of four nodes is given in counter clockwise order . Points 1 to
  4 specify one face of the hex, points 5 to 8 the corresponding face
  opposite (point 5 is logically behind point 1, point 6 behind point
  2 and so on.)

FORMAT:

**quadxyz**/nx,ny,nz/x1

EXAMPLES:

quadxyz/5 25 7/ 0. 0. 0. /1. 0. 0. /1. .0 10. /0. .0 10. /0. 2.0 0. /1.
2. 0. &

         /1.0 2. 10.0 /0. 2. 10.0 /

quadxyz/5 25 7/ 0. 0. 0. /1. 0. 0. /1. 2. 0. /0. 2. 0. /&

0
. 0. 10. /1. 0. 10. /1. 2. 10. /0. 2. 10. /
