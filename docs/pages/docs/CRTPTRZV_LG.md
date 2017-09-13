---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: RZS
---

 

 **CREATEPTS/VECTOR**

  This routine is used to ratio zone the region of space spanned by
  the input number n~i~ of copies of the input vector v~ij~ away from
  the initial point v~0j~ using the desired coordinate system. The
  j-th component of the i-th vector V~ij~ is reduced by r~ij~ at each
  step in the i-th direction away from the initial point. No attempt
  is made to insure that the 3 vectors are independent.

 **FORMAT:**

**createpts/vector**/
[ xyzrtzrtp /

                
[ n1,n2,n3

                 /v11,v12,v13/v21,v22,v23/v31,v32,v33

                  /v01,v02,v03

                 /r11,r12,r13/r21,r22,r23/r31,r32,r33 
]

default = 0:      n~i~, v~i~, v~0j~

default = 1:      r~ij~

 

**EXAMPLES:**

spiral of points

**createpts/vector/rtz**/n1,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9

sc (simple cubic) point distribution

**createpts/vector/xyz**/n1,n2,n3/1,0,0/0,1,0/0,0,1

        same as

       **rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

bcc (body centered cubic) point distribution

**createpts/vector/xyz**/n1,n2,n3/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/

compare the two command sequence (different bounding box)

**rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

**rz/xyz**/n1  ,n2  ,n3  /0,0,0/n1,n2,n3/0,0,0

fcc (face centered cubic) point distribution

**createpts/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/

compare the four command sequence (different bounding box)

**rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

**rz/xyz**/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1

**rz/xyz/n**1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0

**rz/xyz**/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0

hexagonal lattice of points in x,y plane, repeated in z direction

**createpts/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1

diamond point distribution (two command sequence)

**createpts/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5

 **createpts/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/.25,.25,.25

compare the eight command sequence (different bounding box)

**rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

          **rz/xyz**/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1

       **rz/xyz**/n1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0

       **rz/xyz**/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0

**rz/xyz**/n1+1,n2+1,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,1,1

**rz/xyz**/n1  ,n2  ,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,0,1

**rz/xyz**/n1  ,n2+1,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,1,0

**rz/xyz**/n1+1,n2  ,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,0,0

 hcp (hexagonal close pack) point distribution  (two command sequence)

**createpts/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/

**createpts/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/.5,0.289,.5

   CAVEATS -

      
* filter should be used afterwards to remove possibly duplicate
points

      
* this can create some really bizzare point distributions

      
* mistyped input after "rzv/
[cgeom
]" always returns successful
point addition,

      but may be very different than desired
