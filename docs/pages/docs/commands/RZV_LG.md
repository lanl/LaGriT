---
GENERATOR: 'Mozilla/4.75 
[en
] (X11; U; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: RZS
---

 

 **RZV**

  This routine is used to ratio zone the region of space spanned by
  the input number n~i~ of copies of the input vector v~ij~ away from
  the initial point v~0j~ using the desired coordinate system. No
  attempt is made to insure that the 3 vectors are independent.

  For ratio\_method = component (default), the j-th component of the
  i-th vector v~ij~ is reduced by  r~ij ~ after the k~i~ -th

  step in the i-th direction away from the initial point.  For this
  ratio\_method the ratio flags f~i~ are not used.  In this case an
  initial step of 1 for the j-th component of the i-th direction would
  become, for r~ij ~ =  1/2, a step of the j-th component of the i-th
  direction of 1/2 at k~i~ =  1, 1/4 at  k~i~ =  2, 1/8 at  k~i~ =  3,
  1/16 at k~i~ =  4,etc.

  For ratio\_method = vector and fj =1 (the default), the j-th vecor
  is reduced by r~ij ~ after the k~i~ -th step in the i-th direction. 
  In this case an initial step of 1 in the j-th direction would
  become, for  r~ij ~ =  1/2, a setp in the j-th direction of 1/2 at
  k~i~ =  1, 1/4 at  k~i~ =  2, 1/8 at  k~i~ =  3, 1/16 at k~i~ = 
  4,etc.

  For ratio\_method = vector and fj =0, the j-th vecor is reduced by
  
[1 - (1-r~ij ~ )
*2/(k~i~ +  1)
] after the k~i~ -th step in the
  i-th direction.  In this case an initial step of 1 in the j-th
  direction would become, for  r~ij ~ =  1/2, a step in the j-th
  direction of 1/2 at k~i~ =  1, 1/3 at  k~i~ =  2, 1/4 at  k~i~ =  3,
  1/5 at k~i~ =  4,etc.

 **FORMAT:**

**rzv/xyz****rtz****rtp** /

                
[ n1,n2,n3

                 /v11,v12,v13/v21,v22,v23/v31,v32,v33

                  /v01,v02,v03

                 /r11,r12,r13/r21,r22,r23/r31,r32,r33

                 /omponent****vector**

                 /f1,f2,f3
]

default = **xyz**

default = 0:      n~i~, v~i~, v~0j~

default = 1:      r~ij~

default = omponent**

**EXAMPLES:**

spiral of points

**rzv/rtz**/n1,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9

sc (simple cubic) point distribution

**rzv/xyz**/n1,n2,n3/1,0,0/0,1,0/0,0,1

        same as

        **rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

bcc (body centered cubic) point distribution

**rzv/xyz**/n1,n2,n3/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/

compare the two command sequence (different bounding box)

**rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

**rz/xyz**/n1  ,n2  ,n3  /0,0,0/n1,n2,n3/0,0,0

fcc (face centered cubic) point distribution

**rzv/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/

compare the four command sequence (different bounding box)

**rz/xyz**/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1

**rz/xyz**/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1

**rz/xyz/n**1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0

**rz/xyz**/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0

hexagonal lattice of points in x,y plane, repeated in z direction

**rzv/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1

diamond point distribution (two command sequence)

**rzv/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5

          **rzv/xyz**/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/.25,.25,.25

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

**rzv/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/

**rzv/xyz**/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/.5,0.289,.5

nice 2-d distribution of points in a circle of radius 1

**rzv/xyz**/10,60,0/0.1,0,0/0,60,0/0,0,1/0,0,0/1,0.5,1/1,1,1/1,1,1**/vector**/0,0,0



   CAVEATS -

      
* filter should be used afterwards to remove possibly duplicate
points

      
* this can create some really bizzare point distributions

      
* mistyped input after "rzv/
[cgeom
]" always returns successful
point addition,

      but may be very different than desired

      
* ratio\_flag might better be a scalar or a matix, and its use
might want to be extended to ratio\_method=component.
