---
title: RZS
tags: rzs
---

# RZV

-------------------

This command is deprecrated, see [CREATEPTS](createpts.md).


This routine is used to ratio zone the region of space spanned by the input number n(i) of copies of the input vector v(ij) away from
  the initial point v(0j) using the desired coordinate system. The j-th component of the i-th vector V(ij) is reduced by r(ij) at each
  step in the i-th direction away from the initial point. No attempt is made to insure that the 3 vectors are independent.

## SYNTAX

<pre>
<b>rzv</b>/[<b> xyz rtz rtp </b> /          &
  [ n1,n2,n3                              &
  /v11,v12,v13/v21,v22,v23/v31,v32,v33   &
  /v01,v02,v03                          &
  /r11,r12,r13/r21,r22,r23/r31,r32,r33 ] &
 / <b>component</b> or <b>vector</b> &
 / [f1,f2,f3]
</pre>


For `ratio_method` = **component** (default), the j-th component of the
  i-th vector vij is reduced by  rij after the ki-th
  step in the i-th direction away from the initial point.  For this
  ratio_method the ratio flags `f1,f2,f3` are not used.  In this case an
  initial step of 1 for the j-th component of the i-th direction would
  become, for rij  =  1/2, a step of the j-th component of the i-th
  direction of 1/2 at ki =  1, 1/4 at  ki =  2, 1/8 at  ki =  3, 1/16 at ki =  4,etc.


For `ratio_method` = **vector** and `fj` =1 (the default), the j-th vecor
  is reduced by rij  after the ki -th step in the i-th direction. 
  In this case an initial step of 1 in the j-th direction would
  become, for  rij  =  1/2, a setp in the j-th direction of 1/2 at
  ki =  1, 1/4 at  ki =  2, 1/8 at  ki =  3, 1/16 at ki =  4,etc.

For `ratio_method` = **vector** and `fj` =0, the j-th vecor is reduced by
  [1 - (1-rij  ) *2/(ki +  1)] after the ki -th step in the i-th direction.  In this case an initial step of 1 in the j-th
  direction would become, for  rij  =  1/2, a step in the j-th direction of 1/2 at ki =  1, 1/3 at  ki =  2, 1/4 at  ki =  3, 1/5 at ki =  4,etc.


default = **xyz**

default = 0:      ni, vi, v0j

default = 1:      rij

default = component

<hr>

## EXAMPLES


```
rzv/rtz/n1,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9
```
spiral of points


```
rzv/xyz/n1,n2,n3/1,0,0/0,1,0/0,0,1

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
```
simple cubic point distribution, both lines have the same result


```
rzv/xyz/n1,n2,n3/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1  ,n2  ,n3  /0,0,0/n1,n2,n3/0,0,0
```
body centered cubic point distribution, compare with the **rz** two command sequence with different bounding box.



```
rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1?|  ,n2?|  ,n3+1/0,0,0/n1,n2,n3/0,0,1
rz/xyz/n1?|  ,n2+1,n3?|  /0,0,0/n1,n2,n3/0,1,0
rz/xyz/n1+1,n2?|  ,n3?|  /0,0,0/n1,n2,n3/1,0,0
```
face centered cubic point distribution compare with the four **rz** command sequence (different bounding box).


```
rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1
```
hexagonal lattice of points in x,y plane, repeated in z direction


```
rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5     
rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/.25,.25,.25

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1?|  ,n2?|  ,n3+1/0,0,0/n1,n2,n3/0,0,1
rz/xyz/n1?|  ,n2+1,n3?|  /0,0,0/n1,n2,n3/0,1,0
rz/xyz/n1+1,n2?|  ,n3?|  /0,0,0/n1,n2,n3/1,0,0
rz/xyz/n1+1,n2+1,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,1,1
rz/xyz/n1?|  ,n2?|  ,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,0,1
rz/xyz/n1?|  ,n2+1,n3?|  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,1,0
rz/xyz/n1+1,n2?|  ,n3?|  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,0,0
```
diamond point distribution (two command sequence) compare the eight **rz** command sequence (different bounding box).


```
rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/
rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/.5,0.2,.5
```
hexagonal close pack point distribution?|  (two command sequence)




```
rzv/xyz/10,60,0/0.1,0,0/0,60,0/0,0,1/0,0,0/1,0.5,1/1,1,1/1,1,1/vector/0,0,0
```
nice 2-d distribution of points in a circle of radius 1


**CAVEATS:**:
      
  * filter should be used afterwards to remove possibly duplicate
  points
  * this can create some really bizzare point distributions
  * mistyped input after "rzv/[cgeom]" always returns successful
  point addition, but may be very different than desired
  * ratio\_flag might better be a scalar or a matix, and its use might want to be extended to ratio\_method=component.


