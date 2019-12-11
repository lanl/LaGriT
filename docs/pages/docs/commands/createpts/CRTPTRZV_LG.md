---
title: CREATEPTS/VECTOR
tags: createpts vector
---


# CREATEPTS/VECTOR

-----------------------------



This routine is used to ratio zone the region of space spanned by the input number n(i) of copies of the input vector v(ij) away from
  the initial point v(0j) using the desired coordinate system. The j-th component of the i-th vector V(ij) is reduced by r(ij) at each
  step in the i-th direction away from the initial point. No attempt is made to insure that the 3 vectors are independent.

This command replaces [RZV](../RZV_LG.md).


## SYNTAX

<pre>
<b>ceatepts/vector</b>/[<b> xyz rtz rtp </b> /          &
                [ n1,n2,n3                              &
                 /v11,v12,v13/v21,v22,v23/v31,v32,v33   &
                  /v01,v02,v03                          &
                 /r11,r12,r13/r21,r22,r23/r31,r32,r33 ]
</pre>

default = 0:      n(i), v(i), v(0j)

default = 1:      r(ij)

 
<hr>

## EXAMPLES


```
createpts/vector/rtz/n1,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9
```
spiral of points


```
createpts/vector/xyz/n1,n2,n3/1,0,0/0,1,0/0,0,1

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
```
simple cubic point distribution, both lines have the same result


```
createpts/vector/xyz/n1,n2,n3/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1  ,n2  ,n3  /0,0,0/n1,n2,n3/0,0,0
```
body centered cubic point distribution, compare with the **rz** two command sequence with different bounding box.



```
createpts/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1
rz/xyz/n1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0
rz/xyz/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0
```
face centered cubic point distribution compare with the four **rz** command sequence (different bounding box).


```
createpts/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1
```
hexagonal lattice of points in x,y plane, repeated in z direction


```
createpts/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5
createpts/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/.25,.25,.25

rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
rz/xyz/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1
rz/xyz/n1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0
rz/xyz/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0
rz/xyz/n1+1,n2+1,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,1,1
rz/xyz/n1  ,n2  ,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,0,1
rz/xyz/n1  ,n2+1,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,1,0
rz/xyz/n1+1,n2  ,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,0,0
```
diamond point distribution (two command sequence) compare the eight **rz** command sequence (different bounding box).

```
createpts/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/
createpts/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/.5,0.2,.5
```
hexagonal close pack point distribution  (two command sequence)


CAVEATS -

      
* [filter](../FILTER.md) should be used afterwards to remove possibly duplicate points
      
* this can create some really bizzare point distributions
      
* mistyped input after "rzv/[cgeom]" always returns successful point addition,  but may be very different than desired
