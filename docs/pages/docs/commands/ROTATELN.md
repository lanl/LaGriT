---
title: ROTATELN
tags: ok
---

 **ROTATELN**

  Rotates a point distribution (specified by ifirst,ilast,istride)
  about a line. The **copy** option allows the user to make a copy of
  the original points as well as the rotated points, while **nocopy**
  just keeps the rotated points themselves. The line of rotation
  defined by x1 through z2 needs to be defined such that the endpoints
  extend beyond the point distribution being rotated. theta (in
  degrees) is the angle of rotation whose positive direction is
  determined by the right-hand-rule, that is, if the thumb of your
  right hand points in the direction of the line (1 to 2), then your
  fingers will curl in the direction of rotation. xcen,ycen,zcen is
  the point where the line can be shifted to before rotation takes
  place.

  If the **copy** option is chosen, the new points will have only
  coordinate values (**xic, yic**, **zic**); no values will be set for
  any other mesh object attribute for these points.
  
 ** Note: The end points of the line segment must extend well beyond the point set being rotated.**

**FORMAT:**

**rotateln** /ifirst,ilast,istride/ [**no**] **copy** /
x1,y1,z1/x2,y2,z2/theta/xcen,ycen,zcen/

**EXAMPLE:**

input.cylrot use rotateln and trans to move cylinder
create a cylinder centered around x=.5,z=.5, radius = .1
the cylinder is aligned parallel to the y-axis.
inside a box of width =2 , length=2 ,1
the regions are air for the cylinder - solid outside the cyl.
points are spread by surrounding the whole object with
a cylinder shell of points and then creating rays between
these points and the major axis of the cylinder.  Points
are distributed along these rays inside the cylindrical region.
a background rectangular grid of points is spread outside the
cylinder.

     cmo/create/3dmesh
     surface/box1/reflect /box/-1.0,-1.0,0.0/ 1.0, 1.0, 1.0/
     surface/h1/intrface /cylinder/ 0.5, -1.,0.5/ 0.5, 1.0, 0.5/.1/
     region/H1/ le box1 and le h1 /
     region/Fill/ le box1 and gt h1 /
     mregion/Air/ le box1 and lt h1 /
     mregion/Solid/Fill
     createpts/xyz/11,11,1/-1.,-1.,1.1/1.0,1.0,1.1/,1,1,0/
     pset/rays/seq/1,0,0/
     regnpts/Fill/11/pset,get,rays/xyz/ 0.0,0.0,-0.1/1.0,1.0,-0.1/ 
     0.0,1.0,-0.1/0,0/

 
the rz command always distributes points with the z-axis as
the axis of symmetry
use the rotateln and trans commands to move the point
distribution after it is created.

     createpts/rtz/1,13,11/5.,0.,-1./5.,360.,1./0,1,1/0,0,0/
     pset/ray1/seq/0,0,0/
     rotateln/pset,get,ray1/nocopy/-100.,0.,0./100.,0.,0./-90./0.,0.,0./
     trans/pset,get,ray1/0.,0.,0./0.5,0.,0.5/
     regnpts/H1/3/pset,get,ray1/rtz/0.5,-1.1,0.5/0.5,1.1,0.5/0,0/
     filter/1,0,0/
     cmo/setatt//itp1/pset,get,rays/21
     cmo/setatt//itp1/pset,get,ray1/21
     setpts
     connect
     settets
     dump/gmv/gmv.cylrot/3dmesh
     finish
 
<a href="https://lanl.github.io/LaGriT/assets/images/cylrot.gif"> Click here for image </a>
