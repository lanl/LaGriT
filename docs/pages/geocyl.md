---
description: 
    Description metadata should be 25-30 words and written from the specific
    to the general. Put your top keywords first but don't repeat title
    information.
keywords:  
title: Geometry
---



 


* input.cylrot use rotateln and trans to move cylinder


* create a cylinder centered around x=.5,z=.5, radius = .1


* the cylinder is aligned parallel to the y-axis.


* inside a box of width =2 , length=2 ,1


* the regions are air for the cylinder - solid outside the cyl.


* points are spread by surrounding the whole object with


* a cylinder shell of points and then creating rays between


* these points and the major axis of the cylinder. Points


* are distributed along these rays inside the cylindrical region.


* a background rectangular grid of points is spread outside the


* cylinder.

cmo/create/3dmesh

surface/box1/reflect /box/-1.0,-1.0,0.0/ 1.0, 1.0, 1.0/

surface/h1/intrface /cylinder/ 0.5, -1.,0.5/ 0.5, 1.0, 0.5/.1/

region/H1/ le box1 and le h1 /

region/Fill/ le box1 and gt h1 /

mregion/Air/ le box1 and lt h1 /

mregion/Solid/Fill

rz/xyz/11,11,1/-1.,-1.,1.1/1.0,1.0,1.1/,1,1,0/

pset/rays/seq/1,0,0/

regnpts/Fill/11/pset,get,rays/xyz/ 0.0,0.0,-0.1/1.0,1.0,-0.1/ &

0.0,1.0,-0.1/0,0/


* the rz command always distributes points with the z-axis as


* the axis of symmetry


* use the rotateln and trans commands to move the point


* distribution after it is created.

rz/rtz/1,13,11/5.,0.,-1./5.,360.,1./0,1,1/0,0,0/

pset/ray1/seq/0,0,0/

rotateln/pset,get,ray1/nocopy/-100.,0.,0./100.,0.,0./-90./0.,0.,0./

trans/pset,get,ray1/0.,0.,0./0.5,0.,0.5/

regnpts/H1/3/pset,get,ray1/rtz/0.5,-1.1,0.5/0.5,1.1,0.5/0,0/

filter/1,0,0/

zq/imt/1,0,0/1/

zq/itp/pset,get,rays/21

zq/itp/pset,get,ray1/21

setpts

search

settets

dump/gmv/gmvr.cylrot/3dmesh

finish

 
=

 

 

 

 

[Return to LaGriT Home Page](index.md)

 



