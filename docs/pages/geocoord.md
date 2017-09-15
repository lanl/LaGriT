---
description: 
    Description metadata should be 25-30 words and written from the specific
    to the general. Put your top keywords first but don't repeat title
    information.
keywords:  
title: Geometry
---

 

* input.coordsys

* input.coordsys

*create a cylinder parallel to the y-axis inside a box

*use the coordsys command

cmo/create/3dmesh

surface/box/reflect /box/ 0.0,0.0, 0.0/ 1.0, 1.0, 1.0/

surface/h1/intrface /cylinder/ 0.2, -0.1, 0.8/ 0.2, 1.1, 0.8/.1/

region/H1/ le box and le h1 /

region/Fill/ le box and gt h1 /

mregion/Air/ le box and lt h1 /

mregion/Solid/Fill

rz/xyz/11,11,1/0.0,0.0,1.1/1.0,1.0,1.1/1,1,1/

pset/rays/seq/1,0,0/

regnpts/Fill/11/pset,get,rays/xyz/ 0.0,0.0,-0.1/1.0,1.0,-0.1/ &

0.0,1.0,-0.1/0,0/

coordsys/define/0.2,0.,0.8/1.,1.,0.8/0.2,2.0,0.8/

rz/rtz/1,7,11/5.,0.,0./5.,360.,1./0,1,1/0,0,0/

coordsys/normal

pset/ray1/seq/0,0,0/

regnpts/H1/3/pset,get,ray1/rtz/ 0.2,0.0, 0.8/0.2,1.0, 0.8/0,0/

filter/1,0,0/

zq/imt/1,0,0/1/

zq/itp/pset,get,ray1/dud/

zq/itp/pset,get,rays/dud/

setpts

search

settets

dump/gmv/gmvr.coordsys/3dmesh

finish

 
=

 

 

 

 

[Return to LaGriT Home Page](index.md)

 

