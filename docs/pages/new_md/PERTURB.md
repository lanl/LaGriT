---
GENERATOR: 'Mozilla/4. [en] (Win98; U) [Netscape]'
Generator: Microsoft Word 98
title: PSET
---

 

 **PERTURB**

This command moves node coordinates in the following manner.

Three pairs random numbers between 0 and 1 are obtained. These pairs
refer to the x, y and z coordinates of the nodes respectively. The first
random number of each pair is multiplied by the factor given in
thecommand. The second random number is used to determine if the
calculated offset is to be added or subtracted from the coordinate. No
checking is done to see if elements are inverted by this perturbation. 
It is assumed that the mesh is not yet connected.

 

**FORMAT:**

**perturb/pset,get,psetnam**e/xfactor,yfactor,zfactor

 

EXAMPLES

**perturb**/1,0,0/0.5,0,0   add offsets to only the xcoordinates of all
nodes

**perturb/pset,get,mypset**/0.001,0.001,0.001  add small offsets to all
coordinates of the nodes in the **pset** named **mypset.**

 
[](demos/pset/test/md/main_pset.md)
