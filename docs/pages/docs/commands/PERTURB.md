---
title: PERTURB
tags: perturb
--- 

# PERTURB

This command moves node coordinates in the following manner.


Three pairs of random numbers between 0 and 1 are generated. These pairs
refer to the x, y and z coordinates of the nodes respectively. The first
random number of each pair is multiplied by the factor given in the
command. The second random number is used to determine if the calculated
offset is to be added or subtracted from the coordinate.


WARNING: No checking is done to see if elements are inverted by this
perturbation. If one uses too large a value for the perturbation, one
can easily cause element inversions that will flip the normal vector of
triangles and cause 3D cells to have negative volumes.
Use the [**`QUALITY`**](QUALITY.md) command to report element volumes.


## SYNTAX

<pre>
<b>perturb</b>/ <b>pset,get</b>,psetname / xfactor,yfactor,zfactor
</pre>


`pset,get,psetname` is the range of nodes to offset, 1,0,0 means all nodes.


`xfactor,yfactor,zfactor` perturbation values

 

## EXAMPLES

```
perturb/1,0,0/0.5,0,0   
```
add offsets to only the xcoordinates of all nodes



```
perturb/pset,get,mypset/0.001,0.001,0.001  
```
add small offsets to all coordinates of the nodes in the pset named mypset

## DEMO using FACTOR parameters

```
# For a unit cell divided into PTS**3 sub-cells,
# delta-x = 0.01 = distance between sub-cell centers
# x_p = delta-x / 100 = 0.0001 = 1% perturbations
# eps_1 = sqrt(3*x_p) = 0.000173 = max dist. from origin

define/XP/0.0001
define/EPS1/0.00018
define/PTS/101

cmo/create/mo1
createpts/xyz/PTS,PTS,PTS/0.,0.,0./1.,1.,1./1,1,1/
pset/po1/seq/0,0,0/
cmo/setatt/mo1/imt/pset,get,po1/1

cmo/create/mo2
createpts/xyz/PTS,PTS,PTS/0.,0.,0./1.,1.,1./1,1,1/
pset/po2/seq/0,0,0/
cmo/setatt/mo2/imt/pset,get,po2/2
perturb/pset,get,po2/XP,XP,XP/

addmesh/merge/mfilter/mo1/mo2
cmo/copy/mfilterkd/mfilter

cmo/select/mfilter
filter/1 0 0/EPS1

cmo/printatt/mfilter/imt/minmax
cmo/printatt/mfilter/-xyz-/minmax

cmo/select/mfilter
rmpoint/compress

cmo/printatt/mfilter/imt/minmax
cmo/printatt/mfilter/-xyz-/minmax

finish
```


