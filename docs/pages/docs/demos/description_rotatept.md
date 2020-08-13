---
title: Example  rotatept 
tags: example rotatept
---

# Example ROTATEPT 

The objective is to use the **rotatept** command to modify a point distribution by rotation.

The output consists of three gmv files, one showing the original point distribution, one showing a rotation in the xy-plane, and one showing
 a rotation away from the z-axis, each using **rotatept/rtz**.


LaGriT Input Command File [lagrit_input_rotatept](input/lagrit_input_rotatept.txt)



### Initial point distribution

```
cmo/create/abc/tet
createpts/rtz/9,5,2/0.,0.,0./10.,360.,2./1,1,1/
pset/rays/seq/1,0,0
zq/xic/pset,get,rays
```

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/rotatept1.gif">

### rotation in xy-plane about z-axis

```
rotatept/pset,get,rays/nocopy/0.,0.,0./0./30.
```

<img width="300" src="https://lanl.github.io/LaGriT/assets/images/rotatept2.gif">

### rotation in xy-plane and away from z-axis

```
rotatept/pset,get,rays/nocopy/0.,0.,0./15./00.
```

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/rotatept3.gif">
