---
title: CREATEPTS/INTERP
tags: CREATEPTS/interp 
---

 
# CREATEPTS/INTERP  

-----------


 This command adds points to a mesh object. It creates points by linear interpolation of coordinates between two point sets. For some cases, it produces element connectivity between the point sets.


 The interpolation will create projections resulting in the following valid combinations:

 
| Input  | Output  |
| :--- | :--- |
| point | point | 
| line  | quad  |
| tri  | prism  |
| quad | hex |
| hybrid quad tri | hybrid hex prism  |


 
## SYNTAX

<pre>
<b>createpts/interp</b> /npoint/i1,i2,i3/j1,j2,j3/cmo_new
</pre>


`npoint` specifies the number of points to create between the specified point sets.


`i1,i2,i3` specifies the point set coordinate interpolation starts FROM.


`j1,j2,j3` specifies the point set coordinate interpolation goes TO.


`mo_new`  is the name of the output mesh object the result goes into. The source input mesh object is the current mesh object.


`i1,i2,i3` and `j1,j2,j3` point sets can be defined by istart, ilast, istride or pset,get,pset_name.
 It is up to the user to insure that the number of nodes for both sets are equal. An Error will be reported if the set counts differ.

 If the 1, 0, 0 format is used the code will use:

 ```
 i1 = 1, i2 = nnodes/2, i3 = 1, j1 = (nnodes/2)+1, j2 = nnodes, j3 = 1
 ```


## EXAMPLES

```
createpts/line/10///-1.,0.,0.,1.,0.,0.
createpts/line/10///0.,-1.,1.,0.,1.,1.
createpts/ interp / 10 / 1 0 0 / 1 0 0 / cmo_pts
```
Creates a point distribution between the two point sets created by the createpts/line commands


## EXAMPLE QUADS projected to HEX

  Create a hex mesh by linear interpolation between two quad surfaces (from bottom to top in image).
  It is up to the user to be sure that the ordering of the quads are
  as intended. The first node of first quad surface (cmo1) is
  interpolated to the first node of the second quad surface (cmo2)
  
<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/createptsinterp_input.png"><img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/createptsinterp_input.png"> </a> 
<a href="https://lanl.github.io/LaGriT/pages/docs/demos/output/createptsinterp_hex.png"><img width="300" src="https://lanl.github.io/LaGriT/pages/docs/demos/output/createptsinterp_hex.png"> </a> 

  
```
cmo create cmo1///quad
quadxy 5 5/ 0. 0. 0. / 20. 0. 0./20. 20. 0. / 0. 20. 0.
createpts/brick/xyz/5,5,1/1,0,0/connect

cmo create cmo2///quad
quadxy 5 5/ 0. 0. 10. / 20. 0. 25./20. 20. 15. / 0. 20. 35.
createpts/brick/xyz/5,5,1/1,0,0/connect

addmesh / merge / cmo3 / cmo1 / cmo2
cmo / select / cmo3

createpts / interp / 6 / 1 0 0 / 1 0 0 / cmo_hex
```

Starting with cmo3 from above, this will interpolate from node 1 of cmo3 to node 49 of cmo3 and interpolate from node 2 of cmo3 to node 50 of cmo3.

```
createpts / interp / 20 / 1 2 1 / 49 50 1 / cmo_pts
```

## DEMOS 

[Spherical Quad to Hex Sphere to Stacked Hex Cylinder](../demos/demo_creatept_interp.md)

  

