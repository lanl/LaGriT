---
title: SMOOTH
tags: smooth
---


# SMOOTH

----------------------

Smooth a 2D or 3D mesh object. 

For adaptive smoothing see the **[radapt](RADAPT.md)** command. **smooth** takes a 2D or 3D mesh object 
and moves nodes, without changing the connectivity of the grid, in order to improve the aspect ratios and distribution of
elements in the mesh.

There are nine smoothing algorithms available:

| &nbsp;[2D](#2d)&nbsp; | [2D and 3D](#2d3d) | &nbsp;[3D](#3d)&nbsp; |
| :--------: | :--------: | :--------: | 
| [esug](#esug) | [laplace](#laplace) | [mega](#mega) |
| [elliptic](#elliptic) | [aspect](#aspect) | [network](#network) |
 [random](#random) |  [lpfilter](#lpfilter) | [geometry](#geometry) |




## SYNTAX 

For all commands:

**`position`** results in the positions of nodes being changed. Other options have not been implemented.

`ifirst,ilast,istride` is the selection of points designated by node numbers or **pset**,get,pname. 1,0,0 will select all.


The internal variable **maxiter_sm** (default=25) controls the maximum number of iterations 
and can be changed using the **[assign](ASSIGN.md)** command. e.g.  (assign /// maxiter_sm/10). 


## SYNTAX 2D  <a name="2d"></a>
<pre>
<b>smooth/position</b>/<b>esug elliptic  random</b>/ [ifirst,ilast,istride ]/[control] 
</pre>

**`esug`** <a name="esug"></a>
 Elliptic Smoothing for Unstructured Grids with guards against folding. This is the default for 2D mesh objects. It can only be used on triangular 2D mesh objects.


*Ref.: Randolph E. Bank and R. Kent Smith, “Mesh Smoothing Using A Posteriori Error Estimates”, SIAM J. Num. Anal. Vol. 34, Issue 3, pp. 979-997, 1997.*


**`elliptic`** <a name="elliptic"></a>
similar to **esug** except the 'guards' which prevent a grid from folding are turned off. (Thus esug is preferred.) 


**`random`** <a name="random"></a>
 a node's position is set to a randomly weighted average position of its neighbors. 'Guards' keep the elements from inverting. 


`control` (default 0 )  from standard smoothing scheme to 1 which causes the scheme to be progressively more controlled, at 1 there is no mesh movement.


## SYNTAX 2D and 3D <a name="2d3d"></a>
<pre>
<b>smooth/position/aspect</b>    // [ifirst,ilast,istride]/ [toldamage] 
<b>smooth/position/lpfilter</b> // [ifirst,ilast,istride] /[filtdeg]/[k_pb]/<b>network</b>
<b>smooth/position/laplace</b> / [ifisrt,ilast,istride]/[rlxwt]/[ntimes]/[nwtty]/[useisn]/[extrnbr] 
</pre>


**`aspect`**  <a name="aspect"></a>
Adjusts node positions such that the aspect ratio of the elements is improved.
  - `toldamge` (default is infinity) is the damage tolerance, so it can be used as a general smooth which has the effect of improving worst aspect ratio.


**`lpfilter`**  <a name="lpfilter"></a>
This smooths surface networks by a low-pass filtering of the coordinate data. 
- `fltdeg` (default 30)
- `k_pb` (default 0.1)
- **network** applies to a network of curves in 2D or 3D, or to a network of surfaces in 3D.  The cell materials are ignored.


**`laplace`**  <a name="laplace"></a>
On a 3D tetahedral mesh moves a node to the average position of its neighbors where neighbor is defined as the set of nodes connected to the candidate node by an edge where the node types (itp1) and node constraints (icr1) are a 'subset' of the candidate node type and constraints.  A node will not be moved if the result is an inverted element. The following controls may be supplied: 
-  `rlxwt` (default 0.5 ) weight for underrelaxed  Laplacian smothing              
-  `ntimes` (default 5 )  number of smoothing iterations  
-  `nwttry` (default 3 )  number of attempts to not tangle the mesh by halving the  smoothing weight.               
-  `useisn` (default 1 )  means interface nodes are smoothed based along an edge with the same materials as the candidate node. 
0 means interface nodes are smoothed based on all interface neighbors.             
- `extrnbr` (default **inclusive**) means do not restrict neighbors.  **exclusive** means restrict neighbors to pset nodes.     


## SYNTAX 3D  <a name="3d"></a> 
<pre>
<b>smooth/position</b>/<b>mega geometry</b>/ [ifirst,ilast,istride ]/[control] 
<b>smooth/position/network</b>/[ifisrt,ilast,istride]/[niter]/[weight]/[<b>check nocheck</b>]
</pre>


`control` (default 0 )  from standard smoothing scheme to 1 which causes the scheme to be progressively more controlled, at 1 there is no mesh movement.


**`mega`**  <a name="mega"></a>
Minimum Error Gradient Adaption. This option creates a smoothed grid which is adapted to the standard function with constant
Hessian f(x,y,z)=x2+y2+z2. Can be used on hybrid 3D meshes and guards against mesh folding. 
Adaption to this function creates a uniform isotropic mesh.

*Ref.: Randolph E. Bank and R. Kent Smith, "Mesh Smoothing Using A Posteriori Error Estimates", SIAM J. Num.  Anal. tol. 34, Issue 3, pp. 9-9 (19)*


**`geometry`**  <a name="geometry"></a>
Geometry ("plump element") adaption. Default for 3D.
Can be used on hybrid 3D meshes It uses the **mega** algorithm but retains only the leading geometry term; 
the term containing the Hessian has been dropped. This algorithm guards against mesh folding.


**`network`**  <a name="network"></a>
This option smooths the surface network of a 3D tetrahedral grid.  Volume nodes are not moved.  
The material volumes are conserved.  Combining this type of smooth with volume smoothing will help to avoid element inversions.
- `niter` (default 10 ) number of iterations
- `weight` (default 1. ) controls the amount of movement (from 0. to 1.). 
- **check** (default) By default a check is performed to verify that no elements are inverted.
- **nocheck** turns off check for inverted elements. This option will not work correctly (will not conserve volume)
on grids which have two areas of a material connected at a single node or edge; each material region must have face connectivity.  


<hr>


## EXAMPLES

```
smooth
```
Smooth all nodes in the mesh using esug in 2D or geometry in 3D.


```
smooth / / / 1,0,0 / 0.5
```
Smooth all nodes in the mesh, using controlled smoothing with control=0.5.


```
pset/p2/attribute/itp1/1,0,0/0/eq
smooth /position /network/1,0,0/3/1./check
smooth/position/geometry/pset,get,p2
smooth /position /network/1,0,0/3/1./check
smooth/position/geometry/pset,get,p2
```
Smooth a 3D grid by combining network and volume smooths.

```
compute/distance_field/CMO_SINK mobj_sm dfield
pset/pin/attribute dfield/1,0,0/ le 120.

assign///maxiter_sm/6                                                           
smooth/position/esug/pset,get,pin
```
Smooth a 2D grid within 120 distance from mobj_sm and control the number of iterations by using the **assign** command.

