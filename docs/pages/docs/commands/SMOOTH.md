---
GENERATOR: 'Mozilla/4.79C-SGI 
[en
] (X11; U; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: SMOOTH
---

 

 **SMOOTH**

The **smooth** command smooths 2D or 3D mesh objects. For adaptive
smoothing see the **[radapt](RADAPT.md)** command. **smooth** takes a
2D or 3D mesh object and moves nodes, without changing the connectivity
of the grid, in order to improve the aspect ratios and distribution of
elements in the mesh.

An optional control value between zero and one for options **esug**,
** mega**, **geometry**, **elliptical** and **random** affects the amount
of node movement. The default (control =0.) results in the standard
smoothing scheme. Increasing control towards 1. causes the scheme to be
progressively more controlled (moving the mesh less), until at control
=1., there is no mesh movement whatsoever.

By default, the second argument is **position**. This results in the
positions of the nodes being changed. (Other options will be added in
the future for implementation of smoothing using node velocities.)

There are nine smoothing algorithms available. **esug**, **elliptic**
and **random**  are for 2D grids, **laplace**, **aspect** and
**lpfilte**r  for 2D or 3D grids, ** mega, network** and **geometry** are
for 3D grids:

1
. **esug** --- Elliptic Smoothing for Unstructured Grids. This is the
default for 2D mesh objects. It can only be used on triangular 2D mesh
objects. (Ref.: Ahmed Khamayseh and Andrew Kuprat, "[Anisotropic
Smoothing and Solution Adaption for Unstructured
Grids](../../<a href="https://lanl.github.io/LaGriT/assets/images/ahmandrew1.pdf" download> </a>", Int. J. Num. Meth. Eng., Vol. 39,
pp. 3163-3174 (1996).)

2.**elliptic** --- Similar to esug but the 'guards' which prevent a grid
from folding are turned off. (Thus esug is preferred.)

3. **random** -- a node's position is set to a randomly weighted average
position of its neighbors. 'Guards' keep the elements from inverting.

4. **laplace** ---On a 3D tetahedral mesh moves a node to the average
position of its neighbors where neighbor is defined as the set of nodes
connected to the candidate node by an edge where the node types (itp1)
and node constraints (icr1) are a 'subset' of the candidate node type
and constraints.  A node will not be moved if the result is an inverted
element. The following controls may be supplied:

 


  rlxwt default(0.5)                weight for underrelaxed         
                                     Laplacian smothing              

  ntimes default(5)                 number of smoothing iterations  

  nwttry default(3)                 number of attempts to not       
                                     tangle the mesh by halving the  
                                     smoothing weight.               

  useisn default(1)                 1 means interface nodes are     
                                     smoothed based alonga           
                                     multimaterial edge with all the 
                                     same materials as the candidate 
                                     node. 0 means interface nodes   
                                     are smoothed based on all       
                                     interface neighbors             

  extrnbr default(**inclusive**)    **inclusive** means do not      
                                     restrict neighbors 
            
                                     **exclusive** means restrict    
                                     neighbors to nodes in pset      


 5. **aspect**---Adjusts node positions such that the aspect ratio of
the elements is improved.  The default damage tolerance for
**smooth//aspect** is infinity, so it can be used as a general smooth
which has the effect of improving worst aspect ratio.

6
. **lpfilter**---This smooths surface networks by a low-pass filtering
of the coordinate data.

(filtdeg default = 30, k\_pb default = 0.1)

 7**. mega ---** Minimum Error Gradient Adaption. This option creates a
smoothed grid which is adapted to the standard function with constant
Hessian f(x,y,z)=x2+y2+z2. Can be used on hybrid 3D meshes and guards
against mesh folding. Adaption to this function creates a uniform
isotropic mesh.  The code variable ** maxiter\_sm** (default=25) controls
the maximum number of ** mega** iterations.  The value of ** maxiter\_sm**
may be changed using the **[assign](ASSIGN.md)** command
(**assign**//**/ maxiter\_sm**/10).  (Ref.: Randolph E. Bank and R. Kent
Smith, "Mesh Smoothing Using A Posteriori Error Estimates", SIAM J. Num.
Anal. tol. 34, Issue 3, pp. 979-997 (1997).)

8. **geometry** --- Geometry ("plump element") adaption. Default for 3D.
Can be used on hybrid 3D meshes It uses the ** mega** algorithm but
retains only the leading geometry term; the term containing the Hessian
has been dropped. This algorithm guards against mesh folding.

9. **network** --- This option smooths the surface network of a 3D
tetrahedral grid.  Volume nodes are not moved.  The material volumes are
conserved.  By default a check is performed to verify that no elements
are inverted; the user may turn this check off with the **nocheck**
option.  This option will not work correctly (will not conserve volume)
on grids which have two areas of a material connected at a single node
or edge; each material region must have face connectivity.  The number
of iterations is controlled by the niter argument (default is 10) and
the weight argument controls the amount of movement (from 0. to 1.
default is 1.).  Combining this type of smooth with volume smoothing
will help to avoid element inversions.

**FORMAT:**

**smooth****/position****/esug**** mega****geometry****elliptic****random**/

[ifirst,ilast,istride 
]/
[control
]

**smooth****/position****/lpfilter**// 
[ifirst,ilast,istride
]
/
[filtdeg
]/
[k\_pb
]**/network**

**network** smoothing applies to a network of curves in 2D or 3D, or to
a network of surfaces in 3D.  The materiality of the cells (if any) is
ignored.

**smooth****/position****/aspect**//
[ifirst,ilast,istride/toldamage
]

**smooth****/position/laplace**/
[ifisrt,ilast,istride
]/
[rlxwt
]/
[ntimes
]/
[nwtty
]/
[useisn
]/
[extrnbr
]

**smooth****/position/network**/
[ifisrt,ilast,istride
]/
[niter
]/
[weight
]/
[heck****nocheck**

**EXAMPLES:**

 

1
. Smooth all nodes in the mesh using **esug** in 2D or **geometry** in
3D.

**smooth**

2. Smooth all nodes in the mesh, using controlled smoothing with control
=0.5

**smooth** / / / 1,0,0 / 0.5

3. Smooth a 3D grid by combining network and volume smooths.

**pset**/p2**/attribute**/itp1/1,0,0/0**/eq**

**smooth****/position****/network**/1,0,0/3/1./heck**

**smooth/position/geometry/pset,get,**p2

**smooth****/position****/network**/1,0,0/3/1./heck**

**smooth/position/geometry/pset,get,**p2
