---
title: "RADAPT"
tags: radapt, adaptive smoothing 
---


# RADAPT

--------------------

The command radapt performs r-adaption smoothing on 2D or 3D mesh objects. 
For a more general version of smoothing see command [**`SMOOTH`**](SMOOTH.md).


This command takes a 2D or 3D mesh object and moves nodes (specifically the nodes selected by
  `ifirst,ilast,istride`), without changing the connectivity of the mesh, in order to adapt the mesh to best capture the behavior of a specified field or to an adaption function **fadpt** supplied by the user.

## SYNTAX

<pre>
<b>radapt</b> /[<b>position</b>] / <b>esug</b> or <b>mega</b>/ [ifirst,ilast,istride] / [field]/ [<b>refresh</b> or <b>stale</b>]

<b>radapt</b> / [<b>position</b>] / <b>esug</b> or <b>mega</b>/  [ifirst,ilast,istride] /  [<b>user</b>]
</pre>

## SYNTAX for the adaption function

<pre>
subroutine <b>fadpt</b>(xvec,yvec,zvec, imtvec, nvec, time, fvec) 
</pre>
xvec, yvec, zvec - Vectors of x, y, and z coordinates of the points where the function is to be evaluated. 
imtvec - Vector of imt values for the case where function value depends on material type as well as position (ie. functions with discontinuities). 
nvec - Vector length (= number of places where function is to be evaluated).
time - Time (scalar), for time-dependent functions.
fvec - Vector of adaption function values.

## DESCRIPTION

There are two adaptive smoothing algorithms available:


  1. **`esug`**  Elliptic Smoothing for Unstructured Grids. This can
  only be used on triangular 2D mesh objects. If field is specified in
  the command line, **esug** will attempt to adapt the grid to the
  specified field. If the keyword **user** is specified in the command
  line, **esug** will attempt to adapt the grid to an *adaption
  function* defined by the user-supplied subroutine **`fadpt`**. 
<br>
Ahmed Khamayseh and Andrew Kuprat, "Anisotropic Smoothing and Solution
  Adaption for Unstructured Grids", Int. J. Num. Meth. Eng., Vol. 39, pp. 3163-3174 (1996)
  
  2. **`mega`** Minimum Error Gradient Adaption. For adaptive
  smoothing purposes, **mega** can only be used on 3D meshes, and only
  in conjunction with a user-supplied subroutine **`fadpt`** or with a user specified attribute `field`. If adaption is to an attribute field,
  then **radapt** may be instructed to use the interpolation mode associated with the attribute to **refresh** the attribute values.
  The default is **stale** in which case the attribute value will not be updated to reflect the new node position. In either case, the
  user is cautioned to carefully consider the validity of the data used for the adaption. **mega** can be used to adapt hybrid meshes
  as well as tetrahedral meshes. 
<br>
Randolph E. Bank and R. Kent Smith, "Mesh Smoothing Using A Posteriori Error Estimates", SIAM J. Num.  Anal. Vol. 34, Issue 3, pp. 9-9 (19)


If `field` adaption is used, the user has specified a valid `field`
  from the current mesh object, and r-adaption is to be based upon
  this field. Typically, if the field has large gradients or curvature
  in a particular region, r-adaption using this field will cause nodes
  to be attracted to the region of interest. (**esug** adapts
  especially to large gradients, **mega** adapts especially to large
  second derivatives---"curvature".) 

If adaption is to an attribute field, then **`radapt`** may be instructed to use the interpolation
  mode associated with the attribute field to **refresh** the
  attribute values. The default is **stale** in which case the
  attribute value will not be updated to reflect the new node position
  adaption. In this case, the user should reduce the number of
  adaption iterations to less than 4, since r-adaption with stale data
  becomes meaningless.

If  **`refresh`** is specified, the r-adaption routine will automatically
  interpolate the new field values every iteration, using a call to
  the **`doping`** command. In this case, the number of adaption
  iterations need not be reduced from the default value of 25. In
  either case, the user is cautioned to carefully consider the
  validity of the data used for the adaption.

If **`user`** is specified, the mesh will r-adapt to the function returned
  by the subroutine **`fadpt`** which must be supplied by the user.


If  **`position`** is specified, it signifies that the x-y-z values of the nodes
  in the current mesh object will be altered. (Other argument values allow for modification options that are not yet implemented.)


If **`esug`** is used (currently available in 2D only), the degree of
  node adaption will depend on the scale of the specified field. In
  this case, the results of adaption of the grid to the field can be
  altered by using one or more `field` commands beforehand to modify the field. For example, by increasing the scale of a field using
  **field** **/scale**, the **esug** algorithm will produce grids with
  increased numbers of nodes in the regions where the field
  experiences relatively large gradients. By volume averaging a field
  using `field`**/volavg**, **esug** will cause a more gentle form
  of adaption with a better grading of elements. By composing the
  values of the field with **log** or **asinh** using **field**
  /**compose**, one can cause **esug** to shift nodes to where the
  logarithm (or hyperbolic arcsine) of the field has interesting
  features, rather than where the field itself has interesting
  features. 

Note: Since the* **mega** adaptive smoothing algorithm is rigorously based on error minimization, it is in general of
  little or no value to modify the adaption function for this algorithm. In particular, rescaling has no effect on the output.


The variable **MAXITER_SM** (Default: 25)  can be set using the **`ASSIGN`** command before calling **`RADAPT`**.  
This controls the maximum number of adaption iterations to be performed.  If convergence is detected beforehand, less iterations will be
  performed.  If field data is allowed to become "stale" during the course of r-adaption, **MAXITER_SM** should be reduced (e.g. less than 5).



# EXAMPLES

 
```
radapt /  / esug / 1,0,0 / density
```
Using **esug**, adapt all nodes in 2dmesh to the density field. Do not update data.
 
 
```
radapt /  /  / 1,0,0 / user
doping / user / density / set /1,0,0/
```
Assuming a default 3D cmo, use **mega** to adapt the mesh to the adaption function supplied by the user via subroutine fadpt.
Afterwards dope (interpolate) the density field with the fadpt function values.


``` 
assign / / / maxiter_sm / 50
``` 
This  changes the maximum number of iterations to 50. If **radapt** detects a sufficient amount of convergence, it will terminate
  smoothing in less than **maxiter_sm** iterations.

 
## DEMOS with SAMPLE FUNCTIONS 

The following demonstrate adaptive smoothing using **mega**.




### 1. Boron density function 

Load the file fadpt_boron.f ahead of the **LaGriT** libraries; this will cause the default fadpt
  subroutine to be displaced by the one in this file. The result is that
  now 3D adaptive smoothing will attempt to adapt 3D tetrahedral or hybrid
  meshes to the boron density function devised by Kent Smith of Bell Labs.
  This function has a maximum value of 1.1 x 1018, and drops rapidly to
  zero; the function attains its largest values on a T-shaped region in
  space and provides very challenging isosurfaces to capture. Two input
  decks use this function:
 
input.boron.3dtet. This deck generates and adapts a tetrahedral mesh to the boron function. A snapshot of the adapted grid may be seen at  <a href="https://lanl.github.io/LaGriT/assets/images/boron.png"> boron.png </a>.

input.boron.3dhex. This deck generates and adapts a hexahedral mesh to the boron function. A snapshot of the adapted grid may be seen <a href="https://lanl.github.io/LaGriT/assets/images/boron.hex.png"> boron_hex.png </a>

-  Fortran subroutine [fadpt_boron.f](../../fadpt_boron.f)
-  LaGriT command file [input.boron.3dtet](../../input.boron.3dtet)
-  LaGriT command file [input.boron.3dhex](../../input.boron.3dhex)
 

### 2. Gyroscope function

This function fadpt_gyro.f, has large second derivatives near three rings of unit diameter which are aligned with
  each of the three coordinate planes which pass through the origin.  Adaption to this function results in the pulling of the grid towards the
  rings when running the following two input decks:
 
input.gyro.3dtet. This deck generates and adapts a tetrahedral mesh to the "gyroscope" function. A snapshot of the adapted grid may be seen at <a href="https://lanl.github.io/LaGriT/assets/images/gyro.png"> gyro.png </a>

input.gyro.3dhex. This deck generates and adapts a hexahedral mesh to the "gyroscope" function. A snapshot of the adapted grid may be seen at <a href="https://lanl.github.io/LaGriT/assets/images/gyro.hex.png"> gyro.hex.png </a>

-  Fortran subroutine [fadpt_gyro.f](../../fadpt_gyro.f)
-  LaGriT command file [input.gyro.3dtet](../../input.gyro.3dtet)
-  LaGriT command file [input.gyro.3dhex](../../input.gyro.3dhex)
 

