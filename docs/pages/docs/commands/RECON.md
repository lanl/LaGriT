---
title: RECON
tags: recon, Delaunay
---

# RECON

----------------------

This command flips connections in the mesh to get restore the Delaunay criterion or to improve element shapes. 

The standard method used by **`recon`** is to flip connections based on the in-sphere test (the circumsphere of a tetrahedral element should contain no other nodes). Additional flipping criteria are available. 


## SYNTAX

<pre>
<b>recon</b> / [<b>1</b> or <b>0</b>] / [damage] / [<b>checkaxy</b>]
</pre>


**`0`** or no arguments is the default and specifies that no points are to be added on the boundaries. 


**`1`** adds points on the boundaries if needed to make Delaunay (recommended for 2D meshes only). 

The aim of `recon / 1` is a Delaunay mesh with every Voronoi centre (the circumcentre of each triangle) inside the exterior boundary, which gives positive coupling coefficients in two-point flux codes such as FEHM, PFLOTRAN and TOUGH. The method is described in M. Murphy and C. Gable, *Strategies for Nonobtuse Boundary Delaunay Triangulations*, 7th International Meshing Roundtable, 2000. In 3D the problem is harder; see M. Murphy, D. Mount and C. Gable, *A point-placement strategy for conforming Delaunay tetrahedralization*, Int. J. Comput. Geom. Appl. 11 (2001), 669-682.

What `recon / 1` does:

- **2D:** rounds of flips, then a point on each boundary (or interface) edge whose opposite angle is obtuse (its Voronoi coefficient is negative), repeated (up to 50 rounds) until no boundary angle is obtuse. The result is Delaunay, with every Voronoi centre inside the boundary and no obtuse angle at the boundary, and the boundary and the area of each material are kept.
  For example, Poisson disk points of a square stretched to a 10 x 1 rectangle and connected leave 39 Voronoi centres outside; `recon / 1` adds 42 vertices on the boundary and leaves none (test/suite/test_recon, rc06_2d_poisson_x10). `createpts / poisson_disk / 2d_polygon / ... / connect` already ends with `smooth`, `recon / 0`, `smooth`, `recon / 1`, so its meshes need no further `recon / 1`.
- **3D:** one round of flips, one pass that puts a point on the boundary edge opposite each obtuse angle of a boundary face, then flips again. This does **not** reach the aim: on a Delaunay tet mesh of random points in a cube, the Voronoi centers outside the cube go from 310 to 450, 330 faces are left not Delaunay and the obtuse boundary angles double (test/suite/test_recon, rc12). With two materials it also prints "zero terminated parent/child chain" messages. Use `recon / 0` (or `connect`) for 3D.

In 3D, `recon / 0` reaches Delaunay only as far as its flips can: on a jittered grid with 621 faces not Delaunay it leaves 145 (it prints their number as `nface`).

A mesh with more than one material must have the parent / child vertices that `connect` makes on the interfaces; read from a file without them, `recon` stops LaGriT (test_recon, rc15).


If `damage`  is specified then flips on exterior boundaries are checked to verify that the maximum depth of deformation of the external boundary does not exceed the value of `damage`. The default value of damage is 1% of the problem size. This setting prevents connecting across corners if the external boundary is a reflective box.


If **`checkaxy`** is provided, then 2D flips are suppressed if the new triangles would have xy-projected areas less than EPSILONA.


### Additional Settings


<pre>cmo/setatt//ivoronoi/-2</pre>
The Minimum Error Gradient Adaption (mega) can be invoked by changing the value of the mesh object variable **ivoronoi** to -2. The effect of this option is to generate well shaped elements; however the grid will not be Delaunay. 


<pre>cmo/setatt//ivoronoi/ 2</pre>
If the user has a function to used for adaptive reconnection this option is available by setting the code variable **ivoronoi** to 2. The user will have to supply an external function.


<pre>cmo/setatt//ivoronoi/ 5</pre>
**`recon`** is called by other LaGriT commands such as **`massage`**.  To disable **recon** set **ivoronoi** to 5.


<pre>cmo/setatt//iopt2to2/ 0</pre>
**recon** will by default reconnect across interface edges.  To restrict reconnection to interior faces and exterior boundary faces, set **iopt2to2** to 0. 



## EXAMPLES

```
recon 
```
attempt to restore Delaunay

```
recon / 1 
```
for 2d meshes add nodes on boundaries to guarantee Delaunay


```
 recon / / .001  
```
reconnect limit interface and boundary damage to a maximum of .001


```
recon/0/ .001 / checkaxy  
```
    
for 2d meshes reconnect, limiting damage to a maximum of .001 and preventing creation of any negatively oriented or small triangles (with respect to the xy-plane). 
  
