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
  
