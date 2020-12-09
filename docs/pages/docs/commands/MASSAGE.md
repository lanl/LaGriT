---
title: MASSAGE
tags: massage, agd3d, sgd, recon, cel_chain
--- 

# MASSAGE

-----------------

 **`MASSAGE`** creates, annihilates, and moves nodes and swaps
 connections in a 2D or 3D mesh in order to improve element aspect
 ratios and establish user-desired edge lengths.

 Specifically, **`MASSAGE`** performs up to four iterations of a loop
 which calls AGD3D (a routine for automated merging of nodes), RECON (a
 routine for automated reconnection of edges), and SGD (a routine for
 element aspect ratio improvement using smoothing). **`MASSAGE`** then
 calls CEL_CHAIN which performs Rivara edge refinement and then
 another call to RECON.  In the case of 2-D surface grids, this is then
 followed by a call to CER_CHAIN which is another edge refinement
 routine and then a final call to RECON if necessary.


 AGD3D will attempt to merge edges that are shorter than `merge_length`.  
 CEL_CHAIN will attempt to bisect edges that are longer than `bisection_length` .  
 For 2D surfaces, CER_CHAIN will attempt to bisect edges that deviate from an
 averaged surface normal ("have a roughness of") greater than
 `tolroughness`. 
 RECON will attempt to create 'nice' elements by using face swapping. The LaGriT command **`MODE/RECON`**
 can alter the meaning of 'nice'.  The default is to reconnect to restore the delaunay criterion.  
 
Calling **MODE** / **RECON/GEOM** prior to the **MASSAGE** call will create 'plumper' elements. 
 SGD will attempt to improve element aspect ratios by moving nodes.
 
Note:  Since CEL_CHAIN is called only once at the end of **`MASSAGE`**,
 it may be necessary to call **`MASSAGE`** twice for optimal results. 
 This is because annihilation of nodes is done with an intent to
 improve element aspect ratios, but cannot be effective if there are
 too few nodes initially.

Note: The user may wish to call **`RMPOINT/COMPRESS`** after
 **`MASSAGE`** operations that merge a significant number of nodes.
 
## SYNTAX

<pre>
<b>massage</b>/bisection_length/merge_length/toldamage/[[tolroughness]]/ &
[ifirst,ilast,istride]/ [<b>nosmooth</b>] / [<b>norecon</b>]   &
[<b>strictmergelength</b>]/[<b>ignoremats</b>]/       &
[<b>lite</b>]/[<b>checkaxy</b>]/[<b>semiexclusive</b>]/[<b>exclusive</b>]
</pre>
 
 

### Primary Parameters

`bisection_length`  edge length that will trigger bisection. can either be a scalar value or a
 node attribute. 
 
`merge_length`  is the edge length value that will trigger merging.

`toldamage` is a parameter which controls how much the mesh interface and external boundaries will be deformed.    Roughly, it measures the depths of 'dents' that are invariably introduced when nodes are moved, annihilated, and faces are swapped. 
 
`tolroughness` (for 2D surface grids only)  measure of grid roughness (deviation from average surface
     normal) that triggers refinement.


The following are guidelines for using these primary parameters:

When setting the bisection parameter `bisection_length` as a scalar value, if the edge length is greater than scalar value, the edge will be refined. 
If a node attribute is used, the edge length is compared to the minimum value of the two nodes, if edge length is greater, the edge will be refined. Thus, one should put a minimum floor value (probably equal to
 twice the desired minimum edge lenth) for the field. Otherwise the
 code will refine indefinitely. For an example of an appropriate field see [**`MASSAGE2`**](MASSAGE2.md).

The value of `bisection_length` should not be smaller than `merge_length`, or the action of
 merging nodes together will be largely pointless because the newer,
 longer edges created by merging will simply be bisected again. 
 It is recommended that `bisection_length` be at least three times as large as `merge_length`.
 
If the damage parameter `toldamage` is set to an extremely small number, there will be few if any node movements, but annihilations, or face swaps will be allowed.  Conversely, if this parameter is set too large, boundaries may be significantly deformed. Setting `toldamage` equal to approximately .01 times the diameter of the mesh frequently gives acceptable results. 
 

Merges of edges of length &lt;= `merge_length` are meant to coarsen the mesh, but are not meant to deform surfaces and material interfaces on this scale.  The amount of material/surface deformation set with `toldamage` is meant to be considerably less than `merge_length`. On the other hand, the maximum roughness tolerated in the graph
`tolroughness` should be considerably more than `toldamage`, or roughness refinement will be triggered by actions such as flipping or merging.


The following are guidelines for setting these parameters:


`bisection_length` &gt;= 3 times `merge_length` &gt;&gt; `toldamage`


`tolroughness` &gt;= 10 times `toldamage`  (for 2D surface grids)


For example, for a grid with diameter of order 3, these values are suggested:
`bisection_length`, `merge_length`, `toldamage`, `tolroughness` = .3, .1, .01, .1


If `bisection_length` **or** `merge_length` is omitted, the omitted parameter will be set so that `bisection_length` = 3 times `merge_length`.


If `bisection_length` **and** `merge_length` are omitted, both values will be taken to infinity.


If `toldamage` is omitted, no node annihilation will take place.

 
If `tolroughness` is omitted, no refinement on roughness will occur and thus the format is compatible
 with old commands where refinement on roughness did not occur.


### Additional Parameters

- **nosmooth** will turn off the **`smooth`** step by skipping the call to SGD.

- **norecon** will turn off all **`recon`** steps.

-  **lite** will specify only one iteration of the merging/reconnection/smoothing loop is executed,
    and a reconnection after edge refinement is omitted.  This is
    suitable for applications, such as Gradient Weighted Moving Finite
    Elements, where **`MASSAGE`** is called repeatedly.
    
-   **ignoremats** causes **`MASSAGE`** to process the multimaterial mesh in a single
    material mode; it ignores multi-material interfaces. 
    
-   **strictmergelength** forces strict interpretation of `merge_length` so  that there is no merging along the edges of flat elements.  This is important if **ignoremats** is specified to avoid losing the interfaces.

-   **checkaxy**  for 2D meshes, ensures the output mesh will have positive xy-projected
    triangle areas, provided that the input mesh had them in the first place. 
    
-   **exclusive** applies to point sets, edge refinement operations will only be performed on edges whose
    endpoints are both in the PSET that **`MASSAGE`** is working on.  (As usual, new nodes created by refinement are added to the PSET so that **`MASSAGE`** can refine edges recursively.)  The default
    behavior is **inclusive**, where only ONE edge endpoint has to belong
    to the PSET for the edge to be eligible for refinement.
    
-   **semiexclusive** represents an intermediate case between **inclusive** and **exclusive**. Refinment is triggered only by edges with both endpoints in the PSET, but  some edges with less than two endpoints in the PSET might be refined
    as part of a 'Rivara chain' triggered by the refinement of an edge with both endpoints in the PSET.  

 

 

## EXAMPLES

```
massage/[0.3/0.1/0.01]
```

Mesh edges longer than 0.3 will be bisected; mesh edges shorter than
0.1 might be collapsed if that causes damage (normal surface motion)
to material interfaces or external boundaries less than 0.01 ;
 smoothing of nodes causing damage less than 0.01 is allowed ; face
 swapping causing damage less than 0.01 is allowed.

```
massage/[H_SCALE/0.1/0.01]/
```
Same as above, except that the **`bisection_length`** is a node field called H_SCALE in this case.

```
massage/[[0.3/0.1/0.01/0.1]
```
Same as above but for 2-D surface meshes, roughness greater than 0.1 will trigger refinement.

```
massage/0.3/0.1/0.01/pset,get,[pset1]
```
Mesh edges (containing at least one endpoint in pset1) longer than 0.3
 will be bisected; mesh edges shorter than 0.1 might be collapsed if
 that causes damage (normal surface motion) to material interfaces or
 external boundaries less than 0.01 and if the annihilated node is in
 pset1;  smoothing of nodes in pset1 causing damage less than 0.01 is
 allowed; face swapping causing damage less than 0.01 is allowed
 (unfortunately, LaGriT at this time does not restrict swapping to
 pset1).

```
massage/0.3/0.1/0.01/pset,get,pset1,nosmooth
```
As above, but without smoothing.

```
massage/1.e+20/0.1/0.1/1,0,0/nosmooth
```
Because of the virtually infinite value of [bisection_length],no edges will be
 bisected.  Since merge_length=toldamage=0.1, merging of edges  of
 length less than 0.1 will be considered, and will not be rejected
 because of excessive damage.  Hence we expect that all edges of length
 less than 0.1 will be merged away (except in those cases where merging
 would invert tetrahedra or change material topology).   Because
 **nosmooth** is specified, no smoothing will take place.  Face
 swapping causing damage less than [toldamage] is allowed

```
massage[/1.e+20/1.e-9/1.e-9/1,0,0]/nosmooth/strictmergelength /ignoremats
```
This set of arguments will remove degenerate elements from a mesh by
 merging nodes that have the same coordinate values ( within 1.e-9).
 

