---
GENERATOR: 'Mozilla/4.8 
[en
] (X11; U; SunOS 5.8 sun4u) 
[Netscape
]'
Generator: Microsoft Word 98
Template: 
    Macintosh HD:Applications:Microsoft Office 98:Word Custom
    Install:Microsoft Office 98:Templates:Web Pages:Blank Web Page
title: MASSAGE
---

 

 **MASSAGE**

 **MASSAGE** creates, annihilates, and moves nodes and swaps
 connections in a 2D or 3D mesh in order to improve element aspect
 ratios and establish user-desired edge lengths.

 Specifically, **MASSAGE** performs up to four iterations of a loop
 which calls AGD3D (a routine for automated merging of nodes), RECON (a
 routine for automated reconnection of edges), and SGD (a routine for
 element aspect ratio improvement using smoothing). **MASSAGE** then
 calls CEL\_CHAIN which performs Rivara edge refinement and then
 another call to RECON.  In the case of 2-D surface grids, this is then
 followed by a call to CER\_CHAIN which is another edge refinement
 routine and then a final call to RECON if necessary.

 AGD3D will attempt to merge edges that are shorter than
 [merge\_length]{style="font-weight: bold;".  CEL\_CHAIN will attempt
 to bisect edges that are longer than
 [bisection\_length]{style="font-weight: bold;".  For 2-D surface
 grids, CER\_CHAIN will attempt to bisect edges that deviate from an
 averaged surface normal ("have a roughness of") greater than
 [tolroughness]{style="font-weight: bold;". RECON will attempt to
 create 'nice' elements by using face swapping.  (The LaGriT command
 [MODE]{style="font-weight: bold;"/[RECON]{style="font-weight: bold;"
 can alter the meaning of 'nice'.  The default is to reconnect to
 restore the delaunay criterion.  Calling
 [MODE]{style="font-weight: bold;"/[RECON/GEOM
 ]{style="font-weight: bold;"prior to the [MASSAGE
 ]{style="font-weight: bold;"call will create 'plumper' elements). 
 SGD will attempt to improve element aspect ratios by moving nodes.

 The actions of [MASSAGE]{style="font-weight: bold;" are controlled by
 values of these four parameters:


 -   [bisection\_length]{style="font-weight: bold;"  - edge length
     that will trigger bisection.
 -   [merge\_length]{style="font-weight: bold;" - edge length that
     will trigger merging.
 -   [toldamage]{style="font-weight: bold;" - maximum grid deformation
     of interfaces and external boundaries allowed in a single merge,
     smooth or reconnection event.

 -   [tolroughness]{style="font-weight: bold;" - (for 2D surface grids
     only)  measure of grid roughness (deviation from average surface
     normal) that triggers refinement.


 [toldamage]{style="font-weight: bold;" is a parameter which controls
 how much the grid will be deformed.  The 'damage' is a measure of how
 much interfaces and external boundaries are deformed.  Roughly, it
 measures the depths of 'dents' that are invariably introduced when
 nodes are moved, annihilated, and faces are swapped. We guarantee that
 the damage of any single node movement, node annihilation, or face
 swap is bounded by [toldamage]{style="font-weight: bold;". So if
 [toldamage]{style="font-weight: bold;" is set to an extremely small
 number, one can expect hardly any node movements, annihilations, or
 face swaps will be allowed.  Conversely, if
 [toldamage]{style="font-weight: bold;" is set too large, physical
 interfaces may be significantly deformed by the action of
 **MASSAGE**.  Experience has shown that setting
 [toldamage]{style="font-weight: bold;" equal to approximately .01
 times the diameter of the mesh frequently gives acceptable results.  

 The guidelines for selecting
 [bisection\_length]{style="font-weight: bold;",
 [merge\_length]{style="font-weight: bold;",
 [toldamage]{style="font-weight: bold;" , and
 [tolroughness]{style="font-weight: bold;" are as follows. 
 [bisection\_length]{style="font-weight: bold;" should not be smaller
 than [merge\_length]{style="font-weight: bold;", or the action of
 merging nodes together will be largely pointless because the newer,
 longer edges created by merging will simply be bisected again.  In
 fact, merging all edges of length &gt;
 [merge\_length]{style="font-weight: bold;" together can easily create
 edges of length roughly 3
*[merge\_length]{style="font-weight: bold;"
 in the mesh.  Hence it is recommended that [bisection\_length
 ]{style="font-weight: bold;"be at least three times as large as merge
 length.

 Merges of edges of length &lt;=
 [merge\_length]{style="font-weight: bold;" are meant to coarsen the
 mesh, but are not meant to deform surfaces and material interfaces on
 this scale.  The amount of material/surface deformation
 ([toldamage]{style="font-weight: bold;") is meant to be considerably
 less than [merge\_length]{style="font-weight: bold;".

 On the other hand, the maximum roughness tolerated in the graph
 ([tolroughness]{style="font-weight: bold;") should be considerably
 more than [toldamage]{style="font-weight: bold;", or roughness
 refinement will be triggered by actions such as flipping or merging.

 Hence, our guidelines for selecting the parameters are:

 [bisection\_length]{style="font-weight: bold;" &gt;=
 3
*[merge\_length]{style="font-weight: bold;"&gt;&gt;
 [toldamage]{style="font-weight: bold;"

 [tolroughness]{style="font-weight: bold;" &gt;=
 10
*[toldamage]{style="font-weight: bold;"  (for 2-D surface grids).

 For example, for a grid with diameter of order three, we have used:

 [bisection\_length]{style="font-weight: bold;",
 [merge\_length]{style="font-weight: bold;",
 [toldamage]{style="font-weight: bold;",
 [tolroughness]{style="font-weight: bold;" =.3, .1, .01, .1

 If one of {[bisection\_length]{style="font-weight: bold;",
 [merge\_length]{style="font-weight: bold;" is omitted, the omitted
 one will be set so that
 [bisection\_length]{style="font-weight: bold;"=3
*[merge\_length]{style="font-weight: bold;".

 If they are both omitted, they will both be taken to be infinity.

 If [toldamage]{style="font-weight: bold;" is not specified, no node
 annihilation will take place.

 If [tolroughness]{style="font-weight: bold;" is not specified, no
 refinement on roughness will occur and thus the format is compatible
 with old decks where refinement on roughness did not occur.

 The final, optional keywork argument(s) can be one or more of
 [nosmoot]{style="font-weight: bold;"h, norecon, [lite, ignoremats,
 strictmergelength, checkaxy,
 semiexclusive]{style="font-weight: bold;", and
 [exclusive]{style="font-weight: bold;". 


<div style="margin-left: 40px;">

-   Specifying [nosmooth]{style="font-weight: bold;" will turn off the
    'smooth' step by skipping the call to SGD.
-   Specifying [norecon]{style="font-weight: bold;" will turn off all
    'recon' steps.
-   If [lite]{style="font-weight: bold;" is specified, only one
    iteration of the merging/reconnection/smoothing loop is executed,
    and a reconnection after edge refinement is omitted.  This is
    suitable for applications, such as Gradient Weighted Moving Finite
    Elements, where **MASSAGE** is called repeatedly.
-   The optional argument [ignoremats]{style="font-weight: bold;"
    causes **MASSAGE** to process the multimaterial mesh in a single
    material mode; it ignores the material interfaces. 
-   The optional argument
    [strictmergelength]{style="font-weight: bold;" forces strict
    interpretation of [merge\_length]{style="font-weight: bold;" so
    that there is no merging along the edges of flat elements.  This is
    important if [ignoremats]{style="font-weight: bold;" is specified
    to avoid losing the interfaces.
-    If [checkaxy]{style="font-weight: bold;" is given, then we insure
    that for 2D meshes, the output mesh will have positive xy-projected
    triangle areas, provided that the input mesh had them in the first
    place. 
-   If [exclusive]{style="font-weight: bold;" is given, then edge
    refinement operations will only be performed on edges whose
    endpoints are *both* in the PSET that **MASSAGE** is working on. 
    (As usual, new nodes created by refinement are added to the PSET so
    that **MASSAGE** can refine edges recursively.)  The default
    behavior is 'inclusive', where only ONE edge endpoint has to belong
    to the PSET for the edge to be eligible for refinement.
-   If [semiexclusive]{style="font-weight: bold;" is given, refinement
    will only be triggered by edges with both endpoints in the PSET, but
    some edges with less than two endpoints in the PSET might be refined
    as part of a 'Rivara chain' triggered by the refinement of an edge
    with both endpoints in the PSET.  This represents an intermediate
    case between 'inclusive' and [exclusive]{style="font-weight: bold;"



 Note:  Since CEL\_CHAIN is called only once at the end of **MASSAGE**,
 it may be necessary to call **MASSAGE** twice for optimal results. 
 This is because annihilation of nodes is done with an intent to
 improve element aspect ratios, but cannot be effective if there are
 too few nodes initially.

 Note: The user may wish to issue a "**RMPOINT/COMPRESS**" after
 **MASSAGE** operations that merge a significant number of nodes.

 FORMAT:

 **massage**/bisection\_length/merge\_length/toldamage/
[[tolroughness]{style="font-family: monospace;"
]/
[ifirst,ilast,istride
]/

    
 
[**nosmooth**
]/
[**norecon**
]
[**strictmergelength**
]/
[**ignoremats**
]/
[**lite
]**/
[**checkaxy**
]/
[**semiexclusive**
]**/
[exclusive**
]

 

 EXAMPLES:

 **massage**/[0.3/0.1/0.01]{style="font-family: courier new,courier,monospace;"/

  Mesh edges longer than 0.3 will be bisected; mesh edges shorter than
 0.1 might be collapsed if that causes damage (normal surface motion)
 to material interfaces or external boundaries less than 0.01 ;
 smoothing of nodes causing damage less than 0.01 is allowed ; face
 swapping causing damage less than 0.01 is allowed.

 **massage**/[[0.3/0.1/0.01/0.1]{style="font-family: courier new,courier,monospace;"/]{style="font-family: courier;"

 Same as above but for 2-D surface meshes, roughness greater than 0.1
 will trigger refinement.

 **massage**/0.3/0.1/0.01/[]{style="font-family: courier;"**pset,get,**[pset1]{style="font-family: courier new,courier,monospace;"

 Mesh edges (containing at least one endpoint in pset1) longer than 0.3
 will be bisected; mesh edges shorter than 0.1 might be collapsed if
 that causes damage (normal surface motion) to material interfaces or
 external boundaries less than 0.01 and if the annihilated node is in
 pset1;  smoothing of nodes in pset1 causing damage less than 0.01 is
 allowed; face swapping causing damage less than 0.01 is allowed
 (unfortunately, LaGriT at this time does not restrict swapping to
 pset1).

 **massage**/0.3/0.1/0.01**/pset,get[,]{style="font-family: courier new,courier,monospace;"**[pset1]{style="font-family: courier new,courier,monospace;"**[/]{style="font-family: courier new,courier,monospace;"nosmooth**

  As above, but without smoothing.

 **massage**/1.e+20/0.1/0.1/1,0,0**/nosmooth**

 Because of the virtually infinite value of
 [bisection\_length]{style="font-weight: bold;",no edges will be
 bisected.  Since merge\_length=toldamage=0.1, merging of edges  of
 length less than 0.1 will be considered, and will not be rejected
 because of excessive damage.  Hence we expect that all edges of length
 less than 0.1 will be merged away (except in those cases where merging
 would invert tetrahedra or change material topology).   Because
 **nosmooth** is specified, no smoothing will take place.  Face
 swapping causing damage less than
 [toldamage]{style="font-weight: bold;" is allowed

 **massage**[/1.e+20/1.e-9/1.e-9/1,0,0]{style="font-family: courier new,courier,monospace;"**/nosmooth****/strictmergelength****/ignoremats**

 This set of arguments will remove degenerate elements from a mesh by
 merging nodes that have the same coordinate values ( within 1.e-9).

  



