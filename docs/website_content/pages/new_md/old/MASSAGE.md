---
GENERATOR: 'Mozilla/4.8 [en] (X11; U; SunOS 5.8 sun4u) [Netscape]'
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
 MERGE\_LENGTH.  CEL\_CHAIN will attempt to bisect edges

 that are longer than BISECTION\_LENGTH.  For 2-D surface grids,
 CER\_CHAIN will attempt to bisect edges that deviate from an averaged
 surface normal ("have a roughness of") greater than TOLROUGHNESS.
 **RECON** will attempt to create 'nice' elements by using face
 swapping.  (The value of the LaGriT IVORONOI parameter determines the
 meaning of 'nice'.)  SGD will attempt to improve element aspect ratios
 by moving nodes.

 To ensure that the actions of AGD3D, RECON, and SGD are controlled and
 harmonious, the parameter TOLDAMAGE exists, and there are guidelines
 for choosing the relative values of BISECTION\_LENGTH, MERGE\_LENGTH,
 TOLDAMAGE, and TOLROUGHNESS.

 TOLDAMAGE is a parameter which controls how much the grid will be
 deformed by AGD3D, CEL\_CHAIN, RECON, and SGD.  The 'damage' is a
 measure of how much interfaces and external boundaries are deformed. 
 Roughly, it measures

 the depths of 'dents' that are invariably introduced when nodes are
 moved, annihilated, and faces are swapped.

 We guarantee that the damage of any single node movement, node
 annihilation, or face swap is bounded by TOLDAMAGE.

 So if TOLDAMAGE is set to an extremely small number, one can expect
 hardly any node movements, annihilations, or face swaps will be
 allowed.  Conversely, if TOLDAMAGE is set too large, physical
 interfaces may be significantly deformed by the action of
 **MASSAGE**.  Experience has shown that setting TOLDAMAGE equal to
 approximately .01 times the diameter of the mesh frequently gives
 acceptable results.

 The guidelines for selecting BISECTION\_LENGTH, MERGE\_LENGTH,
 TOLDAMAGE, and TOLROUGHNESS are as follows.  BISECTION\_LENGTH should
 not be smaller than MERGE\_LENGTH, or the action of merging nodes
 together will be largely pointless because the newer, longer edges
 created by merging will simply be bisected again.  In fact, merging
 all edges of length &gt; MERGE\_LENGTH together can easily create
 edges of length roughly 3
*MERGE\_LENGTH in the mesh.  Hence it is
 recommended that BISECTION\_LENGTH be at leastthree times as large as
 merge length.

 Merges of edges of length &lt;= MERGE\_LENGTH are meant to coarsen the
 mesh, but are not meant to deform surfaces

 and material interfaces on this scale.  The amount of material/surface
 deformation (TOLROUGHNESS) is meant to be considerably less than
 MERGE\_LENGTH.

 On the other hand, the maximum roughness tolerated in the graph
 (TOLROUGHNESS) should be considerably more than TOLDAMAGE, or
 roughness refinement will be triggered by actions such as flipping or
 merging.

 Hence, our guidelines for selecting the parameters are

 BISECTION\_LENGTH &gt;= 3
*MERGE\_LENGTH &gt;&gt; TOLDAMAGE

 AND

 TOLROUGHNESS &gt;= 10
*TOLDMAMAGE  (for 2-D surface grids).

 For example, for a grid with diameter of order three, we have used

 BISECTION\_LENGTH, MERGE\_LENGTH, TOLDAMAGE, TOLROUGHNESS =.3, .1,
 .01, .1

 If one of {BISECTION\_LENGTH, MERGE\_LENGTH is omitted, the omitted
 one will be set so that BISECTION\_LENGTH=3
*MERGE\_LENGTH.

 If they are both omitted, they will both be taken to be infinity.

 If TOLDAMAGE is not specified, no node annihilation will take place.

 IF TOLROUGHNESS is not specified, no refinement on roughness will
 occur and thus the format is compatible with old decks where
 refinement on roughness did not occur.

 The final, optional argument can be one or more of **NOSMOOTH**,
 **LITE, IGNOREMATS, STRICTMERGELENGTH**, **CHECKAXY**,
 **SEMIEXCLUSIVE**, and **EXCLUSIVE.  NOSMOOTH** causes **MASSAGE** to
 deviate from the above description, in that node smoothing by SGD is
 not performed.  If **LITE** is specified, only one iteration of the
 merging/reconnection/smoothing loop is executed, and a reconnection
 after edge refinement is

 omitted.  This is suitable for applications, such as Gradient Weighted
 Moving Finite Elements, where **MASSAGE** is called repeatedly.The
 optional argument **IGNOREMATS** causes **MASSAGE** to process the
 multimaterial mesh in a single material mode; it ignores the material
 interfaces.  The optional argument **STRICTMERGELENGTH** forces strict
 interpretation of MERGE\_LENGTH so that there is no merging along the
 edges of flat elements.  This is important if **IGNOREMATS** is
 specified to avoid losing the interfaces.

 If **CHECKAXY** is given, then we insure that for 2D meshes, the
 output mesh will have positive xy-projected triangle areas,

 provided that the input mesh had them in the first place.   If
 **EXCLUSIVE** is given, then edge refinement operations will only

 be performed on edges whose endpoints are *both* in the PSET that
 **MASSAGE** is working on.  (As usual, new nodes created by refinement
 are added to the PSET so that **MASSAGE** can refine edges
 recursively.)  The default behavior is 'inclusive', where only ONE
 edge endpoint has to belong to the PSET for the edge to be eligible
 for refinement. If **SEMIEXCLUSIVE** is given, refinement will only be
 triggered by edges with both endpoints in the PSET, but some edges
 with less than

 two endpoints in the PSET might be refined as part of a 'Rivara chain'
 triggered by the refinement of an edge with both

 endpoints in the PSET.  This represents an intermediate case between
 'inclusive'  and **EXCLUSIVE**.

 Note:  Since CEL\_CHAIN is called only once at the end of **MASSAGE**,
 it may be necessary to call **MASSAGE** twice for optimal results. 
 This is because annihilation of nodes is done with an intent to
 improve element aspect ratios, but cannot be effective if there are
 too few nodes initially.

 Note: The user may wish to issue a "**RMPOINT/COMPRESS**" after
 **MASSAGE** operations that merge a significant number of nodes.

 **FORMAT:**

 **massage**/bisection\_length/merge\_length/toldamage/[tolroughness]/[ifirst,ilast,istride]/

    
 [**nosmooth**]/[**strictmergelength**]/[**ignoremats**]/[**lite]**/[**checkaxy**]/[**semiexclusive**]**/[exclusive**]

 

 **EXAMPLES:**

 **massage**/0.3/0.1/0.01/

  Mesh edges longer than 0.3 will be bisected; mesh edges shorter than
 0.1 might be collapsed if that causes damage (normal surface motion)
 to material interfaces or external boundaries less than 0.01 ;
 smoothing of nodes causing damage less than 0.01 is allowed ; face
 swapping causing damage less than 0.01 is allowed.

 **massage**/0.3/0.1/0.01/0.1/

 Same as above but for 2-D surface meshes, roughness greater than 0.1
 will trigger refinement.

 **massage**/0.3/0.1/0.01**/pset,get,**pset1

  Mesh edges (containing at least one endpoint in pset1) longer than
 0.3 will be bisected; mesh edges shorter than 0.1 might be collapsed
 if that causes damage (normal surface motion) to material interfaces
 or external boundaries less than 0.01 and if the annihilated node is
 in pset1;  smoothing of nodes in pset1 causing damage less than 0.01
 is allowed; face swapping causing damage less than 0.01 is allowed
 (unfortunately, LaGriT at this time does not restrict swapping to
 pset1).

 ** massage**/0.3/0.1/0.01**/pset,get,**pset1**/nosmooth**

   As above, but without smoothing.

 **massage**/1.e+20/0.1/0.1/1,0,0**/nosmooth**

 Because of the virtually infinite value of bisection\_length, no edges
 will be bisected.  Since merge\_length=toldamage=0.1, merging of
 edges  of length less than 0.1 will be considered, and will not be
 rejected because of excessive damage.  Hence we expect that all edges
 of length less than 0.1 will be merged away (except in those cases
 where merging would invert tetrahedra or change material topology).  
 Because **nosmooth** is specified, no smoothing will take place.  Face
 swapping causing damage less than toldamage is allowed

 **massage**/1.e+20/1.e-9/1.e-9/1,0,0**/nosmooth** **/strictmergelength** **/ignoremats**

 This set of arguments will remove degenerate elements from a mesh by
 merging nodes that have the same coordinate values ( within 1.e-9).


