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
 **merge\_length**.  CEL\_CHAIN will attempt
 to bisect edges that are longer than
 **bisection\_length** .  For 2-D surface
 grids, CER\_CHAIN will attempt to bisect edges that deviate from an
 averaged surface normal ("have a roughness of") greater than
 **tolroughness**. RECON will attempt to
 create 'nice' elements by using face swapping.  (The LaGriT command **MODE** **/RECON**
 can alter the meaning of 'nice'.  The default is to reconnect to
 restore the delaunay criterion.  Calling
 **MODE** / **RECON/GEOM** prior to the **MASSAGE* call will create 'plumper' elements). 
 SGD will attempt to improve element aspect ratios by moving nodes.

 The actions of **MASSAGE** are controlled by
 values of these four parameters:

 -   [bisection\_length] - edge length
     that will trigger bisection.
 -   [merge\_length] - edge length that
     will trigger merging.
 -   [toldamage] - maximum grid deformation
     of interfaces and external boundaries allowed in a single merge,
     smooth or reconnection event.

 -   [tolroughness] - (for 2D surface grids
     only)  measure of grid roughness (deviation from average surface
     normal) that triggers refinement.


 **bisection\_length** can either be a scalar value or a node field (a
 node attribute). In the first case, the algorithm directly compares
 the edge length to the **bisection\_length** value. If the edge length
 is greater than the **bisection\_length**, the edge will be refined.
 In the second case, the algorithm compares the edge length to the
 minimum value of the field at the two nodes incident to this edge. If
 the edge lenght is greater than this minimum value, the edge will be
 refined. Thus, one should put a minimum floor value (probably equal to
 twice the desired minimum edge lenth) for the field. Otherwise the
 code will refine indefinitely. For an example of an appropriate field,
 see **MASSAGE2** at the end.


 [toldamage] is a parameter which controls
 how much the grid will be deformed.  The 'damage' is a measure of how
 much interfaces and external boundaries are deformed.  Roughly, it
 measures the depths of 'dents' that are invariably introduced when
 nodes are moved, annihilated, and faces are swapped. We guarantee that
 the damage of any single node movement, node annihilation, or face
 swap is bounded by [toldamage]. So if
 [toldamage] is set to an extremely small
 number, one can expect hardly any node movements, annihilations, or
 face swaps will be allowed.  Conversely, if
 [toldamage] is set too large, physical
 interfaces may be significantly deformed by the action of
 **MASSAGE**.  Experience has shown that setting
 [toldamage] equal to approximately .01
 times the diameter of the mesh frequently gives acceptable results.  

 The guidelines for selecting
 [bisection\_length],
 [merge\_length],
 [toldamage] , and
 [tolroughness] are as follows. 
 [bisection\_length] should not be smaller
 than [merge\_length], or the action of
 merging nodes together will be largely pointless because the newer,
 longer edges created by merging will simply be bisected again.  In
 fact, merging all edges of length &gt;
 [merge\_length] together can easily create
 edges of length roughly 3
*[merge\_length]
 in the mesh.  Hence it is recommended that [bisection\_length
 ]be at least three times as large as merge
 length.

 Merges of edges of length &lt;=
 [merge\_length] are meant to coarsen the
 mesh, but are not meant to deform surfaces and material interfaces on
 this scale.  The amount of material/surface deformation
 ([toldamage]) is meant to be considerably
 less than [merge\_length].

 On the other hand, the maximum roughness tolerated in the graph
 ([tolroughness]) should be considerably
 more than [toldamage], or roughness
 refinement will be triggered by actions such as flipping or merging.

 Hence, our guidelines for selecting the parameters are:

 [bisection\_length] &gt;=
 3
*[merge\_length]&gt;&gt;
 [toldamage]

 [tolroughness] &gt;=
 10
*[toldamage]  (for 2-D surface grids).

 For example, for a grid with diameter of order three, we have used:

 [bisection\_length],
 [merge\_length],
 [toldamage],
 [tolroughness] =.3, .1, .01, .1

 If one of {[bisection\_length],
 [merge\_length] is omitted, the omitted
 one will be set so that
 [bisection\_length]=3
*[merge\_length].

 If they are both omitted, they will both be taken to be infinity.

 If [toldamage] is not specified, no node
 annihilation will take place.

 If [tolroughness] is not specified, no
 refinement on roughness will occur and thus the format is compatible
 with old decks where refinement on roughness did not occur.

 The final, optional keywork argument(s) can be one or more of
 [nosmoot]h, norecon, [lite, ignoremats,
 strictmergelength, checkaxy,
 semiexclusive], and
 [exclusive]. 


-   Specifying [nosmooth]will turn off the
    'smooth' step by skipping the call to SGD.
-   Specifying [norecon] will turn off all
    'recon' steps.
-   If [lite] is specified, only one
    iteration of the merging/reconnection/smoothing loop is executed,
    and a reconnection after edge refinement is omitted.  This is
    suitable for applications, such as Gradient Weighted Moving Finite
    Elements, where **MASSAGE** is called repeatedly.
-   The optional argument [ignoremats]
    causes **MASSAGE** to process the multimaterial mesh in a single
    material mode; it ignores the material interfaces. 
-   The optional argument[strictmergelength] forces strict
    interpretation of [merge\_length] so
    that there is no merging along the edges of flat elements.  This is
    important if [ignoremats] is specified
    to avoid losing the interfaces.
-    If [checkaxy]is given, then we insure
    that for 2D meshes, the output mesh will have positive xy-projected
    triangle areas, provided that the input mesh had them in the first
    place. 
-   If [exclusive] is given, then edge
    refinement operations will only be performed on edges whose
    endpoints are *both* in the PSET that **MASSAGE** is working on. 
    (As usual, new nodes created by refinement are added to the PSET so
    that **MASSAGE** can refine edges recursively.)  The default
    behavior is 'inclusive', where only ONE edge endpoint has to belong
    to the PSET for the edge to be eligible for refinement.
-   If [semiexclusive] is given, refinement
    will only be triggered by edges with both endpoints in the PSET, but
    some edges with less than two endpoints in the PSET might be refined
    as part of a 'Rivara chain' triggered by the refinement of an edge
    with both endpoints in the PSET.  This represents an intermediate
    case between 'inclusive' and [exclusive]



 Note:  Since CEL\_CHAIN is called only once at the end of **MASSAGE**,
 it may be necessary to call **MASSAGE** twice for optimal results. 
 This is because annihilation of nodes is done with an intent to
 improve element aspect ratios, but cannot be effective if there are
 too few nodes initially.

 Note: The user may wish to issue a "**RMPOINT/COMPRESS**" after
 **MASSAGE** operations that merge a significant number of nodes.

 **FORMAT:**

 **massage**/bisection\_length/merge\_length/toldamage/[[tolroughness]]/[ifirst,ilast,istride]/

    
 [**nosmooth**]/[**norecon**][**strictmergelength**]/[**ignoremats**]/[**lite]**/[**heckaxy**]/[**semiexclusive**]**/[exclusive**]

 

 **EXAMPLES:**

 **massage**/[0.3/0.1/0.01]

  Mesh edges longer than 0.3 will be bisected; mesh edges shorter than
 0.1 might be collapsed if that causes damage (normal surface motion)
 to material interfaces or external boundaries less than 0.01 ;
 smoothing of nodes causing damage less than 0.01 is allowed ; face
 swapping causing damage less than 0.01 is allowed.

 **massage**/[H\_SCALE/0.1/0.01]/

  Same as above, except that the **bisection\_length** is a node field
 called H\_SCALE in this case.

 **massage**/[[0.3/0.1/0.01/0.1]

 Same as above but for 2-D surface meshes, roughness greater than 0.1
 will trigger refinement.

 **massage**/0.3/0.1/0.01**/pset,get,**[pset1]
 
 Mesh edges (containing at least one endpoint in pset1) longer than 0.3
 will be bisected; mesh edges shorter than 0.1 might be collapsed if
 that causes damage (normal surface motion) to material interfaces or
 external boundaries less than 0.01 and if the annihilated node is in
 pset1;  smoothing of nodes in pset1 causing damage less than 0.01 is
 allowed; face swapping causing damage less than 0.01 is allowed
 (unfortunately, LaGriT at this time does not restrict swapping to
 pset1).

 **massage**/0.3/0.1/0.01**/pset,get**,pset1,**nosmooth**

 
 As above, but without smoothing.

 **massage**/1.e+20/0.1/0.1/1,0,0**/nosmooth**

 Because of the virtually infinite value of
 [bisection\_length],no edges will be
 bisected.  Since merge\_length=toldamage=0.1, merging of edges  of
 length less than 0.1 will be considered, and will not be rejected
 because of excessive damage.  Hence we expect that all edges of length
 less than 0.1 will be merged away (except in those cases where merging
 would invert tetrahedra or change material topology).   Because
 **nosmooth** is specified, no smoothing will take place.  Face
 swapping causing damage less than
 [toldamage] is allowed

 **massage**[/1.e+20/1.e-9/1.e-9/1,0,0]**/nosmooth** **/strictmergelength** **/ignoremats**

 This set of arguments will remove degenerate elements from a mesh by
 merging nodes that have the same coordinate values ( within 1.e-9).

