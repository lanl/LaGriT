---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: REFINE
---

REFINE
------

 The **refine** command is used to create more elements. The method in
 which these new elements are formed is based on the refine\_option
 chosen. The refine criteria used in these methods are defined in the
 [Grid Refinement](http://lagrit.lanl.gov/REFINE1.md)
 Section.




COMMAND ARGUMENTS:

 The refinement choice is followed on the command line by options that
 are needed for the type of refinement chosen. See the details for each
 refine\_option for a description of parameters specific to the refine
 type. See examples below for various formats. In general the refine
 arguments include:

 **refine** / refine\_option / field / interpolation / refine\_type /
 range / xvalue

 / 
[ xvalue2 / xvalue3 / inclusive\_flag 
]



refine\_option: indicates the choice of refinement method. The choices
for first parameter are:

 -   **junction** will refine object where field crosses xvalue
 -   **constant** will refine object where field &gt; xvalue
 -   **delta** will refine object where delta(field) &gt; xvalue
 -   **lambda** will refine object where lambda(field) &lt; xvalue
 -   **maxsize** will refine object where object &gt; xvalue. Size
     refers to volume for tets, area for face, and length for edges.
 -   **aspect** will refine where aspect ratio &lt; xvalue
 -   **addpts** will refine explicitly by adding a set of nodes
 -   **rivara** edges longer than xvalue will be refined according to
     the principle that a refined edge candidate is the longest edge in
     any element that contains it. This results in a recursive
     refinement procedure that adds neighboring edges to the refinement
     candidate list until no neighbor edge is longer then the
     candidate. refine\_type must be **edge**. Arguments field and
     interpolation are ignored. This method of refinement, when used
     with a pset, produces a nicely graded mesh.
 -   **rivara\_boundary** applies the rivara algorithm, but only bisect
     edges on external boundaries.
 -   **rivera\_truncated** applies the rivara algorithm, but restricts
     the neighborhood search to the edges in the selected pset. If the
     pset is the entire mesh, this option has the same behavior as
     **rivara**.
 -   **roughness** will refine based on the distance of the endpoint of
     an edge to the plane determined by the synthetic normal with
     respect to a specified surface at the other endpoint of the edge.
     This is intended to increase refinement on surfaces near corners
     or around sharp bends in surfaces. xvalue is the distance, the
     surface name must follow the distance argument.
 -   **edge\_list** will bisect a set of edges specified by the node
     numbers of the endpoints of the edges. refine\_type must be
     **edge** followed by a list of end points making up the
     edge\_list.
 -   **element\_set** (or **eltset**) will refine all elements in a
     specified element set. The mesh object may be tri, quad, tet or
     hex.  Internally a node set will be created from the chosen
     elements.  Because of the conversion from element set to point
     set, it is possible that some element not in the original element
     set will have all of its nodes as members of the internally
     constructed points set and hence will be refined. 
     Refinement\_method is **constant**; refine\_type is **element**;
     inclusion\_flag is **exclusive**.  The element range
     **eltset,get,**ename is the only argument after
     **element\_set**.  
 -   **interface** will bisect a set of non-interface edges of tets all
     of whose vertices are interface nodes. Valid only for 3D
     tetrahedral grids and is useful to 'unlock' tetrahedra that are
     stuck because all of their vertices lie on interface surfaces. 
     After the refine operation these tetrahedral will be replaced by
     tetrahedra containing a vertex that is not on the surface - thus
     allowing later smooth or massage operations more freedom to
     improve the grid.

refine\_type specifies what object will be refined and how that object
will be refined:

 **element** in 3D will refine elements by placing a point in the
 center of the element.

      in 2D (triangle) will refine element by refining all edges of the
 triangle.

 **face** in 3D will refine facets by placing a point in the center of
 the facet.

      in 2D (triangle) will refine face by refining all edges of the
 face.

 **edge** will refine edges by placing a point on the midpoint of the
 edge.

 **faceedge** will refine facets by refining all edges of the facet.

 **tetedge** will refine elements by refining all edges of the element.

field must refer to a previously defined attribute of the current Mesh
Object.

interpolation specifies how to interpolate the field to give field
values to the new nodes created. The implemented values are:

 **linear**

 **log**

 **asinh**



range is the selection of points designated by node numbers for
ifirst,ilast,istride or **pset,get**,pname. **/1,0,0**/ will select all
nodes in the Mesh object.

xvalue 
[/xvalue2/xvalue3/
] is the real number usually indicating a
size for the different refine options. Most of the refine options do not
use the second and third values so their places will be empty **//**/.
See examples.

inclusion\_flag is an optional flag specifing if refinement is an
inclusive or an exclusive operation. By default, all operations are
**exclusive**. For **inclusive**, if an edge refinement is specified
restricted to a pset, then an edge is eligible for refinement if either
or both of the end points belong to the pset selected. If the
inclusion\_flag is **exclusive** then both end points must be in the
pset. The implemented values are:

 **inclusive**

 **exclusive**

QUADTREE and OCTREE REFINEMENT:

Quad and hexahedral elements may be refined creating quad tree and
octree meshes. Three new Mesh object attributes are added during this
operation. The refine\_type must be **element**. The refine\_option must
be **constant,** **junction** or **maxsize**. The values for
/xvalue/xvalue2/xvalue3/ should be /-1.,0.,0./. For an element set, use
the shortened syntax **refine/element\_set/eltset,get,**esetname.

The element attributes added to the Mesh object are:

 if **itetlev** &gt; 0, it is a new element and contains the level of
 refinement

 if **itetkid** &gt; 0, it is a parent and contains the number of the
 first child.

 if **itetpar** &gt; 0, it is a child and contains the number of the
 parent.

 Quad meshes will have 4 children for each refined element. Hex meshes
 will have 8 children. The children are generated sequentially; The
 first child will contain the first local node of the parent element,
 the other elements are created in the order shown in this diagram.

 ![octree refinement](dsquare.gif">



 

 One can control refinement so that a hex is broken into either 8, 4 or
 2 elements and a quad is broken into either 4 or 2 elements. This is
 controlled with the principal refine direction choice (prd\_choice)
 parameter. This syntax works assuming imt values are greater or equal
 to zero with principal refine direction chosen through a combination
 of "123" prd\_choice indicators as defined below. The command line
 used is:

 refine/constant/itetclr/linear/element/1,0,0/-1.,0.,0./exclusive/amr
 **prd\_choice**

 or with element selection (based on pset and inclusive/exclusive
 options):
 refine/constant/imt1/linear/element**/pset,get,pname**/-1.,0.,0.**/inclusive**/
 amr **prd\_choice**

 **prd\_choice** indicates the chosen principal refinement direction
 based on the local hex element topology as defined by edge numbers,
 for instance, quad edge 1 is in the x direction relative to the local
 topology.

 = 1 refine along x direction, 1 hex-&gt;2 hex, 1 quad-&gt;2 quad (quad
 edges 1 and 4)

 = 2 refine along y direction, 1 hex-&gt;2 hex, 1 quad-&gt;2 quad (quad
 edges 2 and 3)

 = 3 refine along z direction, 1 hex-&gt;2 hex, 1 quad-&gt;4 quad

 = 12 refine along x and y direction, 1 hex-&gt;4 hex, 1 quad-&gt;4
 quad

 = 13 refine along x and z direction, 1 hex-&gt;4 hex, 1 quad-&gt;4
 quad

 = 23 refine along y and z direction, 1 hex-&gt;4 hex, 1 quad-&gt;4
 quad

 = 123 refine xyz with prd amr routines, 1 hex-&gt;8 hex, 1 quad-&gt;4
 quad

 = 0 refine xyz with default amr refinement, 1 hex-&gt;8 hex, 1
 quad-&gt;4 quad




FORMATS:

 **refine**/refine\_option/ 
[field
]/ 
[interpolation
]/refine\_type
 /ifirst, ilast, istride/xrefine/yrefine/zrefine/inclusive\_flag/

 **refine/roughness///edge**/ifirst,ilast,istride/distance/surface\_name**/exclusiveinclusive**

 **refine/edge\_list///edge**/edge\_list/

 **refine/interface/// edge/pset,get**,psetname

 **refine/element\_set / eltset,get**,esetname

 **refine/eltset / eltset,get**,esetname

EXAMPLES:

 **refine****/maxsize**//**/edge****/pset,get,**something / .25
 will refine element where edge is longer than .25
 **refine****/constant**/concentration**/log****/edge**/
 1,0,0/25.0//**/inclusive**
 will refine where concentration is greater than 25.
 **refine****/addpts**//**/tet****/pset,get,**newpoints/
 refine explicitly by adding the new nodes in the set newpoints
 **refine****/rivara**//**/edge/pset,get,**p1/.5//**/inclusive**
 refine all edges containing at least one node in pset p1 that are
 longer than .5. Using the 'rivera' algorithm may result in edges not
 containing nodes in the pset to be refined.
 **refine/rivara\_truncated///edge/pset,get,p1**/.5//**/exclusive**
 rivera\_truncated, exclusive will refine only edges both of whose
 endpoints are in the selected pset named p1
 **refine/rivara\_boundary///edge/1,0,0**/.25
 rivara\_boundary will only refine boundary edges.
 **refine/roughness///edge/1,0,0**/.28/ptop**/inclusive**
 will refine based on .28 distance to the surface named ptop.
 **refine/edge\_list///edge**/1 2 23 47
 will refine the edge with endpoints 1 and 2 AND the edge with
 endpoints 23 and 47.
 **eltset** / elem3 / id\_elem1 / **eq** / 3

 **refine/eltset** / **eltset,get**, elem3
 will create a node set from the element set named elem3 and refine
 using the constant option.
 **refine/constant**/imt1/linear**/element/pset,get,**pbox
 /-1.,0.,0.**/inclusive**
 create a quadtree refined quad mesh
 **eltset** / elm2 / itetclr / **eq** / 2

 **pset**/ pelm2 / **eltset** elm2

 **refine/constant/imt1/linear/element/pset,get,**pelm2**/-1.,0.,0.**/inclusive**/amr**
 12
 refine the material 2 elements of a hex mesh , do not refine in the
 vertical direction
 **refine/constant/imt1/linear/element/pset,get,**pelm2**/-1.,0.,0.**/inclusive**/amr**
 3
 refine the material 2 elements of a hex mesh , refine only in the
 vertical direction
