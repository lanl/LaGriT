**a. Mesh Object Definition**

The data structure which contains the information necessary to define a
mesh is called a Mesh Object. A Mesh Object consists of attributes.
There is a default template for a Mesh Object composed of the following
attributes:

**name** (character
*32 -- mesh object name)

**scalar** (integer -- defined to have value 1)

**vector** (integer -- defined to have value 3)

**nnodes** (integer -- number of nodes in the mesh)

**nelements** (integer -- number of elements in the mesh,
e.g. triangles, tetrahedra)

**nfaces** (integer -- number of unique topological facets in the mesh,
e.g. number of edges in 2D or number of element faces in 3D) -- (not set
or maintained by LaGriT; may be set and maintained by the user)

**nedges** (integer -- number of unique edges in mesh) -- (not set or
maintained by LaGriT; may be set and maintained by the user)

**mbndry** (integer -- value signifying that if the node
er is greater that mbndry then the node is a boundary node) --
(default 16000000) (must be greater than 48
*nnodes and may be reset by

**[connect](commands/CONNECT1.md)**) (for an example of usage see[Section III.d](meshobjcon.md#mbndry))

**ndimensions\_topo** (integer -- topological
dimensionality,1, 2 or 3, i.e. a non-planar surface would have
ndimensions\_topo = 2 and ndimensions\_geom = 3.)

**ndimensions\_geom** (integer -- 1, 2 or 3 for
dimension of geometry) (default 3)

**nodes\_per\_element** (integer -- value dependent on type of mesh;
e.g. for tetrahedral mesh the value will be 4)

**edges\_per\_element** (integer -- value dependent on type of mesh;
e.g. for tetrahedral mesh the value will be 6)

**faces\_per\_element** (integer -- topological number of facets per
element (i.e. in 1D this number is always 2, for 2D use the number of
edges of the element, for 3D use the number of faces of the element.--(
value dependent on type of mesh; e.g. for tetrahedral mesh the value
will be 4))

**isetwd** (integer array containing pset membership
information, see **[pset](commands/PSET.md)** command definition)

**ialias** (integer array of alternate node numbers, i.e. for merged
points)

**imt1** (integer array of node material)

**itp1** (integer array of node type if type &gt; 20 node will
be invisible)

 

point type | name | description
----------- | --- | ------------
0 |       int |  Interior
2 |          ini | Interface
3 |          vrt |  Virtual
4    |       vin  |  Virtual + interface
8 |         vif |  Virtual + interface + free
9    |       alb |  Virtual + Interface + free + reflective
10    |      rfl | Reflective boundary node 
11    |      fre |  Free boundary node 
12    |      irb |  Interface node on reflective boundary
13    |      ifb |  Interface node on free boundary
14    |      rfb |  Node on intersection of free boundary and reflective boundary
15 |         irf | Interface node on intersection of free boundary and reflective boundary
16    |     vrb |    Virtual node on reflective boundary
17    |      vfb |  Virtual node on free boundary
18    |     vrf |  Virtual node on free + reflective boundary
19    |      vir |  Virtual + interface node on reflective boundary
20    |      mrg |  Merged node
21    |      dud | Dudded node
41    |     par |  Parent node


**icr1** (integer array of constraint numbers for nodes; the
value of this array is an index into the **[icontab](#icontab)** table
of node constraints described later in this section)

**isn1** (integer array of child, parent node correspondence)

Points on material interfaces are given point type 41 (parent). One
child point is spawned for each material meeting at the parent point.
The isn1 field of the parent point will contain the point number of the
first child point. The isn1 field of the first child will contain the
point number of the next child. The isn1 field of the last child will
contain the point number of the parent. The point types of the child
points will be 2, 12, 13, 15 or 19 depending on whether the interface
point is also on an exterior boundary. This parent, child relationship
is established by the **settets** command.

**xic**, **yic**, **zic** (real arrays of node coordinates)

**itetclr** (integer array of element material)

**itettyp** (element shape)  (for an example of usage see[Section III.d](meshobjcon.md#itettyp))

 

  name |     value | description
  ----------- | --- | ------------
  ifelmpnt |  1 |     point
  ifelmlin |  2 |     line
  ifelmtri |  3    |  triangle 
  ifelmqud |   4    |  quadrilateral 
  ifelmtet  |  5    |  tetrahedron 
  ifelmpyr | 6   |  pyramid
  ifelmpri | 7   |  prism 
  ifelmhex | 8   |  hexahedron 
  ifelmhyb | 9 |     hybrid
  ifelmply | 10    | polygon


**[See supported element types.](supported.md)**


**xtetwd** (real array containing eltset membership information, see
eltset command definition )

**itetoff** (index into itet array for an element)  (for an example of
usage see [Section III.d](meshobjcon.md#itetoff))

**jtetoff** (index into jtet array for an element)  (for an example of
usage see [Section III.d](meshobjcon.md#jtetoff))

**itet** (integer array of node vertices for each element)  (for an
example of usage see [Section III.d](meshobjcon.md#itet))

**jtet** (integer array of element connectivity)  (for an example of
usage see [Section III.d](meshobjcon.md#jtet))

**ipolydat** (character default **yes**) flag to

**[dump/gmv](commands/DUMP2.md)**to output polygon data

**vor2d ** (character default **yes**) flag to

**[dump/gmv](commands/DUMP2.md)** to output voronoi cells and median
mesh cells for 2D meshes.

**vor3d ** (character default **no**) flag to

**[dump/gmv](commands/DUMP2.md)** to output voronoi cells and median
mesh cells for 3D meshes.

**epsilon** (real) value of machine epsilon which will be calculated by
the code.

**epsilonl** (real) value of smallest edge that the code can distinguish
will be set internally by the code (see[setsize](commands/SETSIZE.md)).

**epsilona** (real) value of smallest area that the code can distinguish
will be set internally by the code.

**epsilonv** (real) value of smallest volume that the code can
distinguish will be set internally by the code.

**ipointi** (integer) node number of the first node of the last set of
nodes generated, used by  the 0,0,0 **[pset](commands/PSET.md)**
syntax

**ipointj**(integer) node number of the last node of the last set of
nodes generated, used by  the 0,0,0 **[pset](commands/PSET.md)**
syntax

**idebug** (integer) debug flag values greater than 0 produce increasing
levels of output.

**itypconv\_sm** (integer)

**maxiter\_sm** (integer default 25) number of smoothing iterations in
the [smooth](commands/SMOOTH.md)and [radapt](commands/RADAPT.md)
routines.

**tolconv\_sm** (real)

**nnfreq** (integer default 1) flag to control reconnection after[refine](commands/REFINE.md) - set to zero to turn off reconnection.

**ivoronoi** (integer default 1) flag to control reconnection criterion
:

+1 means restore delaunay

-2 means improve geometric quality of the elements

+2 means adaptive reconnection with user supplied routine (see[recon)](commands/RECON.md)

+5 means disable all reconnection


**iopt2to2** (integer default 2) flag to contol boundary flips during
reconnection (see [recon)](commands/RECON.md):

0=exterior boundaries

1=interfaces

2=exterior boundaries and interfaces

3=all


**dumptype** (character default binary) Type of gmv file to write.

**velname** (character default vels) Name of velocity attribute.

**densname** (character default ric) Name of density attribute.

**presname** (character default pic) Name of pressure attribute.

**enername** (character default eic) Name of energy attribute.

**xmin** (real default 1) minimum x coordinate of nodes in mesh set by[setsize](commands/SETSIZE.md))

**xmax** (real default 1) maximum x coordinate of nodes in mesh set by[setsize](commands/SETSIZE.md))

**ymin** (real default 1) minimum y coordinate of nodes in mesh set
by[setsize](commands/SETSIZE.md))

**ymax** (real default 1) maximum y coordinate of nodes in mesh set by[setsize](commands/SETSIZE.md))

**zmin** (real default 1) minimum z coordinate of nodes in mesh set by[setsize](commands/SETSIZE.md))

**zmax** (real default 1) maximum z coordinate of nodes in mesh set by[setsize](commands/SETSIZE.md))

**kdtree\_level** (integer default 0) resolution level
of[kdtree](commands/kdtree.md)-- 0 means terminal nodes contain 1
member.

**max\_number\_of\_sets** (integer default 32) number
of[pset](commands/PSET.md)and [eltsets](commands/ELTSET2.md) allowed
- currently restricted to 32.

**number\_of\_psets** (integer) number of defined psets in the mesh.

**number\_of\_eltsets** (integer) number of defined eltsets in the
mesh.

**geom\_name** (character default -defaultgeom-) name of geometry
associated with this mesh.  (see [geom)](commands/cmo/cmo_geom.md)

 

The current state of a mesh object can be displayed by the[**cmo/status**](commands/cmo/cmo_status.md)command

Note: Many commands and the cmo\_get\_info subroutine accept **itp** as
equivalent to **itp1; icr** to **icr1, isn** to **isn1**; **imt** to

**imt**1. The user should never add an attribute whose name is

**itp,imt,icr,isn.**

 

The default Mesh Object can be expanded by adding user defined
attributes (see [**cmo****/addatt**](commands/cmo/cmo_addatt.md)).

 

The value of parameters can be changed by the cmo/setatt command.

(e.g. **[cmo/setatt/](commands/cmo/cmo_setatt.md)**/epsilonl/1.d-9)

icontabLaGriT will add attributes to the mesh object in certain
instances. For example, if there are any constrained surfaces, reflect,
virtual or intrcons types, the following attributes are added to the
mesh object:

 
    
Command | Description
--- | ---          
NCONBND   |            number of combinations of constrained surfaces 
ICONTAB(50,NCONBND)   
ICONTAB(1,i)    |      number of surfaces contributing to the ith constraint 
ICONTAB(2,i)   |      degree of freedom of the ith constraint 
ICONTAB(2+j,i) |       Surface number of the jth surface contributing to the ith constraint

In order to determine which constraint entry applies to node ip,
retrieve the value

i=icr1 (ip), i.e. ICONTAB(1, icr1(ip)) gives the number of surfaces that
ip is 
`on'.

If icr1(ip) is zero there is no constraint on that node.  The number of
the surfaces that

ip is 'on' are stored in ICONTAB(3,icrl(ip))...ICONTAB(2 +
ICONTAB(1,icr1(ip))).

Command | Description
--- | ---
TENSOR |                   Dimension of XCONTAB 
XCONTAB(TENSOR,NPOINTS) |  This is a 3x3 matrix which multiplied by the velocity vector, constrains the velocity to the degrees of freedom possessed by the node.  May be constructed by calls to constrainv.



**b.   Mesh Object Attribute Definition** :

Each attribute (either default attribute or use added attribute) in a
mesh object consists of the following items:

Each attribute (either default attribute or use added attribute) in a
mesh object consists of the following items:

Attribute | Description
----- |  ---
name | (character) Attribute name
typetype | (character) Attribute type<br> INT- Integer<br> REAL - Real number<br> CHARACTER - character variable of length 32<br> VINT - Vector of integer <br> VDOUBLE - Vector of real<br>*8 (this is the default) <br> VCHAR - Vector of character<br>*32<br>
rank | (character) Attribute rank (must be an attribute for this Mesh object)  default is [scalar](#scalar)
length | (character) Attribute length (must be an attribute for this Mesh object) default is [nnodes](#nnodes)
interpolation |  (character) Interpolation option:
constant | Constant value
sequence |Set to the node number
copy |      Copy values
linear |     Linear interpolation  | this is the default
user |       User provides a subroutine named user_interpolate ([see IV. e.8](miscell.md))
log |         Logarithmic interpolation
asinh |     Asinh interpolation
min |       Set to the minimum
max |      Set to the maximum
incmin |  Set to the minimum plus one (vint attribute only)
incmax |  Set to the maximum plus one (vint attribute only)
and |       'and' the bits
or |         'or' the bits
persistence |     (character) Attribute persistence:
permanent |  Can not be deleted
temporary |  Temporary attribute - this is the default
ioflag | (character) Attribute IO flag:<br> default is alg<br> a Put this attribute on avs dumps<br> g Put this attribute on gmv dumps<br> f  Put this attribute on fehm dumps<br> l  Put this attribute on LaGriT dumps<br> L Do not write this attribute to LaGriT dumps<br> 
default |           (real) Attribute value
