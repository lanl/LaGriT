# Mesh Object Definition

The data structure which contains the information necessary to define a
mesh is called a Mesh Object. A Mesh Object consists of attributes that define the
mesh identified by a mesh object name. Attributes are updated by LaGriT routines and
can be modified by the user. Use the command **cmo/status** to see the attributes for all mesh objects, 
or use **cmo/status**/*cmo_name* for the attributes of the mesh object identified by that name.

These are the default attributes for a Mesh Object:

* **name** (`character*32` -- mesh object name, this is the unique identifier for any particular mesh object)

* **scalar** (`integer` -- defined to have value 1)

* **vector** (`integer` -- defined to have value 3)

* **nnodes** (`integer` -- number of nodes in the mesh)

* **nelements** (`integer` -- number of elements in the mesh, e.g. triangles, tetrahedra)

* **nfaces** (`integer` -- number of unique topological facets in the mesh, e.g. number of edges in 2D or number of element faces in 3D). This is not maintained but may be used by certain commands or by the user.
  
* **nedges** (`integer` -- number of unique edges in mesh) This is not maintained but may be used by certain commands or by the user.
  

* **mbndry** (`integer` -- value signifying that if the node number is greater than **mbndry** then the node is a boundary node; default 16000000) This value must be greater than 48 \* nnodes and may be reset by **[connect](commands/CONNECT1.md)** (for an example of usage see [Mesh Object Connectivity](meshobjcon.md#mbndry)).
  

* **ndimensions_topo** (`integer` -- topological dimensionality; 1, 2 or 3. i.e. a non-planar surface would have
ndimensions_topo = 2 and ndimensions_geom = 3.)

* **ndimensions_geom** (`integer` -- 1, 2 or 3 for dimension of geometry; default 3)

* **nodes_per_element** (`integer` -- value dependent on type of mesh; e.g. for tetrahedral mesh the value will be 4)

* **edges_per_element** (`integer` -- value dependent on type of mesh; e.g. for tetrahedral mesh the value will be 6)

* **faces_per_element** (`integer` -- topological number of facets per
element (i.e. in 1D this number is always 2, for 2D use the number of
edges of the element, for 3D use the number of faces of the element; e.g. for tetrahedral mesh the value
will be 4)


* **isetwd** (`integer array` containing pset membership information, see **[pset](commands/PSET.md)** command definition)

* **ialias** (`integer array` of alternate node numbers, i.e. for merged points)

* **imt1** (`integer array` of node material values, must be values greater than zero )

* **itp1** (`integer array` of node types if type &gt; 20 node will be invisible) These values can be updated anytime with the **resetpts/itp** command. Use **rmpoint/compress** to remove dudded nodes from the mesh object.


| itp1  |  name  |  description
| :---- |  :---- | :------------
|0 |  int | Interior
|2 |  ini | Interface
|3 |  vrt | Virtual
|4 |  vin | Virtual + interface
|8 |  vif | Virtual + interface + free
|9 |  alb | Virtual + Interface + free + reflective
|10 | rfl | Reflective boundary node 
|11 | fre | Free boundary node 
|12 | irb | Interface node on reflective boundary
|13 | ifb | Interface node on free boundary
|14 | rfb | Node on intersection of free boundary and reflective boundary
|15 | irf | Interface node on intersection of free boundary and reflective boundary
|16 | vrb | Virtual node on reflective boundary
|17 | vfb | Virtual node on free boundary
|18 | vrf | Virtual node on free + reflective boundary
|19 | vir | Virtual + interface node on reflective boundary
|20 | mrg | Merged node
|21 | dud | Dudded node 
|41 | par | Parent node for doubly defined nodes


**icr1** (`integer array` of constraint numbers for nodes; the
value of this array is an index into the **[icontab](#icontab)** table
of node constraints described later in this section)

**isn1** (`integer array` of child, parent node correspondence)

Points on material interfaces are given itp1 point type 41 (parent). One
child point is spawned for each material meeting at the parent point.
The isn1 field of the parent point will contain the point number of the
first child point. The isn1 field of the first child will contain the
point number of the next child. The isn1 field of the last child will
contain the point number of the parent. The point types of the child
points will be 2, 12, 13, 15 or 19 depending on whether the interface
point is also on an exterior boundary. This parent, child relationship
is established by the **settets** command and is undone by the **resetpts/parents** command.

**xic**, **yic**, **zic** (`real arrays` of node x, y, z coordinates)

**itetclr** (`integer array` of element material values, should be greater than zero)

**itettyp** (`integer array` of element shape values. For an example of usage see **[Mesh Object Connectivity](meshobjcon.md#itettyp))**. See element conventions at **[Supported element types](supported.md)**


 |  data_name | value | name  | description
 | :--------- | :---- | :---- | :--------
 |ifelmpnt   | 1  | pnt  | point (also known as node or vertex)
 |ifelmlin   | 2  | lin  | line
 |ifelmtri   | 3  | tri  | triangle 
 |ifelmqud   | 4  | qud  | quadrilateral 
 |ifelmtet   | 5  | tet  | tetrahedron 
 |ifelmpyr   | 6  | pyr  | pyramid
 |ifelmpri   | 7  | pri  | prism 
 |ifelmhex   | 8  | hex  | hexahedron 
 |ifelmhyb   | 9  | hyb  | hybrid
 |ifelmply   | 10 | ply  | polygon


**xtetwd** (`integer array` containing eltset membership information, see eltset command definition )

**itetoff** (`integer array` index into itet array list for elements, for an example of usage see [Mesh Object Connectivity](meshobjcon.md#itetoff))

**jtetoff** (`integer array` index into jtet array list for elements, for an example of usage see [Mesh Object Connectivity](meshobjcon.md#jtetoff))

**itet** (`integer array` of node vertices for each element, for an example of usage see [Mesh Object Connectivity (meshobjcon.md#itet))

**jtet** (`integer array` of element connectivity, for an example of usage see [Mesh Object Connectivity](meshobjcon.md#jtet))

**ipolydat**, **vor2d**, **vor3d**  (`character yes or no`, default **yes**. These flags include data when writing GMV output files, these can decrease the file size but limits the information written.) **ipolydat** writes polygon data, **vor2d** writes voronoi and median cells for 2D, **vor3d** writes voronoi and median cells for 3D meshes. See **[dump/gmv](commands/DUMP2.md)**

**epsilon** (`real` value of machine epsilon which will be calculated by the code.)

**epsilonl** (`real` value of smallest edge length that the code can distinguish
and will be set internally by the code, see **[setsize](commands/SETSIZE.md)** )

**epsilona** (`real` value of smallest area that the code can distinguish and will be set internally by the code )

**epsilonv** (`real` value of smallest volume that the code can distinguish and will be set internally by the code )

**ipointi**, **ipointj**  (`integer` value with the node number of the first node and last node of the current set of
nodes, used by the indexing syntax for point sets: / 0,0,0 / **[pset](commands/PSET.md)**)

**idebug** (`integer` value indicating level of debug output, flag values greater than 0 produce increasing levels of output with 5 or greater generating the most information )

**itypconv_sm**, **maxiter_sm** (`integer` values used by the smooth routines. maxiter_sm has default 25 and are the number of smoothing iterations in  **[smooth](commands/SMOOTH.md)** and **[radapt](commands/RADAPT.md)** )

**tolconv_sm** (`real` value used by the smooth routines)

**nnfreq** (`integer` value with default 1, flag to control reconnection after **[refine](commands/REFINE.md)** This is set to zero to turn off reconnection.)

**ivoronoi** (`integer` value with default 1, flag to control reconnection criterion. See **[recon)](commands/RECON.md)** )

```
+1 means restore delaunay
-2 means improve geometric quality of the elements
+2 means adaptive reconnection with user supplied routine
+5 means disable all reconnection
```

**iopt2to2** (`integer` value with default 2, flag to contol boundary flips during
reconnection. See **[recon)](commands/RECON.md)** )

```
0 = exterior boundaries
1 = interfaces
2 = exterior boundaries and interfaces
3 = all
```

**dumptype** (character default binary) Type of gmv file to write.

**velname** (character default vels) Name of velocity attribute.

**densname** (character default ric) Name of density attribute.

**presname** (character default pic) Name of pressure attribute.

**enername** (character default eic) Name of energy attribute.

**xmin**, **xmax** (`real` value with minimum and maximum x coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )

**ymin**, **ymax** (`real` value with minimum and maximum y coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )

**zmin**, **zmax** (`real` value with minimum and maximum z coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )


**kdtree_level** (integer default 0) resolution level
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
attributes (see [**cmo** **/addatt**](commands/cmo/cmo_addatt.md)).

 

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
