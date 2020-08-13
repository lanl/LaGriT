# Mesh Object Definition

The data structure which contains the information necessary to define a
mesh is called a Mesh Object. A Mesh Object consists of attributes that define the
mesh identified by a mesh object name. Attributes are updated by LaGriT routines and
can be modified by the user. Use the command **cmo/status** to see the attributes for all mesh objects, 
or use **cmo/status**/*cmo_name* for the attributes of the mesh object identified by that name.


The default Mesh Object can be expanded by adding user defined
attributes with **[cmo/addatt](commands/cmo/cmo_addatt.md)**. User attributes can be removed with **[cmo/delatt](commands/cmo/cmo_delatt.md)**. Default mesh object attributes can not be removed. Some of the LaGriT routines add attributes to the mesh object. The user should never try to add an attribute whose name is
already listed as a mesh object as these are reserved as mesh object attribute names (See below).


The value of many of the scalar attributes can be changed by the **[cmo/setatt/](commands/cmo/cmo_setatt.md)** command.
For instance, cmo/setatt/cmo_name/epsilonl/1.d-9 will set the attribute epsilonl to 1.d-9 for the mesh object named cmo_name.



## Mesh Object Attributes

These are default attributes for a Mesh Object:


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

* **imt1** (or **imt**) (`integer array` of node material values, must be values greater than zero )

* **itp1** (or **itp**) (`integer array` of node types if type &gt; 20 node will be invisible) These values can be updated anytime with the **resetpts/itp** command. Use **rmpoint/compress** to remove dudded nodes from the mesh object.


| itp   |  name  |  description
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


**icr1** (or **icr**) (`integer array` of constraint numbers for nodes; the
value of this array is an index into the **[icontab](#icontab)** table
of node constraints described later in this section)

**isn1** (or **isn**)(`integer array` of child, parent node correspondence)

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

**dumptype** (`character` with default set to binary, type of gmv file to write)

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

**velname**, **densname**, **presname**, **enername**  (`character` specialized attribute names for velocity, density, pressure, and energy variables.  Default names set to vels, ric, pic, and eic. )

**xmin**, **xmax** (`real` value with minimum and maximum x coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )

**ymin**, **ymax** (`real` value with minimum and maximum y coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )

**zmin**, **zmax** (`real` value with minimum and maximum z coordinate of nodes in the mesh. Values are set internally and by **[setsize](commands/SETSIZE.md)** )


**kdtree_level** (`integer` with default value 0. This is the resolution level, number of octree refinements where 0 means terminal nodes contain 1  0 means terminal nodes contain 1 member.)

**max_number_of_sets** (`integer` with default value of 32. This is the number
of **[pset](commands/PSET.md)** and **[eltset](commands/ELTSET2.md)** allowed for a single mesh object.)


**number_of_psets**, **number_of_eltsets** (`integer` value with the number of psets and eltsets in the mesh.)

**psetnames**, **eltsetnames** (`character` are the set names for pset and eltset)

**geom_name** (`character` with default set to -defaultgeom-. This is the name of geometry
associated with this mesh.  See **[geom)](commands/cmo/cmo_geom.md)**)

**fset_names** (`integer` value with the number of face sets in the mesh.)

**number_of_fsets** (`character` are the set names for fset.)


The above are the default attributes for a mesh object. Multiple mesh objects can be defined, all with their own attributes and attribute values. Added attributes will be listed after this list of defaults if they have been added. The current state and list of attributes of a mesh object can be displayed by using the command **[cmo/status](commands/cmo/cmo_status.md)**.


## Mesh Object Attribute Definition


Each attribute (either default attribute or added attribute) in a mesh object has the following defined:


* **name**  `character` Attribute name, case sensitive.

* **type**  `character` with default VDOUBLE. Attribute type is one of the following, case ignored.

```
    INT- Integer
    REAL - Real number
    CHARACTER - character string (length 32)
    VINT - Vector of integer
    VDOUBLE - Vector of real (this is the default)
    VCHAR - Vector of character strings
```

* **rank**  `character` with default [scalar](#scalar). This is the attribute rank set by an attribute for this Mesh object.

* **length**  `character` with default [nnodes](#nnodes). This is the attribute length set by an attribute for this Mesh object.

* **interpolation**   `character` with default linear. This is the interpolation option for routines to use:

```
    linear   - Linear interpolation  (this is the default)
    constant - Constant value
    sequence - Set to the node number
    copy     - Copy values 
    user     - User provides a subroutine named user_interpolate
    log      - Logarithmic interpolation
    asinh    - Asinh interpolation
    min      - Set to the minimum
    max      - Set to the maximum
    incmin   - Set to the minimum plus one (vint attribute only)
    incmax   - Set to the maximum plus one (vint attribute only)
    and      - 'and' the bits
    or       - 'or' the bits
```

* **persistence**    `character` with default temporary. Attribute persistence where **permanent** can not be deleted, **temporary** can be deleted.


* **ioflag** `character` with default alg. These letters are IO flags to define what type of common output files this attribute can be written to. By default, the dump command will write all attributes, this flag can be used to limit which attributes are dumped.
```
    a - write this attribute on avs dumps
    g - write this attribute on gmv dumps
    f - write this attribute on fehm dumps
    l - write this attribute on LaGriT dumps
    L - Do Not write this attribute on LaGriT dumps
```

* **default**  `real` default is 0.0. This is the attribute value to initialize the data with.





