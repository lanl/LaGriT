

# MSTKLA - Mesh ToolKit interface to LaGriT

Note these pages have not been updated to newer versions of MSTKLA

(LA-UR-99-6711)

------------------------------------------------------------------------

Questions, Comments, Suggestions? Contact:

Rao V Garimella,

Email: rao@lanl.gov

------------------------------------------------------------------------


**MSTKLA** is a C interface to the **LaGriT** code which is designed to
make it easier to use the LaGriT functionality. It tries to hide some of
the internals of LaGriT and provides an API that works in a somewhat
object oriented way. It does not attempt to provide or enhance
interactive commands that exist in LaGriT. Rather, it is designed more
with writing automated mesh generation procedures and complex meshing
algorithms within LaGriT in mind.

MSTKLA has one or more *mesh objects*, which have mesh **regions**,
mesh **faces,** mesh **edges** and mesh **vertices** (nodes).
*Mesh faces and mesh edges are objects defined on the fly in terms of
their vertices.* One can ask these objects various questions or modify
their info through operators. In addition there is a **geometric
model** and **geometric model objects** (In the spirit of consistent
naming, I am labeling this set of functions **GMTKLA**). Since LaGriT
does not really store curves or points in the database, these are also
faked on the fly.

The mesh and model entities are related through the concept of
**CLASSIFICATION**.

"A mesh entity is classified on a model entity if it forms all or
part of that model entity's discretization (grid) but not its
boundary's".

So mesh regions can be classified only on a model region.  Mesh faces
can be classified on a model face or a model region. Mesh vertices can
be classified on a model vertex, model edge, model face or model region.

Classification information can be queried from MSTKLA if it is
available. In particular, in LaGriT, only **external faces** (what
LaGriT calls **"reflective" boundaries**) and **internal faces or
interfaces** created with the option **"intrcons"** in the
**surface**
command have the information required to retrieve full classification
information. Full classification information includes the **type** of
geometric model entity the mesh entity is classified on, the **ID of
that geometric model entity** and **a pointer to the geometric model
object** (which can be further queried). Interface surfaces created
with the surface command option "**intrface**" **do not** have the
necessary information to relate the mesh and the model fully. (In the
future, I may put in code to take the "intrface" type surfaces and add
the info needed to find out the classification information).

Note that the current interface works for querying an existing mesh but
not **yet** for modifying it. The operators for this are on the way.
Operators for calling the actual meshing operators (connect, refine,
smooth, etc) will also be added in the near future.

Finally, the basic interface (low level query and modification
operators) are expected to retain the same format regardless of what the
underlying database is (LaGriT or something else). So, for example, I am
hoping that an operator to retrieve the nodes of an element will retain
the same form shown below regardless of how it functions internally:

void MR_Vertices(PMeshObj mesh, PMeshRegn reg, int nv, PMeshVert rverts);

I will try hard to maintain this API constant but cannot absolutely guarantee it.

parent-child Also see [Parent-Child Node](parent-child.md)


typedefs[TYPEDEFS](prelim.md)


## MSTKLA_Init MSTKLA Interface Operators



**MESH INITIALIZATION:**


void MSTKLA_Init(void);

Initialize the mesh toolkit.


**MESH OPERATORS:**


MESH OBJECT: [MESH OBJECT](Mesh.md)

MESH REGION:[MESH REGION](MeshRegion.md)

MESH FACE:[MESH FACE](MeshFace.md)

MESH EDGE:[MESH EDGE](MeshEdge.md)

MESH VERTEX:[MESH VERTEX](MeshVertex.md)


GENERIC MESH REGION QUERIES:[GENERIC MESH REGION QUERIES](GenRegion.md)


**GMTKLA_Init GEOMETRIC MODEL INITIALIZATION:**

void GMTKLA_Init(void);

Initialize the geometric model interface.


**MODEL OPERATORS:**


GEOMETRIC MODEL:[GEOMETRIC MODEL](Geom.md)

MODEL ENTITY:[MODEL ENTITY](GeomEntity.md)

MODEL REGION:[MODEL REGION](GeomRegion.md)

MODEL FACE:[MODEL FACE](GeomFace.md)

MODEL EDGE:[MODEL EDGE](GeomEdge.md)

MODEL VERTEX: [MODEL VERTEX](GeomVertex.md)  *Not implemented fully*


UTILITIES: [UTILITIES:](utilities.md)


EXAMPLE: [EXAMPLE OF PROGRAMMING WITH MSTKLA](example.md)**

EXECUTABLE: [LIBRARIES, INCLUDE FILES AND CREATING AN EXECUTABLE](linking.md)

