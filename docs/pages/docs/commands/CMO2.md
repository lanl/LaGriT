---
title: CMO
tags: ok
---

**CMO**

  The **mo** command operates on the Mesh Object(MO). There can be
  many Mesh Objects in the code for a given problem. Only one of these
  Mesh Objects may by the Current Mesh Object. There is also one
  Default Mesh Object which is used as the template for generating new
  Mesh Objects.

 **FORMAT:**

Add a user defined attribute to a Mesh Object

[**mo** **/addatt**](cmo/cmo_addatt.md) /mo\_name/att\_name/type/rank/
length/interpolate/persistence/ioflag/value

[**mo** **/addatt**](cmo/cmo_addatt.md) /mo\_name /keyword /
keyword\_parameters

Give the sink mesh the same set of attributes as the source mesh (with
unitialized values)

**[cmo/attribute\_derive](cmo/cmo_att_derive.md)**/[sink\_mo/[src\_mo]

[Change two meshes so they both share the same set of attributes (taking
the union of their sets of
attributes)]

**[cmo/attribute\_union](cmo/cmo_att_derive.md)**/mo\_name\_1/mo\_name\_2

Shorten all memory managed arrays associated with mo\_name to their
actual lengths

**[cmo/compress](cmo/cmo_compress.md)**/mo\_name

Associate the surface constraint information of the mesh object cmo\_src
with cmo\_sink:
[**mo/constraint**/](cmo/cmo_constraint.md)cmo\_sink/cmo\_src

Copy master\_mo to mo\_name:

**[cmo/copy](cmo/cmo_copy.md)**/mo\_name/master\_mo

Copy a mesh object attribute:

**[cmo/copyatt](cmo/cmo_copyatt.md)**/cmosink/cmo\_src/att\_nam\_sink/att\_nam\_src

Create a new mesh object:

**[cmo/create](cmo/cmo_create.md)**/ mo\_name/[npoints/nelements/mesh-type]

Delete an existing mesh object:

**[cmo/delatt](cmo/cmo_delatt.md)**/ mo\_name/att\_name

Delete an existing mesh object even it has 'permanent' persistance:

**[cmo/DELATT](cmo/cmo_delatt.md)**/ mo\_name/att\_name

Copy the structure of master\_mo to mo\_name, but copy no data:

**[cmo/derive](cmo/cmo_derive.md)**/mo\_name/master\_mo

Associate the geometry named geometry\_name with the mesh object
mo\_name:

**[cmo/geometry](cmo/cmo_geom.md)**/ mo\_name/geometry\_name

Print the memory length of attribute att\_name for Mesh Object,
mo\_name:

**[cmo/length](cmo/cmo_length.md)**/mo\_name/att\_name

List all mesh objects:

**[cmo/list](cmo/cmo_list.md)**

Adjust the memory manages arrays associated with mo\_name to the

lengths required by number\_nodes and number\_elements:
[mo**/memory**/](cmo/cmo_memory.md)mo\_name/number\_nodes/number\_elements

Modify an attribute parameter value:
[**mo** **/ modatt**/](cmo/cmo_modatt.md)mo\_name/att\_name/field\_name/new\_field

Change the name of a mesh object:

**[cmo/move](cmo/cmo_move.md)**/mo\_name /master\_mo 

Adjust the memory length of mo\_name based on the values of nnodes and

nelements:

**[cmo/newlen](cmo/cmo_newlen.md)**/mo\_name

Print the value of an attribute:

**[cmo/printatt](cmo/cmo_printatt.md)**/mo\_name/att\_name-**all-** **-xyz-** **nod** / [**minmax** **list** **value** ]  / [ifirst,ilast,istride]

Read values for an attribute from a file:

**[cmo/readatt](cmo/cmo_readatt.md)**/mo\_name/att\_name/ [...]/operation/file\_name

Release a mesh object (same as delete):

**[cmo/release](cmo/cmo_release.md)**/mo\_name

Make mo\_name the active mesh object:

**[cmo/select](cmo/cmo_select.md)**/mo\_name

Set the value of an attribute:

**[cmo/setatt](cmo/cmo_setatt.md)**/mo\_name/att\_name/ifirst,ilast,istride/value

Create an integer attribute that contains the node or element number:

**[cmo/set\_id/](cmo/cmo_setid.md)** mo\_name**/node** **element** **both**/ [att\_nam1]/ [att\_nam2]

Print the mesh object status (all attributes and values of scalars)
[mo/status**/](cmo/cmo_status.md)mo\_name/ [**brief**]

Verifie that memory allocation of Mesh Object mo\_name is consistent:

**[cmo/verify](cmo/cmo_verify.md)**/mo\_name/

CONVENTION: As a result of any command that generates a new mesh object,
the newly generated mesh object becomes active. As a result of any
command the changes a mesh

object (e.g. copyatt) the changed mesh object becomes active.  Use
cmo/select to explicitly

specify the active mesh object.

RESERVED NAMES: The following names are reserved and may not be used for
Mesh Objectnames:

**-cmo-** the Current Mesh Object

**-default-** the Default Mesh Object

**-all-** all Mesh Objects or Attributes

TYPES, DEFAULTS and POSSIBLE VALUES:
  
Attribute | Type, Defaults | Possible Values
------------- |  --------- | -----------------------------------------------------------------------------------------
mo\_name     | character |
att\_name     | character |
mesh\_type    | character | **(tet,hex,pri,pyr,tri,qua,hyb,line,point)**
type          | character, default **VDOUBLE** | **(VDOUBLE, VINT, VCHAR, INT, REAL, CHARACTER)** <br> **VDOUBLE** real array <br>  **VINT** integer array <br> **VCHAR** array of character*32 <br> **INT** a single integer variable (length =1 rank =1 by definition) <br> **REAL** a single real variable (length =1 rank =1 by definition) <br> **CHARACTER** a single character*32 variable (length =1 rank =1 by definition)
rank          | character, default **scalar** | (**scalar,vector,tensor**) <br> **scalar** one entry per array element <br> **vector** 3 entries per array element <br> **tensor** 9 entries per array element <br> any previously defined **INT** attribute including user defined attributes may be used as rank
length        | character, default is **nnodes** | (**nnodes, nelements**) <br> any previously defined **INT** attribute including user defined attributes may be used as length 
interpolate   | character, default **linear** | (**copy, sequence, linear, log, asinh, max, min, user,and,or,incmax**)
ioflag      | character | (**a, g, f, l, no** -- for avs,gmv,fehms,LaGriT)
