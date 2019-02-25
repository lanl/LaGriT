---
title: CMO
tags: ok
---

# CMO

The `cmo` command operates on the Mesh Object(MO). There can be
many Mesh Objects in the code for a given problem. Only one of these
Mesh Objects may by the Current Mesh Object. There is also one
Default Mesh Object which is used as the template for generating new
Mesh Objects.

## FORMAT

Add a user defined attribute to a Mesh Object:

<pre>
<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.md"><b>cmo/addatt</b></a>/mo_name/att_name/type/rank/length/interpolate/persistence/ioflag/value
<a href="cmo/cmo_addatt.md"><b>cmo/addatt</b></a>/mo_name/keyword/keyword_parameters
</pre>

Give the sink mesh the same set of attributes as the source mesh (with
unitialized values):

<pre>
<a href="cmo/cmo_att_derive.md"><b>cmo/attribute_derive</b></a>/[sink_mo/[src_mo]
</pre>

Change two meshes so they both share the same set of attributes (taking
the union of their sets of attributes):

<pre>
<a href="cmo/cmo_att_derive.md"><b>cmo/attribute_union</b></a>/mo_name_1/mo_name_2
</pre>

Shorten all memory managed arrays associated with `mo_name` to their actual lengths:

<pre>
<a href="cmo/cmo_compress.md"><b>cmo/compress</b></a>/mo_name
</pre>

Associate the surface constraint information of the mesh object `cmo_src` with `cmo_sink`:

<pre>
<a href="cmo/cmo_constraint.md"><b>cmo/constraint</b></a>/cmo_sink/cmo_src
</pre>

Copy master\_mo to mo\_name:

<pre>
<a href="cmo/cmo_copy.md"><b>cmo/copy</b></a>/mo_name/master_mo
</pre>

Copy a mesh object attribute:

<pre>
<a href="cmo/cmo_copyatt.md"><b>cmo/copyatt</b></a>/cmosink/cmo_src/att_name_sink/att_name_src
</pre>

Create a new mesh object:

<pre>
<a href="cmo/cmo_create.md"><b>cmo/create</b></a>/mo_name/[npoints/nelements/mesh-type]
</pre>

Delete an existing mesh object:

<pre>
<a href="cmo/cmo_delatt.md"><b>cmo/delatt</b></a>/mo_name/att_name
</pre>

Delete an existing mesh object even it has 'permanent' persistance:

<pre>
<a href="cmo/cmo_delatt.md"><b>cmo/DELATT</b></a>/mo_name/att_name
</pre>

Copy the structure of `master_mo` to `mo_name`, but copy no data:

<pre>
<a href="cmo/cmo_derive.md"><b>cmo/derive</b></a>/mo_name/master_mo
</pre>

Associate the geometry named `geometry_name` with the mesh object `mo_name`:

<pre>
<a href="cmo/cmo_geom.md"><b>cmo/geometry</b></a>/ mo_name/geometry_name
</pre>

Print the memory length of attribute `att_name` for Mesh Object, `mo_name`:

<pre>
<a href="cmo/cmo_length.md"><b>cmo/length</b></a>/mo_name/att_name
</pre>

List all mesh objects:

<pre>
<a href="cmo/cmo_list.md"><b>cmo/list</b></a>
</pre>

Adjust the memory manages arrays associated with `mo_name` to the lengths required by `number_nodes` and `number_elements`:

<pre>
<a href="cmo/cmo_memory.md"><b>cmo/memory</b></a>/mo_name/number_nodes/number_elements
</pre>

Modify an attribute parameter value:

<pre>
<a href="cmo/cmo_modatt.md"><b>cmo/modatt</b></a>/mo_name/att_name/field_name/new_field
</pre>

Change the name of a mesh object:

<pre>
<a href="cmo/cmo_move.md"><b>cmo/move</b></a>/mo_name/master_mo
</pre>

Adjust the memory length of `mo_name` based on the values of *nnodes* and *nelements*:

<pre>
<a href="cmo/cmo_newlen.md"><b>cmo/newlen</b></a>/mo_name
</pre>

Print the value of an attribute:

<pre>
<a href="cmo/cmo_printatt.md"><b>cmo/printatt</b></a>/mo_name/att_name <b>-all-</b> <b>-xyz-</b> <b>nod</b> / [<b>minmax</b> <b>list</b> <b>value</b> ]  / [ifirst,ilast,istride]
</pre>

Read values for an attribute from a file:

<pre>
<a href="cmo/cmo_readatt.md"><b>cmo/readatt</b></a>/mo\_name/att_name/ [...] /operation/file_name
</pre>

Release a mesh object (same as delete):

<pre>
<a href="cmo/cmo_release.md"><b>cmo/release</b></a>/mo_name
</pre>

Make mo\_name the active mesh object:

<pre>
<a href="cmo/cmo_select.md"><b>cmo/select</b></a>/mo_name
</pre>

Set the value of an attribute:

<pre>
<a href="cmo/cmo_setatt.md"><b>cmo/setatt</b></a>/mo_name/att_name/ifirst,ilast,istride/value
</pre>

Create an integer attribute that contains the node or element number:

<pre>
<a href="cmo/cmo_setid.md"><b>cmo/setid</b></a>/mo_name/<b>node</b> <b>element</b> <b>both</b>/ [att_name1]/ [att_name2]
</pre>

Print the mesh object status (all attributes and values of scalars)

<pre>
<a href="cmo/cmo_status.md"><b>cmo/status</b></a>/mo_name/ [<b>brief</b>]
</pre>

Verifiy that memory allocation of Mesh Object `mo_name` is consistent:

<pre>
<a href="cmo/cmo_verify.md"><b>cmo/verify</b></a>/mo_name/
</pre>

### CONVENTION

As a result of any command that generates a new mesh object,
the newly generated mesh object becomes active. As a result of any
command the changes a mesh object (e.g. `copyatt`) the changed mesh object becomes active.
Use `cmo/select` to explicitly specify the active mesh object.

### RESERVED NAMES

The following names are reserved and may not be used for Mesh Object names:

* **`-cmo-`**: the Current Mesh Object
* **`-default-`**: the Default Mesh Object
* **`-all-`**: all Mesh Objects or Attributes

## TYPES, DEFAULTS and POSSIBLE VALUES
  
|  Attribute    | Type, Defaults                    | Possible Values |
|---------------|-----------------------------------|-----------------|
|`mo_name`      | character                         | |
|`att_name`     | character                         | |
|`mesh_type`    | character                         | **(tet,hex,pri,pyr,tri,qua,hyb,line,point)** |
|`type`         | character, default: **`VDOUBLE`** | **(VDOUBLE, VINT, VCHAR, INT, REAL, CHARACTER)** <br> **VDOUBLE** real array <br>  **VINT** integer array <br> **VCHAR** array of character*32 <br> **INT** a single integer variable (length =1 rank =1 by definition) <br> **REAL** a single real variable (length =1 rank =1 by definition) <br> **CHARACTER** a single character*32 variable (length =1 rank =1 by definition) |
|`rank`         | character, default: **`scalar`**  | (**scalar,vector,tensor**) <br> **scalar** one entry per array element <br> **vector** 3 entries per array element <br> **tensor** 9 entries per array element <br> any previously defined **INT** attribute including user defined attributes may be used as rank |
|`length`       | character, default: **`nnodes`**  | (**nnodes, nelements**) <br> any previously defined **INT** attribute including user defined attributes may be used as length |
|`interpolate`  | character, default: **`linear`**  | (**copy, sequence, linear, log, asinh, max, min, user,and,or,incmax**) |
|`ioflag`       | character                         | (**a, g, f, l, no** -- for avs,gmv,fehms,LaGriT) |
