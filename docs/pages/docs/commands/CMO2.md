---
title: CMO
tags: cmo operations options
---

# CMO

The `cmo` command operates on the selected Mesh Object(MO). There can be
many Mesh Objects in the code for a given problem. Only one of these
Mesh Objects may by the Current Mesh Object. There is also one
Default Mesh Object which is used as the template for generating new
Mesh Objects.


| CMO Option         | Short Description (click on option for more details) | Brief Syntax                                       | 
| :----------------- | :--------------------------------------------- | :--------------------------------------------------|
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.html">**`addatt`**  </a> | Add user attribute |<pre><b>cmo/addatt</b>/mo/att_name/type/rank/length</pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.html">**`addatt`**  </a> | Create attributes  |<pre><b>cmo/addatt</b>/mo/keyword/keyword_options</pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_att_derive.html">**`attribute_derive`** </a> | Give mo derived attributes from another mo |<pre><b>cmo/attribute_derive</b>/sink_mo/src_mo </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_att_derive.html">**`attribute_union`** </a> | Combine attributes of two mesh objects |<pre><b>cmo/attribute_union</b>/mo_1/mo_2 </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_compress.html">**`compress`** </a> | Compress mo arrays to actual lengths |<pre><b>cmo/compress</b>/mo_name </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_constraint.html">**`constraint`** </a> | Get surface constraints from mo to another |<pre><b>cmo/constraint</b>/cmo_sink/cmo_src </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_copy.html">**`copy`** </a> | Copy identical mo to mo_new |<pre><b>cmo/copy</b>/mo_new/mo_master </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_copyatt.html">**`copyatt`** </a> | Copy attribute values to another attribute |<pre><b>cmo/copyatt</b>/mo mosrc/att att_src </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_create.html">**`create`** </a> | Create a new mesh object |<pre><b>cmo/create</b>/mo_name/[nnodes/nelements/type] </pre> | 
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_delatt.html">**`delatt`** </a> | Delete a mesh object attribute |<pre><b>cmo/delatt</b>/mo_name/att_name  </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_delatt.html">**`DELATT`** </a> | Force Delete a mesh object attribute |<pre><b>cmo/DELATT</b>/mo_name/att_name  </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_release.html">**`delete`** </a> | Delete an existing mesh object |<pre><b>cmo/delete</b>/mo_name  </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_derive.html">**`derive`** </a> | Copy mo to new mo with empty data |<pre><b>cmo/derive</b>/mo_name/master_mo </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_geom.html">**`geometry`** </a> | Give geometry to mo from another mo |<pre><b>cmo/geometry</b>/mo_name/geometry_name </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_length.html">**`length`** </a> | Print memory length for mo attribute |<pre><b>cmo/length</b>/mo_name/att_name </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_list.html">**`list`** </a> | List all mesh objects |<pre><b>cmo/list</b> </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_memory.html">**`memory`** </a> | Set length for mo memory |<pre><b>cmo/memory</b>/mo_name/num_nodes/num_elements </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_modatt.html">**`modatt`** </a> | Modify mo attribute parameters |<pre><b>cmo/modatt</b>/mo/att_name/parameter/value </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_move.html">**`move`** </a> | Change the name of a mo |<pre><b>cmo/move</b>/mo_new/mo_old </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_newlen.html">**`newlen`** </a> | Adjust memory lengths by nnodes and nelements |<pre><b>cmo/newlen</b>/mo_name </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_printatt.html">**`printatt`** </a> | Print attribute values |<pre><b>cmo/printatt</b>/mo/att_name/[<b>minmax</b>] [1,0,0] </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_readatt.html">**`readatt`** </a> | Read attribute values from file |<pre><b>cmo/readatt</b>/mo/att1,att2,[...] /1,0,0/file </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_release.html">**`release`** </a> | Delete an existing mesh object |<pre><b>cmo/release</b>/mo_name  </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_select.html">**`select`** </a> | Make selected mo current and default |<pre><b>cmo/select</b>/mo_name  </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_setatt.html">**`setatt`** </a> | Set the values of mo attributes |<pre><b>cmo/setatt</b>/mo/att_name/[1,0,0]/value </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_setid.html">**`set_id`** </a> | Create attribute with id values (order) |<pre><b>cmo/set_id</b>/mo/<b>node</b> or <b>element</b>/att_name </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_status.html">**`status`** </a> | Print mesh object status |<pre><b>cmo/status</b>/mo_name/ [<b>brief</b>] </pre> |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_verify.html">**`verify`** </a> | Verify memory for mo is consistent |<pre><b>cmo/verify</b>/mo_name </pre> |



## CONVENTION

As a result of any command that generates a new mesh object,
the newly generated mesh object becomes active. As a result of any
command the changes a mesh object (e.g. `copyatt`) the changed mesh object becomes active.
Use `cmo/select` to explicitly specify the active mesh object.

## RESERVED NAMES

The following names are reserved and may not be used for Mesh Object names:

**`-cmo-`**: the Current Mesh Object
**`-default-`**: the Default Mesh Object
**`-all-`**: all Mesh Objects or Attributes
**`-xyz-`**: Mesh Objects Attributes xic, yic, and zic

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
