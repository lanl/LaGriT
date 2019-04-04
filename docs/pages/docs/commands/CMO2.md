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
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.html">**`addatt`**  </a> | Add user attribute | **cmo/addatt**/mo/att_name/type/rank/length|
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.html">**`addatt`**  </a> | Create attributes  | **cmo/addatt**/mo/keyword/keyword_options |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_att_derive.html">**`attribute_derive`** </a> | Give mo derived attributes from another mo | **cmo/attribute_derive**/sink_mo/src_mo |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_att_derive.html">**`attribute_union`** </a> | Combine attributes of two mesh objects | **cmo/attribute_union**/mo_1/mo_2  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_compress.html">**`compress`** </a> | Compress mo arrays to actual lengths | **cmo/compress**/mo_name |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_constraint.html">**`constraint`** </a> | Get surface constraints from mo to another | **cmo/constraint**/cmo_sink/cmo_src  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_copy.html">**`copy`** </a> | Copy identical mo to mo_new | **cmo/copy**/mo_new/mo_master  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_copyatt.html">**`copyatt`** </a> | Copy attribute values to another attribute | **cmo/copyatt**/mo mosrc/att att_src  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_create.html">**`create`** </a> | Create a new mesh object | **cmo/create**/mo_name [/// mesh_type] | 
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_delatt.html">**`delatt`** </a> | Delete a mesh object attribute | **cmo/delatt**/mo_name/att_name   |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_delatt.html">**`DELATT`** </a> | Force Delete a mesh object attribute | **cmo/DELATT**/mo_name/att_name  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_release.html">**`delete`** </a> | Delete an existing mesh object | **cmo/delete**/mo_name   |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_derive.html">**`derive`** </a> | Copy mo to new mo with empty data  | **cmo/derive**/mo_name/master_mo  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_geom.html">**`geometry`** </a> | Give geometry to mo from another mo | **cmo/geometry**/mo_name/geometry_name |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_length.html">**`length`** </a> | Print memory length for mo attribute | **cmo/length**/mo_name/att_name  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_list.html">**`list`** </a> | List all mesh objects | **cmo/list**  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_memory.html">**`memory`** </a> | Set length for mo memory | **cmo/memory**/mo_name/num_nodes/num_elements |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_modatt.html">**`modatt`** </a> | Modify mo attribute parameters | **cmo/modatt**/mo/att_name/parameter/value  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_move.html">**`move`** </a> | Change the name of a mo | **cmo/move**/mo_new/mo_old  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_newlen.html">**`newlen`** </a> | Adjust memory lengths by nnodes and nelements | **cmo/newlen**/mo_name  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_printatt.html">**`printatt`** </a> | Print attribute values | **cmo/printatt**/mo/att_name/[**minmax**] [1,0,0]  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_readatt.html">**`readatt`** </a> | Read attribute values from file | **cmo/readatt**/mo/att1,att2,[...] /1,0,0/file  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_release.html">**`release`** </a> | Delete an existing mesh object | **cmo/release**/mo_name   |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_select.html">**`select`** </a> | Make selected mo current and default | **cmo/select**/mo_name   |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_setatt.html">**`setatt`** </a> | Set the values of mo attributes | **cmo/setatt**/mo/att_name/[1,0,0]/value  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_setid.html">**`set_id`** </a> | Create attribute with id values (order) | **cmo/set_id**/mo/**node** or **element**/att_name  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_status.html">**`status`** </a> | Print mesh object status | **cmo/status**/mo_name/ [**brief**]  |
|<a href="https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_verify.html">**`verify`** </a> | Verify memory for mo is consistent | **cmo/verify**/mo_name  |



## CONVENTION

As a result of any command that generates a new mesh object, the newly generated mesh object becomes active. 
As a result of any command that changes a mesh object (e.g. `copyatt`) the changed mesh object becomes active.
Use `cmo/select` to explicitly specify the current and active mesh object.

## RESERVED NAMES

The following names are reserved and used in the various cmo commands:

**`-default-`** : the default template for the LaGriT mesh object

**`-def-`** : the default value for command options

**`-cmo-`** : the Current Mesh Object

**`-all-`**: all Mesh Objects or Attributes

**`-xyz-`**: Mesh Object Attributes xic, yic, and zic


## CMO PARAMETERS, DEFAULTS and POSSIBLE VALUES
  
|  Attribute    | Type, Defaults                      | Possible Values                                    |
| :-------------| :-----------------------------------| :--------------------------------------------------|
|`mo_name`      | character                         | |
|`att_name`     | character                         | |
|`mesh_type`    | character                         | **tet,hex,pri,pyr,tri,triplane,quad,hyb,line,pnt** |
|`type`         | character, default: **`VDOUBLE`** |  **VDOUBLE** real array <br>  **VINT** integer array <br> **VCHAR** array of character*32 <br> **INT** a single integer variable (length =1 rank =1 by definition) <br> **REAL** a single real variable (length =1 rank =1 by definition) <br> **CHARACTER** a single character*32 variable (length =1 rank =1 by definition) |
|`rank`         | character, default: **`scalar`**  | **scalar** one entry per array element <br> **vector** 3 entries per array element <br> **tensor** 9 entries per array element <br> any previously defined **INT** attribute including user defined attributes may be used as rank |
|`length`       | character, default: **`nnodes`**  |  any previously defined **INT** attribute including user defined attributes may be used as length |
|`interpolate`  | character, default: **`linear`**  | **copy, sequence, linear, log, asinh, max, min, user,and,or,incmax** |
|`ioflag`       | character                         | **a, g, f, l, no** -- for avs, gmv, fehm, LaGriT |


## EXAMPLE COMMANDS AND OUTPUT

```
cmo/create/motet                                                                
cmo/status/motet/ brief                                                          
 
The current-mesh-object(CMO) is: motet                                          
 
  2 Mesh Object name: motet                                                     
    number of nodes =             0        number of elements =            0    
    dimensions geometry =         3        element type =                tet    
    dimensions topology =         3        4 nodes      4 faces      6 edges    
    boundary flag =        16000000        status =                   active   
```
Create and print a brief status of the mesh object named motet.

```
cmo/addatt/moquad/zsave/VDOUBLE/scalar/nnodes/linear/permanent/gxaf/0.0         
cmo/copyatt/moquad moquad/zsave zic
cmo/printatt/moquad/-all- minmax  

ATTRIBUTE NAME              MIN               MAX         DIFFERENCE    LENGTH  
 -def-              0.000000000E+00  0.000000000E+00 0.000000000E+00     14241  
 scalar                           1                1               0         1  
 vector                           3                3               0         1  
 nnodes                       14241            14241               0         1  
 nedges                           0                0               0         1  
 nfaces                           0                0               0         1  
 nelements                    14000            14000               0         1  
 mbndry                    16000000         16000000               0         1  
 ndimensions_topo                 2                2               0         1  
 ndimensions_geom                 3                3               0         1  
 nodes_per_element                4                4               0         1  
 edges_per_element                4                4               0         1  
 faces_per_element                4                4               0         1  
 isetwd                           0                0               0     14241  
 ialias                           0                0               0     14241  
 imt1                             1                1               0     14241  
 itp1                             0               10              10     14241  
 icr1                             0                0               0     14241  
 isn1                             0                0               0     14241  
 xic                4.980000000E+05  5.005000000E+05 2.500000000E+03     14241  
 yic                5.370000000E+05  5.405000000E+05 3.500000000E+03     14241  
 zic                1.638043335E+03  1.856468628E+03 2.184252930E+02     14241  
 xtetwd                           0                0               0     14000  
 itetclr                          1                1               0     14000  
 itettyp                          4                4               0     14000  
 itetoff                          0            55996           55996     14000  
 jtetoff                          0            55996           55996     14000  
 itet                             1            14241           14240     14000x4
 jtet                             2         16000000        15999998     14000x4
 epsilon            1.000000004E-15  1.000000004E-15 0.000000000E+00         1  
 epsilonl           9.562806528E-10  9.562806528E-10 0.000000000E+00         1  
 epsilona           4.118418852E-06  4.118418852E-06 0.000000000E+00         1  
 epsilonv           4.243763815E-04  4.243763815E-04 0.000000000E+00         1  
 ipointi                          1                1               0         1  
 ipointj                      14241            14241               0         1  
 idebug                           0                0               0         1  
 itypconv_sm                      1                1               0         1  
 maxiter_sm                      25               25               0         1  
 tolconv_sm         1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 nnfreq                           1                1               0         1  
 ivoronoi                         1                1               0         1  
 iopt2to2                         2                2               0         1  
 xmin               4.980000000E+05  4.980000000E+05 0.000000000E+00         1  
 ymin               5.370000000E+05  5.370000000E+05 0.000000000E+00         1  
 zmin               1.638043335E+03  1.638043335E+03 0.000000000E+00         1  
 xmax               5.005000000E+05  5.005000000E+05 0.000000000E+00         1  
 ymax               5.405000000E+05  5.405000000E+05 0.000000000E+00         1  
 zmax               1.856468628E+03  1.856468628E+03 0.000000000E+00         1  
 kdtree_level                     0                0               0         1  
 max_number_sets                 64               64               0         1  
 number_of_psets                  0                0               0         1  
 number_of_eltsets                0                0               0         1  
 number_of_fsets                  0                0               0         1  
 zsave              1.638043335E+03  1.856468628E+03 2.184252930E+02     14241  
```
Add the attribute zsave to the mesh object and copy zic attribute values into zsave attribute. 
    
    





