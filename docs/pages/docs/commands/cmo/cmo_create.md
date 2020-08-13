---
Title: "cmo/create"
Tags:
---

# cmo/create

------

Creates a new Mesh Object which becomes the Current Mesh Object.

## SYNTAX

<pre>
<b>cmo/create</b>/mo_name/[npoints/nelements/ mesh_type]/
</pre>


If a Mesh is created using the first (mesh_type) format, then values are supplied for the other parameters as follows:
  
|  mesh name |  ndimension geom | ndimension topo  | nodes per element |  faces per element |  edges per element |
| :--- | :--- | :--- | :--- | :--- | :--- |
|  **tet** |  &nbsp;&nbsp;    3 |  &nbsp;&nbsp;      3 |   &nbsp;&nbsp;      4 |   &nbsp;&nbsp;     4 |    &nbsp;&nbsp;    6 |
|  **hex** |   &nbsp;&nbsp;   3 |  &nbsp;&nbsp;      3 |  &nbsp;&nbsp;       8 |   &nbsp;&nbsp;     6 |    &nbsp;&nbsp;    12 |
|  **pri**(sm) | &nbsp;&nbsp;  3      |  &nbsp;&nbsp; 3      | &nbsp;&nbsp;   6      | &nbsp;&nbsp;  5      |  &nbsp;&nbsp; 9 |
|  **pyr**(amid) | &nbsp;&nbsp;   3      | &nbsp;&nbsp;  3      | &nbsp;&nbsp;   5      | &nbsp;&nbsp;  5      |  &nbsp;&nbsp; 8 |
|  **tri**(angle) | &nbsp;&nbsp;  3      | &nbsp;&nbsp;  2      | &nbsp;&nbsp;   3      | &nbsp;&nbsp;  3      | &nbsp;&nbsp;  3 |
|  **qua**(d)   | &nbsp;&nbsp;   3 |  &nbsp;&nbsp;      2 |  &nbsp;&nbsp;       4 |   &nbsp;&nbsp;     4 |   &nbsp;&nbsp;     4 |
|  **hyb**(rid) | &nbsp;&nbsp; 3      | &nbsp;&nbsp;  3      | &nbsp;&nbsp;   10      | &nbsp;&nbsp; 10      | &nbsp;&nbsp; 12 |
| **lin(e)**    | &nbsp;&nbsp;   3 |   &nbsp;&nbsp;     1 |  &nbsp;&nbsp;       2 |   &nbsp;&nbsp;     2 |   &nbsp;&nbsp;     1 |
| **triplane** | &nbsp;&nbsp; 2      | &nbsp;&nbsp;  2      | &nbsp;&nbsp;   3      | &nbsp;&nbsp;  3  |  &nbsp;&nbsp;       3 |


`mo_name` required. If mo_name exists nothing happens.

If values are supplied for `npoints` and/or `nelements` then space is
allocated, but values are not entered for the mesh object attributes:
**nnodes** and **nelements**


## EXAMPLES

```
cmo/create/mo_tet2

cmo/create/mo_tet2/0/0/hex
```
