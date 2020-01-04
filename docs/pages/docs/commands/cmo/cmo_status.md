---
Title: "cmo/status"
Tags: cmo status
---

# cmo/status

------------

Prints the status of a selected Mesh Object or all Mesh Objects.  
This includes a header with information about the type of mesh object and the size of the mesh
object.  After the header information, a table lists all variables associated with a mesh object and includes the variable's type, rank, length, interpolation mode, persistence, ioflag and default value. 

## SYNTAX

<pre>
<b>cmo/status</b>/ [mo_name] / [ <b>brief</b> ]
</pre>

`mo_name` is the name of the mesh object, default is **-all-**.

**brief** will print a brief version of the status and show only the header information.


## EXAMPLES

```
cmo/status/ mo_tet2

cmo/status/-cmo-

cmo/status/ mo_tet2/brief

cmo/status

cmo/status/-all-

cmo/status/-default-
```

An example of header information follows:

<pre class="lg-output">

1 Mesh Object name: cmo1
number of nodes= 143988    number of elements = 314159
dimensions geometry = 3    element type = tet
dimensions topology = 3    4 nodes   4 faces   6 edges
boundary flag = 16000000   status = inactive

</pre>
