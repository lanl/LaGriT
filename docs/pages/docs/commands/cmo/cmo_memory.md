---
Title: "cmo/memory"
Tags: cmo memory
---

# cmo/memory

--------

 Allows the user to preset the size of the memory managed arrays for
 the named Mesh Object.


## SYNTAX

<pre>
<b>cmo/memory</b>/ mo_name / number_nodes/ number_elements
</pre>

`mo_name`is the name of the mesh object, required. The keyword **-cmo-** may be used to act on current mesh object.

`number_nodes` is the size for the nnodes mesh object arrays, required.

`number_elements` is the size for the nelements mesh object arrays, required.


## EXAMPLES

```
cmo/memory/ mo_tet2/1000/10000

cmo/memory/ -cmo- /1000/10000
```
