---
Title: "cmo/move"
Tags: cmo move
---

# cmo/move

Moves or rename a mesh object to a new name.

---------

## SYNTAX
  
<pre>
<b>cmo/move</b>/ mo_name / master_mo /
</pre>

`mo_name` is the new name for master mesh object, required.

`master_mo` is the mesh object to move or rename, default is **-cmo-**. If
  mo_name is the same as master_mo nothing happens. If mo_name
  exists it is over written.


## EXAMPLES

```
  cmo/move/ mo_tet2/mo_tet1

  cmo/move/-mo-/mo_tet1

  cmo/move/ mo_tet2

  cmo/move/ mo_tet2/-cmo-
```
