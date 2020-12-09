---
Title: "cmo/length"
Tags: cmo length
---

# cmo/length

Returns the memory length of an attribute for a Mesh Object.


------------

## SYNTAX

<pre>
<b>cmo/length</b>/ mo_name / att_name
</pre>

`mo_name` is the name of the mesh object, default is **-all-**

`att_name` is the name of the attribute, default is **-all-**



## EXAMPLES

```
cmo/length/ mo_tet2/boron

cmo/length/-cmo-/boron

cmo/length/-default-/boron

cmo/length/-cmo-/-all-

cmo/length/ mo_tet2/-all-

cmo/length

cmo/length/-all-/-all-

cmo/length/-all-/boron
```
