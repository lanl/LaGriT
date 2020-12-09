---
Title: "cmo/derive"
Tags: cmo derive
---

Create a new mesh object derived from an existing mesh object.

# cmo/derive

----------

## SYNTAX

<pre>
<b>cmo/derive</b>/ mo_name / master_mo/
</pre>

`mo_name` is the new mesh object to create,  it will be an image of the master mesh object but will
contain no data. This mesh object will become the current mesh object.
If mo_name is the same as master_mo nothing happens. If mo_name exists it is over written.

`master_mo` is the template for deriving a new Mesh Object. (default is -cmo- or current mesh object).

 
## EXAMPLES

```
cmo/derive/ mo_tet2/mo_tet1

cmo/derive/-cmo-/ mo_tet1

cmo/derive/ mo_tet2

cmo/derive/ mo_tet2/-cmo-

cmo/derive/-default-/-cmo-

cmo/derive/ mo_tet2/-default-

cmo/derive/-default-/ mo_tet1
```
