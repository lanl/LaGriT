---
Title: cmo/delatt
---

# cmo / delatt

Deletes the attribute att_name from the current Mesh Object, mo_name. This will not
delete an attribute with a persistence of 'permanent'.

## SYNTAX

**cmo** / **delatt**/ `mo_name` / `att_name`

**cmo** / **DELATT**/ `mo_name` / `att_name`


**`delatt`** Deletes the attribute `att_name` from the current Mesh Object, `mo_name`. This will not
delete an attribute with a persistence of 'permanent'.


**`DELATT`** Deletes the attribute `att_name` from the current Mesh Object, `mo_name` even if it has a persistence of 'permanent'.


The mesh object name must be specified, but keywords can be used, such as -cmo- for current mesh object.

The attribute name must be specified, but keywords can be used, such as -all- for all attributes in the mesh object.


 ## EXAMPLES
 
```
cmo/delatt/ mo_tet /boron
cmo/delatt/ -cmo- / boron
```
If mo_tet is the current mesh object, both commands will delete the attribute named boron from mo_tet.

```
cmo/delatt/ -cmo- / -all-
```  
This will delete all attributes from the current mesh object with persistence of temporary.

```
 cmo/DELATT/ mo_tet/ imt1
 cmo/DELATT/ mo_tet/ itp1
 cmo/DELATT/ mo_tet/ icr1
 cmo/DELATT/ mo_tet/ isn1
 ```
Even though imt1, itp1, icr1, and isn1 are permanent, they are removed from mesh object named mo_tet. This can be useful when writing mesh files where these attributes are not needed and can reduce the written file size.
 
