---
Title: cmo/delatt
Tags:
---

# cmo/delatt

------------

Deletes an attribute from a Mesh Object. 

## SYNTAX

<pre>
<b>cmo / delatt </b>/ mo_name / att_name

<b>cmo / DELATT </b>/ mo_name / att_name
</pre>


**`delatt`** delete attribute from the current Mesh Object. This will not
delete an attribute with a persistence of 'permanent'.


**`DELATT`** delete attribute from the current Mesh Object even if it has a persistence of 'permanent'.

`mo_name` is the mesh object. The mesh object name must be specified, but keywords can be used, such as -cmo- for current mesh object.


`att_name` is the name of the attribute to delete from the mesh object. The attribute name must be specified; `-all-` names many attributes at once:

- **`cmo / delatt / mo_name / -all-`** deletes every attribute whose persistence is temporary (the default of **cmo / addatt**). Permanent attributes are not touched, and neither are the standard attributes, those every mesh object has from **cmo / create**.
- **`cmo / DELATT / mo_name / -all-`** deletes every attribute except the standard attributes that exist when **cmo / create** runs, whatever its persistence.

Each prints how many attributes it deleted and how many it kept. (LaGriT V3.4.0 and earlier refused `-all-` with "Attribute does not exist: -all-": GitHub #163, #108.)



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
cmo/DELATT/ mo_tet / -all-
```
This will delete every attribute of mo_tet that is not one of the standard attributes, permanent ones too.

```
 cmo/DELATT/ mo_tet/ imt1
 cmo/DELATT/ mo_tet/ itp1
 cmo/DELATT/ mo_tet/ icr1
 cmo/DELATT/ mo_tet/ isn1
 ```
Even though imt1, itp1, icr1, and isn1 are permanent, they are removed from mesh object named mo_tet. This can be useful when writing mesh files where these attributes are not needed and can reduce the written file size.
 
