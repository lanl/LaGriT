---
Title: cmo/copy
---

# cmo / copy

Makes an exact copy of a Mesh Object including all data.


## SYNTAX

**cmo / copy**/ mo_name /master_mo


- mo_name is type character, required. This will be the new mesh object and will become the Current Mesh Object. If mo_name exists it is over written.

- master_mo is type character, default is '-cmo-'. This is the mesh that will be copied. If mo_name is the same as master_mo nothing happens.


## EXAMPLES

```
cmo/copy/mo_tet2/mo_tet1

cmo/copy/-cmo-/mo_tet1

cmo/copy/mo_tet2

cmo/copy/mo_tet2/-cmo-
```
All examples will create the new mesh object *mo_tet2* copied from master *mo_tet1* assuming *mo_tet1* is current mesh object.

```
cmo/ copy/ mopts / motet

cmo/ create / mopts  
copypts / mopts / motet
```
These examples have different behaviors. **`cmo/copy`** will create a duplicate cmo from *motet* and name it *mopts*. Assuming *motet* is a tet mesh, *mopts* will also be a tet mesh. The example command **`copypts`** will copy all nodes into the empty new mesh object named *mopts*. No elements will be copied.

