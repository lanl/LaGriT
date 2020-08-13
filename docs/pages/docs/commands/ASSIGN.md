---
title: ASSIGN 
tags: ok
---

# ASSIGN

-------------------

Assign a value to a global variable.  The set of global variables
includes: **incycle**, **ttime**, **monitor**, **hextotet_remove_volume**,
**hextotet_check_imt**, **hextotet_radavg**, **hextotet_remove_duplicates**.

Use **`cmo/setatt`** to assign values to mesh object attributes.

See [Mesh Object Definition](https://lanl.github.io/LaGriT/pages/docs/meshobject.md) for all mesh object variables and attributes.

## SYNTAX 

<pre>
<b>assign</b>/category_name/column/variable_name/value.
</pre>


| GLOBALS                  | DEFAULTS |
| :------------------------| :----------|
| **incycle**                    | 0        |
| **ttime**                      | 0        |
| **monitor**                    | no       |
| **hextotet_remove_volume**     | yes      |
| **hextotet_check_imt**         | no       |
| **hextotet_radavg**            | no       |
| **hextotet_remove_duplicates** | no       |




## EXAMPLES

```
assign/time / 3.2

assign/hextotet_remove_duplicates / yes
```
