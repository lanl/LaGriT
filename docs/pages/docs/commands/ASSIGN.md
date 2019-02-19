---
title: ASSIGN 
tags: ok
---

# ASSIGN

Assign a value to a global variable.  The set of global variables
includes: `incycle`, `ttime`, `monitor`, `hextotet_remove_volume`,
`hextotet_check_imt`, `hextotet_radavg`, `hextotet_remove_duplicates`.

The default values of these variables are: 0, 0, no, yes, no, no. Use **`cmo/setatt`** to assign values to
mesh object **attributes**.

| GLOBALS                      | DEFAULTS |
|------------------------------|----------|
| `incycle`                    | 0        |
| `ttime`                      | 0        |
| `monitor`                    | no       |
| `hextotet_remove_volume`     | yes      |
| `hextotet_check_imt`         | no       |
| `hextotet_radavg`            | no       |
| `hextotet_remove_duplicates` | no       |


## FORMAT:

<pre>
<b>assign</b>/category_name/column/variable_name/value.
</pre>

## EXAMPLES:

<pre>
<b>assign/time</b> / 3.2
<b>assign/hextotet_remove_duplicates</b>/ yes
</pre>
