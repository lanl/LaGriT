---
title: MATH
tags: math
---

# MATH

-------------------

The **`math`** routine operates on attributes of a mesh object(s). It
performs arithmetic operations or evaluates mathematical functions on
the source mesh object or objects, and places the results in the sink
mesh object. The source and sink mesh objects can be the same, and
there can be either one or two source objects, depending on the
operation selected.

All attributes must have the same type, rank, and length.



Mathematical functions other than **`floor`**
and **`ceiling`** are not implemented for attributes whose values are
integers.

## SYNTAX

<pre>
<b>math</b>/operation/cmo_sink/attr_sink/1,0,0/cmo_src/attr_src / [value]

<b>math / integrate </b>/cmo_sink/attr_sink/1,0,0/ attr_src_field
</pre>

`operation` The first parameter a keyword that indicates the type of work to perform, see below.

`cmo_sink` `attr_sink`  are the sink cmo and sink attribute for the math
results to be written to. These parameters are required for all math
operations.


`1 0 0` is the selected range istart,istride,istop where 1,0,0 represents all. The selections can also be defined by `pset` or `eltset`.


`value` is required by some math operations and can be of type constant or can be a cmo attribute.
For the standard arithmetic operations, value can be either a constant
or an attribute. These arithmetic operations work for all types of
attributes. For the mathematical functions other than **`floor`** and **`ceiling`**,
value is omitted, and the function is performed on the `src_attr` and
stored in the `sink_attr`. 



`cmo_src2/attr_src2/` where `cmo_src2` may be the same name as the source cmo, or the name of a second source cmo.

`attr_src2` assumes attribute is part of `cmo_src`

`constant` is a numerical value that is provided for some of the operations.


### OPERATIONS

**`plus add minus sub subtract times multiply mult divide min max modulo`** are arithmetic operators; the result is stored in
`sink_attr` where `sink_attr` = (`src_attr`) *operator* `value`. Where `value` can be a constant or mesh object attribute.

**`min max`** are not to be confused with the minimum or maximum value of an attribute; rather the the result is a
comparison of pairs of source values.

**`sin cos tan ln`** (natural log), and **`log10`** are mathematical
functions. The value parameter is omitted, and the function is
performed on the `src_attr` and stored in the `sink_attr`. These
functions are not implemented for integer attributes.

**`floor`** and **`ceiling`** are mathematical functions where value is
used as the lower or upper limit, the value(s) of `src_attr` are
checked against value, and the results are stored in the `sink_attr`.
These functions work for all types of attributes.

**`power`** function uses both value parameters. The first value or
`src_attr` is raised to the power of the second value or attribute. You
cannot use two constants. At least one of the sources must be an
attribute. The result is stored in the `sink_attr`.

**`exp`** and **`exp10`** functions raise the constant *e* or the constant
10 to the power specified by `src_attr` and stores the result in the
`sink_attr`.

**`sum`** adds all node or element values in attr_src, within the
selected range and writes the result to attr_sink. The sink attribute
must be of type 'REAL' or 'INT' (length='scalar') and will be created
if it does not exist.

**`integrate`**  *for elements only*. It is a function that computes the product of 'field_value' times the 'element volume' at each element and either saves these products (or sums) to the sink attribute.


The 'field_value' for an element is either the value
of attr_src (if attr_src is an element attribute and has length
'nelements') or is the average of the values at the vertices of the
element (if attr_src is a node attribute).  

If `sink_attr` does not exist or if it exists and has length
'nelements', the products are stored in `sink_attr`.

If `sink_attr` exists and has length 'scalar', then the
products are summed up and the result is stored in `sink_attr`.

If the user requires just the integrated sum this alternative avoids
having to use the pair of commands **integrate**, **sum** and also avoids creating the
'nelement' long sink attribute.



## EXAMPLES

```
math/multiply/ sink_mo/sink_attribute/50,60,3/src_mo/src_attribute/1.0

math/add/ mo/attribute/50,60,3/mo/attribute/100.0

math/modulo/ mo/attribute/1,0,0/mo/attribute/10

math/sub/ sink_mo/sink_attribute/50,60,3/src_mo1/src_attribute1/src_mo2/src_attribute2/

math/min/ sink_mo/sink_attribute/1,0,0/src_mo1/src_attribute1/src_mo2/src_attribute2/

math/ln/ sink_mo/sink_attribute/1,0,0/src_mo/src_attribute/

math/floor/ sink_mo/sink_attribute/1,0,0/src_mo/src_attribute/2.0/

math/power/ sink_mo/sink_attribute/1,0,0/src_mo/src_attribute/2.0/

math/power/ sink_mo/sink_attribute/1,0,0/2.0/src_mo/src_attribute/

math/power/ sink_mo/sink_attribute/1,0,0/base_mo/base_attribute/power_mo/power_attr

math/exp/ sink_mo/sink_attribute/1,0,0/src_mo/src_attribute/

math/exp10/ sink_mo/sink_attribute/1,0,0/src_mo/src_attribute/

math/integrate/ cmotri /Vf /1,0,0/ cmotri/ Fn

math/sum/cmotri / Vfsum /1,0,0/ cmotri / Vf

math/sum/cmotri / area_sum /1,0,0/ cmotri / darea

```
