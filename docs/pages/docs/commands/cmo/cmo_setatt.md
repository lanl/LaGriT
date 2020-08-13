---
Title: "cmo/setatt"
Tags: cmo setatt
---

# cmo/setatt

-------------

Operation to set mesh object attributes with values.

See the description of mesh object and attributes for setting options. [Mesh Object}(../../meshobject.md)


## SYNTAX

<pre>
<b>cmo/setatt</b> / mo_name / attribute_name /ifirst,ilast,istride/ value

<b>cmo/setatt</b> / mo_name / attribute_name / value
</pre>


`mo_name` is the name of the mesh oject to set values

`attribute_name` is the name in the mesh object to operate on

`ifirst,ilast,istride` is the selection where 1,0,0 will operate on all members of the attribute. The node or element set syntax can be used instead of the integer range. See **pset** and **`eltset`** commands.
If the range selection is not present, all members are applied.

`value` is type integer or real depending on the type of the attribute.
The `value` is assigned to the selected range of `attribute_name'.



## EXAMPLES

```
cmo/setatt/mo/imt/1,0,0/1
cmo/setatt/mo/itp/1,0,0/0
```
Will set all values of node attribute imt to 1 and itp1 to 0, this is often done to set defaults  before using **connect**.

```
cmo/setatt/ 3dmesh /itetclr /eltset,get,blue/ 3
```
Will set all elements of attribute itetclr and in the element set 'blue' to have the value of 3

```
cmo/setatt // ndimensions_geom / 2
```
Will reset the ndimensions geometry attribute of the active mesh object to 2


```
define ATT_RESET xfield
cmo/setatt/ -def- / ATT_RESET /pset,get,p_reset/ 0.0d0
```
The attribute name is defined in a variable call ATT_RESET. The value zero is assigned to the  **p_reset** node selection. This will operate on the current active mesh object, use **cmo/select** to make a mesh object current. 

