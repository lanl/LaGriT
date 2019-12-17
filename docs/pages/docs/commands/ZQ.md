---
title: ZQ
tags: zq, deprecrated 
---

# ZQ (deprecrated)

---------------------

Deprecated command, replaced by [cmo/setatt](cmo/cmo_setatt.md) and [cmo/printatt](cmo/cmo_printatt.md) . 

Set or print node attribute values of a selected set of nodes.  

## SYNTAX

<pre>
<b>zq</b> / att_name / ifirst,ilast,istride/ value

<b>zq</b> / att_name / ifirst,ilast,istride/ 
</pre>

`att_name` is the name of the attribute to set or write

`ifirst,ilast,istride` is the range of nodes to set or write where 1,0,0 are all nodes.

`value` set attribute of selected nodes to this value.


If `value` is ommited, the command will print values instead of set values.
To print, specify any one of a group and all will be printed. 
To set an attribute value, set value and all selected nodes will be set to this value. 

For printing, attributes are grouped as follows: 
- Group1: isq,imt,itp (material type and point types) 
- Group2: x,y,z (coordinates) 

## EXAMPLES

```
zq/imt/1,100,2/ 1/ 
```
will set imt attribute to 1 for all odd numbered nodes between 1 and 100 to 1. 

```
zq/xic/1,0,0/ 
```
will print coordinates of all points


