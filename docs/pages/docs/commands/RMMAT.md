---
title: RMMAT
tags: rmmat, remove material
--- 

# RMMAT


-----------------


This routine is used to remove points that are of a specified material value (**itetclr** for elements or **imt** for nodes). Elements with the specified material value are
  flagged by setting the element material type negative. They are not removed from the mesh object.
 

Remove the dudded elements and update the mesh object arrays with the command [**`RMPOINT/compress`**](RMPOINT.md).


## SYNTAX

<pre>
<b>rmmat</b>/material_number/


<b>rmmat</b>/material_number/[<b>all</b> or <b>node</b> or <b>element</b>]/ [<b>exclusive</b>]
</pre>

`material_number` is the **itetclr** integer value to remove from the mesh object. Elements with the itetclr=material number are flagged by setting the element material value negative.


**`no arguments`**  or **`all`** removes nodes with imt = material number and removes elements with itetclr= material number.


**`node`** removes nodes with imt = material number. 
 

**`element`** removes elements with itetclr= material number. 
 
 
**`exclusive`** removes everything except nodes with imt =material and removes everything except elements with itetclr= material number.


## EXAMPLES

```
rmmat/ 2 
rmpoint/compress
```
Tag elements in mesh object with material values of 2 then remove elements and update the mesh object.

 

Click here for [Examples](../demos/main_rmmat.md)

 
