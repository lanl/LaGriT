---
title: EDIT
tags: edit
---

# EDIT

-------------------

## SYNTAX

<pre>
<b>edit </b>/ iopt / ifirst,ilast,istride / material_# or name/

<b>edit/ angular </b>/ifirst,ilast,istride /material_# or name/xcen 

<b>edit/radial  </b>/ifirst,ilast,istride /material_#  or  name/xcen
 
<b>edit/ points </b> /ifirst,ilast,istride /material_#  or  name/array1,array3,array4/
</pre>

Prints an edit of various quantities based on the value of the
  option argument, the point limits, and/or a material specification.
  iopt specifies what to print as follows:
 
   no value for iopt --edit of sums, averages, and extrema of
   position coordinates (x,y,z), and of mesh object attribute fields

   **cwo** gives same information as the default, but only for the
   two points specified.

   **parts** gives a list of materials types, their names, count and
   sequence.

   **points** lists up to 4 cell-center array values for a set of
   points. Possible array values are: xic,yic,zic,or mesh object
   attribute name

   
## EXAMPLES

```
edit/ parts/
  
edit/point /pset,get,mypoints/
```
 
