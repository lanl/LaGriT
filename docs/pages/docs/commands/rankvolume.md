---
title: RANKVOLUME
tags: rankvolume 
---

# RANKVOLUME

-----------------

**`rankvolume`**  prints out the lowest volume elements from a mesh, ranked
  in increasing order. The default is to print out the 100 lowest volume elements, but this number can be changed by specifying it as
  an optional second argument to the command. Also printed are the number of exterior boundary faces and number of interfaces faces for
  each of these elements.

## SYNTAX

<pre>
<b>rankvolume</b>/[number_of_elements_to_rank]
</pre>

## EXAMPLES

```
 rankvolume/10
```
write 10 lowest volume elements in increasing order.

The output will look like:
<pre class="lg-output">
elt. no.       volume     #ext.bound.faces #int.bound.faces
         6  0.666667E+03      4          1 
         5  0.666667E+03      4          0
         4  0.666667E+03      3          1
         1  0.666667E+03      4          0
         2  0.666667E+03      4          0                                      
         3  0.666667E+03      3          0
        12  0.100000E+04      5          0
        11  0.100000E+04      5          0
         7  0.150000E+04      4          0
        10  0.150000E+04      4          0

</pre>
