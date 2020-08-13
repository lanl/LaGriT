---
title: RESETPTS
tags: resetpts, itp
---

# RESETPTS

-----------------------

Reset values for node **imt** material color, **itp** type, or **isn** parent-child for doubly defined nodes.

 
 

## SYNTAX

<pre>
<b>resetpts</b>
<b>resetpts/parent</b>

<b>resetpts/itp</b>

<b>resetpts/cell_color</b>/ [1,0,0 or integer_node_color] 
</pre>





**`resetpts`** or **`resetpts/parent`**   remove child points. the parent child flags are reset.  All child points are eliminated and the connectivity list **isn** is corrected to reference only the parent points.
This parent-child relationship is established by the **`settets`** command.


**`resetpts/itp`** set node type **itp** from connectivity of the mesh object.
  The node  **itp** (also known as **itp1**) array is reset to indicate whether
  each node is in the interior (0), on an interior interface (2), on a
  reflected boundary (10), or on a reflected interface boundary (12) .
 It is good practice to update this array anytime the mesh object is changed, or to be certain that boundary and interfaces are correctly set for a command that depends on it.


**`resetpts/cell_color/`** set node **imt** values from element colors **itetclr**. 

-   If no arguments are given, then, loop through all itetclr values in ascending order, and reset node **imt** to associated element **itetclr** value. Note that if parent-child nodes do not exist, then an interface node will have its **imt** value set to the largest value of **itetclr** of all elements that contain this node.

- `1,0,0` is the range of element **itetclr** values itetclr_min, itetclr_max, itetclr_stride where 1,0,0 means all. This will loop through and reset node **imt** values.  Node colors are reset only for nodes in elements that fall in the range selected.

- `integer_node_color` reset node **imt** for nodes with **imt** equal to integer_node_color, use the itetclr of the element containing the node.  Only nodes with node color **imt** equal to this value will be set to its element **itetclr** value. 
This will introduce a bias since the nodes are modified in the order of the element numbering. To give some
control over the bias the user can specify a negative value for `integer_node_color`. In that case, the element loop is reversed and goes from largest to smallest element number.


## EXAMPLES:

```
resetpts/itp  
```
Set node type array **itp** from connectivity of mesh includes outside boundary and interior interface nodes based on **itetclr** values.

```
resetpts/cell_color/ 1   
```

Replace node color for nodes that currently have imt value of 1 by the cell color of an element containing the node; this is done by looping through all the elements in cell color order, so that the value of imt will be the largest itetclr of the set of elements containing this node.

```
resetpts/cell_color/  
resetpts/cell_color/ -1  
resetpts/cell_color/1,3,1   
```

Thi first command will loop through all element colors and reset all node imt values and replace node color for nodes that currently have imt value of 1 by the cell color of an element containing the node; this is done by looping through all the elements in desending cell color order, so that the value of imt will be the smallest itetclr of the set of elements containing this node.

The second command does the same thing as the first, in reversed order.

The third command loops through colors from itetclr=1 to itetclr=3.

```
cmo/select/cmotet                                                               
resetpts/parent                                                                 
rmpoint compress                                                                
filter/1,0,0         
```
These commands will reset double defined nodes in a mesh to be singly defined. This is used when writing mesh files for applications that can not handle duplicate nodes. This will reset the parent-child arrays, then remove duplicate nodes from the mesh object records so they are not written to files.

