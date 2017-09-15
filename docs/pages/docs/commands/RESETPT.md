---
title: RESETPTS
---

 **RESETPTS**

  Reset node values.
 
  If option is **parent** (default) the parent child flags are reset.
  All child points are eliminated and the connectivity list is
  corrected to reference only the parent points.
 
  If option is **itp** the itp1 array is reset to indicate whether
  each node is in the interior (0), on an interior interface (2), on a
  reflected boundary (10), or on a reflected interface boundary (12) .
  Resetting itp would be used if nodes were removed (such as with
  **rmmat**) leaving new boundaries
 
  If option is **ell\_color** then node color (**imt**) is set based
  on element color(**itetclr**).  There are three behaviors possible
  depending on whether 0, 1 or 3 arguments are specified.
 
  -   If no arguments are given, then, loop through all **itetclr**
      values in ascending order, and reset node **imt** to element
      colors, (**itetclr**). Note that if parent/child nodes do not
      exist, then an interface node will have its **imt** value set to
      the largest value of **itetclr** of all elements that contain
      this node.
  -   If three arguments are given, then these 3 arguments are
      interpreted as itetclr\_min, itetclr\_max, itetclr\_stride. 
      Node colors are reset only for nodes in elements that fall in
      the subset selected.  See examples given below.
  -   If one argument is given, this argument is an node **imt** value
      and only nodes with node color (**imt** ) equal to this value
      will be set to the element color (**itetclr**).  This option
      loops through each node of each element and if the node color
      (**imt**) is equal to the user specified value
      (integer\_node\_color) then it is changed to the element color
      (**itetclr**). This will introduce a bias since the nodes are
      modified in the order of the element numbering. To give some
      control over the bias the user can specify a negative value for
      integer\_node\_color. In that case, the element loop goes from
      largest to smallest element number.

 **FORMAT:**

**resetpts**   

remove child points

**resetpts/parent**   

remove child points

**resetpts/itp**

set node type (**itp**) from connectivity of mesh

**resetpts/cell\_color**/

set all node colors (**imt**) from element colors(**itetclr**)

**resetpts/cell\_color**/istart,iend,istride     

set all node colors (**imt**) from element colors(**itetclr**)

**resetpts/cell\_color**/ integer\_node\_color   

reset node **imt** for nodes with **imt** currently = integer\_node\_color from the **itetclr** of an element that contains the node

**EXAMPLES:**

**resetpts/itp**  

             set node type from connectivity of mesh

**resetpts/cell\_color**/1   

          replace node color for nodes that currently have **imt** value of 1 by the cell color of an element containing the node; this is done by looping through all the elements in cell color order, so that the value of **imt** will be the largest **itetclr** of the set of elements containing this node.

    **resetpts/cell\_color**/  

            loop through all element colors and reset all node **imt** values
    
**resetpts/cell\_color**/-1  

          replace node color for nodes that currently have **imt** value of 1 by the cell color of an element containing the node; this is done by looping through all the elements in desending cell color order, so that the value of **imt** will be the smallest **itetclr** of the set of elements containing this node.

    **resetpts/cell\_color**/1,0,0   

      loop through all element colors and reset all node **imt** values (same as previous example)

    **resetpts/cell\_color**/1,3,1   

      loop through colors from **itetclr**=1 to **itetclr**=3

    **resetpts/cell\_color**/3,1,-1** ** 

  loop through colors from **itetclr**=3 to **itetclr**=1
