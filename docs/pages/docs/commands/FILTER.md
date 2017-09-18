
 **FILTERKD**

 **FILTER**

 **FILTER/ELEMENT**

  This command is used to filter (mark for deletion) a set of nodes
  that are geometricly close, within the tolerance value. This command
  changes the node type of the deleted nodes to type 'dudded' (itp=21)
  but does not remove them from the point list. There must be least
  one point must be specified in the point range in order for this
  command to work properly.

  

  Dudded nodes are not removed from the mesh object, but are ignored
  by many routines because of their dudded status. Use the command [rmpoint/compress](RMPOINT.md) to
  remove dudded nodes and update the mesh object.

  
  
  If the mesh object has connected elements, the itet values in the
  connectivity array are adjusted. This may change topology. To filter
  duplicate elements or flat elements, see **filter/element** or
  **filterkd** with the **zero\_element** option.

 **FORMAT**:

  **filterkd**/ ifirst,ilast,istride/ [tolerance] / [**nocheck** OR **zero\_element**]

  

  **filter**/ ifirst,ilast,istride / [tolerance] / [ **min OR max** ] / attribute]

  *This version of filter is being replaced with the kd-tree search
  algorithm used in filterkd that more accurately identifies nodes
  within the tolerance value.*

 
   iirst,ilast,istride defines the node set where 1,0,0 are all nodes
   in the mesh object.

  
   [tolerance]

  This is the epsilon value used to measure node
   distances. If this value is not included, the mesh object epsilon
   value is used.
  
   **nocheck** 

   This is the default for **filterkd** and will skip the code
   that removes zero elements. The behavior is similar to original
   **filter** except a kdtree structure is used instead of a binning
   method for finding nodes within epsilon value. This version has
   been shown to be more accurate where precision matters.
  
   **zero\_element** 

   This option is available with **filterkd** where
   a mesh has connected elements. The algorithm removes flat elements
   as using the tolerence value as mininum edge length. Duplicate
   nodes not associated with these flat elements are not removed.
  
   **minmax** / attribute 

   This option is available with the **filter**
   command. Nodes for deletion are detected based on the standard
   geometric criteria however, the choice about which node is
   retained is determined based on comparison of the attribute values
   and the node with either the ** min** or ** max** value is retained.
 
  **filter**/[element]/[search\_range]/ [**nodelete** OR **delete**]
 
  Search a mesh object for duplicate elements. A duplicate element
   is defined as having the exact same set of nodes in the element
   connectivity list (itet). The order of the nodes in the
   connectivity does not matter. The element with the larger itetclr
   value (master) wi ll be kept. The duplicate element will have its
   material color (itetclr) changed to max(itetclr) + 1. Two new
   element attributes (iclr1, iclr2) are added to the mesh object to
   keep track of the correspondence of
   master(retained)/duplicate(removed) elements and their original
   material id (itetclr).  The option **nodelete**is the default and
   elements are not removed. Use option **delete** to remove the
   elements marked as duplicate. ]{style="font-family: times;"

   The search\_range can be set by the user. This number limits the
   number of elements above and below an element (in numerical order)
   to search. The default is 10.

   For more on **filter/element** see [filter/element
   details.](FILTER_element.md)

  **EXAMPLES:**

  **filterkd**/1,0,0

  **filter**/1,0,0

  Filter all nodes and delete duplicates with tolerance distance set
  to mesh object epsilon value. Both filter and filterkd behave the
  same but filterkd performs with better precision.

  

  **filterkd** / 1 0 0 / 1.e-3

  Filter all nodes and delete duplicates where epsilon tolerance is
  set by user to 1.e-3.

  

  **filterkd** / [pset] / get_point\_set

  Filter a subset of the nodes and delete duplicates with epsilon
  tolerance set automaticly.

  

  **filterkd** / 1 0 0 / 1.e-3 / **zero\_element**

  Identify and remove all flat elements with edge length less than
  1.e-3 as defined by user.

  

  **filter** / 1 0 0 / / min / imt

  Filter all nodes and delete duplicates with epsilon tolerance set
  automaticly. When duplicate nodes are detected the imt attribute is
  examined and the node with minimum imt value is retained.

  

  **filter** / [element]/ [nodelete]

  Filter elements and set itetclr of duplicates to max(itetclr) + 1.
  Assign values to iclr1 and iclr2 arrays.

  

  **filter** / [element] / 1e20 / [delete]
  
  Filter all elements (assuming there are less than 1e20)  with an
  exhaustive search and delete duplicate elements. Assign values to
  iclr1 and iclr2 arrays.

