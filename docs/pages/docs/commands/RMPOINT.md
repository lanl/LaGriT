---
title: RMPOINT
tags: ok
---

 **RMPOINT**

  Removes nodes or marks nodes for removal points or removes elements
  from a mesh.   The first option sets the node type flag 
  [itp=**ifitpdud** (21)] to indicate that the set of nodes are
  treated as invisible, but does not actually remove the nodes. 
  Elements will also be removed.  If  **inclusive** is specified, any
  element containing a marked node will be removed.  If  **exclusive**
  is specified (default), any element containing a retained node is
  retained.  The second option, **compress**, removes the invisible
  nodes (i.e. those nodes whose itp1 value is 21) from the data
  structure and material-wise resequences all remaining nodes.  The
  third option, **zero\_volume**, will remove elements whose volumes
  are less than or equal to the specified threshold.  The fourth
  option, **element,** will remove all marked elements from the mesh. 
  Marked elements have a negative value for the first entry in the
  itet vertex list.  The fifth option will remove a specified list of
  elements from the mesh.  The sixth option will remove elements that
  are specified in a named element set from the mesh. The seventh
  option, **womesh** will delete stray nodes that are not connected to
  any element and that are not parent nodes.

**FORMAT:**

**rmpoint**/ifirst,ilast,istride/[**exclusive** **inclusive** ]

**rmpoint**/**compress**/

**rmpoint/zero\_volume**/threshold

**rmpoint/element**

**rmpoint/element**/tet list

**rmpoint/element/eltset,**get,esetname

**rmpoint/womesh**

**EXAMPLES:**

    rmpoint/pset, get, pset1

mark all the nodes in pset1 for removal.  Remove elements all of
whose vertices are members of pset1.

    rmpoint/compress

remove all marked nodes and correct the itet array

    rmpoint/zero\_volume/1.e-16

remove all elements with volumes less than 1.e-16

    rmpoint/element/27 259 1009

remove the three specified elements

    rmpoint/element/eltset, get, myeset
