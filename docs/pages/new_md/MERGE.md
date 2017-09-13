---
GENERATOR: 'Mozilla/4.51C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: MERGE
---

 

 **MERGE**

  Merge pairs of points together. On return, the first\_point of a
  pair is the survivor unless first\_point may not be removed ( a
  corner point for example), then the command operates as if
  first\_point and second\_point have been interchanged. If there is
  no confirmation of the merge, one or both of the points may be
  inactive, or the merge may be illegal because the points are not
  neighbors or because this merge is disallowed by the merge tables.
  Merging may trigger other merges by the reconnection step that
  follows the merge.

  The command [massage](MASSAGE.md) may be used to merge nodes
  together based on the edge distance separating the nodes.

 FORMAT:

  **merge**/first\_point/second\_point

  **merge**/1st\_of\_pair1/2nd\_of\_pair1/1st\_of\_pair2/2nd\_of\_pair2/../
  1st\_of\_pairn/2nd\_of\_pairn

EXAMPLE:

**merge**/21,22/
