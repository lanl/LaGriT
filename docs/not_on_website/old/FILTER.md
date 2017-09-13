---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: FILTER
---

 

 **FILTER**

  Used to filter (delete) points that are too close ( default distance
  &lt;=1.e-16), closer than the tolerance specified by the user, or
  duplicate points. This command records the deleted points as dudded
  out points (itp=21) but does not remove them from the point list.
  Note that at least one point must be specified in the point sequence
  numbers (ifirst,ilast,istride) in order for this command to work
  properly.

 FORMAT:

  **filter** / ifirst,ilast,istride / 
[tolerance
]


