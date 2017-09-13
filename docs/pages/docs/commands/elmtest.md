---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: FIELD
---

 

 **ELMTEST**

  This command test a mesh for valid jtet connectivity.  If the mesh
  is a network it allows for jtet loops; in this case the jtet
  relationship is not reflexive but, for example if the loop has
  length 3.
  Normally degenerate faces may not have neighbors with a different
  number of nodes; however, if  a scalar mesh attribute
  'jtet\_reduce\_nnd' is defined and has the value 1, faces will match
  if the node numbers are the same even if a node number appears more
  that once.

 FORMAT:

  **elmtest** /
[/nwrite
] where nwrite is the number of warning
  messages to print.  The default for nwrite is 20.

 EXAMPLES:

  **elmtest**

  **elmtest**//100
