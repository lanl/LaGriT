---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
---

CREATE\_GRAPH

 Create a node or dual (element) adjacency graph. If node option is
 selected, the graph of node adjacency is created, if dual option is
 selected, the graph of element adjacency (dual graph) is created.

 For details of METIS algorithms and descriptions of the third command
 line argument see:

 [http://www-users.cs.umn.edu/
~karypis/metis](http://www-users.cs.umn.edu/~karypic/metis)

 See [METIS](metis.md)documentation for description of graph format.

 The default name of the attributes that are created are different
 depending on which option (metis or lagrit) is used.
 create\_graph / metis / 
[node  dual
] / 
[nxadj
] / 
[nadjncy
]

 create\_graph/ lagrit / dual / jtetoff / jtet



LIMITATIONS

 The metis option will not work on a hybrid mesh. Supported element
 types are tri, tet, quad, hex.

 The lagrit option will only produce the dual adjacency graph. The only
 option for the name of the graph arrays are jtetoff and jtet. The
 present implementation is just a wrapper on the geniee command.

 METIS Interface to LAGriT

 The standard libraries, liblagrit.a and libutil.a do not contain
 METIS. In order to utilize the METIS functions one must download the
 METIS package, build the METIS libraries on your local system and link
 them with the LAGriT libraries. See instructions in documentation of
 the [metis](metis.md) command.

FORMAT:

 create\_graph / metis / 
[node  dual
] / 
[nxadj
] / 
[nadjncy
]

EXAMPLES:

 create\_graph / metis / dual / -def- / -def-

 create\_graph / metis / node / -def- / -def-

 create\_graph / metis / dual / ie1 / ieadj1

 create\_graph / metis / node / in1 / inadj1

 create\_graph/ lagrit / dual / jtetoff / jtet
