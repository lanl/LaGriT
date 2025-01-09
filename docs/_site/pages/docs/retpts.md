**Retrieving Point Sets and Element Sets**

**eltlimc** returns an array of element numbers where the elements
belong to the **eltset** given in the argument list. ** ** Eltsets must
be specified by name. On return the array pointed to by ipmpary will
contain the mpno element numbers that belong to the eltset.

**eltlimc**(ich1, ich2, ich3, ipmary, mpno, ntets, xtetwd)

Argument | Description
--- | ---
ich1,ich2,ich3 | eset,get,eltset_name (character*32) 
ipmpary |     pointer to array of elements of eltset_name that is filled on output 
mpno |         integer number of elements in eltset_name (output) 
ntets |           integer number of elements in mesh object (input) 
xtetwd |       array of eltset membership information (input) see See III.a

**pntlimc, pntlimn** return an array of node numbers where the nodes
belong to the **pset** given in the argument list. On return the array
pointed to by ipmpary will contain mpno node numbers. These numbers
are the nodes that belong to the **pset**.

**pntlimc**(ich1, ich2, ich3, ipmary, mpno, npoints, isetwd, itp1)

Argument | Description
--- | ---
ich1,ich2,ich3   |  pset,get,pset_name (character*32) 
ipmpary |   pointer to array of node number of pset_name that is filled on output 
mpno |        integer number of nodes in pset_name (output) 
npoints |     integer number of nodes in mesh object (input) 
isetwd |      array of pset membership information See III.a 
itp1 |          array of point types See III.a32G

**pntlimn**(ifirst, ilast, istride, ipmary, mpno, npoints, isetwd, itp1)

Argument | Description
--- | ---
ifirst,ilast,istride | point range: first node, last node, stride between nodes (integers) 
ipmpary |   pointer to array of node number of pset_name that is filled on output 
mpno |        integer number of nodes in pset_name (output) 
npoints |     integer number of nodes in mesh object (input) 
isetwd |      array of pset membership information See III.a 
itp1 |          array of point types See III.a

