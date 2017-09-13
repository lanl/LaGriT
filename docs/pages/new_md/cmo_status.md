---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
---

**status**/
[mo\_name
]/
[**brief**
]
mo\_name is type character, default is '**-all-'**

Prints the status of Mesh Objects.  This includes a header with
information about the type of mesh object and the size of the mesh
object.  After the header information, a table lists all variables
associated with a mesh object and includes the variable's type, rank,
length, interpolation mode, persistence, ioflag and default value.  If
**brief** is specified only the header information is printed.
** ****EXAMPLES:**
**cmo/status**/mo\_tet2

**cmo/status/-cmo-**

**cmo/status**/mo\_tet2**/brief**

**cmo/status**

**cmo/status/-all-**

**cmo/status/-default-**
An example of header information follows:

1 Mesh Object name: cmo1

number of nodes= 143988  number of elements = 314159

dimensions geometry = 3  element type = tet

dimensions topology = 3  4 nodes   4 faces   6 edges

boundary flag = 16000000  status = inactive
