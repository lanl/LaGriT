---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

  

 **move**/mo\_name/master\_mo/

  mo\_name is type character, required.

  master\_mo is type character, default is **-cmo-**'

  Changes the name of Mesh Object, master\_mo, to mo\_name. The output
  Mesh Object, mo\_name, will become the Current Mesh Object. If
  mo\_name is the same as master\_mo nothing happens. If mo\_name
  exists it is over written.

 **EXAMPLES:**

  **cmo/move**/mo\_tet2/mo\_tet1

  **cmo/move/-**cmo-/mo\_tet1

  **cmo/move**/mo\_tet2

  **cmo/move**/mo\_tet2**/-cmo-**
