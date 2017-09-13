---
Author: Jan Wills
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
---

 **read/gmvfreeformat**

  read an ascii gmv file.  This command requires that the mesh object
  name be specified or that a mesh object has been created
  previously.  This file format contains no geometry information.  The
  file will be read using free format  read statement, i.e.
  read(unit,
*)

 **FORMAT:**

  **read**/gmvfreeformat/filename/
[cmo-name
]

  **EXAMPLES:**

  **read**/gmvfreeformat/file.gmv/cmo1

  **read**/gmvfreeformat/file.gmv  ( a mesh object must alread exist)

   


