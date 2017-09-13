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

   


