
 **read/gmv**

  read an ascii or binary gmv file.  This command requires that the
  mesh object name be specified or that a mesh object has been created
  previously.  This file format contains no geometry information.  The
  file may be either ascii or binary;  The code will determine the
  format based on the file header.

 SHORT **FORMAT:**

  **read** / filename.gmv / cmo_name

 
  (Note that the filename is case-sensitive, though the extension itself is not.)

 LONG **FORMAT:**

  **read/gmv** / file_name / [cmo-name]

 **EXAMPLES:**

  **read/gmv** / file.gmv / cmo1

  **read/gmv** / file.gmv
  (a mesh object must alread exist)
  
  For more about GMV and documentation:
  
  [GMV (General Mesh Viewer)](http://www.gmv-barracuda.com/index.html)
  
  The GMV Application is no longer supported but code is available from Github at:
  
  [CPFD Software GMV](https://github.com/CPFDSoftware/gmv)
  
  
  
  
