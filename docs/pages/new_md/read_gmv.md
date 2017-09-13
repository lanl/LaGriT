---
Author: Jan Wills
GENERATOR: 'Mozilla/4.72 [en] (X11; U; Linux 2.2.14-5.0 i686) [Netscape]'
---

 **read/gmv**

  read an ascii or binary gmv file.  This command requires that the
  mesh object name be specified or that a mesh object has been created
  previously.  This file format contains no geometry information.  The
  file may be either ascii or binary;  The code will determine the
  format.

 **FORMAT:**

  **read/gmv**/file\_name/[cmo-name]

 **EXAMPLES:**

  **read/gmv**/file.gmv/cmo1

  **read/gmv**/file.gmv  (a mesh object must alread exist)
