---
Author: Jan Wills
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
---

 **read/lagrit**

  read in a lagrit restart file

 **FORMAT:**

  **read****/lagrit**/file\_name/
[cmo-name
]/
[**ascii**
  **binary**
]

       cmo\_name    -    ignored all mesh objects are read from the
  file

      The default is **ascii**, but the code will determine file type
  if nessary.

 **EXAMPLES:**

  **read****/lagrit**/file1        file1 will be read as ascii.  If
  the read fails to find the ascii check string, the file will be
  closed and reopened for a binary read.
  **read****/lagrit**/file2/**/binary**
