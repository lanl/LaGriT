---
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
Generator: Microsoft Word 98
title: ASSIGN
---

 

 **ASSIGN**

  Assign a value to a global variable.  The set of global variables
  includes; incycle, time, monitor, hextotet\_remove\_volume,
  hextotet\_check\_imt, hextotet\_radavg,
  hextotet\_remove\_duplicates.  The default values of these variables
  are; 0,  0, no, yes, no, no. use **cmo/setatt** to assign values to
  mesh object **attributes.**

 **FORMAT:**

  **assign**/category\_name/column/variable\_name/value.

 **EXAMPLES:**

  **assign****/time**/3.2

  **assign/hextotet\_remove\_duplicates**/yes
