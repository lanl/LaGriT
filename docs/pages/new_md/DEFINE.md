---
GENERATOR: 'Mozilla/4.72 
[en
] (X11; U; Linux 2.2.14-5.0 i686) 
[Netscape
]'
Generator: Microsoft Word 98
title: DEFINE
---

  

 **DEFINE**

  Allows a number to be associated with a character string, such that
  the character string can be used in input decks in place of the 
  number.

  **FORMAT:**

  **define**/name/value

 EXAMPLE:

  **define**/nx/3/

  **define**/ny/4

  **define**/nz/5

  **define**/bottom/0.1/

  **define**/top/4.6

  **define**/left/9.8

  **surface**/s1/reflect/box/0.0,left,bottom/1.0,right,top

  **rz**/xyz/nx/nz/0.0,left,bottom/1.0,right,top/1,1,1
