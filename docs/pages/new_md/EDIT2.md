---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: EDIT
---

**** 

 **EDIT**

  Prints an edit of various quantities based on the value of the
  option argument, the point limits, and/or a material specification.
  iopt specifies what to print as follows:
 
   no value for iopt --edit of sums, averages, and extrema of
   position coordinates (x,y,z), and of mesh object attribute fields

   **two**--gives same information as the default, but only for the
   two points specified.

   **parts**--gives a list of materials types, their names, count and
   sequence.

   **points**--lists up to 4 cell-center array values for a set of
   points. Possible array values are: xic,yic,zic,or mesh object
   attribute name

 FORMAT:

  **edit** / iopt / ifirst,ilast,istride / material\_
#\_or\_name/

  **edit**/ **angular**/ifirst,ilast,istride
  /material\_
#\_or\_name/xcen,**edit**/ **radial**
  /ifirst,ilast,istride /material\_
#\_or\_name/xcen,**edit/ points
  **/ifirst,ilast,istride
  /material\_
#\_or\_name/array1,array3,array4/

   

 EXAMPLES:

  

  **edit**/ **parts**/

  **edit**/
  **edit****/points****/pset**,**get**,some+points/

 
