---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---




 **II. Faces:** Each face is tested separately for refinement or
 derefinement. For the tetrahedral face defined by nodes 1, 2, and 3,
 we have

  

  <img height="300" width="300" src="image1.jpg">"154" "111"

   where A is the area of the face.

      Criteria:

 1
) Junction: Refine if any of the faces field values straddle c.

           Tag for refinement if

       f(1) &gt; c and f(2), f(3) &lt; c

  or

        f(2) &gt; c and f(1), f(3) &lt; c
 
  or

        f(3) &gt; c and f(1), f(2) &lt; c
 
  or

  all of the above with &gt; and &lt; interchanged.

  example: For c = 0, refine if f changes sign between any of the
  three nodes.

  2) Constant: Refine if any of the face field values exceed c.

       Tag for refinement if

  

       f(1) &gt; 2 or f(2) &gt; c or f(3) &gt; c.

  

 3)Maxsize:  Refine if the face area exceeds c.

     Tag if A &gt; c.

 4
) Aspect Ratio: Refine if the faces aspect ratio is less than c.

  The aspect ratio (AR) is defined as the ratio of the radius of the
  inscribed circle of the  triangular face to the radius of the
  circumscribed circle. We renormalize this ratio of multiplying by 2
  so that the ratio equals one for an equilateral triangle.

   
 
   AR = 2 RIN/ROUT  where
  
     RIN = radius of inscribed circle

     ROUT = radius of circumscribed circle

     AR is never greater than one.
 
  Tag if AR &lt; c.  Generally the smaller AR is the more elongated
  the face is.

 5
) Lambda Refine: Refine if lamda/dx  &lt;c.  Where dx  is taken to be
 the radius of the circumscribed circle  RIN of the triangular face

  

     lambda = f(xcen)/grad f

     f(xcen) = (f(1) + f(2) +f(3))/3 where

      

 xcen is the centroid of  the triangular face,  and we have assumed a
 linear interpolation of f. grad f is evaluated on the face by a
 suitable approximation involving a linear interpolation of f and  the
 line integral around the edge of the face.
