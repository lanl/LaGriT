*deck dihangle_face
      subroutine dihangle_face(points1,xic1,yic1,zic1,
     *                         points2,xic2,yic2,zic2,
     *                                           angle)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the dihedral angle between two faces
C
C     INPUT ARGUMENTS -
C
C        points1                : The number of points on face 1.
C        (xic1(),yic1(),zic1()  : The coordinates of the face 1 points.
C        points3                : The number of points on face 2.
C        (xic2(),yic2(),zic2()  : The coordinates of the face 2 points.
C
C     OUTPUT ARGUMENTS -
C
C        angle : The dihedral angle between face 1 and face 2 in radians.
C
C     CHANGE HISTORY -
C
C $Log: dihangle_face.f,v $
C Revision 2.00  2007/11/05 19:45:52  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:07:46 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
C
      real*8 angle
      integer points1, points2
      real*8 xic1(1000000), yic1(1000000), zic1(1000000)
      real*8 xic2(1000000), yic2(1000000), zic2(1000000)
C  coefficients of the normals to the faces
      real*8 a1,b1,c1,a2,b2,c2
C
      real*8 dot,norm1,norm2
      integer i
C
C ######################################################################
C
C
C  Compute the normals to the faces
C  Use the more robust method of projecting to each axis and using
C  all the points on the face to generate normal
      a1=0
      b1=0
      c1=0
      a2=0
      b2=0
      c2=0
      do i=1,points1-1
         a1 = a1 + (zic1(i) + zic1(i+1)) * (yic1(i) - yic1(i+1))
         b1 = b1 + (xic1(i) + xic1(i+1)) * (zic1(i) - zic1(i+1))
         c1 = c1 + (yic1(i) + yic1(i+1)) * (xic1(i) - xic1(i+1))
      enddo
      a1 = a1 + (zic1(points1) + zic1(1)) * (yic1(points1) - yic1(1))
      b1 = b1 + (xic1(points1) + xic1(1)) * (zic1(points1) - zic1(1))
      c1 = c1 + (yic1(points1) + yic1(1)) * (xic1(points1) - xic1(1))
 
      norm1 = sqrt(a1*a1 + b1*b1 + c1*c1)
      a1 = a1 / norm1
      b1 = b1 / norm1
      c1 = c1 / norm1
 
      do i=1,points2-1
         a2 = a2 + (zic2(i) + zic2(i+1)) * (yic2(i) - yic2(i+1))
         b2 = b2 + (xic2(i) + xic2(i+1)) * (zic2(i) - zic2(i+1))
         c2 = c2 + (yic2(i) + yic2(i+1)) * (xic2(i) - xic2(i+1))
      enddo
      a2 = a2 + (zic2(points2) + zic2(1)) * (yic2(points2) - yic2(1))
      b2 = b2 + (xic2(points2) + xic2(1)) * (zic2(points2) - zic2(1))
      c2 = c2 + (yic2(points2) + yic2(1)) * (xic2(points2) - xic2(1))
 
      norm2 = sqrt(a2*a2 + b2*b2 + c2*c2)
      a2 = a2 / norm2
      b2 = b2 / norm2
      c2 = c2 / norm2
 
      dot = a1*a2 + b1*b2 + c1*c2
C  Negate because want angle between faces, not normals.
      angle = acos(-dot)
 
      return
      end
