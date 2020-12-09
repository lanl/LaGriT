      subroutine dot_product_6_planes(vector, dot)

C #####################################################################
C
C     PURPOSE -
C
C          Take the dot product of the input vector with 6 internaly
C          defined unit vectors and return the 6 results.
C
C          We wish to determine which of 6 quadrents a vector lies in.
C          To do this we first take the dot product of the vector with
C          the unit normal to 6 planes defined by the unit vector defined
C          below.
C                             sqrt2d2, sqrt2d2, 0.0d0,
C                            nsqrt2d2, sqrt2d2, 0.0d0,
C                             sqrt2d2, 0.0d0  , sqrt2d2,
C                            nsqrt2d2, 0.0d0  , sqrt2d2,
C                             0.0d0  , sqrt2d2, sqrt2d2,
C                             0.0d0  ,nsqrt2d2, sqrt2d2
C
C     INPUT ARGUMENTS 
C
C         vector -    a 3 component vector
C
C     OUTPUT ARGUMENTS -
C
C         dot   - a 6 component array with the dot product of
C                 vector with 6 planes.
C
C     AUTHOR
C
C         Carl W. Gable  gable@lanl.gov (Los Alamos National Laboratory)
C
C     CHANGE HISTORY -
C
C        $Log: dot_product_6_planes.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:43:28 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:35:52 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      real*8 dot(6)
      real*8 vector(3)
      real*8 fnormal(3,6)
      integer i, j
C
c
c     Define unit vectors to 6 planes that split a region into 6 sectors
c
      real*8 sqrt2, sqrt2d2, nsqrt2d2
      parameter ( sqrt2   = 0.141421356237310E+01, 
     1            sqrt2d2 = 0.707106781186548E+00, 
     2           nsqrt2d2 =-0.707106781186548E+00)
c
c     define the unit normal to each plane
*
      data ((fnormal(i,j),i=1,3),j=1,6)  / 
     *                             sqrt2d2, sqrt2d2, 0.0d0,
     *                            nsqrt2d2, sqrt2d2, 0.0d0,
     *                             sqrt2d2, 0.0d0  , sqrt2d2,
     *                            nsqrt2d2, 0.0d0  , sqrt2d2,
     *                             0.0d0  , sqrt2d2, sqrt2d2,
     *                             0.0d0  ,nsqrt2d2, sqrt2d2
     *                          /
c
c     Take the dot product of each of the unit normals with the input vector.
c
      dot(1) = fnormal(1,1)*vector(1) +
     *         fnormal(2,1)*vector(2) +
     *         fnormal(3,1)*vector(3)
      dot(2) = fnormal(1,2)*vector(1) +
     *         fnormal(2,2)*vector(2) +
     *         fnormal(3,2)*vector(3)
      dot(3) = fnormal(1,3)*vector(1) +
     *         fnormal(2,3)*vector(2) +
     *         fnormal(3,3)*vector(3)
      dot(4) = fnormal(1,4)*vector(1) +
     *         fnormal(2,4)*vector(2) +
     *         fnormal(3,4)*vector(3)
      dot(5) = fnormal(1,5)*vector(1) +
     *         fnormal(2,5)*vector(2) +
     *         fnormal(3,5)*vector(3)
      dot(6) = fnormal(1,6)*vector(1) +
     *         fnormal(2,6)*vector(2) +
     *         fnormal(3,6)*vector(3)
c  
      return
      end
